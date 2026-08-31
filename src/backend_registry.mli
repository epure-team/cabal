(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Central backend capability and baseline registry — Story #476.

    This module is the single source of truth for built-in backend metadata:
    stable baseline versions, capability flags, MCP support modes, project
    config surfaces, and file-reading/tool classification.

    Static facts (what Épure supports at the stable baseline) live here.
    Runtime detection (installed version, available binary) is separate and
    never mutates this data. Central dispatch does not independently consult this
    catalog: it uses the effective descriptor bound into a validated
    {!Registry.entry} snapshot.

    Note that [Adapter_loader.register_all ()] remains the extensible YAML-only
    loader. Hosts and tests that need strict built-in runtime semantics should
    use [Runtime_bootstrap.Hardened_builtins], which stages approved embedded
    adapters and installs the five handwritten replacements after validation.

    {b Built-in backends (6):} claude-code, codex, opencode, pi, gemini-cli,
    and copilot-cli. *)

(** {1 Types} *)

(** How a backend receives MCP server configuration. *)
type mcp_support =
  | Mcp_none  (** Backend ignores [task_spec.mcp_servers]; MCP not supported. *)
  | Mcp_config_file
      (** Backend reads MCP config from a file passed explicitly at invocation
          (e.g. [claude --mcp-config <path>]) or from a project config file
          written by Épure (e.g. [opencode.json] or [.github/mcp.json]). *)
  | Mcp_config_flag
      (** Backend receives MCP config via a dedicated CLI flag. *)
  | Mcp_user_settings
      (** Backend reads MCP config from settings files rather than a
          per-invocation path (e.g. Gemini user or workspace
          [settings.json]); not programmatically injectable per invocation. *)

(** How Épure can supply project-level configuration to a backend. *)
type project_config_surface =
  | Config_none
      (** No project-level config surface; backend uses global user config. *)
  | Config_explicit_flag
      (** Config file is passed via an explicit CLI flag at invocation. *)
  | Config_fixed_path
      (** Backend reads a fixed project-relative path (e.g. [.codex/config.toml],
          [.gemini/settings.json]). Épure must write to that path with
          managed-file markers or sidecar metadata. *)
  | Config_env_var
      (** Configuration is passed via environment variables only. *)

(** Confidence that Épure-managed project config takes precedence over global
    user config for this backend. *)
type precedence_confidence =
  | High
      (** Épure has explicit injection mechanisms (flags, explicit config
          paths); project config reliably overrides user config. *)
  | Medium
      (** Partial control; some settings may fall through to user config. *)
  | Low
      (** Limited or no reliable precedence; user config may shadow
          project config silently. *)

(** Evidence-backed media encodings supported by a backend transport.

    [media_types] must be empty when [evidence = None]. Positive claims require
    versioned {!Backend_types.feature_evidence}. *)
type media_support = {
  media_types : Backend_types.media_type list;  (** Supported encodings. *)
  evidence : Backend_types.feature_evidence option;
      (** Reproducible evidence for every positive media claim. *)
}

(** Evidence-backed maximum web access supported by a backend transport.

    [maximum] is hierarchical: [Web_disabled] < [Web_search] <
    [Web_search_and_fetch]. A maximum other than [Web_disabled] requires
    [evidence = Some _]. *)
type web_support = {
  maximum : Backend_types.web_access;  (** Highest supported web policy. *)
  evidence : Backend_types.feature_evidence option;
      (** Reproducible evidence for a positive web claim. *)
}

(** Static capability flags for a backend at its stable baseline version.

    These are build-time facts about what Épure supports for each backend,
    not runtime checks. *)
type capabilities = {
  structured_output : bool;
      (** True when the backend emits machine-readable structured output
          (JSON or JSONL) that Épure can parse for cost, session IDs, etc. *)
  streaming_output : bool;
      (** True when the backend can emit output incrementally (stream-json or
          NDJSON), enabling real-time display. *)
  session_resume : bool;
      (** True when the backend supports resuming a prior CLI session via
          [task_spec.resume_session_id]. *)
  mcp_support : mcp_support;
      (** How MCP servers are supplied to this backend. *)
  read_only_support : bool;
      (** True when the backend has a native mechanism to restrict the agent
          to read-only operations (no shell, no file writes). *)
  project_config_surface : project_config_surface;
      (** The mechanism by which project-level configuration reaches the backend. *)
  precedence_confidence : precedence_confidence;
      (** Confidence level that Épure project config overrides user config. *)
  generated_lsp_config : bool;
      (** True when Épure generates backend-native Language Server Protocol
          configuration for detected project languages and the backend consumes
          it during invocation.  Backends with [false] still track LSP tooling
          requirements separately, but their generated project config must not
          embed LSP server definitions. *)
  file_reading : bool;
      (** True when the backend can read arbitrary file paths supplied as
           references in the prompt (file-reading/tool classification).
           Claude Code and OpenCode currently expose this capability. *)
  media_support : media_support;
      (** Evidence-backed media types accepted by the backend transport. *)
  web_support : web_support;
      (** Evidence-backed maximum web access accepted by the backend. *)
  native_json_schema_output : bool;
      (** True when the backend supports native JSON schema enforcement via a
          CLI flag at invocation time (e.g. [--output-schema <schema-json>]).
          When [false], the enforcer uses the validate-and-retry path instead.
          Set to [true] only when [native_json_schema_output_evidence] carries
          a [Some _] record. *)
  native_json_schema_output_evidence : Backend_types.capability_evidence option;
      (** Evidence record required when [native_json_schema_output = true].
          Must be [Some _] for every backend that sets the flag to [true];
          checked by the structural CI test iterating [Backend_registry.all ()].
          Set to [None] for backends that do not support native schema output. *)
}

(** Static descriptor for a built-in backend. *)
type descriptor = {
  id : string;
      (** Canonical identifier matching the runtime backend id
          (e.g. ["claude-code"]). *)
  display_name : string;  (** Human-readable name (e.g. ["Claude Code"]). *)
  binary_name : string;
      (** Name of the binary on PATH used to invoke this backend
          (e.g. ["claude"], ["codex"], ["copilot"]).  Used for PATH
          availability checks and remediation messages.  This is the
          single source of truth for binary names — never hardcode them
          elsewhere. *)
  baseline_version : string;
      (** Stable upstream version Épure requires as minimum.
          Checked at backend startup; below-baseline → refuse to run. *)
  capabilities : capabilities;
      (** Static capability flags for this backend at baseline. *)
}

(** Evidence-backed note explaining why a built-in backend capability is marked
    unsupported ([false]) in {!capabilities}. *)
type unsupported_capability_note = {
  backend_id : string;  (** Backend identifier, e.g. ["codex"]. *)
  feature : string;
      (** Boolean capability field name, e.g. ["generated_lsp_config"]. *)
  evidence_url : string;  (** Official documentation or direct evidence URL. *)
  note : string;
      (** Human-readable summary of the documented absence or positive surface
          that justifies the unsupported capability flag. *)
}

(** {1 Registry Access} *)

(** [register_descriptor d] registers an extra descriptor for compatibility.

    Not reflected in [all ()] or [read_only_safe_backend_ids ()], which remain
    built-in-only. Affects [find] and all capability queries ([supports_*]).

    Idempotent: re-registering the same [d.id] is a no-op. Not thread-safe;
    intended for single-threaded startup or test setup. New production code
    should prefer [Runtime_bootstrap.register_custom], which validates and
    installs the descriptor and runtime backend as one additive contract. *)
val register_descriptor : descriptor -> unit

(** Descriptor insertion failure for additive registration. *)
type descriptor_registration_error = Descriptor_id_already_registered

(** [add_descriptor d] adds [d] only when its id is absent from both the
    built-in and additive descriptor sets. It never replaces metadata.

    This non-replacing primitive supports the atomic startup contract in
    [Runtime_bootstrap.register_custom]. Registry mutation is single-domain
    startup state; concurrent registration is unsupported.

    @return [Error Descriptor_id_already_registered] on any collision. *)
val add_descriptor :
  descriptor -> (unit, descriptor_registration_error) result

(** [all ()] returns descriptors for all built-in backends in canonical order.

    {pre}
    (none)

    {post}
    Returns exactly 6 descriptors in canonical order: claude-code, codex,
    opencode, pi, gemini-cli, copilot-cli.

    {violators}
    (none)

    {violates}
    (none) *)
val all : unit -> descriptor list

(** [find id] returns the descriptor for backend [id], or [None] if not found.

    {pre}
    (none)

    {post}
    Returns [Some desc] when [id] matches a built-in backend or an extra
    compatibility or host-custom descriptor. Returns [None]
    otherwise.
    Lookup is O(n) over the built-in list then O(m) over extra descriptors.

    {violators}
    (none)

    {violates}
    (none) *)
val find : string -> descriptor option

(** [unsupported_capability_notes ()] returns evidence-backed notes for every
    [false] boolean capability on built-in backend descriptors. *)
val unsupported_capability_notes : unit -> unsupported_capability_note list

(** {1 Capability Queries} *)

(** [supports_file_reading backend_id] returns [true] when the backend with
    [backend_id] can read arbitrary file paths from the prompt.

    This replaces the ad-hoc [backend_supports_file_reading] heuristic in
    [Build_flow_helpers] and routes capability decisions through the central
    registry (AC4).

    {pre}
    (none)

    {post}
    Returns [true] exactly when the backend descriptor for [backend_id] has
    [capabilities.file_reading = true]. Returns [false] for unknown backends.

    {violators}
    (none)

    {violates}
    (none) *)
val supports_file_reading : string -> bool

(** [supports_generated_lsp_config backend_id] returns [true] when the backend
    declares [generated_lsp_config = true].  Unknown backends return [false]. *)
val supports_generated_lsp_config : string -> bool

(** [supports_read_only backend_id] returns [true] when the backend declares
    [read_only_support = true], meaning it has a stable non-mutating invocation
    mode suitable for validator tasks.

    Returns [false] for unknown backends (not in the built-in registry).
    Fail-closed: a backend not explicitly declared safe must be treated as
    unsafe. This prevents a custom or unregistered backend from bypassing the
    validator read-only gate. *)
val supports_read_only : string -> bool

(** [read_only_safe_backend_ids ()] returns the IDs of all built-in backends
    that declare [read_only_support = true].

    Used by the routing layer to list alternatives when a validator is routed
    to an unsafe backend. *)
val read_only_safe_backend_ids : unit -> string list

(** [get_capability_evidence backend_id] returns the recorded
    [Backend_types.capability_evidence] for [backend_id], or [None] when
    the backend is unknown or has no native-schema evidence pinned. *)
val get_capability_evidence : string -> Backend_types.capability_evidence option

(** [capability_evidence_table ()] returns an association list of
    [(backend_id, capability_evidence)] for every built-in backend that has
    a recorded native-schema evidence record. Backends without evidence are
    omitted. *)
val capability_evidence_table :
  unit -> (string * Backend_types.capability_evidence) list
