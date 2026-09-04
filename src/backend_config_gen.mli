(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Project-owned backend configuration generation.

    Generates backend-native project configuration files for each supported
    backend.  The generated artifacts are attributable to the host application
    via the active [managed_namespace], idempotent, and protected against
    silent overwrites of user-authored content.

    Two ownership models are supported:
    - {b Host-owned} (constructor [Epure_owned], retained for source
      compatibility): the file lives under the managed namespace's
      [config_dir] (default [.cabal/backend-config/]) and is passed
      explicitly to the backend at invocation via a CLI flag or env var.
      Cabal freely regenerates these files on the host's behalf.
    - {b Backend-project}: the backend requires a fixed project-relative path
      (e.g. [.codex/config.toml], [opencode.json], [.gemini/settings.json]).
      Cabal writes there only when the file is absent or carries the
      namespace's managed metadata, and refuses to overwrite user-authored
      content silently. *)

(** {1 Types} *)

(** Ownership classification of a generated config artifact. *)
type ownership = Backend_config_writer.ownership =
  | Epure_owned
      (** Host-owned artifact (constructor name preserved for source
          compatibility).  File lives under the managed namespace's
          [config_dir] (default [.cabal/backend-config/]); passed explicitly
          to the backend at invocation.  Cabal freely regenerates these
          files. *)
  | Backend_project
      (** File lives at the backend's required fixed project path.  Cabal
          writes only to managed files (those bearing the namespace marker
          and content hash).  Unmarked user files are skipped. *)

(** A generated backend configuration artifact. *)
type artifact = Backend_config_writer.artifact = {
  backend_id : string;  (** Backend identifier (e.g. ["codex"]). *)
  ownership : ownership;
      (** Whether this file is host-owned or at a fixed backend path. *)
  managed_namespace : Backend_types.managed_namespace;
      (** Namespace controlling managed markers and generated artifact suffixes. *)
  project_relative_path : string;
      (** Path relative to the project root where the file should be written. *)
  content : string;
      (** Full file content.  Strict JSON artifacts may use sidecar metadata
          rather than inline managed headers. *)
}

(** Result of writing an artifact to disk. *)
type write_result = Backend_config_writer.write_result =
  | Written of string  (** Path was written successfully. *)
  | Already_current  (** File already contains the expected content. *)
  | Refused_hash_mismatch of string
      (** File was modified by a user (hash mismatch); use [--force] to
          backup and overwrite.  The string contains an actionable message. *)
  | Backed_up_and_written of {path : string; backup_path : string}
      (** [--force] mode: original file backed up, new content written. *)
  | Skipped_user_content of string
      (** A user-authored file (no namespace marker) exists at the path;
          it was not touched.  The string is the full path. *)
  | Unsafe_project_path of string
      (** A path was malformed, unreadable, non-regular, or contained a
          symlink component, so no write was attempted. *)
  | Invalid_managed_namespace of string
      (** Artifact write refused before touching the filesystem because the
          managed namespace is unsafe.  The string contains a validation error. *)

(** Per-artifact write result reported by setup. *)
type artifact_write_outcome = Backend_config_writer.artifact_write_outcome = {
  artifact : artifact;  (** Artifact whose write was attempted. *)
  project_path : string;  (** Absolute path under the project root. *)
  result : write_result;  (** Result returned by [write_artifact]. *)
}

(** {1 Content inspection} *)

(** [is_managed_content content] returns [true] when [content] contains the
    active namespace's managed marker, indicating this file was generated
    by Cabal on behalf of the host application.

    {pre}
    (none)

    {post}
    Returns [true] if and only if the namespace's [<id>-managed] marker (or
    the legacy [epure-managed] marker, for migration of existing files)
    appears as a substring of [content].

    {violators}
    (none)

    {violates}
    (none) *)
val is_managed_content :
  ?managed_namespace:Backend_types.managed_namespace -> string -> bool

(** [extract_hash content] extracts the hex MD5 hash embedded in [content]'s
    managed header.  Returns [None] when no hash is present (e.g. host-owned
    files, or non-managed content).

    {pre}
    (none)

    {post}
    Returns [Some hex] where [hex] is a 32-character lowercase hex MD5 string,
    or [None] when no [<id>-hash] marker (current or legacy [epure-hash]) is
    found in [content].

    {violators}
    (none)

    {violates}
    (none) *)
val extract_hash :
  ?managed_namespace:Backend_types.managed_namespace -> string -> string option

(** [managed_body_hash ~backend_id content] computes the hash Cabal stores in
    a managed backend-project config header for [content].

    {pre}
    (none)

    {post}
    Returns the 32-character lowercase MD5 hash after applying the same
    backend-specific body normalization used by [write_artifact].

    {violators}
    (none)

    {violates}
    (none) *)
val managed_body_hash :
  ?managed_namespace:Backend_types.managed_namespace ->
  backend_id:string ->
  string ->
  string

(** {1 Generation} *)

(** [generate ~backend_id] generates a project configuration artifact for the
    backend identified by [backend_id].  Returns [None] when the backend has no
    project config surface ([Config_none]) or is unknown.

    {pre}
    (none)

    {post}
    Returns [Some artifact] for backends with a non-[Config_none] project
    config surface: [claude-code] ([Epure_owned]), [codex] ([Backend_project]
    at [.codex/config.toml]), [opencode] ([Backend_project] at
    [opencode.json]), [gemini-cli] ([Backend_project] at
    [.gemini/settings.json]), and [copilot-cli] ([Backend_project] at
    [.github/copilot-instructions.md]).  Note: [Epure_owned] retains its
    constructor name for source compatibility but denotes a host-owned
    artifact under the active managed namespace's [config_dir].  Returns
    [None] for unknown backends
    and any backend whose [project_config_surface] is [Config_none].  Calling
    [generate] twice with the same [backend_id] always produces identical
    [artifact.content] values (idempotent).

    {violators}
    (none)

    {violates}
    (none) *)
val generate : backend_id:string -> artifact option

(** [generate_all ~backend_id ~mcp_servers] generates every project
    configuration artifact required by [backend_id].  Most backends produce a
    single artifact; Gemini CLI produces [.gemini/settings.json], and Copilot
    CLI additionally produces [.github/copilot/settings.json] and
    [.github/lsp.json], but deliberately no project MCP artifact. Strict JSON
    artifacts carry no inline managed
    metadata; write-time ownership is tracked by a sidecar file.

    {pre}
    (none)

    {post}
    Returns artifacts in write order, with the primary artifact first.  Returns
    [[]] for unknown backends or backends without a project config surface.
    Supplying [mcp_servers] serializes them into backend-native strict JSON
    artifacts where the backend supports MCP; Copilot ignores them because it
    advertises [Mcp_none]. Pass [[]] to produce empty MCP server maps where
    applicable.

    {violators}
    (none)

    {violates}
    (none) *)
val generate_all :
  mcp_servers:Backend_types.mcp_server_config list ->
  backend_id:string ->
  artifact list

(** [generate_all_with_options ?managed_namespace ?lsp_servers ~backend_id
    ~mcp_servers ()] is the configurable variant of [generate_all] used by host
    call sites that need a non-default managed namespace or host-provided LSP
    definitions.

    {pre}
    (none)

    {post}
    Returns the same artifact set as [generate_all], rendered under
    [managed_namespace] and with [lsp_servers] included only when [backend_id]
    declares generated LSP support.

    {violators}
    (none)

    {violates}
    (none) *)
val generate_all_with_options :
  ?managed_namespace:Backend_types.managed_namespace ->
  ?lsp_servers:Backend_types.lsp_server_config list ->
  mcp_servers:Backend_types.mcp_server_config list ->
  backend_id:string ->
  unit ->
  artifact list

(** {1 Writing} *)

(** [write_artifact ~project_dir ~force artifact] writes [artifact] to disk
    under [project_dir].

    For [Epure_owned] artifacts: always writes the file, creating parent
    directories as needed.  Returns [Already_current] when the file already
    contains [artifact.content].

    For [Backend_project] artifacts:
    - File absent → writes and returns [Written path].
    - File present, no managed marker → returns [Skipped_user_content path].
    - File present, managed marker, hash matches current body → content
      unchanged → [Already_current]; or overwrite with new template and
      return [Written path] if the template changed.
    - File present, managed marker, hash mismatch → user modified the file.
      With [~force:false] returns [Refused_hash_mismatch msg].
      With [~force:true] backs up to [path ^ ".<id>-backup"] (where [<id>]
      is the active namespace id) and writes, returning
      [Backed_up_and_written].

    Strict JSON backend-project artifacts cannot carry inline comments or
    schema-invalid metadata.  These are governed by the same policy, but the
    managed marker and hash live in a sidecar file next to the artifact.

    {pre}
    [project_dir] must be an existing directory.

    {post}
    On [Written] or [Backed_up_and_written]: the file at [path] contains
    [artifact.content].  On [Already_current]: the file is unchanged.
    On [Skipped_user_content] or [Refused_hash_mismatch]: no file is written
    or modified.

    {violators}
    (none)

    {violates}
    (none) *)
val write_artifact :
  project_dir:string -> force:bool -> artifact -> write_result

(** [write_result_was_applied result] returns [true] for outcomes that mean the
    generated artifact content is present after setup. *)
val write_result_was_applied : write_result -> bool

(** {1 Precedence warnings} *)

(** [precedence_warning_for ~backend_id ~write_outcome] returns
    [Some warning_text] when the backend's project config precedence over
    user-global config cannot be fully guaranteed, and [None] when strict
    precedence is ensured (High confidence, e.g. Claude Code with
    [--settings]).

    [write_outcome] is the primary artifact result of the preceding
    [setup_project_config] call (or [None] when no config write was attempted).
    The warning message is conditioned on whether the primary config was
    actually applied: a refused or skipped write produces a different
    message than a successful write, so callers never see a warning that
    falsely asserts "the config was written" when it was not.

    AC2 (story #479): Claude Code returns [None]; Codex and OpenCode return
    [Some] (Medium — partial override possible via user-global settings).
    AC3 (story #479): Gemini CLI and Copilot CLI return [Some] (Low — limited
    or absent project config surface; user settings may silently win).
    AC4 (story #479): callers must emit this warning non-fatally before
    invoking the backend so operators know about the limitation.

    Returns [None] for unknown backends.

    {pre}
    (none)

    {post}
    Returns [None] exactly when the backend has [High] precedence confidence
    or is unknown.  Returns [Some msg] for [Medium] or [Low] confidence.
    [msg] always includes the backend display name for actionability and
    accurately reflects whether the project config was applied.

    {violators}
    (none)

    {violates}
    (none) *)
val precedence_warning_for :
  backend_id:string -> write_outcome:write_result option -> string option

(** {1 Invocation helpers} *)

(** Result of a [setup_project_config] call. *)
type setup_result = Backend_config_writer.setup_result = {
  project_config_path : string option;
      (** Absolute path for [Epure_owned] artifacts that callers must pass
          explicitly to the backend (e.g. via [--settings] or [--config]).
          [None] for [Backend_project] artifacts (fixed-path discovery),
          [Config_none] backends, and failed or skipped writes. *)
  write_outcome : write_result option;
      (** The raw result of the write attempt.  [None] when no artifact was
          generated (unknown backend or [Config_none]).  Pass this to
          [precedence_warning_for] so the warning message accurately reflects
          whether the project config was applied. *)
  write_outcomes : artifact_write_outcome list;
      (** Per-artifact outcomes in write order.  Includes secondary strict JSON
          artifacts such as [.github/copilot/settings.json] and
          [.github/lsp.json], so runtime adapters can detect when configuration
          was skipped or refused even if the primary instruction file was
          written. *)
}

(** [setup_project_config ~mcp_servers ~backend_id ~project_dir ~force]
    generates the project configuration artifacts for [backend_id] and writes
    them to [project_dir] using the default managed namespace and no generated
    LSP definitions.  The [mcp_servers] list is serialized into backend-native
    project artifacts; pass [[]] when no MCP servers should be activated.  Use
    [setup_project_config_with_options] when the host has LSP definitions or a
    custom managed namespace.

    Returns a [setup_result] carrying the config path (for [Epure_owned]
    artifacts), the primary write outcome (for accurate precedence warnings),
    and per-artifact outcomes for secondary config files.

    Idempotent: calling twice on the same repository produces the same result.

    {pre}
    [project_dir] must be an existing directory.

    {post}
    For [Epure_owned] artifacts: [project_config_path = Some path] and
    [write_outcome = Some (Written _ | Already_current | ...)] on success.
    For [Backend_project] artifacts: [project_config_path = None] and
    [write_outcome = Some result].
    For [Config_none] backends: [project_config_path = None] and
    [write_outcome = None].  [write_outcomes] contains one entry per generated
    artifact and is empty when no artifact is generated.
    The compatibility wrapper never embeds generated LSP definitions.

    {violators}
    (none)

    {violates}
    (none) *)
val setup_project_config :
  mcp_servers:Backend_types.mcp_server_config list ->
  backend_id:string ->
  project_dir:string ->
  force:bool ->
  setup_result

(** [setup_project_config_with_options ?managed_namespace ?lsp_servers
    ~mcp_servers ~backend_id ~project_dir ~force ()] is the configurable
    variant of [setup_project_config] for host-owned LSP discovery and custom
    managed namespaces.

    {pre}
    [project_dir] must be an existing directory.

    {post}
    Writes the generated artifact set under [project_dir].  When [lsp_servers]
    is non-empty and [backend_id] declares generated LSP support, written
    config contains backend-native LSP definitions; otherwise LSP definitions
    are omitted.

    {violators}
    (none)

    {violates}
    (none) *)
val setup_project_config_with_options :
  ?managed_namespace:Backend_types.managed_namespace ->
  ?lsp_servers:Backend_types.lsp_server_config list ->
  mcp_servers:Backend_types.mcp_server_config list ->
  backend_id:string ->
  project_dir:string ->
  force:bool ->
  unit ->
  setup_result

(** {1 LSP-aware generation} *)

(** [generate_with_lsp_defs ~backend_id ~lsp_servers] generates a
    project configuration artifact for [backend_id], embedding LSP server
    definitions only when [Backend_registry.supports_generated_lsp_config
    backend_id] is [true].

    For Claude Code and OpenCode the returned primary [artifact.content]
    contains an [lsp] section listing each host-provided LSP server command and
    args.  Copilot CLI writes LSP definitions to a secondary
    [.github/lsp.json] artifact returned by [generate_all]/provider-specific
    generation.  When [lsp_servers] is empty the output is identical to
    [generate ~backend_id] (no lsp section).

    For backends without generated-LSP support the function passes an empty
    LSP list to the provider.  The artifact is still returned when the
    backend has a project config surface, but no LSP definitions are embedded.

    Returns [None] when the backend has no project config surface or is unknown.

    {pre}
    (none)

    {post}
    Returns [Some artifact] for backends with a non-[Config_none] project
    config surface.  When [lsp_servers] is non-empty and the backend
    declares generated LSP support, [artifact.content] contains lsp entries for
    every supplied LSP server.
    Idempotent: same inputs always produce identical [artifact.content].

    {violators}
    (none)

    {violates}
    (none) *)
val generate_with_lsp_defs :
  ?managed_namespace:Backend_types.managed_namespace ->
  mcp_servers:Backend_types.mcp_server_config list ->
  backend_id:string ->
  lsp_servers:Backend_types.lsp_server_config list ->
  unit ->
  artifact option
