(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** GitHub Copilot CLI agentic backend.

    This module implements the [AGENTIC_BACKEND.S] interface for the
    GitHub Copilot CLI tool at the investigated 1.0.54 baseline. Runtime task
    execution is quarantined because this version cannot disable every MCP
    discovery source before process start.

    {b Configuration:}
    Copilot CLI is expected to be installed and accessible in the PATH.
    Authenticated 1.0.54 attachment behavior is a historical,
    investigation-only observation; it supplies no positive media evidence or
    capability claim because complete MCP discovery isolation is unproven.
    Copilot is excluded from executable media E2E selection. The dormant
    candidate builds the prompt as one argv value and would supply each centrally
    prepared PNG/JPEG once with a repeated [--attachment] flag; its private
    staging directory would be explicitly added without a blanket path grant.
    The candidate invocation would pin
    [--prefer-version 1.0.54], disable remote/update/experimental behavior, and
    limit visible tools to [view], [grep], and [glob]. Copilot requires
    [--allow-all-tools] in prompt mode; that candidate approval is bounded by
    the visible list and explicit shell/write/memory/URL denials. User
    configuration and logs are routed through a fresh private [COPILOT_HOME]
    removed after the attempt. Cleanup failure emits only fixed telemetry.
    These mechanics remain tested for a future upstream version but are not
    reached at 1.0.54.

    {b MCP Integration:}
    Copilot 1.0.54 can discover MCP servers from user configuration, workspace
    [.mcp.json] files, installed plugins, built-ins, and account-controlled ODR.
    [--disable-builtin-mcps] covers only built-ins and a private [COPILOT_HOME]
    isolates only local user/plugin state. No flag disables every source. Cabal
    therefore generates no MCP artifact. Hardened central dispatch rejects every
    task immediately after validated registry lookup, before input/capability
    preflight, project setup, or backend spawn; the direct adapter rejection is
    defense in depth. The protocol validator independently rejects any nonempty
    MCP-loaded event.

    {b LSP Integration:}
    Copilot supports project LSP server configuration via [.github/lsp.json].
    Épure renders host-provided LSP servers into that strict JSON artifact.

    {b Output Format:}
    Authenticated [--output-format json --stream off] JSONL was observed, and the
    dormant parser recursively rejects duplicate object keys in every parsed
    value before semantic parsing, including opaque tool arguments, attachment
    objects, and skills. It then validates exact envelope, payload, usage,
    session, and tool fields before reconstructing public data. Tool requests
    require an object [arguments] field that is structurally checked and then
    discarded. Callback JSON is rebuilt only from validated assistant text,
    session identity, and output-token usage; unknown/error/truncated/mixed/
    post-terminal streams fail closed. Raw stdout and stderr are never projected.
    Because central execution is quarantined, [structured_output] remains false
    until this transport is usable.

    Media has no positive evidence. Positive web access, read-only requests,
    resume/reuse, and MCP are currently unsupported. Native JSON Schema stays
    false. *)

(** @inline *)
include Agentic_backend.S

(** {1 Additional Utilities} *)

(** [project_config_artifacts ~mcp_servers ~lsp_servers] returns the
    Copilot project config artifacts owned by this backend provider.

    {pre}
    (none)

    {post}
    Returns [.github/copilot-instructions.md], strict JSON
    [.github/copilot/settings.json], and strict JSON [.github/lsp.json]. The
    [mcp_servers] argument is retained for provider compatibility but never
    rendered because this backend advertises [Mcp_none].

    {violators}
    (none)

    {violates}
    (none) *)
val project_config_artifacts :
  managed_namespace:Backend_types.managed_namespace ->
  mcp_servers:Backend_types.mcp_server_config list ->
  lsp_servers:Backend_types.lsp_server_config list ->
  Backend_config_writer.artifact list

(** Test and compatibility seams that are not part of the backend abstraction.

    {b Migration note:} the dormant/quarantined command and parser seams moved
    out of the production-level module surface. Replace
    [Copilot_cli.build_command] with [Copilot_cli.Private.build_command], and
    replace [Copilot_cli.parse_stdout_text] with
    [Copilot_cli.Private.parse_stdout_text]. No deprecated forwarding aliases are
    retained because that would continue presenting the quarantined seams as
    production APIs. *)
module Private : sig
  (** A fail-closed non-interactive invocation. [redacted_argv] contains no
      prompt, model, configuration path, or attachment path. *)
  type backend_invocation = {
    argv : string list;
    stdin : string option;
    redacted_argv : string list;
  }

  val build_invocation :
    ?attachment_paths:string list ->
    ?attachment_delivery:Backend_types.attachment_delivery ->
    config_home:string ->
    mcp_config_path:string option ->
    Backend_types.task_spec ->
    (backend_invocation, string) result

  val build_command :
    mcp_config_path:string option ->
    Backend_types.task_spec ->
    string list * string

  type verified_terminal = {
    text : string;
    session_id : string option;
    cost : Backend_types.cost option;
  }

  (** Validate the complete pinned JSONL protocol and return only sanitized
      terminal data. *)
  val verify_terminal_stdout : string -> (verified_terminal, string) result

  (** Return only synthesized host-neutral events after whole-stream
      validation. *)
  val normalized_events_of_stdout :
    string -> (Task_event.payload list, string) result

  (** Return only whole-stream-validated callback JSON reconstructed from
      public assistant text, session identity, and output-token usage. Raw
      envelopes and tool arguments are never returned. *)
  val reconstructed_callback_lines_of_stdout :
    string -> (string list, string) result

  val parse_stdout_text : string -> string
  val cleanup_retry_limit : int

  (** Deterministic isolated-home cleanup seam. Production uses the same
      descriptor/no-follow removal and bounded retry implementation. *)
  val with_isolated_config_home_for_test :
    ?on_cleanup_attempt:(unit -> unit) ->
    (string -> Backend_types.task_result) ->
    Backend_types.task_result
end
