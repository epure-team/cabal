(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** GitHub Copilot CLI agentic backend.

    This module implements the [AGENTIC_BACKEND.S] interface for the
    GitHub Copilot CLI tool at the enforced 1.0.54 media baseline.

    {b Configuration:}
    Copilot CLI is expected to be installed and accessible in the PATH.
    The prompt is one argv value. Each centrally prepared PNG/JPEG is supplied
    once with a repeated [--attachment] flag and its private staging directory
    is explicitly added without a blanket path grant. The invocation pins
    [--prefer-version 1.0.54], disables remote/update/experimental behavior,
    and limits visible tools to [view], [grep], and [glob]. Copilot requires
    [--allow-all-tools] in prompt mode; that approval is bounded by the visible
    list and explicit shell/write/memory/URL denials. User configuration and
    logs are routed through a fresh private [COPILOT_HOME] removed after the
    attempt. Cleanup failure emits only fixed telemetry.

    {b MCP Integration:}
    The artifact generator retains [.github/mcp.json] compatibility, but the
    hardened prompt transport excludes MCP tools. A task requesting MCP servers
    is rejected before config mutation or process execution.

    {b LSP Integration:}
    Copilot supports project LSP server configuration via [.github/lsp.json].
    Épure renders host-provided LSP servers into that strict JSON artifact.

    {b Output Format:}
    Copilot runs with [--output-format json --stream off]. The complete public
    JSONL stream is validated before assistant text, UUID session identity,
    output-token usage, tool lifecycle events, or verified raw callbacks are
    released. Unknown/error/truncated/mixed/post-terminal streams fail closed.
    Raw stdout and stderr are discarded from returned results because public
    records can repeat prompts, attachment paths, and tool arguments.

    Positive web access, read-only requests, resume/reuse, and MCP are currently
    unsupported. Native JSON Schema stays false; schema enforcement therefore
    uses the shared bounded fresh-retry path. *)

(** @inline *)
include Agentic_backend.S

(** {1 Additional Utilities} *)

(** [project_config_artifacts ~mcp_servers ~lsp_servers] returns the
    Copilot project config artifacts owned by this backend provider.

    {pre}
    (none)

    {post}
    Returns [.github/copilot-instructions.md], strict JSON
    [.github/copilot/settings.json], strict JSON [.github/lsp.json], and strict
    JSON [.github/mcp.json].

    {violators}
    (none)

    {violates}
    (none) *)
val project_config_artifacts :
  managed_namespace:Backend_types.managed_namespace ->
  mcp_servers:Backend_types.mcp_server_config list ->
  lsp_servers:Backend_types.lsp_server_config list ->
  Backend_config_writer.artifact list

(** A fail-closed non-interactive Copilot invocation. [redacted_argv] has the
    same shape as [argv] but contains no prompt, model, configuration path, or
    attachment path. *)
type backend_invocation = {
  argv : string list;
  stdin : string option;
  redacted_argv : string list;
}

(** [build_invocation ~config_home ~mcp_config_path spec] builds a hardened
    prompt-mode invocation. [attachment_paths] must be the absolute paths of
    the ordered, sealed files authorized for [spec.attachments]. Unsupported
    media, web, read-only, resume, reuse, or MCP requests return a sanitized
    error before process execution. *)
val build_invocation :
  ?attachment_paths:string list ->
  ?attachment_delivery:Backend_types.attachment_delivery ->
  config_home:string ->
  mcp_config_path:string option ->
  Backend_types.task_spec ->
  (backend_invocation, string) result

(** Compatibility wrapper around {!build_invocation}. It raises
    [Invalid_argument] for an unsupported request. New callers should use
    {!build_invocation} so rejection remains explicit. *)
val build_command :
  mcp_config_path:string option ->
  Backend_types.task_spec ->
  string list * string

(** Validated terminal projection of Copilot's public JSONL protocol. *)
type verified_terminal = {
  text : string;
  session_id : string option;
  cost : Backend_types.cost option;
}

(** [verify_terminal_stdout stdout] validates the complete public Copilot JSONL
    stream, including canonical session identity, bounded timestamp shapes,
    paired tool lifecycles, and exactly one final [result] record. Unknown record
    types, malformed required fields, non-zero terminal results, and
    post-terminal data fail with a sanitized diagnostic. *)
val verify_terminal_stdout : string -> (verified_terminal, string) result

(** [normalized_events_of_stdout stdout] returns host-neutral assistant text,
    session, tool-lifecycle, and token-usage events only after the complete
    stream passes {!verify_terminal_stdout}. Raw prompts, tool arguments, and
    attachment paths are never included. *)
val normalized_events_of_stdout :
  string -> (Task_event.payload list, string) result

(** [parse_stdout_text stdout] returns the final public assistant text only when
    the complete JSONL stream passes {!verify_terminal_stdout}; malformed,
    incomplete, plain-text, and error output returns the empty string. *)
val parse_stdout_text : string -> string
