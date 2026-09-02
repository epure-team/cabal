(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** OpenAI Codex CLI agentic backend.

    This module implements the [AGENTIC_BACKEND.S] interface for the
    OpenAI Codex CLI tool. It spawns [codex exec] in non-interactive mode
    with JSONL output and collects results.

    {b Configuration:}
    Codex CLI is expected to be installed and accessible in the PATH.
    The backend uses [codex exec --json --full-auto] for non-interactive
    sandboxed execution (or [-s read-only] for validators).

    Media/web argv construction is implemented and authenticated at the
    enforced 0.131.0 baseline. {!run_task} applies the effective descriptor gate
    before config I/O or spawn: the built-in contract accepts PNG/JPEG images
    and web access through search-and-fetch, with versioned evidence mirrored in
    the independent hardened runtime capability snapshot.

    {b MCP Integration:}
    Codex 0.131.0 supports MCP via [.codex/config.toml].  Épure generates
    this file with a template MCP section (disabled/comment-only by default)
    and serializes approved runtime [mcp_servers] into Codex TOML
    [mcp_servers.<name>] tables.  The config file is discovered automatically
    by Codex at the fixed project path. Persistent MCP [env] entries store
    environment-variable references (for example, [$EPURE_DB]) rather than raw
    runtime values. *)

(** @inline *)
include Agentic_backend.S

(** Extract safe normalized payloads from one Codex JSONL event. Agent text,
    canonical lowercase UUID thread identity, safe tool identity, and
    non-negative integer token counts are retained; raw tool arguments and
    outputs are omitted. Invalid token fields are ignored independently. *)
val normalized_events_of_line : string -> Task_event.payload list

(** Prepared Codex process invocation. [argv] is passed directly to the process
    launcher without a shell. [stdin] carries the prompt/instructions, while
    [redacted_argv] preserves useful flag names and attachment counts without
    paths, session/model values, schema paths, workspace paths, or prompt data. *)
type backend_invocation = {
  argv : string list;
  stdin : string option;
  redacted_argv : string list;
}

(** [build_invocation ?schema_path ~attachment_delivery spec] validates the
    Codex-specific transport request and constructs its actual and redacted
    argv. [Upload_attachments] adds one [-i] pair per workspace-relative image;
    [Reuse_session_attachments] is accepted only for resume and adds none.
    Web policy is forced with an invocation-scoped config override, independently
    of user config. A schema-bearing [spec] requires [schema_path]. Resume IDs
    must use the canonical lowercase UUID syntax emitted in Codex
    [thread.started] records; validation occurs before config I/O or spawn. *)
val build_invocation :
  ?schema_path:string ->
  ?attachment_delivery:Backend_types.attachment_delivery ->
  mcp_config_path:string option ->
  Backend_types.task_spec ->
  (backend_invocation, string) result

(** [with_output_schema_file schema f] writes [schema] to a private temporary
    file for Codex, calls [f path], and unlinks the file on every exit path. *)
val with_output_schema_file : Yojson.Safe.t -> (string -> 'a) -> 'a

(** {1 Additional Utilities} *)

(** [project_config_artifacts ~mcp_servers ~lsp_servers] returns the
    Codex project config artifact owned by this backend provider.

    {pre}
    (none)

    {post}
    Returns a managed backend-project artifact at [.codex/config.toml].  When
    [mcp_servers] is non-empty, the artifact contains active
    [mcp_servers.<name>] / [mcp_servers.<name>.env] tables in addition to the
    commented default template.

    {violators}
    (none)

    {violates}
    (none) *)
val project_config_artifacts :
  managed_namespace:Backend_types.managed_namespace ->
  mcp_servers:Backend_types.mcp_server_config list ->
  lsp_servers:Backend_types.lsp_server_config list ->
  Backend_config_writer.artifact list

(** [parse_jsonl_output stdout] parses Codex's JSONL output and extracts only
    the last protocol-proven completed agent message and completed-turn usage.
    Malformed, reasoning, error, and unknown records are ignored; raw stdout is
    never used as fallback text. Token fields must be non-negative JSON integers;
    zero is preserved, invalid fields are ignored independently, and totals
    saturate at [max_int].

    {pre}
    (none)

    {post}
    Returns a pair of the last extracted message text and optional cost data parsed from the JSONL output.

    {violators}
    (none)

    {violates}
    (none)
*)
val parse_jsonl_output : string -> string * Backend_types.cost option

(** [parse_stdout_text stdout] is the compatibility extractor for the final
    [agent_message] text from
    Codex's JSONL stdout.  Codex emits one JSON event per line; the final
    assistant reply is the latest [{"type":"item.completed","item":{"type":
    "agent_message","text":"..."}}] event. It retains the historical raw
    [stdout] fallback; normal runtime execution uses the strict
    {!parse_public_stdout_text} parser instead.

    {pre}
    (none)

    {post}
    Returns the [text] field of the last [agent_message] item, or raw [stdout]
    if no such item is found.

    {violators}
    (none)

    {violates}
    (none) *)
val parse_stdout_text : string -> string

(** Strict final public assistant text parser for Codex JSONL. *)
val parse_public_stdout_text : string -> string

(** Strict session parser accepting only the [thread_id] field of
    [thread.started] when it is a canonical lowercase UUID
    ([8-4-4-4-12] hexadecimal syntax). *)
val parse_public_session_id : string -> string option

(** [build_command ~mcp_config_path spec] constructs the Codex CLI command and
    stdin content for a task invocation.  When [spec.read_only] is [true],
    passes [-s read-only] (OS-level sandbox); otherwise
    [--full-auto]. Attachment delivery defaults to a fresh upload; use
    {!build_invocation} to test explicit resume-reuse behavior.
    Exported for testing.

    {pre}
    (none)

    {post}
    Returns [(cmd_list, stdin_content)].  When [spec.read_only = true] the
    command includes [-s read-only]; when [false] it includes [--full-auto].
    With a schema, this testing helper creates the referenced temporary file and
    the caller must unlink it. Normal {!run_task} owns and unlinks that file with
    {!with_output_schema_file} on success, failure, timeout, and cancellation.

    {violators}
    (none)

    {violates}
    (none) *)
val build_command :
  mcp_config_path:string option ->
  Backend_types.task_spec ->
  string list * string
