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
    sandboxed execution.

    {b MCP Integration:}
    Codex 0.122.0 supports MCP via [.codex/config.toml].  Épure generates
    this file with a template MCP section (disabled/comment-only by default)
    and serializes approved runtime [mcp_servers] into Codex TOML
    [mcp_servers.<name>] tables.  The config file is discovered automatically
    by Codex at the fixed project path. Persistent MCP [env] entries store
    environment-variable references (for example, [$EPURE_DB]) rather than raw
    runtime values. *)

(** @inline *)
include Agentic_backend.S

(** Extract safe normalized payloads from one Codex JSONL event. Agent text,
    session/tool identity, and token counts are retained; raw tool arguments
    and outputs are omitted. *)
val normalized_events_of_line : string -> Task_event.payload list

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

(** [parse_jsonl_output stdout] parses Codex's JSONL output and extracts
    the last message text and optional cost information.

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

(** [parse_stdout_text stdout] extracts the final [agent_message] text from
    Codex's JSONL stdout.  Codex emits one JSON event per line; the final
    assistant reply is the latest [{"type":"item.completed","item":{"type":
    "agent_message","text":"..."}}] event.  Returns the raw [stdout] when no
    agent message could be extracted (e.g., malformed output or non-JSON
    payloads).

    {pre}
    (none)

    {post}
    Returns the [text] field of the last [agent_message] item, or the raw
    [stdout] if no such item is found.

    {violators}
    (none)

    {violates}
    (none) *)
val parse_stdout_text : string -> string

(** Strict final public assistant text parser for Codex JSONL. *)
val parse_public_stdout_text : string -> string

(** Strict session parser accepting only [thread.started]. *)
val parse_public_session_id : string -> string option

(** [build_command ~mcp_config_path spec] constructs the Codex CLI command and
    stdin content for a task invocation.  When [spec.read_only] is [true],
    passes [-s read-only] (OS-level sandbox); otherwise [--full-auto].
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
