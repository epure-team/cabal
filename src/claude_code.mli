(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Claude Code agentic backend.

    This module implements the [AGENTIC_BACKEND.S] interface for Claude Code CLI.
    It spawns the [claude] command-line tool to execute tasks and collects
    results via JSON output parsing and git diff detection.

    {b Configuration:}
    Claude Code is expected to be installed and accessible in the PATH.
    The backend uses Claude's newline-delimited stream JSON input/output
    protocol for non-interactive output. Media and positive web transport remain
    disabled until authenticated baseline evidence is available.

    {b MCP Integration:}
    The backend generates a temporary MCP configuration file that points to
    Épure's MCP server, enabling Claude Code to access story data,
    conventions, and submit structured reports.

    See DESIGN.md Section 5.1 "Agentic Execution Model" and Section 6
    "Agentic Backend Layer". *)

(** @inline *)
include Agentic_backend.S

(** {1 Additional Utilities} *)

(** [project_config_artifacts ~mcp_servers ~lsp_servers] returns the
    Claude Code project settings artifact owned by this backend provider.

    {pre}
    (none)

    {post}
    Returns an Épure-owned [settings.json] artifact under
    the configured managed namespace directory with LSP command/args entries
    for host-provided servers.

    {violators}
    (none)

    {violates}
    (none) *)
val project_config_artifacts :
  managed_namespace:Backend_types.managed_namespace ->
  mcp_servers:Backend_types.mcp_server_config list ->
  lsp_servers:Backend_types.lsp_server_config list ->
  Backend_config_writer.artifact list

(** [write_mcp_config ~path configs] writes MCP server configurations to a
    JSON file suitable for [claude --mcp-config].

    The generated file has the format:
    {[
      {
        "mcpServers": {
          "server-name": {
            "command": "cmd",
            "args": ["arg1", "arg2"],
            "env": {"KEY": "VALUE"}
          }
        }
      }
    ]}

    {pre}
    The parent directory of [path] must exist and be writable.

    {post}
    Creates or overwrites [path] with the Claude Code MCP config JSON.

    {violators}
    (none)

    {violates}
    (none) *)
val write_mcp_config :
  env:Eio_unix.Stdenv.base ->
  path:string ->
  Backend_types.mcp_server_config list ->
  unit

(** [parse_json_output json] parses Claude Code's JSON output format and
    extracts relevant information for the task result.

    Returns [(stdout_text, cost_info)] where cost_info contains token counts
    if available in the output.

    {pre}
    [json] must be a valid JSON value produced by Claude Code's
    [--output-format json] mode.

    {post}
    Returns a [(text, cost option)] pair where [text] is the extracted
    response text and [cost] contains token usage if present in [json].

    {violators}
    (none)

    {violates}
    (none) *)
val parse_json_output : Yojson.Safe.t -> string * Backend_types.cost option

(** [get_git_diff ~sw ~env ~working_dir] runs [git diff --name-only] in the
    working directory and returns the list of changed files.

    Returns an empty list if git is not available or the directory is not
    a git repository.

    {pre}
    [sw] must be active. [working_dir] must be an existing directory.

    {post}
    Returns a list of relative file paths changed since HEAD; returns [[]]
    when git is unavailable or the directory is not a repository.

    {violators}
    (none)

    {violates}
    (none) *)
val get_git_diff :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  working_dir:string ->
  string list

(** [get_git_diff_content ~sw ~env ~working_dir] runs [git diff] in the
    working directory and returns the full unified diff output as a string.

    Returns an empty string if git is not available or the directory is not
    a git repository.

    {pre}
    [sw] must be active. [working_dir] must be an existing directory.

    {post}
    Returns the full unified diff string; returns [""] when git is
    unavailable or the directory is not a repository.

    {violators}
    (none)

    {violates}
    (none) *)
val get_git_diff_content :
  sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> working_dir:string -> string

(** Prepared Claude process invocation. [argv] and [stdin] are the actual
    process inputs. [redacted_argv] replaces settings/MCP paths, schema, model,
    and resume values; [redacted_stdin] contains no prompt or content data.
    Only redacted fields are suitable for diagnostics. *)
type backend_invocation = {
  argv : string list;
  stdin : string;
  redacted_argv : string list;
  redacted_stdin : string;
}

(** [build_invocation ... spec] constructs an attachment-free,
    [Web_disabled] Claude stream-JSON invocation. It validates resume IDs and
    emits them using the parity SDK's [--resume <uuid>] shape. Native
    [--json-schema] composition is preserved.

    Attachment-bearing and positive-web requests are rejected with sanitized
    errors. [attachment_paths] is accepted only to make this fail-closed
    boundary explicit; paths are never opened or reflected. A future media
    encoder must consume central sealed delivery and use the matching
    preflight-validated attachment references as its size and MIME bounds. *)
val build_invocation :
  ?attachment_paths:string list ->
  ?attachment_delivery:Backend_types.attachment_delivery ->
  ?project_config_path:string option ->
  mcp_config_path:string option ->
  Backend_types.task_spec ->
  (backend_invocation, string) result

(** [build_command ~mcp_config_path spec] is the compatibility projection of
    {!build_invocation}. It constructs the Claude Code CLI command and
    stream-JSON stdin content for an attachment-free, [Web_disabled] task.
    Includes [--resume <id>] when [spec.resume_session_id] is set.
    Unsupported media/web requests and malformed resume IDs raise
    [Invalid_argument]. Exported for testing.

    {pre}
    (none)

    {post}
    Returns [(cmd_list, stdin_content)] where [cmd_list] is the full
    argument vector for [claude] and [stdin_content] is one user-message JSONL
    record to pipe to the process's stdin.

    {violators}
    (none)

    {violates}
    (none) *)
val build_command :
  ?streaming:bool ->
  ?project_config_path:string option ->
  mcp_config_path:string option ->
  Backend_types.task_spec ->
  string list * string

(** [parse_session_id_from_stdout stdout] extracts a canonical lowercase UUID
    [session_id] from a public Claude [system/init] or successful [result]
    record. Returns [None] for other shapes or invalid identifiers.

    {pre}
    (none)

    {post}
    Returns [Some id] if a valid public [session_id] is found, [None] otherwise.

    {violators}
    (none)

    {violates}
    (none) *)
val parse_session_id_from_stdout : string -> string option

(** [parse_stdout_text stdout] extracts the agent reply from Claude Code's
    JSON envelope, by reading the top-level [result] field.  Returns the raw
    [stdout] when it is not valid JSON or has no [result] field.

    {pre}
    (none)

    {post}
    Returns the [result] field of the JSON envelope, or the raw [stdout] on
    parse failure / unexpected shape.

    {violators}
    (none)

    {violates}
    (none) *)
val parse_stdout_text : string -> string

(** Strict public-output parser used by normalized runtime paths. Malformed,
    error, user, reasoning, system, and unknown records contribute no text. *)
val parse_public_stdout_text : string -> string

(** Strict session parser accepting only documented init or successful result
    records. *)
val parse_public_session_id : string -> string option

(** Strict usage parser accepting only successful result records. *)
val parse_public_cost : string -> Backend_types.cost option

(** [parse_stream_event line] parses a stream-json event line and extracts only
    display-safe public content. Tool-use blocks retain a bounded tool name but
    never arguments; user/tool-result, thinking, error, malformed, and unknown
    records are filtered out. Session initialization renders a fixed marker,
    not the session identifier.

    {pre}
    [line] should be a single line of Claude Code's stream-json output.

    {post}
    Returns [Some text] with human-readable content to display, or [None]
    for lines that produce no displayable output (e.g., tool-result blocks).

    {violators}
    (none)

    {violates}
    (none) *)
val parse_stream_event : string -> string option

(** [normalized_events_of_stream_line line] extracts only public assistant
    text, bounded tool identity, canonical session UUIDs, and independently
    validated non-negative usage fields from one Claude stream event. Input
    prompts/images, thinking, tool arguments/results, errors, unsafe identifiers,
    and malformed input produce no sensitive normalized payload. *)
val normalized_events_of_stream_line : string -> Task_event.payload list

(** [run_task_streaming ~sw ~env ~on_stdout spec] runs a task with streaming
    stdout output. The [on_stdout] callback is called for each line of output
    as it arrives, enabling real-time display in the UI.

    This is used by DEMO mode to show Builder output in real-time.

    {pre}
    [sw] must be active. [spec.working_dir] must be an existing directory.

    {post}
    Returns a [task_result] after the process completes; [on_stdout] is
    called for each output line during execution.

    {violators}
    (none)

    {violates}
    (none) *)
val run_task_streaming :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  on_stdout:(string -> unit) ->
  ?context:Task_execution_context.t ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  Backend_types.task_result
