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
    The backend uses [--print --output-format json] for non-interactive output.

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

(** [build_command ~mcp_config_path spec] constructs the Claude Code CLI
    command and stdin content for a task invocation.
    Includes [--resume <id>] when [spec.resume_session_id] is set.
    Exported for testing.

    {pre}
    (none)

    {post}
    Returns [(cmd_list, stdin_content)] where [cmd_list] is the full
    argument vector for [claude] and [stdin_content] is the prompt text to
    pipe to the process's stdin.

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

(** [parse_session_id_from_stdout stdout] extracts the [session_id] field
    from Claude Code's JSON output. Returns [None] if not present.

    {pre}
    (none)

    {post}
    Returns [Some id] if a [session_id] field is found in the JSON output,
    [None] otherwise.

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

(** [parse_stream_event line] parses a stream-json event line and extracts
    displayable content. Returns [Some text] if there is content to display,
    [None] otherwise.

    Tool-use blocks are formatted as ["→ <tool_name> <arg>"] and
    tool-result blocks are filtered out.

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
    text, tool identity, and session identity from one Claude stream event.
    Thinking blocks, tool arguments, and malformed input produce no sensitive
    normalized payload. *)
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
