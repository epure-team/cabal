(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Shared process execution for agentic backends.

    This module provides the common infrastructure used by all CLI-based
    agentic backends (Claude Code, Codex, Gemini, OpenCode, Copilot).
    It handles process spawning, output capture, timeout, and git diff
    detection so each backend only needs to implement command construction
    and output parsing. *)

open Backend_types

(** {1 Process Execution} *)

(** Result of a subprocess invocation: status, stdout, stderr, exit code,
    elapsed time, and optional cost parsed from output. *)
type process_result = {
  status : result_status;
  stdout : string;
  stderr : string;
  exit_code : int;
  elapsed : duration;
  cost : cost option;
  session_id : string option;
}

(** [validate_task_namespace spec] checks the managed namespace before any
    backend path, suffix, or temporary-file operation uses it.

    {pre}
    (none)

    {post}
    Returns [None] when [spec.managed_namespace] is valid.  Returns
    [Some task_result] with [Failed msg] when invalid, suitable for immediate
    return from backend [run_task] implementations before touching the
    filesystem.

    {violators}
    (none)

    {violates}
    (none) *)
val validate_task_namespace : task_spec -> task_result option

(** [run_process ~sw ~env ~cmd ~working_dir ~timeout_seconds ~parse_cost ~on_stdout]
    spawns a subprocess with the given command in an owned process group,
    captures stdout/stderr concurrently, and handles timeout via
    [Eio.Time.with_timeout]. When [context] is present, the process receives at
    most the time remaining on the task's shared absolute deadline.

    @param parse_cost Optional function to extract cost from stdout.
      If not provided, cost is always [None].
    @param on_stdout Optional callback called for each line of stdout as it arrives.
      Used for streaming output to UI in real-time.
    @param working_dir Directory to run the process in.
    @param timeout_seconds Maximum wall-clock seconds before initiating bounded,
      ownership-aware process cleanup.
    @param context Optional task lifecycle/deadline context. Process ownership,
      termination, escalation, and exit transitions are emitted through it.

    {pre}
    [sw] must be active. [cmd] must be a non-empty list with a valid
    executable as the first element. [working_dir] must be an existing
    directory. [timeout_seconds] must be positive.

    {post}
    Returns a [process_result] with captured stdout/stderr, exit code,
    elapsed time, and optional cost. Backend success additionally requires an
    exact [Process_group.Established] handshake; timeout, invalid protocol, or a
    launcher-reported pre-exec failure returns an explicit [Failed] result even
    if the status FD reports exit zero. [status] is [Timeout] if an established
    process exceeded [timeout_seconds], otherwise it reflects the exit code.
    Timeout, cancellation, invalid handshake, and exceptional cleanup follow the
    ownership established by the handshake. With a confirmed PGID, cleanup
    terminates the group with TERM, grace, then KILL. Without a confirmed PGID,
    cleanup performs bounded direct-process termination only; the official
    launcher cannot fork the backend before its ownership ACK. Both paths inherit
    {!Process_group.terminate}'s guarantee that concurrent cleanup completes only
    after the direct supervisor's actual reap, including when malformed or
    missing backend status requires fallback to that reaped status. Cleanup
    drains stdout and stderr concurrently with bounded termination so TERM
    handlers cannot block on a full pipe. Once backend status and all I/O have
    completed, normal RELEASE and supervisor reaping occur outside the
    configured timeout as an atomic completion boundary.

    {violators}
    (none)

    {violates}
    (none) *)
val run_process :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  cmd:string list ->
  ?stdin_content:string option ->
  working_dir:string ->
  timeout_seconds:float ->
  ?context:Task_execution_context.t ->
  ?parse_cost:(string -> cost option) ->
  ?on_stdout:(string -> unit) ->
  ?guardian:Resource_guardian.t ->
  unit ->
  process_result

(** {1 Git Diff Detection} *)

(** Pathspec exclusions ([(exclude)…] form) appended to every [git diff] call.
    Filters build-artefact directories (node_modules, _build, .vite, dist,
    vendor) that inflate diff size and cause context-window overflows in
    review agents.  Shared by [build_flow_helpers.get_git_diff_since]. *)
val diff_exclude_pathspecs : string list

(** [get_git_diff ~sw ~env ~working_dir] returns the list of changed files,
    including both tracked modifications ([git diff --name-only HEAD]) and
    untracked new files ([git ls-files --others --exclude-standard]).

    Returns an empty list if git is not available or the directory is not
    a git repository.

    {pre}
    [sw] must be active. [working_dir] must be an existing directory.

    {post}
    Returns a list of relative file paths that have been modified or added
    since HEAD; returns [[]] when git is unavailable or the directory is not
    a repository.

    {violators}
    (none)

    {violates}
    (none) *)
val get_git_diff :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  working_dir:string ->
  string list

(** [get_git_diff_content ~sw ~env ~working_dir] returns the full unified
    diff output as a string. Includes tracked changes ([git diff HEAD]) and
    generates pseudo-diffs for untracked new files so that reviewers can see
    their content.

    Returns an empty string if git is not available or the directory is not
    a git repository.

    {pre}
    [sw] must be active. [working_dir] must be an existing directory.

    {post}
    Returns the full unified diff as a string, including pseudo-diffs for
    untracked files; returns [""] when git is unavailable or the directory
    is not a repository.

    {violators}
    (none)

    {violates}
    (none) *)
val get_git_diff_content :
  sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> working_dir:string -> string

(** {1 MCP Config} *)

(** [write_mcp_config ~env ~path configs] writes MCP server configurations
    to a JSON file suitable for tools that accept MCP config files
    (e.g., Claude Code [--mcp-config] or other explicit config-file flags).

    {pre}
    The parent directory of [path] must exist and be writable.

    {post}
    Creates or overwrites the file at [path] with a JSON object mapping
    each server name to its connection details.

    {violators}
    (none)

    {violates}
    (none) *)
val write_mcp_config :
  env:Eio_unix.Stdenv.base -> path:string -> mcp_server_config list -> unit

(** [setup_mcp_config ~env spec] creates a temporary MCP config file in
    the working directory if [spec.mcp_servers] is non-empty.
    Returns [Some path] if created, [None] otherwise.

    {pre}
    [spec.working_dir] must be an existing, writable directory.

    {post}
    Returns [Some path] with the path to the newly created config file, or
    [None] when [spec.mcp_servers] is empty.

    {violators}
    (none)

    {violates}
    (none) *)
val setup_mcp_config : env:Eio_unix.Stdenv.base -> task_spec -> string option

(** [cleanup_mcp_config ~env path] removes the temporary MCP config file.

    {pre}
    [path] should refer to a file previously created by [setup_mcp_config].

    {post}
    Deletes the file at [path]; silently succeeds if the file no longer
    exists.

    {violators}
    (none)

    {violates}
    (none) *)
val cleanup_mcp_config : env:Eio_unix.Stdenv.base -> string -> unit

(** {1 Availability Check} *)

(** Result of one combined version/availability process probe. *)
type version_probe_result = {
  command_available : bool;
      (** [true] exactly when the version command exited successfully. *)
  output : string option;
      (** Captured stdout, or stderr when stdout was empty. *)
  timed_out : bool;  (** Whether the command exceeded the probe timeout. *)
}

(** [probe_version_command ~env cmd] runs one bounded process and reports both
    clean-exit availability and optional version output. Non-zero exits may
    still carry output. Missing commands and timeouts return unavailable with
    no output. Eio cancellation is propagated. *)
val probe_version_command :
  env:Eio_unix.Stdenv.base ->
  ?timeout_seconds:float ->
  string list ->
  version_probe_result

(** [capture_version_output ~env cmd] runs [cmd] and returns the captured
    stdout as [Ok output], or stderr as [Ok output] if stdout is empty.
    Returns [Error _] only when the binary produces no output at all (not
    installed).  Non-zero exit codes are intentionally tolerated — some
    backends exit non-zero on [--version].

    {pre}
    [cmd] must be a non-empty list.

    {post}
    Returns [Ok s] when [s] is non-empty output from the process, or
    [Error _] when the binary is absent or produces no output.

    {violators}
    (none)

    {violates}
    (none) *)
val capture_version_output :
  env:Eio_unix.Stdenv.base ->
  ?timeout_seconds:float ->
  string list ->
  (string, string) result

(** [check_available ~env cmd] runs [cmd] (e.g., ["claude"; "--version"])
    and returns [true] if it exits successfully, [false] otherwise.

    {pre}
    [cmd] must be a non-empty list.

    {post}
    Returns [true] if the command exits with code 0, [false] on any
    non-zero exit or if the executable is not found.

    {violators}
    (none)

    {violates}
    (none) *)
val check_available :
  env:Eio_unix.Stdenv.base -> ?timeout_seconds:float -> string list -> bool

(** {1 Task Execution Helper} *)

(** [run_task_with ~sw ~env ~spec ~build_command ~parse_cost ~parse_stdout ~on_stdout] is
    the standard task execution flow shared by all backends:
    1. Setup MCP config if needed
    2. Build command via [build_command]
    3. Run process with timeout (prompt passed via stdin)
    4. Get git diff
    5. Cleanup MCP config
    6. Build [task_result]

    @param build_command Takes [mcp_config_path option] and [task_spec],
      returns [(command_list, stdin_content)] tuple. The stdin_content is
      written to the process's stdin to avoid "Argument list too long" errors.
    @param parse_cost Optional function to extract cost from stdout.
    @param parse_stdout Optional function to extract the response text
      from raw stdout. If not provided, raw stdout is used as-is.
      Backends with structured output (JSON, JSONL) should provide this
      to extract the actual response text for callers.
    @param parse_session_id Optional function to extract a CLI session ID
      from stdout for resume support.
    @param on_stdout Optional callback called for each line of stdout as it arrives.
      Used for streaming output to UI.
    @param context Optional shared task context. Its remaining budget caps the
      process timeout and receives process lifecycle events.

    {pre}
    [sw] must be active. [spec.working_dir] must be an existing directory.
    [spec.timeout] must be positive.

    {post}
    Returns a fully populated [task_result] reflecting the process outcome,
    git diff, cost, and any structured data extracted from stdout.

    {violators}
    (none)

    {violates}
    (none) *)
val run_task_with :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  spec:task_spec ->
  build_command:
    (mcp_config_path:string option -> task_spec -> string list * string) ->
  ?context:Task_execution_context.t ->
  ?parse_cost:(string -> cost option) ->
  ?parse_stdout:(string -> string) ->
  ?parse_session_id:(string -> string option) ->
  ?on_stdout:(string -> unit) ->
  ?guardian:Resource_guardian.t ->
  unit ->
  task_result
