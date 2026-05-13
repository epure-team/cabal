(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** OpenCode CLI agentic backend.

    This module implements the [AGENTIC_BACKEND.S] interface for the
    OpenCode CLI tool. It spawns [opencode run] in non-interactive mode
    with JSON output and collects results.

    {b Configuration:}
    OpenCode is expected to be installed and accessible in the PATH.
    The backend uses [opencode run --format json] for non-interactive
    execution. Permissions can be configured via the [OPENCODE_PERMISSION]
    environment variable.

    {b MCP Integration:}
    OpenCode supports MCP servers via its config file. The [mcp_servers]
    field of [task_spec] is written into [opencode.json] before each run only
    when the project config setup result proves the file is Épure-managed;
    user-authored or hash-mismatched files are left untouched.  After that
    merge succeeds, the runtime invocation clears [mcp_servers] so the shared
    runner does not create an unused transient MCP config file.

    {b LSP Integration:}
    OpenCode supports project LSP server configuration via the [lsp] map in
    [opencode.json]. Épure renders host-provided LSP servers using OpenCode's
    native config schema. *)

(** @inline *)
include Agentic_backend.S

(** {1 Additional Utilities} *)

(** [project_config_artifacts ~mcp_servers ~lsp_servers] returns the
    OpenCode project config artifact owned by this backend provider.

    {pre}
    (none)

    {post}
    Returns a managed backend-project artifact at [opencode.json]. When
    [lsp_servers] contains host-provided file associations, the artifact
    includes a native [lsp] map.

    {violators}
    (none)

    {violates}
    (none) *)
val project_config_artifacts :
  managed_namespace:Backend_types.managed_namespace ->
  mcp_servers:Backend_types.mcp_server_config list ->
  lsp_servers:Backend_types.lsp_server_config list ->
  Backend_config_writer.artifact list

(** [parse_json_events stdout] parses OpenCode's JSON event output and
    extracts the final response text and optional cost information.

    {pre}
    (none)

    {post}
    Returns a pair of the extracted response text and optional cost data parsed from the JSON event stream.

    {violators}
    (none)

    {violates}
    (none)
*)
val parse_json_events : string -> string * Backend_types.cost option

(** [parse_stdout_text stdout] extracts the agent response text from
    OpenCode's structured JSON event output, returning the empty string when
    no response could be extracted.

    {pre}
    (none)

    {post}
    Returns the response text reconstructed from OpenCode's JSON events.

    {violators}
    (none)

    {violates}
    (none) *)
val parse_stdout_text : string -> string

(** [build_command ~mcp_config_path spec] constructs the OpenCode CLI command
    and stdin content.  OpenCode 1.14.20 has no native read-only sandbox;
    [spec.read_only] is acknowledged as documented limitation and the baseline
    command is returned unchanged.  Exported for testing.

    {pre}
    (none)

    {post}
    Returns [(cmd_list, stdin_content)].  Includes [-m <model>] when
    [spec.model] is set.  Command is otherwise identical for [read_only = true]
    and [read_only = false] (no native restriction model).

    {violators}
    (none)

    {violates}
    (none) *)
val build_command :
  mcp_config_path:string option ->
  Backend_types.task_spec ->
  string list * string

(** [ensure_mcp_in_opencode_json ~env spec] merges [spec.mcp_servers] into
    the project [opencode.json] file before an OpenCode invocation.

    {pre}
    [spec.working_dir] points to the project directory.  If [spec.mcp_servers]
    is empty the function is a no-op.

    {post}
    For non-empty [spec.mcp_servers], [opencode.json] contains an [mcp] object
    with those server entries.  Existing non-Épure config fields are preserved,
    legacy invalid Épure schema keys are not emitted, and a leading Épure JSONC
    managed header is preserved when present.  If an existing file cannot be
    parsed as supported JSONC, the file is left unchanged.

    {violators}
    (none)

    {violates}
    (none) *)
val ensure_mcp_in_opencode_json :
  env:Eio_unix.Stdenv.base -> Backend_types.task_spec -> unit

(** [ensure_mcp_if_config_applied ~env ~setup_outcome spec] merges MCP config
    only when the preceding setup result proves [opencode.json] is Épure-owned
    and current.

    {pre}
    [setup_outcome] is the primary [opencode.json] result from
    [Backend_config_writer.setup_artifacts].

    {post}
    Returns [Ok ()] after merging MCP when config setup was applied, or when no
    MCP servers were requested.  Returns [Error msg] and leaves [opencode.json]
    untouched when MCP was requested but setup skipped user-authored content or
    refused a hash mismatch.

    {violators}
    (none)

    {violates}
    (none) *)
val ensure_mcp_if_config_applied :
  env:Eio_unix.Stdenv.base ->
  setup_outcome:Backend_config_writer.write_result option ->
  Backend_types.task_spec ->
  (unit, string) result

(** [user_owned_backup_needed write_outcome] returns [true] when
    [write_outcome] indicates that a user-authored opencode.json was found at
    the project path — i.e. the mutation guard of Story #515 should be armed.
    Returns [false] for Épure-managed files (AC4), hash-mismatch files (AC5),
    and all other write outcomes.

    {pre}
    (none)

    {post}
    Returns [true] only when [write_outcome = Some (Skipped_user_content _)].
    Returns [false] for [None], [Some Already_current], [Some (Written _)],
    [Some (Backed_up_and_written _)], and [Some (Refused_hash_mismatch _)].

    {violators}
    (none)

    {violates}
    (none) *)
val user_owned_backup_needed : Backend_config_writer.write_result option -> bool

(** [read_opencode_backup ~env ~config_path] reads the current bytes of the
    file at [config_path] as a pre-run backup (Story #515 AC1/AC3).

    {pre}
    [config_path] is an absolute path to an existing file.

    {post}
    Returns [Ok content] when the file is readable.
    Returns [Error msg] when the read fails for any reason (AC3 abort path).

    {violators}
    (none)

    {violates}
    (none) *)
val read_opencode_backup :
  env:Eio_unix.Stdenv.base -> config_path:string -> (string, string) result

(** [check_opencode_mutation ~env ~config_path ~backup result] detects and
    undoes mutation of user-owned opencode.json by the OpenCode binary
    (Story #515 AC2).

    Reads the post-run bytes of [config_path] and compares them to [backup].
    If they differ, restores [backup] to [config_path] and returns a [Failed]
    result with empty stdout and no session ID.  If identical, returns [result]
    unmodified.

    {pre}
    [config_path] is an absolute path.
    [backup] is the byte-for-byte pre-run content of the file.

    {post}
    When no mutation is detected (bytes identical to backup): returns [result]
    unchanged.
    When mutation or deletion is detected: attempts to restore [backup] to
    [config_path], then returns a [Failed] result with [stdout = ""] and
    [session_id = None].  The failure message states whether the restore
    succeeded ("original file has been restored") or failed ("restore
    failed: <exn>").
    Deletion of the file by OpenCode is treated as mutation and produces the
    same [Failed] result.

    {violators}
    (none)

    {violates}
    (none) *)
val check_opencode_mutation :
  env:Eio_unix.Stdenv.base ->
  config_path:string ->
  backup:string ->
  Backend_types.task_result ->
  Backend_types.task_result
