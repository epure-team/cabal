(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** GitHub Copilot CLI agentic backend.

    This module implements the [AGENTIC_BACKEND.S] interface for the
    GitHub Copilot CLI tool. It spawns [copilot] in non-interactive mode
    and collects results.

    {b Configuration:}
    Copilot CLI is expected to be installed and accessible in the PATH.
    The stable Copilot CLI expects the prompt as a CLI argument
    ([--prompt <text>] / [-p <text>]), so the backend passes the full prompt in
    argv rather than through stdin. It also uses [--yolo -s --no-ask-user] for
    non-interactive execution with auto-approval and silent output (no stats,
    just response).

    {b MCP Integration:}
    Copilot supports project MCP servers via [.github/mcp.json]. When
    [task_spec.mcp_servers] is non-empty, Épure writes approved entries to
    that project config file before invocation and relies on Copilot's project
    discovery rather than a transient CLI flag.  If the project MCP artifact is
    user-authored or hash-mismatched, the backend fails clearly instead of
    invoking Copilot without the requested MCP servers.

    {b LSP Integration:}
    Copilot supports project LSP server configuration via [.github/lsp.json].
    Épure renders host-provided LSP servers into that strict JSON artifact.

    {b Output Format:}
    Copilot CLI does not support JSON output. The response is captured
    as plain text from stdout. Token usage is not available. *)

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

(** [build_command ~mcp_config_path spec] constructs the Copilot CLI command
    and stdin content.  Stable Copilot CLI expects [--prompt <text>] / [-p
    <text>] rather than reading stdin for [-p -], so the returned command embeds
    the full prompt argument and the returned stdin content is empty.  Copilot
    CLI 1.0.34 has no native read-only sandbox;
    [spec.read_only] is acknowledged as documented limitation and the baseline
    command is returned unchanged.  [mcp_config_path] is ignored because
    Copilot discovers the project [.github/mcp.json] written by [run_task].
    Exported for testing.

    {pre}
    (none)

    {post}
    Returns [(cmd_list, stdin_content)].  Includes [--model <model>] when
    [spec.model] is set and returns empty [stdin_content].  Command is otherwise
    identical for [read_only = true] and [read_only = false] (no native
    restriction model).

    {violators}
    (none)

    {violates}
    (none) *)
val build_command :
  mcp_config_path:string option ->
  Backend_types.task_spec ->
  string list * string
