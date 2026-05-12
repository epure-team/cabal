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

(** [build_command ~mcp_config_path spec] constructs the Codex CLI command and
    stdin content for a task invocation.  When [spec.read_only] is [true],
    passes [-s read-only] (OS-level sandbox); otherwise [--full-auto].
    Exported for testing.

    {pre}
    (none)

    {post}
    Returns [(cmd_list, stdin_content)].  When [spec.read_only = true] the
    command includes [-s read-only]; when [false] it includes [--full-auto].

    {violators}
    (none)

    {violates}
    (none) *)
val build_command :
  mcp_config_path:string option ->
  Backend_types.task_spec ->
  string list * string
