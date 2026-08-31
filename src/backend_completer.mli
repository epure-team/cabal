(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Create completer callbacks from agentic backends.

    This module bridges the agent completer interface with the agentic
    backend execution model. A completer wraps a backend invocation:
    each call spawns the backend CLI with the system prompt and user
    prompt, returning the text response.

    When [resume_session_id] is provided, the backend CLI resumes an
    existing session (e.g., Claude Code [--resume]).  The system prompt
    is omitted (already in the CLI's context) and only the new user
    prompt is sent. *)

(** Result of a completion call. *)
type completion_result = {
  text : string;  (** Response text from the backend. *)
  backend_session_id : string option;
      (** CLI session ID for subsequent resumption. *)
}

(** A text completion callback. Takes a system prompt, user prompt,
    an optional inline JSON Schema for structured-output enforcement, and
    optional session ID to resume. *)
type completer =
  system_prompt:string ->
  prompt:string ->
  json_schema:Yojson.Safe.t option ->
  resume_session_id:string option ->
  (completion_result, string) result

val complete_with_workspace :
  completer ->
  workspace:Virtual_workspace.workspace ->
  system_prompt:string ->
  prompt:string ->
  json_schema:Yojson.Safe.t option ->
  resume_session_id:string option ->
  (completion_result, string) result

(** [make ~sw ~env ~backend ~working_dir] creates a completer that
    delegates to the given agentic backend.

    This is a low-level compatibility path for callers that intentionally hold
    an explicit backend snapshot. It routes through {!Json_schema_enforcer} but
    bypasses CBL-03 registry consistency and {!Task_preflight} guarantees. Use
    {!make_by_name} for centrally validated call-time resolution.

    Each call spawns the backend CLI with the combined system/user prompt
    and returns the text response from stdout.

    @param sw Eio switch for resource management
    @param env Eio environment for process spawning
    @param backend The agentic backend to delegate to
    @param working_dir Target project directory for the backend

    {pre}
    The switch [sw] must be active. [backend] must be available.

    {post}
    Returns a [completer] function that delegates each call to the given backend CLI.

    {violators}
    (none)

    {violates}
    (none) *)
val make :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  backend:Agentic_backend.t ->
  working_dir:string ->
  ?model:string ->
  ?mcp_servers:Backend_types.mcp_server_config list ->
  unit ->
  completer

(** [run_gate_for_output ~backend_name ~version_output] checks whether the
    version string [version_output] (raw stdout from the backend binary)
    satisfies the stable baseline for [backend_name].

    Pure function — no subprocess calls.  Suitable for unit testing by
    injecting a canned version string.

    Returns [Ok ()] when:
    - [backend_name] is not a built-in backend (gate skipped), or
    - [version_output] cannot be parsed as [N.N.N] (fail-safe skip), or
    - the parsed version is at or above the baseline.

    Returns [Error msg] with an actionable message (naming the required
    baseline and the [--force-backend] override) when the parsed version
    is below the baseline.

    {pre}
    (none)

    {post}
    Returns [Ok ()] iff the backend is not built-in, the version is
    unparseable, or [installed >= baseline].

    {violators}
    (none)

    {violates}
    (none) *)
val run_gate_for_output :
  backend_name:string -> version_output:string -> (unit, string) result

(** [run_version_gate ~env ~backend_name] detects the installed version of
    [backend_name] by running its [--version] command and then calls
    {!run_gate_for_output}.

    Fail-safe: returns [Ok ()] when the binary is absent or produces no
    parseable output, so missing tools never crash the startup check.

    {pre}
    (none)

    {post}
    Returns [Ok ()] when the gate passes or is skipped.  Returns
    [Error msg] only when the binary is present, its version is parseable,
    and the version is below the baseline.

    {violators}
    (none)

    {violates}
    (none) *)
val run_version_gate :
  env:Eio_unix.Stdenv.base -> backend_name:string -> (unit, string) result

(** [make_by_name ~sw ~env ~backend_name ~working_dir ?model ?mcp_servers ()]
    performs only side-effect-free routing-id validation at construction, then
    creates a completer whose every invocation resolves, validates, version- and
    availability-checks, preflights, and invokes [backend_name] through
    {!Runtime_dispatch.run_task}. No runtime lookup or adapter command occurs at
    construction. A later registry override is therefore used by the next call
    rather than the backend present at construction time.

    The legacy completer type has no attachment or web-policy parameters, so
    this wrapper deliberately constructs attachment-free, [Web_disabled] tasks
    and applies a private zero-attachment preflight policy. This is a wrapper
    constraint, not a Cabal library default.

    @param model Optional model to pass to the backend (e.g., "opus", "sonnet")
    @param mcp_servers Optional MCP servers to make available to the backend
    Returns [Error msg] at construction only for a malformed routing id.

    {pre}
    The switch [sw] must be active when the returned completer is invoked.

    {post}
    Returns [Ok completer] for a structurally valid id. Dynamic registration,
    consistency, version, availability, and preflight failures are reported by
    the returned completer at invocation time.

    {violators}
    (none)

    {violates}
    (none) *)
val make_by_name :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  backend_name:string ->
  working_dir:string ->
  ?model:string ->
  ?mcp_servers:Backend_types.mcp_server_config list ->
  unit ->
  (completer, string) result

(** [check_read_only_routing ~backend_name ()] is the canonical read-only
    routing gate for validator tasks (Story #517, AC3).

    Returns [Ok ()] when [backend_name] declares [read_only_support = true]
    in the registry (built-in or registered via
    {!Backend_registry.register_descriptor}).  Fail-closed: unregistered
    backends return [Error] and are never granted validator access.

    Returns [Error msg] when [backend_name] is a known backend that lacks
    [read_only_support].  The error names the requested backend, lists
    available read-only-safe alternatives, and references the optional
    [~role_str] label (default ["validator"]) in the message.

    Both {!make_validator_by_name} and the high-level routing call-site in
    [Build_flow_run] call this function, so the check cannot be bypassed by
    going through either path alone.

    {pre}
    (none)

    {post}
    Returns [Ok ()] iff [backend_name] passes the read-only gate.

    {violators}
    (none)

    {violates}
    (none) *)
val check_read_only_routing :
  ?role_str:string -> backend_name:string -> unit -> (unit, string) result

(** [make_validator_by_name ~sw ~env ~backend_name ~working_dir ?model
    ?mcp_servers ()] creates a completer for validator tasks, enforcing that
    the selected backend declares [read_only_support = true].

    The returned completer resolves the backend at each invocation through
    {!Runtime_dispatch} and submits [task_spec.read_only = true]. It shares the
    legacy attachment-free/[Web_disabled] constraint of {!make_by_name}.

    Calls {!check_read_only_routing} internally — the same gate used by the
    high-level routing layer — so enforcement is consistent at both the
    routing and invocation-boundary levels (Story #517, AC3).

    Returns [Error msg] immediately (without spawning any process) when
    [backend_name] has [read_only_support = false].  The error message names
    the requested backend and lists available read-only-safe alternatives.

    {pre}
    The switch [sw] must be active when the returned completer is later invoked.

    {post}
    Returns [Error msg] at construction when [backend_name] lacks
    [read_only_support]. Dynamic registration/version/availability failures are
    returned only when the completer is invoked. Returns [Ok completer]
    otherwise.

    {violators}
    (none)

    {violates}
    (none) *)
val make_validator_by_name :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  backend_name:string ->
  working_dir:string ->
  ?model:string ->
  ?mcp_servers:Backend_types.mcp_server_config list ->
  unit ->
  (completer, string) result
