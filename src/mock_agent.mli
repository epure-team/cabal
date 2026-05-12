(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Mock agent backend for integration tests.

    Returns scripted, deterministic responses from a fixture file without
    making any real LLM calls.  The fixture file path is read from the
    [EPURE_MOCK_AGENT_FIXTURES] environment variable.

    This backend is registered through the normal {!Registry} path so that
    backend-selection bugs remain detectable in tests.  It is only
    {!available} when the env var points to an existing fixture file; in all
    other environments it silently remains unavailable.

    Fixture file format (JSON):
    {[
      { "rules": [
          { "contains": "keyword",
            "stdout":   "canned response text",
            "status":   "success",
            "limit":    1 }
        ] }
    ]}
    Rules are matched in order; the first rule whose [contains] substring is
    found (case-insensitive) in the incoming prompt wins.  An empty [contains]
    field matches every prompt (useful as a catch-all last rule).  When no
    rule matches the test fails immediately with a diagnostic message.

    The optional [limit] field (default 0 = unlimited) caps how many times a
    rule may fire within a single process invocation.  Once a rule's call
    count reaches its limit it is skipped and matching continues with the
    next rule.  This enables reject→retry→success scenarios without needing
    role-aware prompt routing.

    The [status] field must be exactly ["success"] or ["failed"]; any other
    value (including the empty string produced by a missing field) causes an
    immediate test failure with a diagnostic (QG-5). *)

(** Registry identifier: ["mock-agent"]. *)
val id : string

(** Human-readable name. *)
val name : string

(** Returns [true] iff [EPURE_MOCK_AGENT_FIXTURES] names an existing file. *)
val available : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> bool

(** Query whether the backend supports resumed sessions.

    {pre}
    The caller requests whether mock-agent sessions can resume.

    {post}
    The value is always [false], because every mock-agent session starts fresh.

    {violators}
    (none)

    {violates}
    (none)
 *)
val supports_session_resume : bool

(** Classify whether a task result is a resume-specific failure.

    {pre}
    The caller provides a task result for inspection.

    {post}
    The value is always [false], because the mock backend never reports
    backend-specific resume failures.

    {violators}
    (none)

    {violates}
    (none)
 *)
val is_resume_failure : Backend_types.task_result -> bool

(** Report that mock-agent has no project-owned config validation surface.

    {pre}
    The caller may pass any open switch, environment, project root, and setup
    result; mock-agent does not own generated project config artifacts.

    {post}
    Returns [Config_check_unsupported reason] with a non-empty reason and does
    not inspect or mutate project files.

    {violators}
    (none)

    {violates}
    (none) *)
val check_project_config :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  project_dir:string ->
  setup_result:Backend_config_writer.setup_result ->
  Agentic_backend.config_check_result

(** Match [task_spec.prompt] against fixture rules and return the canned
    result.  Fails the task (with a diagnostic) when no rule matches. *)
val run_task :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  Backend_types.task_result
