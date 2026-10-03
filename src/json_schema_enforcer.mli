(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** JSON Schema enforcement — native and validate-and-retry paths — Stories
    #624 and #625.

    Wraps a backend invocation with optional schema validation.  When the
    caller attaches [task_spec.json_schema = Some schema], the enforcer routes
    to one of two paths based on the backend's declared capability:

    {b Native path} (Story #625, when {!native_path_applies} holds — the
    backend declares [native_json_schema_output = true] {i and} its native
    flag can carry this schema; for [codex] that means the schema lies in the
    OpenAI strict subset, see {!Openai_strict_schema}): the schema is already present in
    [spec.json_schema]; the backend's own [run_task] wires it to the CLI
    flag.  The validate-and-retry loop is NOT executed.  The original backend
    result is returned in [Ok], including [Failed], [Timeout], and
    [Cancelled]; callers must inspect its status.  No fallback is performed
    (D-5), because a non-success result can be a schema rejection but can
    equally be a rate limit, network failure, bad flag, or killed process.

    {b Validate-and-retry path} (Story #624, when {!native_path_applies}
    does not hold — including a native backend handed a schema its flag
    would reject): the enforcer validates
    [task_result.agent_text] and makes at most one corrective re-invocation on
    failure.

    {b Retry budget.}  Hard cap of two backend calls per [run_task] invocation
    (validate-and-retry path only).  No backoff, no configurable budget, no
    internal timeout beyond the existing [task_spec.timeout] which applies per
    attempt.  Callers wanting more attempts must call [run_task] again
    themselves.

    {b Validate-and-retry sub-paths.}
    - {i Session-resume path} (when [backend] advertises [session_resume] and
      the first result carries a session id): the session is resumed via
      {!Backend_types.make_resume_task_spec} with a prompt containing only the
      schema block and the compliance instruction; the original prompt is
      omitted.
    - {i Fresh-call path} (otherwise): a new invocation whose prompt contains
      the original prompt, the schema block, and the compliance instruction. *)

open Backend_types

(** Template for the session-resume retry prompt.
    Substituted with [(schema_json, validation_error)] in that order. *)
val resume_retry_template : string

(** Template for the fresh-call retry prompt.
    Substituted with [(original_prompt, schema_json, validation_error)] in
    that order. *)
val fresh_retry_template : string

(** [native_path_applies ~backend schema] decides the route for [schema].

    {pre}
    (none)

    {post}
    [true] iff [Agentic_backend.native_json_schema_output backend] and, when
    [backend] is [codex], [Codex_cli.native_json_schema_accepts schema].
    Pure.

    {violators}
    (none)

    {violates}
    (none) *)
val native_path_applies : backend:Agentic_backend.t -> Yojson.Safe.t -> bool

(** [run_task ~sw ~env ?on_raw_line ~backend spec] executes [spec] with
    optional JSON schema enforcement.

    {pre}
    [backend] must be available.  [sw] must be an open switch with sufficient
    lifetime for up to two backend calls.

    {post}
    - When [spec.json_schema = None]: exactly one backend call; returns
      [Ok result] identical to calling [Agentic_backend.run_task] directly.
    - When [spec.json_schema = Some s] and [native_path_applies ~backend s]
      (native path): exactly one backend
      call; returns [Ok result] with the backend's original status, including
      [Failed], [Timeout], and [Cancelled] — no retry.
    - When [spec.json_schema = Some s] and not [native_path_applies ~backend s]
      (validate-and-retry path):
      exactly one call when the first response is valid; otherwise at most one
      corrective re-invocation.
    - When both validate-and-retry attempts fail validation: returns [Error msg]
      carrying both validation error messages; neither is discarded.

    {violators}
    (none)

    {violates}
    (none) *)
val run_task :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?on_raw_line:(string -> unit) ->
  backend:Agentic_backend.t ->
  task_spec ->
  (task_result, string) result
