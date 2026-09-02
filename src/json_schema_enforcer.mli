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

    {b Native path} (Story #625, when [backend] declares
    [native_json_schema_output = true]): the schema is already present in
    [spec.json_schema]; the backend's own [run_task] wires it to the CLI
    flag.  The validate-and-retry loop is NOT executed.  Any [Failed] result
    is returned as [Error] immediately; no fallback is performed (D-5).  The
    error does not claim the schema was at fault — this path is reached for
    any non-zero exit while a schema was in force — and it carries the
    backend's own stderr, bounded, so the caller can tell which it was.

    {b Validate-and-retry path} (Story #624, when [backend] declares
    [native_json_schema_output = false]): the enforcer validates
    [task_result.agent_text] and makes at most one corrective re-invocation on
    failure.

    {b Retry budget.}  Hard cap of two backend calls per [run_task] invocation
    (validate-and-retry path only).  No backoff, no configurable budget, no
    internal timeout beyond [task_spec.timeout]. Under the central
    {!Task_runtime} path, one absolute deadline is shared by both attempts;
    direct low-level calls retain their historical per-backend-call behavior.
    Callers wanting more attempts must call [run_task] again themselves.

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

(** [render_error error] renders a detailed schema-enforcement error with the
    exact compatibility text used by {!run_task}. Double failures retain the
    historical [Attempt 1] and [Attempt 2] labels. Native failures retain the
    historical bounded-stderr suffix. This renderer never includes attachment
    metadata, prompts, or task-result output beyond that existing native stderr
    compatibility surface. *)
val render_error : task_execution_error -> string

(** [run_task_detailed ~sw ~env ?context ?on_raw_line ~backend spec] executes
    [spec] and returns ordered attempt telemetry.

    This is the detailed low-level counterpart of {!run_task}. It preserves the
    same native/pass-through/validate-and-retry routing and the hard maximum of
    two backend calls. Every backend call that returns a [task_result] produces
    exactly one {!Backend_types.task_attempt}; a retry suppressed before
    invocation by the shared deadline or cancellation produces no attempt.

    [task_attempt.attempt_elapsed] and [task_execution.total_elapsed] use
    [env]'s monotonic clock. Total elapsed is measured once around the complete
    enforcer, including resume-failure classification, and is not a sum. Costs
    are aggregated with {!Backend_types.aggregate_costs}, and the final session
    is the last nonblank trimmed session id.

    Initial and fresh attempts request [Upload_attachments]. Resumed attempts
    request [Reuse_session_attachments] while retaining attachment
    references/digests and [web_access] in both their task spec and detailed
    delivery telemetry. The same requested intent is installed in [context]
    before the backend invocation for transport inspection. It is not proof of
    preflight approval, content loading, or transport compliance. When [context]
    has a bounded absolute deadline, the
    current remaining duration replaces the original timeout in the retry spec;
    an expired deadline suppresses the retry transition and backend call.

    Resume is selected only for a nonblank trimmed session id. A failed invoked
    resume is returned as structured [Schema_retry_failed] with [Resume_failure].
    If backend resume-failure classification raises an ordinary exception, the
    completed result is conservatively [Transport_failure] and a sanitized
    diagnostic is emitted; cancellation and fatal runtime exceptions propagate.
    A failed resume never triggers an automatic third fresh call. Under the
    two-call cap, a caller wanting that fresh fallback starts a new enforcer
    invocation; that new initial/fresh attempt requests attachment upload.

    [backend] must be available and [sw] must remain open for the invocation.
    The function guarantees:

    - [json_schema = None] and native-schema paths invoke exactly once.
    - Validate-and-retry invokes once or twice, never three times.
    - [Ok execution] selects the successful final attempt when validation
      succeeds, or preserves an existing propagated non-success/deadline result.
    - [Error detail] retains all completed attempts and classifies native,
      validator, backend transport, and recognized resume failures. *)
val run_task_detailed :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?context:Task_execution_context.t ->
  ?on_raw_line:(string -> unit) ->
  backend:Agentic_backend.t ->
  task_spec ->
  (task_execution, task_execution_error) result

(** Internal cancellation-safe attempt progress used by central dispatch. *)
module Private : sig
  type progress

  val create_progress : unit -> progress

  (** Immutable snapshot of every backend result committed so far, in
      invocation order. An in-flight call is never included. *)
  val completed_attempts : progress -> task_attempt list

  (** Detailed execution with caller-owned progress. Every returned backend
      result is paired with its attempt-finished event and committed to
      [progress] inside one narrow cancellation-protected section. *)
  val run_task_detailed :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    ?context:Task_execution_context.t ->
    ?on_raw_line:(string -> unit) ->
    progress:progress ->
    backend:Agentic_backend.t ->
    task_spec ->
    (task_execution, task_execution_error) result
end

(** [run_task ~sw ~env ?context ?on_raw_line ~backend spec] executes [spec] with
    optional JSON schema enforcement.

    This remains a low-level compatibility API. It intentionally accepts an
    explicit backend snapshot and does not resolve registries or run CBL-03
    preflight. Hosts should normally call [Runtime_dispatch.run_task]; direct
    use is appropriate only when the caller owns those checks. It is implemented
    solely as a projection of {!run_task_detailed}; detailed errors are rendered
    through {!render_error}.

    {pre}
    [backend] must be available.  [sw] must be an open switch with sufficient
    lifetime for up to two backend calls.

    {post}
    - When [spec.json_schema = None]: exactly one backend call; returns
      [Ok result] identical to calling [Agentic_backend.run_task] directly.
    - When [spec.json_schema = Some _] and backend declares
      [native_json_schema_output = true] (native path): exactly one backend
      call; returns [Ok result] on [Success]/[Timeout]/[Cancelled], or
      [Error "native-backend call failed with a schema in force: <msg>"]
      (plus the backend's bounded stderr) on [Failed] — no retry.
    - When [spec.json_schema = Some _] and backend declares
      [native_json_schema_output = false] (validate-and-retry path):
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
  ?context:Task_execution_context.t ->
  ?on_raw_line:(string -> unit) ->
  backend:Agentic_backend.t ->
  task_spec ->
  (task_result, string) result
