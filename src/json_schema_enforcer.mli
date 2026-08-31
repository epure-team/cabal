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

(** [run_task ~sw ~env ?on_raw_line ~backend spec] executes [spec] with
    optional JSON schema enforcement.

    This remains a low-level compatibility API. It intentionally accepts an
    explicit backend snapshot and does not resolve registries or run CBL-03
    preflight. Hosts should normally call [Runtime_dispatch.run_task]; direct
    use is appropriate only when the caller owns those checks.

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
  ?on_raw_line:(string -> unit) ->
  backend:Agentic_backend.t ->
  task_spec ->
  (task_result, string) result
