(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** JSON Schema enforcement — validate-and-retry wrapper around agentic
    backends — Story #624.

    Wraps a backend invocation with optional schema validation.  When the
    caller attaches [task_spec.json_schema = Some schema], the enforcer
    validates [task_result.agent_text] and makes at most one corrective
    re-invocation on failure.

    {b Retry budget.}  Hard cap of two backend calls per [run_task] invocation.
    No backoff, no configurable budget, no internal timeout beyond the existing
    [task_spec.timeout] which applies per attempt.  Callers wanting more
    attempts must call [run_task] again themselves.

    {b Retry paths.}
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

    {pre}
    [backend] must be available.  [sw] must be an open switch with sufficient
    lifetime for up to two backend calls.

    {post}
    - When [spec.json_schema = None]: exactly one backend call; returns
      [Ok result] identical to calling [Agentic_backend.run_task] directly.
    - When [spec.json_schema = Some _] and the first response is valid:
      exactly one backend call; returns [Ok result].
    - When the first response fails validation and the backend supports session
      resume: the session is resumed exactly once; returns [Ok result] when the
      second response is valid, or [Error msg] carrying both validation error
      messages when it also fails.
    - When the first response fails validation and the backend does not support
      session resume: a fresh corrective call is made; same two-call semantics.
    - When both attempts fail validation: returns [Error msg] carrying both
      validation error messages; neither is discarded.

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
