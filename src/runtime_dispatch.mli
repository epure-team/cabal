(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Central validated task invocation.

    Every call resolves one bound {!Registry.Validated} entry snapshot. Raw
    runtime-only registrations are rejected and no independent descriptor lookup
    can lend static claims to an override. Dispatch applies the caller's
    explicit {!Task_preflight.limits}, rejects a typed entry quarantine before
    any preflight or process side effect, checks the entry's effective
    capabilities before allocating or reading sealed inputs, applies its
    installed-version policy, checks availability, and only then invokes
    {!Json_schema_enforcer.run_task}. Whole-entry replacements
    installed after a dispatcher/completer is constructed are visible on the
    next invocation. The resolved entry and one sealed attachment set are
    retained across the complete schema-enforcement attempt, including retry,
    then cleaned before outcome delivery. *)

(** Typed invocation failure. *)
type error =
  | Invalid_timeout
  | Backend_not_registered
  | Runtime_registration_untrusted
  | Backend_quarantined of Runtime_entry.quarantine_reason
      (** The resolved validated entry disables central task dispatch. *)
  | Preflight_failed of Task_preflight.error
  | Backend_version_unsupported
  | Version_check_failed
  | Backend_unavailable
  | Availability_check_failed
  | Prepared_already_consumed
  | Backend_execution_failed
  | Schema_enforcement_failed of string
      (** Untrusted low-level error. Use {!render_error}, which redacts it,
          rather than displaying this payload directly. *)

(** Central detailed failure. [Dispatch_failure] covers resolution, preflight,
    version, availability, and protected execution-boundary failures before a
    backend result completes. [Dispatch_failure_with_execution] is the same
    sanitized classification after completed progress exists and retains that
    partial execution. [Execution_failure] retains CBL-05's structured
    schema-enforcement error and all completed attempts. Neither type derives
    serialization. *)
type detailed_error =
  | Dispatch_failure of error
  | Dispatch_failure_with_execution of {
      failure : error;
      execution : Backend_types.task_execution;
    }
      (** Sanitized ordinary central failure after at least one backend result
          completed. [execution] retains only those committed attempts. *)
  | Execution_failure of Backend_types.task_execution_error

(** Result of one central detailed invocation. *)
type detailed_outcome = (Backend_types.task_execution, detailed_error) result

(** [render_error error] produces a sanitized diagnostic. Preflight rendering
    inherits {!Task_preflight.render_error}'s path/digest/byte exclusion;
    low-level schema-enforcement strings are redacted before display. Ordinary
    availability, version-probe, and execution exceptions carry no untrusted
    payload in their typed errors. Event callbacks drain asynchronously in
    sequence order and cannot delay handle outcome resolution. *)
val render_error : error -> string

(** [render_detailed_error error] produces the same sanitized compatibility
    diagnostic as projecting the detailed outcome through {!run_task}. Complete
    task results remain available only through the typed value and are not
    serialized into this string. *)
val render_detailed_error : detailed_error -> string

(** Immutable, one-shot dispatch snapshot validated against one resolved
    registry entry. It owns any sealed attachment artifacts until its sole
    execution or switch release. Owner release permanently revokes transport
    authorization before retryable physical cleanup begins. *)
type prepared

(** Resolve, reject any typed quarantine, validate capabilities, preflight and
    seal attachment bytes,
    version-check, and availability-check a task exactly once, returning the
    entry snapshot used by execution and retries. Quarantine rejection occurs
    immediately after validated entry lookup; capability rejection occurs before
    staging allocation or attachment reads. Abandoned pending values are cleaned
    when [sw] releases; an executing owner retains sole cleanup responsibility. *)
val prepare :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?context:Task_execution_context.t ->
  Backend_types.task_spec ->
  (prepared, error) result

(** Execute a prepared task without consulting mutable registry state again.
    This and {!execute_prepared_detailed} share one atomic claim: exactly one
    call may consume a [prepared], including when it has no attachments. Every
    later or concurrent call returns [Prepared_already_consumed] before backend
    or process side effects. Sealed cleanup uses a fixed bounded retry policy and
    completes before the outcome is returned. Both sealed accessors reject after
    release even when physical cleanup fails. *)
val execute_prepared :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?context:Task_execution_context.t ->
  ?on_raw_line:(string -> unit) ->
  prepared ->
  (Backend_types.task_result, error) result

(** Execute one prepared immutable backend snapshot through
    {!Json_schema_enforcer.run_task_detailed}. Registry state is not consulted
    between attempts. The returned execution reports sanitized central cleanup
    status. Persistent cleanup failure does not replace a non-success terminal
    result, a structured schema-execution failure, or a fatal exception being
    propagated. Shares the one-shot claim documented on {!execute_prepared}. *)
val execute_prepared_detailed :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?context:Task_execution_context.t ->
  ?on_raw_line:(string -> unit) ->
  prepared ->
  detailed_outcome

(** Internal handle primitives used by {!Task_runtime}. Hosts should prefer the
    named facade there; this submodule exists to keep the compatibility
    {!run_task} entry point implemented through the same handle state machine.
*)
module Private : sig
  type task_handle

  val cleanup_retry_limit : int

  (** Deterministic sealed-input lifecycle seam for tests. *)
  val prepare_with_input_hooks :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    limits:Task_preflight.limits ->
    backend_id:string ->
    ?context:Task_execution_context.t ->
    ?on_prepare_inputs:(unit -> unit) ->
    ?on_staging_directory:(string -> unit) ->
    ?on_staged_file:(string -> Unix.file_descr -> unit) ->
    ?on_cleanup_attempt:(unit -> unit) ->
    Backend_types.task_spec ->
    (prepared, error) result

  val start_task_with_input_hooks :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    limits:Task_preflight.limits ->
    backend_id:string ->
    ?on_event:(Task_event.t -> unit) ->
    ?on_raw_line:(string -> unit) ->
    ?on_prepare_inputs:(unit -> unit) ->
    ?on_staging_directory:(string -> unit) ->
    ?on_staged_file:(string -> Unix.file_descr -> unit) ->
    ?on_cleanup_attempt:(unit -> unit) ->
    Backend_types.task_spec ->
    task_handle

  val start_task :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    limits:Task_preflight.limits ->
    backend_id:string ->
    ?on_event:(Task_event.t -> unit) ->
    ?on_raw_line:(string -> unit) ->
    Backend_types.task_spec ->
    task_handle

  val cancel : task_handle -> unit
  val await : task_handle -> (Backend_types.task_result, error) result
  val await_detailed : task_handle -> detailed_outcome
  val await_event_delivery : task_handle -> unit

  (** No-context compatibility projection for {!execute_prepared_detailed}.
      Unlike contextual task handles, a corrective backend Timeout/Cancelled
      remains a schema-enforcement error. *)
  val project_prepared_detailed_outcome :
    detailed_outcome -> (Backend_types.task_result, error) result
end

(** [run_task ~sw ~env ~limits ~backend_id spec] performs central resolution, of
    one validated entry, input/capability preflight, entry-specific
    installed-version policy, availability checks, and schema-enforced
    execution. Under {!Runtime_entry.Enforce_baseline}, parseable installed
    versions below the effective descriptor baseline fail before backend task
    execution; missing or unparseable output skips only the comparison. Under
    {!Runtime_entry.No_version_gate}, stability probing/comparison is skipped.
    Availability must still pass under both policies.

    [limits] is mandatory caller policy; Cabal supplies no product default.
    Attachment size, digest, magic, and staged bytes come from one authorized
    opened descriptor/read. Staged files live outside the workspace in a private
    task directory, are reused across fresh retries, and are retained but not
    uploaded on resumed-session reuse. Preflight failures happen before any
    version process spawn or availability side effect. Validation, preflight,
    version, and availability failures all happen before [backend.run_task],
    project config generation, or the task process spawn. Ordinary
    operational/backend exceptions become sanitized typed errors. Eio
    cancellation is normalized to a [Cancelled] task result after cleanup;
    [Out_of_memory], [Stack_overflow], and [Sys.Break] are re-raised by handle
    await after best-effort enqueue of one generic failed terminal. Process
    cleanup and reaping precede terminal enqueue; callback completion can be
    awaited separately through {!Task_runtime.await_event_delivery}.

    Direct use of {!Agentic_backend.run_task} or
    {!Json_schema_enforcer.run_task} remains source-compatible but bypasses
    these CBL-03 guarantees. *)
val run_task :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  (Backend_types.task_result, error) result

(** [run_task_detailed] is the central structured counterpart of {!run_task}. It
    uses the same cancellable owner, one absolute deadline, normalized event
    sink, validated preflight and version/availability ordering, and one
    prepared immutable entry snapshot for every CBL-05 attempt. Every returned
    backend result is committed together with its attempt-finished event inside
    a narrow cancellation-protected section. An outer timeout or cancellation
    therefore produces a synthetic final status while retaining all fully
    completed attempts, their aggregate cost, last nonblank session, and
    monotonic total elapsed time. An interrupted call is never fabricated as an
    attempt. Event delivery remains asynchronous; callers that need callback
    completion should use a {!Task_runtime} handle and
    {!Task_runtime.await_event_delivery}. *)
val run_task_detailed :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  detailed_outcome
