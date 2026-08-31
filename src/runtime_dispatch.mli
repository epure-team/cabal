(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Central validated task invocation.

    Every call resolves one bound {!Registry.Validated} entry snapshot. Raw
    runtime-only registrations are rejected and no independent descriptor lookup
    can lend static claims to an override. Dispatch applies the caller's explicit
    {!Task_preflight.limits}, checks the entry's effective capabilities, applies
    its installed-version policy, checks availability, and only then invokes
    {!Json_schema_enforcer.run_task}. Whole-entry replacements installed after a
    dispatcher/completer is constructed are visible on the next invocation. The
    resolved entry is retained across the complete schema-enforcement attempt,
    including retry. *)

(** Typed invocation failure. *)
type error =
  | Invalid_timeout
  | Backend_not_registered
  | Runtime_registration_untrusted
  | Preflight_failed of Task_preflight.error
  | Backend_version_unsupported
  | Version_check_failed
  | Backend_unavailable
  | Availability_check_failed
  | Backend_execution_failed
  | Schema_enforcement_failed of string
      (** Untrusted low-level error. Use {!render_error}, which redacts it,
          rather than displaying this payload directly. *)

(** [render_error error] produces a sanitized diagnostic. Preflight rendering
    inherits {!Task_preflight.render_error}'s path/digest/byte exclusion;
    low-level schema-enforcement strings are redacted before display. Ordinary
    availability, version-probe, and execution exceptions carry no untrusted
    payload in their typed errors. *)
val render_error : error -> string

(** Immutable dispatch snapshot validated against one resolved registry entry. *)
type prepared

(** Resolve, preflight, version-check, and availability-check a task exactly
    once, returning the entry snapshot used by execution and retries. *)
val prepare :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?context:Task_execution_context.t ->
  Backend_types.task_spec ->
  (prepared, error) result

(** Execute a prepared task without consulting mutable registry state again. *)
val execute_prepared :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?context:Task_execution_context.t ->
  ?on_raw_line:(string -> unit) ->
  prepared ->
  (Backend_types.task_result, error) result

(** Internal handle primitives used by {!Task_runtime}. Hosts should prefer the
    named facade there; this submodule exists to keep the compatibility
    {!run_task} entry point implemented through the same handle state machine. *)
module Private : sig
  type task_handle

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
end

(** [run_task ~sw ~env ~limits ~backend_id spec] performs central resolution,
    of one validated entry, input/capability preflight, entry-specific
    installed-version policy, availability checks, and schema-enforced
    execution. Under {!Runtime_entry.Enforce_baseline}, parseable installed
    versions below the effective descriptor baseline fail before backend task
    execution; missing or unparseable output skips only the comparison. Under
    {!Runtime_entry.No_version_gate}, stability probing/comparison is skipped.
    Availability must still pass under both policies.

    [limits] is mandatory caller policy; Cabal supplies no product default.
    Preflight failures happen before any version process spawn or availability
    side effect. Validation, preflight, version, and availability failures all
    happen before [backend.run_task], project config generation, or the task
    process spawn. Ordinary operational/backend exceptions become sanitized
    typed errors. Eio cancellation is normalized to a [Cancelled] task result
    after cleanup; [Out_of_memory], [Stack_overflow], and [Sys.Break] are
    re-raised by handle await.

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
