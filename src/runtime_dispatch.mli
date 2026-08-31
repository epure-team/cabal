(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Central validated task invocation.

    Every call resolves the named runtime backend and same-id descriptor from
    the current registries, validates their final consistency, enforces the
    installed-version baseline and availability, applies the caller's explicit
    {!Task_preflight.limits}, checks requested capabilities, and only then invokes
    {!Json_schema_enforcer.run_task}. Registry overrides installed after a
    dispatcher/completer is constructed are therefore visible on the next
    invocation. The resolved first-class backend is retained as one snapshot for
    the complete schema-enforcement attempt, including retry. *)

(** Typed invocation failure. *)
type error =
  | Backend_not_registered
  | Descriptor_not_registered
  | Runtime_descriptor_invalid of Runtime_bootstrap.validation_error
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

(** [run_task ~sw ~env ~limits ~backend_id spec] performs central resolution,
    consistency validation, installed-version and availability checks,
    input/capability preflight, and schema-enforced execution. Parseable
    installed versions below the descriptor baseline fail before backend task
    execution. Missing or unparseable version output preserves the established
    compatibility policy and skips only the version comparison; availability
    must still pass.

    [limits] is mandatory caller policy; Cabal supplies no product default.
    Validation, version, availability, and preflight failures happen before
    [backend.run_task], project config generation, or the task process spawn.
    Ordinary probe/backend exceptions become sanitized typed errors. Eio
    cancellation is always re-raised.

    Direct use of {!Agentic_backend.run_task} or
    {!Json_schema_enforcer.run_task} remains source-compatible but bypasses
    these CBL-03 guarantees. *)
val run_task :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  (Backend_types.task_result, error) result
