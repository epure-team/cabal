(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Cancellable task handles over central validated runtime dispatch. *)

(** Opaque running or completed task handle. *)
type t

(** [start_task] starts one owner fiber under a private cancellation scope.
    The caller switch remains the parent scope, so its cancellation propagates
    to the task. Each handle is isolated from sibling handles. *)
val start_task :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  t

(** Request cancellation. Safe and idempotent before, during, or after task
    completion. *)
val cancel : t -> unit

(** Await the memoized terminal result. Concurrent and repeated calls return
    the same value. This never waits for event callbacks. It is safe to invoke
    from this handle's own callback, including process and terminal callbacks. *)
val await : t -> (Backend_types.task_result, Runtime_dispatch.error) result

(** Await the memoized CBL-05 execution or structured central failure. Like
    {!await}, this never waits for event callbacks and is safe to invoke from
    this handle's own callback. Repeated and concurrent calls return the same
    value. *)
val await_detailed : t -> Runtime_dispatch.detailed_outcome

(** Wait until the terminal event callback has returned and all earlier queued
    callbacks have completed. Call this after {!await} when deterministic event
    delivery is required. Do not call it from this handle's own callback. *)
val await_event_delivery : t -> unit

(** Synchronous compatibility entry point implemented through a task handle. *)
val run_task :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  (Backend_types.task_result, Runtime_dispatch.error) result

(** Synchronous central detailed entry point. This starts and awaits the same
    handle as {!run_task}, retaining ordered attempts and structured execution
    errors. Callback delivery is still independent; use {!start_task},
    {!await_detailed}, then {!await_event_delivery} when all normalized events
    must be included in a response. *)
val run_task_detailed :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  Runtime_dispatch.detailed_outcome
