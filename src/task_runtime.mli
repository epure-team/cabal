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
    the same value. *)
val await : t -> (Backend_types.task_result, Runtime_dispatch.error) result

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
