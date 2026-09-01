(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Normalized, backend-neutral task lifecycle events.

    Events contain only public assistant output and bounded operational
    metadata. Raw backend stream lines remain available exclusively through
    the legacy [on_raw_line] callback. *)

(** A normalized tool identity. Raw tool arguments are deliberately omitted. *)
type tool = {id : string option; name : string}

(** Attempt kinds within one task deadline. *)
type attempt_kind = Initial_attempt | Fresh_attempt | Resumed_attempt

(** Stable transport-level outcome of one invoked backend attempt. *)
type attempt_outcome =
  | Attempt_succeeded
  | Attempt_failed
  | Attempt_timed_out
  | Attempt_cancelled

(** Retry transitions selected by schema enforcement. *)
type retry_kind = Fresh_retry | Resume_retry

(** Exactly one terminal outcome is emitted for a task. *)
type terminal = Succeeded | Failed of string | Timed_out | Cancelled

(** Safe normalized event payloads. *)
type payload =
  | Task_started
  | Backend_selected of {backend_id : string}
  | Preflight_started
  | Preflight_completed
  | Version_probe_started
  | Version_probe_completed
  | Availability_check_started
  | Availability_check_completed
  | Attempt_started of attempt_kind
  | Attempt_finished of attempt_outcome
  | Retry_transition of {kind : retry_kind; reason : string}
  | Process_started of {pid : int option}
  | Process_termination_requested
  | Process_kill_escalated
  | Process_exited of {exit_status : string}
  | Session_id of string
  | Agent_text_delta of string
  | Tool_started of tool
  | Tool_finished of {id : string option; name : string option}
  | Token_usage of Backend_types.cost
  | Terminal of terminal

(** One event in a task-local sequence. [timestamp] is monotonic elapsed time
    in seconds since task start. *)
type t = {seq : int; attempt : int; timestamp : float; payload : payload}

(** Opaque task-local event sequencer. *)
type sink

(** [create_sink ~sw ~now ?on_event ()] creates a sequencer and, when a
    callback is supplied, one task-local callback-drain fiber. [now] must return
    monotonic elapsed seconds. Callback exceptions are isolated and reported
    through [Diagnostics] without their exception text. *)
val create_sink :
  sw:Eio.Switch.t ->
  now:(unit -> float) ->
  ?on_event:(t -> unit) ->
  unit ->
  sink

(** Emit a non-terminal payload unless the task is already terminal. Emission
    must occur inside an Eio execution context. *)
val emit : sink -> payload -> unit

(** Emit an attempt-start event. The initial attempt remains attempt 1. *)
val begin_attempt : sink -> attempt_kind -> unit

(** Emit the outcome paired with the current attempt. *)
val finish_attempt : sink -> attempt_outcome -> unit

(** Emit a retry transition, increment the attempt, and emit its start event. *)
val transition_to_retry : sink -> kind:retry_kind -> reason:string -> unit

(** Enqueue the task's terminal event after every previously accepted event.
    Subsequent emissions are ignored. Callback delivery is asynchronous; use
    {!Task_runtime.await_event_delivery} after task await when the callback must
    have observed the terminal event. *)
val emit_terminal : sink -> terminal -> unit

(** Internal delivery state used by {!Task_runtime}. *)
module Private : sig
  (** Resolves after the terminal callback returns. It resolves immediately
      after terminal enqueue when the sink has no callback. *)
  val delivery_complete : sink -> unit Eio.Promise.t
end
