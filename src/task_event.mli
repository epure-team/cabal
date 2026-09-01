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

(** Attempt kinds within one task deadline, shared with
    {!Backend_types.attempt_kind} so detailed executions and normalized events
    cannot diverge. *)
type attempt_kind = Backend_types.attempt_kind =
  | Initial_attempt
  | Fresh_attempt
  | Resumed_attempt

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

(** Bounded summary of callback-delivery omissions. [agent_text_bytes] counts
    omitted bytes, including bytes removed from a partially retained delta.
    Every field is a saturating non-negative count and contains no raw data. *)
type delivery_truncation = {
  agent_text_events : int;
  agent_text_bytes : int;
  token_usage_events : int;
  session_events : int;
  tool_events : int;
  control_events : int;
}

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
  | Event_delivery_truncated of delivery_truncation
  | Terminal of terminal

(** One event in a task-local sequence. [timestamp] is monotonic elapsed time
    in seconds since task start. Delivered sequence numbers are strictly
    increasing but may have gaps for omitted observations or abusive excess
    controls. The first omission or partial text truncation enqueues one marker
    immediately after the affected event/gap; that marker accumulates later
    omissions until the drain takes it, bounding marker state even when data and
    controls alternate. *)
type t = {seq : int; attempt : int; timestamp : float; payload : payload}

(** Opaque task-local event sequencer. *)
type sink

(** Maximum callback events waiting behind the currently executing callback. *)
val max_pending_events : int

(** Maximum accumulated bytes retained by pending {!Agent_text_delta} events. *)
val max_pending_agent_text_bytes : int

(** Maximum bytes retained from any one {!Agent_text_delta}. Longer values are
    reduced to a prefix and reported by {!Event_delivery_truncated}. *)
val max_agent_text_delta_bytes : int

(** Maximum pending observational events. The remainder of
    {!max_pending_events} is statically reserved for lifecycle controls, one
    truncation marker, and the terminal event. *)
val max_pending_observational_events : int

(** Number of pending slots reserved for controls. The canonical hard
    two-attempt path with one owned process per attempt uses 21 non-terminal
    controls; the larger ordinary cap leaves adapter headroom plus one dedicated
    marker slot and one terminal slot. Abusive excess controls are summarized
    rather than retained. *)
val control_event_reserve : int

(** [create_sink ~sw ~now ?on_event ()] creates a sequencer and, when a
    callback is supplied, one task-local callback-drain fiber. [now] must return
    monotonic elapsed seconds. Callback exceptions are isolated and reported
    through [Diagnostics] without their exception text.

    Pending delivery is bounded by {!max_pending_events},
    {!max_pending_observational_events}, and
    {!max_pending_agent_text_bytes}. Producers never wait for callback progress.
    A callback must eventually return for delivery completion to resolve. *)
val create_sink :
  sw:Eio.Switch.t ->
  now:(unit -> float) ->
  ?on_event:(t -> unit) ->
  unit ->
  sink

(** Emit a non-terminal payload unless the task is already terminal. Emission
    must occur inside an Eio execution context. Under callback backpressure,
    public text/usage/session/tool observations may be omitted and summarized by
    {!Event_delivery_truncated}. Legitimate lifecycle controls use a separate
    static reserve. *)
val emit : sink -> payload -> unit

(** Emit an attempt-start event. The initial attempt remains attempt 1. *)
val begin_attempt : sink -> attempt_kind -> unit

(** Emit the outcome paired with the current attempt. *)
val finish_attempt : sink -> attempt_outcome -> unit

(** Emit a retry transition, increment the attempt, and emit its start event. *)
val transition_to_retry : sink -> kind:retry_kind -> reason:string -> unit

(** Enqueue the task's terminal event after every previously retained event.
    Subsequent emissions are ignored. Callback delivery is asynchronous; use
    {!Task_runtime.await_event_delivery} after task await when the callback must
    have observed the terminal event. *)
val emit_terminal : sink -> terminal -> unit

(** Internal delivery state used by {!Task_runtime}. *)
module Private : sig
  type pending_delivery = {event_count : int; agent_text_bytes : int}

  (** Resolves after the terminal callback returns. It resolves immediately
      after terminal enqueue when the sink has no callback. *)
  val delivery_complete : sink -> unit Eio.Promise.t

  (** Current retained queue size, excluding the callback currently running.
      Exposed for deterministic bound tests. *)
  val pending_delivery : sink -> pending_delivery
end
