(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Backend-neutral execution context shared by one task and all retries. *)

(** Opaque context carrying the task-local event sink and absolute deadline. *)
type t

(** Construct a context. [remaining_time] returns [None] for an unbounded task
    and otherwise the non-negative seconds left on its absolute deadline. *)
val create : remaining_time:(unit -> float option) -> Task_event.sink -> t

(** Return the current remaining time on the shared absolute deadline. *)
val remaining_time : t -> float option

(** Whether the shared deadline has expired. *)
val deadline_expired : t -> bool

(** Emit a normalized task event. *)
val emit : t -> Task_event.payload -> unit

(** Emit an attempt-start event. *)
val begin_attempt : t -> Task_event.attempt_kind -> unit

(** Emit the transport-level outcome of the current backend attempt. *)
val finish_attempt : t -> Task_event.attempt_outcome -> unit

(** Emit a retry transition and begin its next attempt. *)
val transition_to_retry :
  t -> kind:Task_event.retry_kind -> reason:string -> unit

(** Whether a backend parser already emitted public assistant text. *)
val agent_text_emitted : t -> bool

(** Whether a backend parser already emitted a session identifier. *)
val session_id_emitted : t -> bool

(** Whether a backend parser already emitted token usage. *)
val token_usage_emitted : t -> bool

(** Mark that a structured backend parser is authoritative for result text.
    Its non-empty final text is suppressed until {!mark_final_public_text}. *)
val claim_structured_text : t -> unit

(** Whether a structured parser claimed authority over public text events. *)
val structured_text_claimed : t -> bool

(** Mark the current [task_result.agent_text] as having been produced by the
    authoritative public-output parser rather than raw-output fallback. *)
val mark_final_public_text : t -> unit

(** Whether final result text was proven public by the authoritative parser. *)
val final_public_text : t -> bool
