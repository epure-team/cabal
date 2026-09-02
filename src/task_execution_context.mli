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

(** Mark that a structured backend parser is authoritative for result text. Its
    non-empty final text is suppressed until {!mark_final_public_text}. *)
val claim_structured_text : t -> unit

(** Whether a structured parser claimed authority over public text events. *)
val structured_text_claimed : t -> bool

(** Mark the current [task_result.agent_text] as having been produced by the
    authoritative public-output parser rather than raw-output fallback. *)
val mark_final_public_text : t -> unit

(** Whether final result text was proven public by the authoritative parser. *)
val final_public_text : t -> bool

(** [requested_delivery context] returns the attachment/web delivery intent
    selected for the backend call currently being invoked. Schema enforcement
    sets it before every backend invocation. Future media-aware transports may
    inspect it to distinguish initial/fresh attachment upload from
    resumed-session reuse.

    This is requested intent, not proof that a transport honored the request and
    not a substitute for attachment preflight. It is [None] before an enforcer
    has selected an attempt policy. *)
val requested_delivery : t -> Backend_types.attempt_delivery option

(** [authorized_attachment_paths context ...] returns the sealed transport paths
    installed by central dispatch exactly when backend identity, attachment
    references, and web policy match the immutable authorization snapshot.
    Owner release atomically and permanently invalidates the snapshot before
    physical deletion starts, including when deletion fails and is retried.
    Returned paths are backend-transport data and must never be logged,
    serialized, or included in diagnostics. *)
val authorized_attachment_paths :
  t ->
  backend_id:string ->
  attachment_references:Backend_types.media_attachment list ->
  web_access_policy:Backend_types.web_access ->
  (string list, string) result

(** Delivery-aware sealed transport input selected for one backend attempt. *)
type sealed_delivery = {
  attachment_delivery : Backend_types.attachment_delivery;
  attachment_paths : string list;
}

(** [sealed_attachment_delivery context ...] validates the current requested
    delivery against the immutable central authorization. Upload attempts return
    the exact sealed list; session-reuse attempts return [[]]. Adapters should
    prefer this accessor over independently combining {!requested_delivery} and
    {!authorized_attachment_paths}. It rejects every access after owner release,
    regardless of the physical cleanup outcome. *)
val sealed_attachment_delivery :
  t ->
  backend_id:string ->
  attachment_references:Backend_types.media_attachment list ->
  web_access_policy:Backend_types.web_access ->
  (sealed_delivery, string) result

module Private : sig
  (** Enforcer-owned setter. Backend transports should use {!requested_delivery}
      and must not rewrite the selected policy. *)
  val set_requested_delivery : t -> Backend_types.attempt_delivery -> unit

  (** Install the central immutable transport authorization exactly once.
      Released prepared inputs cannot be authorized again. *)
  val authorize_transport :
    t ->
    backend_id:string ->
    attachment_references:Backend_types.media_attachment list ->
    web_access_policy:Backend_types.web_access ->
    prepared_inputs:Task_preflight.prepared_inputs ->
    (unit, string) result
end
