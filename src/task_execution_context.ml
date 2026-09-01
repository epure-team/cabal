(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type t = {
  remaining_time : unit -> float option;
  event_sink : Task_event.sink;
  agent_text_emitted : bool Atomic.t;
  session_id_emitted : bool Atomic.t;
  token_usage_emitted : bool Atomic.t;
  structured_text_claimed : bool Atomic.t;
  final_public_text : bool Atomic.t;
  requested_delivery : Backend_types.attempt_delivery option Atomic.t;
}

let create ~remaining_time event_sink =
  {
    remaining_time;
    event_sink;
    agent_text_emitted = Atomic.make false;
    session_id_emitted = Atomic.make false;
    token_usage_emitted = Atomic.make false;
    structured_text_claimed = Atomic.make false;
    final_public_text = Atomic.make false;
    requested_delivery = Atomic.make None;
  }

let remaining_time context = context.remaining_time ()

let deadline_expired context =
  match remaining_time context with Some seconds -> seconds <= 0.0 | None -> false

let emit context payload =
  (match payload with
  | Task_event.Agent_text_delta _ -> Atomic.set context.agent_text_emitted true
  | Task_event.Session_id _ -> Atomic.set context.session_id_emitted true
  | Task_event.Token_usage _ -> Atomic.set context.token_usage_emitted true
  | _ -> ()) ;
  Task_event.emit context.event_sink payload

let begin_attempt context kind = Task_event.begin_attempt context.event_sink kind

let finish_attempt context outcome =
  Task_event.finish_attempt context.event_sink outcome

let transition_to_retry context ~kind ~reason =
  Task_event.transition_to_retry context.event_sink ~kind ~reason

let agent_text_emitted context = Atomic.get context.agent_text_emitted

let session_id_emitted context = Atomic.get context.session_id_emitted

let token_usage_emitted context = Atomic.get context.token_usage_emitted

let claim_structured_text context = Atomic.set context.structured_text_claimed true

let structured_text_claimed context = Atomic.get context.structured_text_claimed

let mark_final_public_text context = Atomic.set context.final_public_text true

let final_public_text context = Atomic.get context.final_public_text

let requested_delivery context = Atomic.get context.requested_delivery

module Private = struct
  let set_requested_delivery context delivery =
    Atomic.set context.requested_delivery (Some delivery)
end
