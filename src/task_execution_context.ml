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
  transport_authorization : transport_authorization option Atomic.t;
}

and transport_authorization = {
  backend_id : string;
  attachment_references : Backend_types.media_attachment list;
  web_access_policy : Backend_types.web_access;
  prepared_inputs : Task_preflight.prepared_inputs;
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
    transport_authorization = Atomic.make None;
  }

let remaining_time context = context.remaining_time ()

let deadline_expired context =
  match remaining_time context with
  | Some seconds -> seconds <= 0.0
  | None -> false

let emit context payload =
  (match payload with
  | Task_event.Agent_text_delta _ -> Atomic.set context.agent_text_emitted true
  | Task_event.Session_id _ -> Atomic.set context.session_id_emitted true
  | Task_event.Token_usage _ -> Atomic.set context.token_usage_emitted true
  | _ -> ());
  Task_event.emit context.event_sink payload

let begin_attempt context kind =
  Task_event.begin_attempt context.event_sink kind

let finish_attempt context outcome =
  Task_event.finish_attempt context.event_sink outcome

let transition_to_retry context ~kind ~reason =
  Task_event.transition_to_retry context.event_sink ~kind ~reason

let agent_text_emitted context = Atomic.get context.agent_text_emitted
let session_id_emitted context = Atomic.get context.session_id_emitted
let token_usage_emitted context = Atomic.get context.token_usage_emitted

let claim_structured_text context =
  Atomic.set context.structured_text_claimed true

let structured_text_claimed context = Atomic.get context.structured_text_claimed
let mark_final_public_text context = Atomic.set context.final_public_text true
let final_public_text context = Atomic.get context.final_public_text
let requested_delivery context = Atomic.get context.requested_delivery

let authorization_mismatch =
  "central prepared transport authorization does not match the task"

let authorization_missing =
  "central prepared transport authorization is required"

let authorization_revoked =
  "central prepared transport authorization has been released"

let authorized_attachment_paths context ~backend_id ~attachment_references
    ~web_access_policy =
  match Atomic.get context.transport_authorization with
  | None -> Error authorization_missing
  | Some authorization
    when authorization.backend_id = backend_id
         && authorization.attachment_references = attachment_references
         && authorization.web_access_policy = web_access_policy
         && Task_preflight.Private.active authorization.prepared_inputs ->
      let staged =
        Task_preflight.Private.staged_attachments authorization.prepared_inputs
      in
      if List.map fst staged = attachment_references then
        Ok (List.map snd staged)
      else Error authorization_mismatch
  | Some _ -> Error authorization_mismatch

type sealed_delivery = {
  attachment_delivery : Backend_types.attachment_delivery;
  attachment_paths : string list;
}

let sealed_attachment_delivery context ~backend_id ~attachment_references
    ~web_access_policy =
  match
    authorized_attachment_paths context ~backend_id ~attachment_references
      ~web_access_policy
  with
  | Error _ as error -> error
  | Ok authorized_paths -> (
      match requested_delivery context with
      | None -> Error "central attempt delivery policy is required"
      | Some delivery
        when delivery.Backend_types.attachment_references = attachment_references
             && delivery.web_access_policy = web_access_policy ->
          let attachment_paths =
            match delivery.attachment_delivery with
            | Backend_types.Upload_attachments -> authorized_paths
            | Backend_types.Reuse_session_attachments -> []
          in
          Ok
            {
              attachment_delivery = delivery.attachment_delivery;
              attachment_paths;
            }
      | Some _ -> Error authorization_mismatch)

module Private = struct
  let set_requested_delivery context delivery =
    Atomic.set context.requested_delivery (Some delivery)

  let authorize_transport context ~backend_id ~attachment_references
      ~web_access_policy ~prepared_inputs =
    let authorization =
      { backend_id; attachment_references; web_access_policy; prepared_inputs }
    in
    if not (Task_preflight.Private.active prepared_inputs) then
      Error authorization_revoked
    else
      match Atomic.get context.transport_authorization with
    | Some existing
      when existing.backend_id = backend_id
           && existing.attachment_references = attachment_references
           && existing.web_access_policy = web_access_policy
           && existing.prepared_inputs == prepared_inputs ->
        if Task_preflight.Private.active prepared_inputs then Ok ()
        else Error authorization_revoked
    | Some _ -> Error authorization_mismatch
    | None ->
        if
          Atomic.compare_and_set context.transport_authorization None
            (Some authorization)
        then
          if Task_preflight.Private.active prepared_inputs then Ok ()
          else Error authorization_revoked
        else Error authorization_mismatch
end
