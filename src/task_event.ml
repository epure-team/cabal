(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type tool = {id : string option; name : string}

type attempt_kind = Initial_attempt | Fresh_attempt | Resumed_attempt

type retry_kind = Fresh_retry | Resume_retry

type terminal = Succeeded | Failed of string | Timed_out | Cancelled

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

type t = {seq : int; attempt : int; timestamp : float; payload : payload}

type sink = {
  now : unit -> float;
  on_event : (t -> unit) option;
  lock : Eio.Mutex.t;
  mutable seq : int;
  mutable attempt : int;
  mutable terminal : bool;
}

let create_sink ~now ?on_event () =
  {now; on_event; lock = Eio.Mutex.create (); seq = 0; attempt = 1; terminal = false}

let report_callback_exception () =
  try Diagnostics.warn "task event callback raised; exception details omitted"
  with _ -> ()

let sanitize_identifier value =
  let safe_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  if
    value <> "" && String.length value <= 128
    && String.for_all safe_character value
  then value
  else Printf.sprintf "[redacted:%d chars]" (String.length value)

let redact_payload = function
  | Backend_selected {backend_id} ->
      Backend_selected {backend_id = sanitize_identifier backend_id}
  | Session_id id -> Session_id (sanitize_identifier id)
  | Tool_started {id; name} ->
      Tool_started
        {
          id = Option.map sanitize_identifier id;
          name = sanitize_identifier name;
        }
  | Tool_finished {id; name} ->
      Tool_finished
        {
          id = Option.map sanitize_identifier id;
          name = Option.map sanitize_identifier name;
        }
  | Retry_transition {kind; reason} ->
      Retry_transition
        {kind; reason = Backend_event_redaction.redact_error_message reason}
  | Terminal (Failed reason) ->
      Terminal
        (Failed (Backend_event_redaction.redact_error_message reason))
  | payload -> payload

let make_event_unlocked sink payload =
  sink.seq <- sink.seq + 1 ;
  {
    seq = sink.seq;
    attempt = sink.attempt;
    timestamp = max 0.0 (sink.now ());
    payload = redact_payload payload;
  }

let notify sink event =
  match sink.on_event with
  | None -> ()
  | Some callback -> (
      try callback event with _ -> report_callback_exception ())

let with_lock sink f =
  Eio.Mutex.use_rw ~protect:true sink.lock f

let emit sink payload =
  let event =
    with_lock sink (fun () ->
      if not sink.terminal then
        match payload with
        | Terminal terminal ->
            sink.terminal <- true ;
            Some (make_event_unlocked sink (Terminal terminal))
        | _ -> Some (make_event_unlocked sink payload)
      else None)
  in
  Option.iter (notify sink) event

let begin_attempt sink kind =
  let event =
    with_lock sink (fun () ->
        if not sink.terminal then
          Some (make_event_unlocked sink (Attempt_started kind))
        else None)
  in
  Option.iter (notify sink) event

let transition_to_retry sink ~kind ~reason =
  let events =
    with_lock sink (fun () ->
      if not sink.terminal then begin
        let transition =
          make_event_unlocked sink (Retry_transition {kind; reason})
        in
        sink.attempt <- sink.attempt + 1 ;
        let attempt_kind =
          match kind with
          | Fresh_retry -> Fresh_attempt
          | Resume_retry -> Resumed_attempt
        in
        let attempt = make_event_unlocked sink (Attempt_started attempt_kind) in
        [transition; attempt]
      end
      else [])
  in
  List.iter (notify sink) events

let emit_terminal sink terminal = emit sink (Terminal terminal)
