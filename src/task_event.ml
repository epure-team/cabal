(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type tool = {id : string option; name : string}

type attempt_kind = Backend_types.attempt_kind =
  | Initial_attempt
  | Fresh_attempt
  | Resumed_attempt

type attempt_outcome =
  | Attempt_succeeded
  | Attempt_failed
  | Attempt_timed_out
  | Attempt_cancelled

type retry_kind = Fresh_retry | Resume_retry

type terminal = Succeeded | Failed of string | Timed_out | Cancelled

type delivery_truncation = {
  agent_text_events : int;
  agent_text_bytes : int;
  token_usage_events : int;
  session_events : int;
  tool_events : int;
  control_events : int;
}

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

type t = {seq : int; attempt : int; timestamp : float; payload : payload}

type payload_class =
  | Agent_text_observation of string
  | Token_usage_observation
  | Session_observation
  | Tool_observation
  | Nonterminal_control
  | Delivery_truncation_marker
  | Terminal_event

let classify_payload = function
  | Agent_text_delta text -> Agent_text_observation text
  | Token_usage _ -> Token_usage_observation
  | Session_id _ -> Session_observation
  | Tool_started _ | Tool_finished _ -> Tool_observation
  | Event_delivery_truncated _ -> Delivery_truncation_marker
  | Terminal _ -> Terminal_event
  | Task_started | Backend_selected _ | Preflight_started
  | Preflight_completed | Version_probe_started | Version_probe_completed
  | Availability_check_started | Availability_check_completed
  | Attempt_started _ | Attempt_finished _ | Retry_transition _
  | Process_started _ | Process_termination_requested
  | Process_kill_escalated | Process_exited _ ->
      Nonterminal_control

let max_pending_events = 256

let max_pending_agent_text_bytes = 64 * 1024

let max_agent_text_delta_bytes = 16 * 1024

let control_event_reserve = 64

let max_pending_observational_events =
  max_pending_events - control_event_reserve

(* The reserve includes a dedicated marker and terminal slot. The ordinary
   control cap of 62 is almost three times Cabal's 21-event hard two-attempt
   lifecycle, so legitimate runtime controls cannot be displaced by data. *)
let max_pending_control_events = control_event_reserve - 2

type queued_kind =
  | Observation of {agent_text_bytes : int}
  | Control
  | Truncation_marker
  | Queued_terminal

type queued_event = {mutable event : t; kind : queued_kind}

type sink = {
  now : unit -> float;
  lock : Eio.Mutex.t;
  mutable seq : int;
  mutable attempt : int;
  mutable terminal : bool;
  queue : queued_event Queue.t option;
  condition : Eio.Condition.t option;
  mutable pending_event_count : int;
  mutable pending_observational_events : int;
  mutable pending_control_events : int;
  mutable pending_agent_text_bytes : int;
  mutable pending_marker : queued_event option;
  mutable pending_terminal_events : int;
  delivery_complete : unit Eio.Promise.t;
  resolve_delivery_complete : unit Eio.Promise.u;
}

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
  | Process_exited {exit_status} ->
      Process_exited {exit_status = sanitize_identifier exit_status}
  | Terminal (Failed reason) ->
      Terminal
        (Failed (Backend_event_redaction.redact_error_message reason))
  | Event_delivery_truncated counts ->
      Event_delivery_truncated
        {
          agent_text_events = max 0 counts.agent_text_events;
          agent_text_bytes = max 0 counts.agent_text_bytes;
          token_usage_events = max 0 counts.token_usage_events;
          session_events = max 0 counts.session_events;
          tool_events = max 0 counts.tool_events;
          control_events = max 0 counts.control_events;
        }
  | payload -> payload

let make_event_unlocked sink payload =
  sink.seq <- sink.seq + 1 ;
  {
    seq = sink.seq;
    attempt = sink.attempt;
    timestamp = max 0.0 (sink.now ());
    payload = redact_payload payload;
  }

let notify callback event =
  try callback event with _ -> report_callback_exception ()

let with_lock sink f =
  Eio.Mutex.use_rw ~protect:true sink.lock f

let queued_event sink kind payload =
  {event = make_event_unlocked sink payload; kind}

let add_pending_unlocked sink queue queued =
  assert (sink.pending_event_count < max_pending_events) ;
  (match queued.kind with
  | Observation {agent_text_bytes} ->
      assert
        (sink.pending_observational_events
        < max_pending_observational_events) ;
      assert
        (sink.pending_agent_text_bytes + agent_text_bytes
        <= max_pending_agent_text_bytes)
  | Control -> assert (sink.pending_control_events < max_pending_control_events)
  | Truncation_marker -> assert (Option.is_none sink.pending_marker)
  | Queued_terminal -> assert (sink.pending_terminal_events = 0)) ;
  Queue.add queued queue ;
  sink.pending_event_count <- sink.pending_event_count + 1 ;
  (match queued.kind with
  | Observation {agent_text_bytes} ->
      sink.pending_observational_events <-
        sink.pending_observational_events + 1 ;
      sink.pending_agent_text_bytes <-
        sink.pending_agent_text_bytes + agent_text_bytes
  | Control -> sink.pending_control_events <- sink.pending_control_events + 1
  | Truncation_marker -> sink.pending_marker <- Some queued
  | Queued_terminal ->
      sink.pending_terminal_events <- sink.pending_terminal_events + 1) ;
  Option.iter Eio.Condition.broadcast sink.condition

let take_pending_unlocked sink queue =
  let queued = Queue.take queue in
  sink.pending_event_count <- sink.pending_event_count - 1 ;
  (match queued.kind with
  | Observation {agent_text_bytes} ->
      sink.pending_observational_events <-
        sink.pending_observational_events - 1 ;
      sink.pending_agent_text_bytes <-
        sink.pending_agent_text_bytes - agent_text_bytes
  | Control -> sink.pending_control_events <- sink.pending_control_events - 1
  | Truncation_marker ->
      (match sink.pending_marker with
      | Some pending when pending == queued -> sink.pending_marker <- None
      | Some _ | None -> ())
  | Queued_terminal ->
      sink.pending_terminal_events <- sink.pending_terminal_events - 1) ;
  queued

let create_sink ~sw ~now ?on_event () =
  let delivery_complete, resolve_delivery_complete = Eio.Promise.create () in
  let queue = Option.map (fun _ -> Queue.create ()) on_event in
  let condition = Option.map (fun _ -> Eio.Condition.create ()) on_event in
  let sink =
    {
      now;
      lock = Eio.Mutex.create ();
      seq = 0;
      attempt = 1;
      terminal = false;
      queue;
      condition;
      pending_event_count = 0;
      pending_observational_events = 0;
      pending_control_events = 0;
      pending_agent_text_bytes = 0;
      pending_marker = None;
      pending_terminal_events = 0;
      delivery_complete;
      resolve_delivery_complete;
    }
  in
  (match on_event, queue, condition with
  | Some callback, Some queue, Some condition ->
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Cancel.protect (fun () ->
              let rec drain () =
                let queued =
                  with_lock sink (fun () ->
                      while Queue.is_empty queue do
                        Eio.Condition.await condition sink.lock
                      done ;
                      take_pending_unlocked sink queue)
                in
                notify callback queued.event ;
                match queued.kind with
                | Queued_terminal ->
                    ignore
                      (Eio.Promise.try_resolve
                         sink.resolve_delivery_complete
                         ())
                | Observation _ | Control | Truncation_marker -> drain ()
              in
              drain ()))
  | None, None, None -> ()
  | Some _, None, _ | Some _, _, None | None, Some _, _ | None, _, Some _ ->
      assert false) ;
  sink

let saturating_add left right =
  if right <= 0 then left
  else if left >= max_int - right then max_int
  else left + right

type truncation =
  | Agent_text_omitted of int
  | Token_usage_omitted
  | Session_omitted
  | Tool_omitted
  | Control_omitted

let add_truncation counts = function
  | Agent_text_omitted bytes ->
      {
        counts with
        agent_text_events = saturating_add counts.agent_text_events 1;
        agent_text_bytes = saturating_add counts.agent_text_bytes bytes;
      }
  | Token_usage_omitted ->
      {
        counts with
        token_usage_events = saturating_add counts.token_usage_events 1;
      }
  | Session_omitted ->
      {counts with session_events = saturating_add counts.session_events 1}
  | Tool_omitted ->
      {counts with tool_events = saturating_add counts.tool_events 1}
  | Control_omitted ->
      {counts with control_events = saturating_add counts.control_events 1}

let empty_truncation =
  {
    agent_text_events = 0;
    agent_text_bytes = 0;
    token_usage_events = 0;
    session_events = 0;
    tool_events = 0;
    control_events = 0;
  }

let merge_delivery_truncation left right =
  {
    agent_text_events =
      saturating_add left.agent_text_events right.agent_text_events;
    agent_text_bytes =
      saturating_add left.agent_text_bytes right.agent_text_bytes;
    token_usage_events =
      saturating_add left.token_usage_events right.token_usage_events;
    session_events = saturating_add left.session_events right.session_events;
    tool_events = saturating_add left.tool_events right.tool_events;
    control_events = saturating_add left.control_events right.control_events;
  }

type bounded_collector = {
  mutable collected_rev : t list;
  mutable collected_observations : int;
  mutable collected_controls : int;
  mutable collected_agent_text_bytes : int;
  mutable collected_omissions : int;
  mutable collected_marker : t option;
  mutable collected_terminal : bool;
}

type collected_delivery = {events : t list; omitted_events : int}

let create_bounded_collector () =
  {
    collected_rev = [];
    collected_observations = 0;
    collected_controls = 0;
    collected_agent_text_bytes = 0;
    collected_omissions = 0;
    collected_marker = None;
    collected_terminal = false;
  }

let saturating_increment value = if value = max_int then max_int else value + 1

let omit_collected collector =
  collector.collected_omissions <-
    saturating_increment collector.collected_omissions

let retain_collected collector event =
  collector.collected_rev <- event :: collector.collected_rev

let collect_observation collector event agent_text_bytes =
  if
    collector.collected_observations < max_pending_observational_events
    && agent_text_bytes
       <= max_pending_agent_text_bytes
          - collector.collected_agent_text_bytes
  then begin
    collector.collected_observations <- collector.collected_observations + 1 ;
    collector.collected_agent_text_bytes <-
      collector.collected_agent_text_bytes + agent_text_bytes ;
    retain_collected collector event
  end
  else omit_collected collector

let merge_collected_marker collector event counts =
  match collector.collected_marker with
  | None ->
      collector.collected_marker <- Some event ;
      retain_collected collector event
  | Some marker ->
      let merged =
        match marker.payload with
        | Event_delivery_truncated previous ->
            {
              marker with
              payload =
                Event_delivery_truncated
                  (merge_delivery_truncation previous counts);
            }
        | Task_started | Backend_selected _ | Preflight_started
        | Preflight_completed | Version_probe_started
        | Version_probe_completed | Availability_check_started
        | Availability_check_completed | Attempt_started _
        | Attempt_finished _ | Retry_transition _ | Process_started _
        | Process_termination_requested | Process_kill_escalated
        | Process_exited _ | Session_id _ | Agent_text_delta _
        | Tool_started _ | Tool_finished _ | Token_usage _ | Terminal _ ->
            assert false
      in
      collector.collected_marker <- Some merged ;
      collector.collected_rev <-
        List.map
          (fun (retained : t) ->
            if retained.seq = marker.seq then merged else retained)
          collector.collected_rev ;
      omit_collected collector

let collect_bounded collector event =
  match classify_payload event.payload with
  | Agent_text_observation text ->
      collect_observation collector event (String.length text)
  | Token_usage_observation | Session_observation | Tool_observation ->
      collect_observation collector event 0
  | Nonterminal_control ->
      if collector.collected_controls < max_pending_control_events then begin
        collector.collected_controls <- collector.collected_controls + 1 ;
        retain_collected collector event
      end
      else omit_collected collector
  | Delivery_truncation_marker -> (
      match event.payload with
      | Event_delivery_truncated counts ->
          merge_collected_marker collector event counts
      | Task_started | Backend_selected _ | Preflight_started
      | Preflight_completed | Version_probe_started | Version_probe_completed
      | Availability_check_started | Availability_check_completed
      | Attempt_started _ | Attempt_finished _ | Retry_transition _
      | Process_started _ | Process_termination_requested
      | Process_kill_escalated | Process_exited _ | Session_id _
      | Agent_text_delta _ | Tool_started _ | Tool_finished _ | Token_usage _
      | Terminal _ ->
          assert false)
  | Terminal_event ->
      if collector.collected_terminal then omit_collected collector
      else begin
        collector.collected_terminal <- true ;
        retain_collected collector event
      end

let collected_delivery collector =
  {
    events = List.rev collector.collected_rev;
    omitted_events = collector.collected_omissions;
  }

let record_truncation_unlocked sink truncation =
  match sink.queue with
  | None -> ()
  | Some queue -> (
      match sink.pending_marker with
      | Some queued ->
          let counts =
            match queued.event.payload with
            | Event_delivery_truncated counts -> counts
            | _ -> assert false
          in
          queued.event <-
            {
              queued.event with
              payload =
                Event_delivery_truncated (add_truncation counts truncation);
            }
      | None ->
          let counts = add_truncation empty_truncation truncation in
          add_pending_unlocked
            sink
            queue
            (queued_event
               sink
               Truncation_marker
               (Event_delivery_truncated counts)))

let truncate_utf8_prefix text limit =
  let length = String.length text in
  if length <= limit then text
  else
    let rec boundary offset =
      if offset = 0 then 0
      else
        let code = Char.code text.[offset] in
        if code land 0xc0 = 0x80 then boundary (offset - 1) else offset
    in
    String.sub text 0 (boundary limit)

let enqueue_observation_unlocked sink payload truncation =
  match sink.queue with
  | None -> ignore (make_event_unlocked sink payload)
  | Some queue ->
      let event = make_event_unlocked sink payload in
      if
        sink.pending_observational_events
        < max_pending_observational_events
      then
        let agent_text_bytes =
          match payload with Agent_text_delta text -> String.length text | _ -> 0
        in
        add_pending_unlocked
          sink
          queue
          {event; kind = Observation {agent_text_bytes}}
      else record_truncation_unlocked sink truncation

let emit_agent_text_unlocked sink text =
  match sink.queue with
  | None -> ignore (make_event_unlocked sink (Agent_text_delta text))
  | Some queue ->
      let available_text_bytes =
        max 0
          (max_pending_agent_text_bytes - sink.pending_agent_text_bytes)
      in
      let retain_limit =
        min max_agent_text_delta_bytes available_text_bytes
      in
      let retained =
        if
          sink.pending_observational_events
          >= max_pending_observational_events
        then ""
        else truncate_utf8_prefix text retain_limit
      in
      let retained_bytes = String.length retained in
      let omitted_bytes = String.length text - retained_bytes in
      let event = make_event_unlocked sink (Agent_text_delta retained) in
      let event_retained =
        sink.pending_observational_events
        < max_pending_observational_events
        && (retained_bytes > 0 || text = "")
      in
      if event_retained then
        add_pending_unlocked
          sink
          queue
          {event; kind = Observation {agent_text_bytes = retained_bytes}} ;
      if (not event_retained) || omitted_bytes > 0 then
        record_truncation_unlocked sink (Agent_text_omitted omitted_bytes)

let enqueue_control_unlocked sink payload =
  match sink.queue with
  | None -> ignore (make_event_unlocked sink payload)
  | Some queue ->
      let event = make_event_unlocked sink payload in
      if sink.pending_control_events < max_pending_control_events then
        add_pending_unlocked sink queue {event; kind = Control}
      else record_truncation_unlocked sink Control_omitted

let emit_nonterminal_unlocked sink payload =
  match classify_payload payload with
  | Agent_text_observation text -> emit_agent_text_unlocked sink text
  | Token_usage_observation ->
      enqueue_observation_unlocked sink payload Token_usage_omitted
  | Session_observation ->
      enqueue_observation_unlocked sink payload Session_omitted
  | Tool_observation ->
      enqueue_observation_unlocked sink payload Tool_omitted
  | Nonterminal_control | Delivery_truncation_marker ->
      enqueue_control_unlocked sink payload
  | Terminal_event -> assert false

let enqueue_terminal_unlocked sink terminal =
  match sink.queue with
  | Some queue ->
      add_pending_unlocked
        sink
        queue
        (queued_event sink Queued_terminal (Terminal terminal))
  | None ->
      ignore (make_event_unlocked sink (Terminal terminal)) ;
      ignore (Eio.Promise.try_resolve sink.resolve_delivery_complete ())

let emit sink payload =
  with_lock sink (fun () ->
      if not sink.terminal then
        match payload with
        | Terminal terminal ->
            sink.terminal <- true ;
            enqueue_terminal_unlocked sink terminal
        | payload -> emit_nonterminal_unlocked sink (redact_payload payload))

let begin_attempt sink kind =
  with_lock sink (fun () ->
      if not sink.terminal then
        enqueue_control_unlocked sink (Attempt_started kind))

let finish_attempt sink outcome = emit sink (Attempt_finished outcome)

let transition_to_retry sink ~kind ~reason =
  with_lock sink (fun () ->
      if not sink.terminal then begin
        enqueue_control_unlocked
          sink
          (redact_payload (Retry_transition {kind; reason})) ;
        sink.attempt <- sink.attempt + 1 ;
        let attempt_kind =
          match kind with
          | Fresh_retry -> Fresh_attempt
          | Resume_retry -> Resumed_attempt
        in
        enqueue_control_unlocked sink (Attempt_started attempt_kind)
      end)

let emit_terminal sink terminal = emit sink (Terminal terminal)

module Private = struct
  type nonrec payload_class = payload_class =
    | Agent_text_observation of string
    | Token_usage_observation
    | Session_observation
    | Tool_observation
    | Nonterminal_control
    | Delivery_truncation_marker
    | Terminal_event

  type nonrec bounded_collector = bounded_collector

  type nonrec collected_delivery = collected_delivery = {
    events : t list;
    omitted_events : int;
  }

  type pending_delivery = {
    event_count : int;
    observational_event_count : int;
    control_event_count : int;
    agent_text_bytes : int;
    truncation_marker_retained : bool;
    terminal_event_count : int;
  }

  let classify_payload = classify_payload

  let create_bounded_collector = create_bounded_collector

  let collect_bounded = collect_bounded

  let collected_delivery = collected_delivery

  let delivery_complete sink = sink.delivery_complete

  let pending_delivery sink =
    with_lock sink (fun () ->
        {
          event_count = sink.pending_event_count;
          observational_event_count = sink.pending_observational_events;
          control_event_count = sink.pending_control_events;
          agent_text_bytes = sink.pending_agent_text_bytes;
          truncation_marker_retained = Option.is_some sink.pending_marker;
          terminal_event_count = sink.pending_terminal_events;
        })
end
