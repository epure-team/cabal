(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

let contains value fragment =
  let value_length = String.length value in
  let fragment_length = String.length fragment in
  let rec loop offset =
    offset + fragment_length <= value_length
    &&
    (String.sub value offset fragment_length = fragment || loop (offset + 1))
  in
  fragment_length = 0 || loop 0

let test_sequence_attempts_terminal_and_timestamps () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let timestamp = ref 0.0 in
  let events = ref [] in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () ->
        let value = !timestamp in
        timestamp := value +. 0.25 ;
        value)
      ~on_event:(fun event -> events := event :: !events)
      ()
  in
  Task_event.emit sink Task_event.Task_started ;
  Task_event.begin_attempt sink Task_event.Initial_attempt ;
  Task_event.finish_attempt sink Task_event.Attempt_succeeded ;
  Task_event.emit
    sink
    (Task_event.Backend_selected {backend_id = "mock"}) ;
  Task_event.transition_to_retry
    sink
    ~kind:Task_event.Resume_retry
    ~reason:"schema error /private/secret" ;
  Task_event.emit sink (Task_event.Agent_text_delta "ok") ;
  Task_event.emit_terminal sink Task_event.Succeeded ;
  Task_event.emit sink (Task_event.Agent_text_delta "must-not-be-emitted") ;
  Task_event.emit_terminal sink (Task_event.Failed "second terminal") ;
  Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
  let events = List.rev !events in
  Alcotest.(check int) "eight events" 8 (List.length events) ;
  List.iteri
    (fun index event ->
      Alcotest.(check int) "strict sequence" (index + 1) event.Task_event.seq)
    events ;
  let rec timestamps = function
    | first :: (second :: _ as rest) ->
        Alcotest.(check bool)
          "timestamps monotonic"
          true
          (first.Task_event.timestamp <= second.timestamp) ;
        timestamps rest
    | [] | [_] -> ()
  in
  timestamps events ;
  let attempts = List.map (fun event -> event.Task_event.attempt) events in
  Alcotest.(check (list int))
    "attempt changes only at retry"
    [1; 1; 1; 1; 1; 2; 2; 2]
    attempts ;
  let terminal_count =
    List.fold_left
      (fun count event ->
        match event.Task_event.payload with
        | Task_event.Terminal _ -> count + 1
        | _ -> count)
      0
      events
  in
  Alcotest.(check int) "exactly one terminal" 1 terminal_count ;
  match List.nth events 4 with
  | {Task_event.payload = Retry_transition {reason; _}; _} ->
      Alcotest.(check bool) "retry path redacted" false (contains reason "private")
  | _ -> Alcotest.fail "expected retry transition"

let test_callback_exception_isolated_and_diagnostic_redacted () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let diagnostics = ref [] in
  Diagnostics.set_handler (fun event -> diagnostics := event :: !diagnostics) ;
  Fun.protect
    ~finally:Diagnostics.reset_handler
    (fun () ->
      let sink =
        Task_event.create_sink
          ~sw
          ~now:(fun () -> 1.0)
          ~on_event:(fun event ->
            events := event :: !events ;
            failwith "callback-secret /private/callback/path")
          ()
      in
      Task_event.emit sink Task_event.Task_started ;
      Task_event.begin_attempt sink Task_event.Initial_attempt ;
      Task_event.emit_terminal sink Task_event.Cancelled ;
      Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
      Alcotest.(check int)
        "callback failures do not stop delivery"
        3
        (List.length !events) ;
      let rendered =
        List.map
          (function
            | Diagnostics.Log (_, message)
            | Diagnostics.User_warning message ->
                message)
          !diagnostics
        |> String.concat "\n"
      in
      Alcotest.(check bool)
        "exception text is not logged"
        false
        (contains rendered "callback-secret") ;
      Alcotest.(check bool)
        "exception path is not logged"
        false
        (contains rendered "/private/callback"))

let test_operational_identifiers_are_sanitized () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () -> 0.0)
      ~on_event:(fun event -> events := event :: !events)
      ()
  in
  Task_event.emit
    sink
    (Task_event.Backend_selected {backend_id = "/private/backend"}) ;
  Task_event.emit sink (Task_event.Session_id "https://user:secret@example.test") ;
  Task_event.emit
    sink
    (Task_event.Tool_started {id = Some "/private/tool-id"; name = "/bin/read"}) ;
  Task_event.emit_terminal sink Task_event.Succeeded ;
  Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
  let rendered =
    List.rev !events
    |> List.map (fun event ->
        match event.Task_event.payload with
        | Backend_selected {backend_id} -> backend_id
        | Session_id id -> id
        | Tool_started {id; name} -> Option.value ~default:"" id ^ name
        | _ -> "")
    |> String.concat "\n"
  in
  Alcotest.(check bool) "backend path omitted" false (contains rendered "/private") ;
  Alcotest.(check bool) "credential URL omitted" false (contains rendered "secret") ;
  Alcotest.(check bool) "tool path omitted" false (contains rendered "/bin/read")

let test_callback_delivery_is_fifo_serialized_and_non_blocking () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, resolve_first_started = Eio.Promise.create () in
  let release_first, resolve_release_first = Eio.Promise.create () in
  let delivered = ref [] in
  let active = ref 0 in
  let max_active = ref 0 in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () -> 0.0)
      ~on_event:(fun event ->
        incr active ;
        max_active := max !max_active !active ;
        if event.Task_event.seq = 1 then begin
          Eio.Promise.resolve resolve_first_started () ;
          Eio.Promise.await release_first
        end ;
        delivered := event.seq :: !delivered ;
        decr active)
      ()
  in
  Task_event.emit sink Task_event.Task_started ;
  Eio.Promise.await first_started ;
  let concurrent_emitters_returned = ref false in
  Eio.Fiber.all
    (List.init 20 (fun index () ->
         Task_event.emit
           sink
           (Task_event.Agent_text_delta (string_of_int index)))) ;
  concurrent_emitters_returned := true ;
  Task_event.emit_terminal sink Task_event.Succeeded ;
  Alcotest.(check bool)
    "concurrent producers are independent from blocked callback"
    true
    !concurrent_emitters_returned ;
  Eio.Promise.resolve resolve_release_first () ;
  Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
  Alcotest.(check int) "callbacks never overlap" 1 !max_active ;
  Alcotest.(check (list int))
    "delivery order equals sequence order"
    (List.init 22 (fun index -> index + 1))
    (List.rev !delivered)

let truncation_totals events =
  List.fold_left
    (fun (text_events, text_bytes, control_events) event ->
      match event.Task_event.payload with
      | Task_event.Event_delivery_truncated counts ->
          ( text_events + counts.agent_text_events,
            text_bytes + counts.agent_text_bytes,
            control_events + counts.control_events )
      | _ -> (text_events, text_bytes, control_events))
    (0, 0, 0)
    events

let test_pending_delivery_is_bounded_and_retains_lifecycle () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, resolve_first_started = Eio.Promise.create () in
  let release_first, resolve_release_first = Eio.Promise.create () in
  let delivered = ref [] in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () -> 0.0)
      ~on_event:(fun event ->
        if event.Task_event.seq = 1 then begin
          Eio.Promise.resolve resolve_first_started () ;
          Eio.Promise.await release_first
        end ;
        delivered := event :: !delivered)
      ()
  in
  Task_event.emit sink Task_event.Task_started ;
  Eio.Promise.await first_started ;
  Task_event.begin_attempt sink Task_event.Initial_attempt ;
  let emitted_text_events = Task_event.max_pending_observational_events * 8 in
  for index = 1 to emitted_text_events do
    Task_event.emit sink (Task_event.Agent_text_delta (string_of_int index))
  done ;
  Task_event.emit sink (Task_event.Session_id "session-1") ;
  Task_event.emit
    sink
    (Task_event.Tool_started {id = Some "tool-1"; name = "Read"}) ;
  Task_event.emit
    sink
    (Task_event.Tool_finished {id = Some "tool-1"; name = Some "Read"}) ;
  Task_event.emit
    sink
    (Task_event.Token_usage
       {
         Backend_types.tokens_input = Some 1;
         tokens_output = Some 1;
         cost_usd = None;
         cache_creation_input_tokens = None;
         cache_read_input_tokens = None;
       }) ;
  let pending_before_controls =
    Task_event.Private.pending_delivery sink
  in
  Alcotest.(check bool)
    "pending event count is bounded"
    true
    (pending_before_controls.event_count <= Task_event.max_pending_events) ;
  Alcotest.(check bool)
    "pending public text bytes are bounded"
    true
    (pending_before_controls.agent_text_bytes
    <= Task_event.max_pending_agent_text_bytes) ;
  Task_event.finish_attempt sink Task_event.Attempt_failed ;
  Task_event.transition_to_retry
    sink
    ~kind:Task_event.Fresh_retry
    ~reason:"schema mismatch" ;
  Task_event.emit sink (Task_event.Process_started {pid = Some 42}) ;
  Task_event.emit sink Task_event.Process_termination_requested ;
  Task_event.emit sink Task_event.Process_kill_escalated ;
  Task_event.emit sink (Task_event.Process_exited {exit_status = "failed"}) ;
  Task_event.finish_attempt sink Task_event.Attempt_cancelled ;
  Task_event.emit_terminal sink Task_event.Cancelled ;
  Task_event.emit sink (Task_event.Process_started {pid = Some 99}) ;
  Task_event.emit sink (Task_event.Agent_text_delta "post-terminal") ;
  let pending_with_terminal = Task_event.Private.pending_delivery sink in
  Alcotest.(check bool)
    "terminal remains inside the total pending bound"
    true
    (pending_with_terminal.event_count <= Task_event.max_pending_events) ;
  Eio.Promise.resolve resolve_release_first () ;
  Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
  let delivered = List.rev !delivered in
  let text_events, _, control_events = truncation_totals delivered in
  Alcotest.(check int)
    "every excess small text event is reported"
    (emitted_text_events - Task_event.max_pending_observational_events)
    text_events ;
  Alcotest.(check int) "legitimate controls are not dropped" 0 control_events ;
  let marker =
    List.find_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Event_delivery_truncated counts -> Some counts
        | _ -> None)
      delivered
    |> function
    | Some counts -> counts
    | None -> Alcotest.fail "missing truncation marker"
  in
  Alcotest.(check int) "saturated session is categorized" 1 marker.session_events ;
  Alcotest.(check int) "saturated tool events are categorized" 2 marker.tool_events ;
  Alcotest.(check int)
    "saturated usage is categorized"
    1
    marker.token_usage_events ;
  let rec check_increasing_with_gap saw_gap = function
    | first :: (second :: _ as rest) ->
        Alcotest.(check bool)
          "delivered sequence stays increasing"
          true
          (first.Task_event.seq < second.seq) ;
        check_increasing_with_gap
          (saw_gap || second.seq > first.seq + 1)
          rest
    | [] | [_] -> saw_gap
  in
  Alcotest.(check bool)
    "omitted events leave a documented sequence gap"
    true
    (check_increasing_with_gap false delivered) ;
  let lifecycle =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Task_started -> Some "task-started"
        | Task_event.Attempt_started Task_event.Initial_attempt ->
            Some "attempt-1-started"
        | Task_event.Attempt_finished Task_event.Attempt_failed ->
            Some "attempt-1-finished"
        | Task_event.Retry_transition _ -> Some "retry"
        | Task_event.Attempt_started Task_event.Fresh_attempt ->
            Some "attempt-2-started"
        | Task_event.Process_started {pid = Some 42} -> Some "process-started"
        | Task_event.Process_termination_requested -> Some "process-term"
        | Task_event.Process_kill_escalated -> Some "process-kill"
        | Task_event.Process_exited _ -> Some "process-exited"
        | Task_event.Attempt_finished Task_event.Attempt_cancelled ->
            Some "attempt-2-finished"
        | Task_event.Terminal Task_event.Cancelled -> Some "terminal"
        | _ -> None)
      delivered
  in
  Alcotest.(check (list string))
    "lifecycle and terminal survive saturation in FIFO order"
    [ "task-started";
      "attempt-1-started";
      "attempt-1-finished";
      "retry";
      "attempt-2-started";
      "process-started";
      "process-term";
      "process-kill";
      "process-exited";
      "attempt-2-finished";
      "terminal";
    ]
    lifecycle ;
  Alcotest.(check bool)
    "terminal is the final callback and post-terminal events are rejected"
    true
    (match List.rev delivered with
    | {Task_event.payload = Terminal Cancelled; _} :: _ ->
        not
          (List.exists
             (fun event ->
               match event.Task_event.payload with
               | Process_started {pid = Some 99}
               | Agent_text_delta "post-terminal" ->
                   true
               | _ -> false)
             delivered)
    | _ -> false)

let test_oversized_delta_is_bounded_and_reported () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, resolve_first_started = Eio.Promise.create () in
  let release_first, resolve_release_first = Eio.Promise.create () in
  let delivered = ref [] in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () -> 0.0)
      ~on_event:(fun event ->
        if event.Task_event.seq = 1 then begin
          Eio.Promise.resolve resolve_first_started () ;
          Eio.Promise.await release_first
        end ;
        delivered := event :: !delivered)
      ()
  in
  Task_event.emit sink Task_event.Task_started ;
  Eio.Promise.await first_started ;
  let oversized_length = Task_event.max_agent_text_delta_bytes * 4 in
  Task_event.emit
    sink
    (Task_event.Agent_text_delta (String.make oversized_length 'x')) ;
  let additional_full_deltas = 10 in
  for _ = 1 to additional_full_deltas do
    Task_event.emit
      sink
      (Task_event.Agent_text_delta
         (String.make Task_event.max_agent_text_delta_bytes 'y'))
  done ;
  let pending = Task_event.Private.pending_delivery sink in
  Alcotest.(check int)
    "accumulated pending text reaches but never exceeds its byte bound"
    Task_event.max_pending_agent_text_bytes
    pending.agent_text_bytes ;
  Task_event.emit_terminal sink Task_event.Succeeded ;
  Eio.Promise.resolve resolve_release_first () ;
  Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
  let delivered = List.rev !delivered in
  let retained_lengths =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Agent_text_delta text -> Some (String.length text)
        | _ -> None)
      delivered
  in
  let retained_length =
    match retained_lengths with
    | first :: _ -> first
    | [] -> Alcotest.fail "missing retained oversized delta prefix"
  in
  Alcotest.(check int)
    "oversized delta is truncated to the per-event limit"
    Task_event.max_agent_text_delta_bytes
    retained_length ;
  Alcotest.(check bool)
    "every delivered delta respects the per-event limit"
    true
    (List.for_all
       (fun length -> length <= Task_event.max_agent_text_delta_bytes)
       retained_lengths) ;
  let retained_bytes = List.fold_left ( + ) 0 retained_lengths in
  Alcotest.(check int)
    "delivered text respects the accumulated byte limit"
    Task_event.max_pending_agent_text_bytes
    retained_bytes ;
  let text_events, text_bytes, _ = truncation_totals delivered in
  let emitted_bytes =
    oversized_length
    + (additional_full_deltas * Task_event.max_agent_text_delta_bytes)
  in
  let retained_delta_capacity =
    Task_event.max_pending_agent_text_bytes
    / Task_event.max_agent_text_delta_bytes
  in
  Alcotest.(check int)
    "every partial or fully omitted delta is counted"
    (1 + additional_full_deltas - (retained_delta_capacity - 1))
    text_events ;
  Alcotest.(check int)
    "omitted bytes are reported exactly"
    (emitted_bytes - retained_bytes)
    text_bytes

let test_saturated_empty_delta_is_reported_as_omitted_event () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, resolve_first_started = Eio.Promise.create () in
  let release_first, resolve_release_first = Eio.Promise.create () in
  let delivered = ref [] in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () -> 0.0)
      ~on_event:(fun event ->
        if event.Task_event.seq = 1 then begin
          Eio.Promise.resolve resolve_first_started () ;
          Eio.Promise.await release_first
        end ;
        delivered := event :: !delivered)
      ()
  in
  Task_event.emit sink Task_event.Task_started ;
  Eio.Promise.await first_started ;
  for _ = 1 to Task_event.max_pending_observational_events do
    Task_event.emit sink (Task_event.Agent_text_delta "x")
  done ;
  Task_event.emit sink (Task_event.Agent_text_delta "") ;
  Task_event.emit_terminal sink Task_event.Succeeded ;
  Eio.Promise.resolve resolve_release_first () ;
  Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
  let delivered = List.rev !delivered in
  Alcotest.(check bool)
    "saturated empty delta is not delivered"
    false
    (List.exists
       (fun event -> event.Task_event.payload = Task_event.Agent_text_delta "")
       delivered) ;
  match List.rev delivered with
  | terminal :: marker :: previous :: _ ->
      (match marker.Task_event.payload with
      | Task_event.Event_delivery_truncated counts ->
          Alcotest.(check int)
            "empty omitted delta counts as one event"
            1
            counts.agent_text_events ;
          Alcotest.(check int)
            "empty omitted delta contributes no bytes"
            0
            counts.agent_text_bytes
      | _ -> Alcotest.fail "missing marker before terminal") ;
      Alcotest.(check bool)
        "fully omitted empty delta leaves one sequence gap"
        true
        (marker.seq = previous.seq + 2) ;
      Alcotest.(check bool)
        "marker is immediately followed by terminal"
        true
        (terminal.seq = marker.seq + 1) ;
      Alcotest.(check bool)
        "terminal remains last"
        true
        (terminal.payload = Task_event.Terminal Task_event.Succeeded)
  | _ -> Alcotest.fail "missing saturated empty-delta lifecycle events"

let test_repeated_dense_delivery_has_bounded_liveness () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  for iteration = 1 to 20 do
    let first_started, resolve_first_started = Eio.Promise.create () in
    let release_first, resolve_release_first = Eio.Promise.create () in
    let terminal_seen = ref false in
    let sink =
      Task_event.create_sink
        ~sw
        ~now:(fun () -> 0.0)
        ~on_event:(fun event ->
          if event.Task_event.seq = 1 then begin
            Eio.Promise.resolve resolve_first_started () ;
            Eio.Promise.await release_first
          end ;
          match event.Task_event.payload with
          | Task_event.Terminal _ -> terminal_seen := true
          | _ -> ())
        ()
    in
    Task_event.emit sink Task_event.Task_started ;
    Eio.Promise.await first_started ;
    for index = 1 to Task_event.max_pending_observational_events * 4 do
      Task_event.emit
        sink
        (Task_event.Agent_text_delta
           (Printf.sprintf "%d-%d" iteration index))
    done ;
    let pending = Task_event.Private.pending_delivery sink in
    Alcotest.(check bool)
      "dense pending count remains bounded"
      true
      (pending.event_count <= Task_event.max_pending_events) ;
    Alcotest.(check bool)
      "dense pending bytes remain bounded"
      true
      (pending.agent_text_bytes <= Task_event.max_pending_agent_text_bytes) ;
    Task_event.emit_terminal sink Task_event.Succeeded ;
    Eio.Promise.resolve resolve_release_first () ;
    Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
    Alcotest.(check bool) "dense terminal drains" true !terminal_seen
  done

let test_alternating_abusive_controls_use_one_bounded_marker () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, resolve_first_started = Eio.Promise.create () in
  let release_first, resolve_release_first = Eio.Promise.create () in
  let delivered = ref [] in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () -> 0.0)
      ~on_event:(fun event ->
        if event.Task_event.seq = 1 then begin
          Eio.Promise.resolve resolve_first_started () ;
          Eio.Promise.await release_first
        end ;
        delivered := event :: !delivered)
      ()
  in
  Task_event.emit sink Task_event.Task_started ;
  Eio.Promise.await first_started ;
  let emitted_per_category = 1_000 in
  for _ = 1 to emitted_per_category do
    Task_event.emit sink (Task_event.Agent_text_delta "x") ;
    Task_event.emit sink Task_event.Preflight_started
  done ;
  Task_event.emit_terminal sink Task_event.Succeeded ;
  let pending = Task_event.Private.pending_delivery sink in
  Alcotest.(check int)
    "all data, control, marker, and terminal slots are statically bounded"
    Task_event.max_pending_events
    pending.event_count ;
  Eio.Promise.resolve resolve_release_first () ;
  Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
  let markers =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Event_delivery_truncated counts -> Some counts
        | _ -> None)
      !delivered
  in
  Alcotest.(check int) "alternating pressure retains one marker" 1 (List.length markers) ;
  match markers with
  | [counts] ->
      Alcotest.(check int)
        "all omitted observations are counted"
        (emitted_per_category - Task_event.max_pending_observational_events)
        counts.agent_text_events ;
      Alcotest.(check int)
        "all abusive controls beyond the ordinary reserve are counted"
        (emitted_per_category - (Task_event.control_event_reserve - 2))
        counts.control_events
  | [] | _ :: _ :: _ -> Alcotest.fail "expected one truncation marker"

let test_published_delivery_bounds_are_exact () =
  Alcotest.(check int) "total pending bound" 256 Task_event.max_pending_events ;
  Alcotest.(check int)
    "observation bound"
    192
    Task_event.max_pending_observational_events ;
  Alcotest.(check int) "control reserve" 64 Task_event.control_event_reserve ;
  Alcotest.(check int)
    "ordinary non-terminal controls"
    62
    Task_event.max_pending_control_events ;
  Alcotest.(check int)
    "marker and terminal consume remainder"
    Task_event.control_event_reserve
    (Task_event.max_pending_control_events + 2) ;
  Alcotest.(check int)
    "aggregate public text bytes"
    (64 * 1024)
    Task_event.max_pending_agent_text_bytes ;
  Alcotest.(check int)
    "per-delta public text bytes"
    (16 * 1024)
    Task_event.max_agent_text_delta_bytes

let test_shared_bounded_collector_retains_marker_and_terminal () =
  Eio_posix.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, resolve_first_started = Eio.Promise.create () in
  let release_first, resolve_release_first = Eio.Promise.create () in
  let callback_count = ref 0 in
  let collector = Task_event.Private.create_bounded_collector () in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () -> 0.0)
      ~on_event:(fun event ->
        incr callback_count ;
        Task_event.Private.collect_bounded collector event ;
        if event.Task_event.seq = 1 then begin
          Eio.Promise.resolve resolve_first_started () ;
          Eio.Promise.await release_first
        end)
      ()
  in
  Task_event.emit sink Task_event.Task_started ;
  Eio.Promise.await first_started ;
  let text_events =
    Task_event.max_pending_agent_text_bytes
    / Task_event.max_agent_text_delta_bytes
  in
  for _ = 1 to text_events do
    Task_event.emit
      sink
      (Task_event.Agent_text_delta
         (String.make Task_event.max_agent_text_delta_bytes 'x'))
  done ;
  let usage : Backend_types.cost =
    {
      tokens_input = Some 1;
      tokens_output = Some 1;
      cost_usd = None;
      cache_creation_input_tokens = None;
      cache_read_input_tokens = None;
    }
  in
  for _ = text_events + 1 to Task_event.max_pending_observational_events do
    Task_event.emit sink (Task_event.Token_usage usage)
  done ;
  Task_event.emit sink (Task_event.Token_usage usage) ;
  let emitted_controls = 1_000 in
  for _ = 1 to emitted_controls do
    Task_event.emit sink Task_event.Preflight_started
  done ;
  Task_event.emit_terminal sink Task_event.Succeeded ;
  let pending = Task_event.Private.pending_delivery sink in
  Alcotest.(check int)
    "pending queue reaches exact total bound"
    Task_event.max_pending_events
    pending.event_count ;
  Alcotest.(check int)
    "pending observations reach exact bound"
    Task_event.max_pending_observational_events
    pending.observational_event_count ;
  Alcotest.(check int)
    "pending ordinary controls reach exact cap"
    Task_event.max_pending_control_events
    pending.control_event_count ;
  Alcotest.(check int)
    "pending text reaches exact 64 KiB cap"
    Task_event.max_pending_agent_text_bytes
    pending.agent_text_bytes ;
  Alcotest.(check bool)
    "pending marker has a dedicated slot"
    true
    pending.truncation_marker_retained ;
  Alcotest.(check int)
    "pending terminal has a dedicated slot"
    1
    pending.terminal_event_count ;
  Eio.Promise.resolve resolve_release_first () ;
  Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
  let collected = Task_event.Private.collected_delivery collector in
  Alcotest.(check int)
    "one active callback plus bounded pending queue was delivered"
    (Task_event.max_pending_events + 1)
    !callback_count ;
  Alcotest.(check int)
    "post-delivery collector remains bounded"
    Task_event.max_pending_events
    (List.length collected.events) ;
  Alcotest.(check int)
    "one excess delivered ordinary control is omitted"
    1
    collected.omitted_events ;
  let observations, controls, markers, terminals, text_bytes =
    List.fold_left
      (fun (observations, controls, markers, terminals, text_bytes) event ->
        match Task_event.Private.classify_payload event.Task_event.payload with
        | Task_event.Private.Agent_text_observation text ->
            ( observations + 1,
              controls,
              markers,
              terminals,
              text_bytes + String.length text )
        | Task_event.Private.Token_usage_observation
        | Task_event.Private.Session_observation
        | Task_event.Private.Tool_observation ->
            (observations + 1, controls, markers, terminals, text_bytes)
        | Task_event.Private.Nonterminal_control ->
            (observations, controls + 1, markers, terminals, text_bytes)
        | Task_event.Private.Delivery_truncation_marker ->
            (observations, controls, markers + 1, terminals, text_bytes)
        | Task_event.Private.Terminal_event ->
            (observations, controls, markers, terminals + 1, text_bytes))
      (0, 0, 0, 0, 0)
      collected.events
  in
  Alcotest.(check int)
    "collector observation partition"
    Task_event.max_pending_observational_events
    observations ;
  Alcotest.(check int)
    "collector ordinary-control partition"
    Task_event.max_pending_control_events
    controls ;
  Alcotest.(check int) "collector retains truncation marker" 1 markers ;
  Alcotest.(check int) "collector retains terminal" 1 terminals ;
  Alcotest.(check int)
    "collector retains exactly 64 KiB text"
    Task_event.max_pending_agent_text_bytes
    text_bytes ;
  let marker =
    List.find_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Event_delivery_truncated counts -> Some counts
        | _ -> None)
      collected.events
    |> function
    | Some marker -> marker
    | None -> Alcotest.fail "bounded collector lost truncation marker"
  in
  Alcotest.(check int)
    "marker reports saturated usage"
    1
    marker.token_usage_events ;
  Alcotest.(check int)
    "marker reports abusive pending controls"
    (emitted_controls - Task_event.max_pending_control_events)
    marker.control_events ;
  Alcotest.(check bool)
    "terminal remains the last collected event"
    true
    (match List.rev collected.events with
    | {Task_event.payload = Task_event.Terminal Task_event.Succeeded; _} :: _ ->
        true
    | _ -> false)

let test_claude_parser_emits_only_proven_public_content () =
  let reasoning =
    {|{"type":"assistant","message":{"role":"assistant","content":[{"type":"thinking","thinking":"private-chain-of-thought"}]}}|}
  in
  Alcotest.(check int)
    "reasoning is not promoted"
    0
    (List.length (Claude_code.normalized_events_of_stream_line reasoning)) ;
  let public_text =
    {|{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":"visible answer"},{"type":"tool_use","id":"tool-1","name":"Read","input":{"file_path":"/private/book.jpg"}}]}}|}
  in
  (match Claude_code.normalized_events_of_stream_line public_text with
  | [Task_event.Agent_text_delta text; Task_event.Tool_started tool] ->
      Alcotest.(check string) "public assistant text" "visible answer" text ;
      Alcotest.(check string) "tool name" "Read" tool.Task_event.name ;
      Alcotest.(check bool)
        "tool event omits raw arguments"
        false
        (contains tool.name "/private/book.jpg")
  | _ -> Alcotest.fail "expected normalized text and tool events") ;
  let structured =
    Claude_code.normalized_events_of_stream_line
      {|{"type":"result","is_error":false,"structured_output":{"ok":true}}|}
  in
  Alcotest.(check bool)
    "successful structured result is public"
    true
    (structured = [Task_event.Agent_text_delta {|{"ok":true}|}])

let test_structured_parsers_reject_tempting_private_fields () =
  let assert_empty label events =
    Alcotest.(check int) label 0 (List.length events)
  in
  List.iter
    (fun (label, line) ->
      assert_empty label (Claude_code.normalized_events_of_stream_line line))
    [
      ( "claude error result",
        {|{"type":"result","is_error":true,"result":"private failure","session_id":"tempting","usage":{"input_tokens":9}}|}
      );
      ( "claude user message",
        {|{"type":"assistant","message":{"role":"user","content":[{"type":"text","text":"private prompt"}]}}|}
      );
      ( "claude unknown event",
        {|{"type":"unknown","result":"private unknown"}|} );
    ] ;
  List.iter
    (fun (label, line) ->
      assert_empty label (Gemini_cli.normalized_events_of_line line))
    [
      ( "gemini thinking",
        {|{"type":"thinking","text":"private reasoning","response":"tempting"}|}
      );
      ( "gemini user message",
        {|{"type":"message","role":"user","content":"private prompt"}|} );
      ( "gemini error",
        {|{"type":"error","response":"private error","usage":{"input_tokens":7}}|}
      );
      ( "gemini unknown",
        {|{"type":"unknown","text":"private unknown","session_id":"tempting"}|}
      );
    ] ;
  List.iter
    (fun (label, line) ->
      assert_empty label (Codex_cli.normalized_events_of_line line))
    [
      ( "codex reasoning is not a tool",
        {|{"type":"item.completed","item":{"type":"reasoning","text":"private chain"}}|}
      );
      ( "codex error is not a tool",
        {|{"type":"item.completed","item":{"type":"error","text":"private error"}}|}
      );
      ( "codex unknown is not a tool",
        {|{"type":"item.started","item":{"type":"unknown","name":"private-name"}}|}
      );
    ] ;
  assert_empty
    "opencode unknown"
    (Opencode_cli.normalized_events_of_line
       {|{"type":"reasoning","part":{"text":"private chain"}}|}) ;
  assert_empty
    "pi non-final message"
    (Yaml_adapter.normalized_pi_events_of_line
       {|{"type":"message","message":{"role":"assistant","content":[{"type":"text","text":"private intermediate"}]}}|})

let test_structured_backend_parsers () =
  let codex =
    Codex_cli.normalized_events_of_line
      {|{"type":"item.completed","item":{"type":"agent_message","text":"codex answer"}}|}
  in
  Alcotest.(check int) "codex public events" 1 (List.length codex) ;
  let codex_tool =
    Codex_cli.normalized_events_of_line
      {|{"type":"item.completed","item":{"type":"command_execution","id":"tool-1","command":"cat /private/file"}}|}
  in
  Alcotest.(check bool)
    "codex documented tool event"
    true
    (match codex_tool with
    | [Task_event.Tool_finished {id = Some "tool-1"; name = Some "command_execution"}] ->
        true
    | _ -> false) ;
  let gemini =
    [ {|{"type":"init","session_id":"gemini-s"}|};
      {|{"type":"message","role":"assistant","content":"gemini answer"}|};
      {|{"type":"result","status":"success","usageMetadata":{"promptTokenCount":4,"candidatesTokenCount":5}}|};
    ]
    |> List.concat_map Gemini_cli.normalized_events_of_line
  in
  Alcotest.(check int) "gemini public events" 3 (List.length gemini) ;
  let opencode_text =
    Opencode_cli.normalized_events_of_line
      {|{"type":"text","part":{"text":"opencode answer"}}|}
  in
  Alcotest.(check bool)
    "opencode text"
    true
    (opencode_text = [Task_event.Agent_text_delta "opencode answer"]) ;
  let pi =
    Yaml_adapter.normalized_pi_events_of_line
      {|{"type":"message_end","message":{"role":"assistant","content":[{"type":"thinking","thinking":"secret"},{"type":"text","text":"pi answer"}]}}|}
  in
  Alcotest.(check bool)
    "pi reasoning omitted"
    true
    (pi = [Task_event.Agent_text_delta "pi answer"])

let () =
  Alcotest.run
    "Task_event"
    [
      ( "sequencer",
        [
          Alcotest.test_case
            "sequence attempts terminal timestamps"
            `Quick
            test_sequence_attempts_terminal_and_timestamps;
          Alcotest.test_case
            "callback exception isolation"
            `Quick
            test_callback_exception_isolated_and_diagnostic_redacted;
          Alcotest.test_case
            "operational identifiers sanitized"
            `Quick
            test_operational_identifiers_are_sanitized;
          Alcotest.test_case
            "callback FIFO serialization"
            `Quick
            test_callback_delivery_is_fifo_serialized_and_non_blocking;
          Alcotest.test_case
            "bounded pending delivery retains lifecycle"
            `Quick
            test_pending_delivery_is_bounded_and_retains_lifecycle;
          Alcotest.test_case
            "oversized public delta is bounded"
            `Quick
            test_oversized_delta_is_bounded_and_reported;
          Alcotest.test_case
            "saturated empty delta is reported"
            `Quick
            test_saturated_empty_delta_is_reported_as_omitted_event;
          Alcotest.test_case
            "repeated dense delivery remains live"
            `Quick
            test_repeated_dense_delivery_has_bounded_liveness;
          Alcotest.test_case
            "alternating abusive controls keep one marker"
            `Quick
            test_alternating_abusive_controls_use_one_bounded_marker;
          Alcotest.test_case
            "published delivery bounds are exact"
            `Quick
            test_published_delivery_bounds_are_exact;
          Alcotest.test_case
            "shared post-delivery collector is bounded"
            `Quick
            test_shared_bounded_collector_retains_marker_and_terminal;
        ] );
      ( "backend parser",
        [
          Alcotest.test_case
            "private reasoning is never promoted"
            `Quick
            test_claude_parser_emits_only_proven_public_content;
          Alcotest.test_case
            "tempting private fields are rejected"
            `Quick
            test_structured_parsers_reject_tempting_private_fields;
          Alcotest.test_case
            "structured backend public extraction"
            `Quick
            test_structured_backend_parsers;
        ] );
    ]
