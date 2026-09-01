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
