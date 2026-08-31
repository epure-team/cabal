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
  let timestamp = ref 0.0 in
  let events = ref [] in
  let sink =
    Task_event.create_sink
      ~now:(fun () ->
        let value = !timestamp in
        timestamp := value +. 0.25 ;
        value)
      ~on_event:(fun event -> events := event :: !events)
      ()
  in
  Task_event.emit sink Task_event.Task_started ;
  Task_event.begin_attempt sink Task_event.Initial_attempt ;
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
  let events = List.rev !events in
  Alcotest.(check int) "six events" 7 (List.length events) ;
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
    [1; 1; 1; 1; 2; 2; 2]
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
  match List.nth events 3 with
  | {Task_event.payload = Retry_transition {reason; _}; _} ->
      Alcotest.(check bool) "retry path redacted" false (contains reason "private")
  | _ -> Alcotest.fail "expected retry transition"

let test_callback_exception_isolated_and_diagnostic_redacted () =
  Eio_posix.run @@ fun _env ->
  let events = ref [] in
  let diagnostics = ref [] in
  Diagnostics.set_handler (fun event -> diagnostics := event :: !diagnostics) ;
  Fun.protect
    ~finally:Diagnostics.reset_handler
    (fun () ->
      let sink =
        Task_event.create_sink
          ~now:(fun () -> 1.0)
          ~on_event:(fun event ->
            events := event :: !events ;
            failwith "callback-secret /private/callback/path")
          ()
      in
      Task_event.emit sink Task_event.Task_started ;
      Task_event.begin_attempt sink Task_event.Initial_attempt ;
      Task_event.emit_terminal sink Task_event.Cancelled ;
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
  let events = ref [] in
  let sink =
    Task_event.create_sink
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

let test_claude_parser_emits_only_proven_public_content () =
  let reasoning =
    {|{"type":"assistant","message":{"content":[{"type":"thinking","thinking":"private-chain-of-thought"}]}}|}
  in
  Alcotest.(check int)
    "reasoning is not promoted"
    0
    (List.length (Claude_code.normalized_events_of_stream_line reasoning)) ;
  let public_text =
    {|{"type":"assistant","message":{"content":[{"type":"text","text":"visible answer"},{"type":"tool_use","id":"tool-1","name":"Read","input":{"file_path":"/private/book.jpg"}}]}}|}
  in
  match Claude_code.normalized_events_of_stream_line public_text with
  | [Task_event.Agent_text_delta text; Task_event.Tool_started tool] ->
      Alcotest.(check string) "public assistant text" "visible answer" text ;
      Alcotest.(check string) "tool name" "Read" tool.Task_event.name ;
      Alcotest.(check bool)
        "tool event omits raw arguments"
        false
        (contains tool.name "/private/book.jpg")
  | _ -> Alcotest.fail "expected normalized text and tool events"

let test_structured_backend_parsers () =
  let codex =
    Codex_cli.normalized_events_of_line
      {|{"type":"item.completed","session_id":"codex-s","item":{"type":"agent_message","text":"codex answer"}}|}
  in
  Alcotest.(check int) "codex public events" 2 (List.length codex) ;
  let gemini =
    Gemini_cli.normalized_events_of_line
      {|{"response":"gemini answer","session_id":"gemini-s","usageMetadata":{"promptTokenCount":4,"candidatesTokenCount":5}}|}
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
      {|{"type":"message","message":{"role":"assistant","content":[{"type":"thinking","thinking":"secret"},{"type":"text","text":"pi answer"}]}}|}
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
        ] );
      ( "backend parser",
        [
          Alcotest.test_case
            "private reasoning is never promoted"
            `Quick
            test_claude_parser_emits_only_proven_public_content;
          Alcotest.test_case
            "structured backend public extraction"
            `Quick
            test_structured_backend_parsers;
        ] );
    ]
