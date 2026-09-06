(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

let object_schema = `Assoc [("type", `String "object")]

let valid_json = {|{"result":"ok"}|}

let yojson = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

let limits : Task_preflight.limits =
  {max_attachments = 4; max_file_size_bytes = 1024; max_total_size_bytes = 4096}

let no_attachment_limits : Task_preflight.limits =
  {max_attachments = 0; max_file_size_bytes = 0; max_total_size_bytes = 0}

let feature_evidence : Backend_types.feature_evidence =
  {
    tested_at_version = "1.0.0";
    test_method = E2e_test;
    evidence_url = None;
    notes = "test_rich_completer";
  }

let descriptor_for ?(session_resume = false) ?(read_only = false)
    ?(media_types = []) ?(web = Backend_types.Web_disabled) id =
  {
    Backend_registry.id;
    display_name = "Rich completer test backend";
    binary_name = "rich-completer-test";
    baseline_version = "1.0.0";
    capabilities =
      {
        structured_output = true;
        streaming_output = true;
        session_resume;
        mcp_support = Mcp_none;
        read_only_support = read_only;
        project_config_surface = Config_none;
        precedence_confidence = Low;
        generated_lsp_config = false;
        file_reading = false;
        media_support =
          {
            media_types;
            evidence =
              (if media_types = [] then None else Some feature_evidence);
          };
        web_support =
          {
            maximum = web;
            evidence =
              (if web = Backend_types.Web_disabled then None
               else Some feature_evidence);
          };
        native_json_schema_output = false;
        native_json_schema_output_evidence = None;
      };
  }

type backend_observation = {
  calls : int ref;
  availability_calls : int ref;
  specs : Backend_types.task_spec list ref;
}

let make_backend ?(session_resume = false) ?(available = fun () -> true) ~id
    run =
  let observation =
    {calls = ref 0; availability_calls = ref 0; specs = ref []}
  in
  let module Backend = struct
    let id = id
    let name = "Rich completer test backend"
    let models = []
    let models_probe = None

    let available ~sw:_ ~env:_ =
      incr observation.availability_calls ;
      available ()

    let supports_session_resume = session_resume
    let native_json_schema_output = false
    let is_resume_failure _ = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "test"

    let run_task ~sw:_ ~env ?context ?on_raw_line:_ spec =
      incr observation.calls ;
      observation.specs := spec :: !(observation.specs) ;
      run ~env ~context ~call:!(observation.calls) spec
  end in
  ((module Backend : Agentic_backend.S), observation)

let register ?(session_resume = false) ?(read_only = false) ?(media_types = [])
    ?(web = Backend_types.Web_disabled) id backend =
  let descriptor =
    descriptor_for ~session_resume ~read_only ~media_types ~web id
  in
  let entry =
    match
      Runtime_entry.create
        ~backend
        ~descriptor
        ~runtime_capabilities:descriptor.capabilities
        ~origin:Runtime_entry.Custom
        ~execution_policy:Runtime_entry.Dispatch_enabled
        ~version_policy:Runtime_entry.No_version_gate
    with
    | Ok entry -> entry
    | Error error ->
        Alcotest.fail (Runtime_entry.render_validation_error error)
  in
  Registry.register_validated entry

let success ?(text = valid_json) ?session_id ?cost () =
  Backend_types.make_task_result
    ~status:Backend_types.Success
    ~agent_text:text
    ?session_id
    ?cost
    ()

let cost input output usd : Backend_types.cost =
  {
    tokens_input = Some input;
    tokens_output = Some output;
    cost_usd = Some usd;
    cache_creation_input_tokens = None;
    cache_read_input_tokens = None;
  }

let with_registry f =
  Registry.clear () ;
  Fun.protect ~finally:Registry.clear f

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path) ;
      Unix.rmdir path
    end
    else Unix.unlink path

let with_workspace f =
  let path = Filename.temp_dir "cabal-rich-completer-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> f path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let make_attachment workspace =
  let contents = "\x89PNG\r\n\x1a\nrich-completer-fixture" in
  write_file (Filename.concat workspace "fixture.png") contents ;
  Backend_types.
    {
      id = "fixture";
      path = "fixture.png";
      media_type = Png;
      sha256 = Digestif.SHA256.(to_hex (digest_string contents));
      size_bytes = String.length contents;
    }

let get_rich ~sw ~env ~limits ~backend_name ~working_dir ?(read_only = false) () =
  match
    Backend_completer.make_rich
      ~sw
      ~env
      ~limits
      ~backend_name
      ~working_dir
      ~read_only
      ()
  with
  | Ok completer -> completer
  | Error error -> Alcotest.fail error

let get_legacy ~sw ~env ~backend_name ~working_dir () =
  match
    Backend_completer.make_by_name
      ~sw
      ~env
      ~backend_name
      ~working_dir
      ()
  with
  | Ok completer -> completer
  | Error error -> Alcotest.fail error

let strictly_increasing events =
  let rec loop previous = function
    | [] -> true
    | event :: rest -> event.Task_event.seq > previous && loop event.seq rest
  in
  loop 0 events

let terminal_is_last events =
  match List.rev events with
  | {Task_event.payload = Terminal _; _} :: _ -> true
  | _ -> false

let test_request_defaults () =
  let request =
    Backend_completer.make_completion_request
      ~system_prompt:"system"
      ~prompt:"prompt"
      ()
  in
  Alcotest.(check (option bool))
    "schema defaults absent"
    None
    (Option.map (fun _ -> true) request.json_schema) ;
  Alcotest.(check (option string))
    "session defaults absent"
    None
    request.resume_session_id ;
  Alcotest.(check int) "attachments default empty" 0 (List.length request.attachments) ;
  Alcotest.(check bool)
    "web defaults disabled"
    true
    (request.web_access = Backend_types.Web_disabled) ;
  Alcotest.(check (float 0.0)) "timeout preserves legacy default" max_float request.timeout ;
  Alcotest.(check (option int)) "max turns defaults absent" None request.max_turns

let test_all_request_fields_map_through_task_spec () =
  with_registry @@ fun () ->
  with_workspace @@ fun workspace ->
  let attachment = make_attachment workspace in
  let backend, observation =
    make_backend ~session_resume:true ~id:"rich-mapping"
      (fun ~env:_ ~context:_ ~call:_ _ ->
        success
          ~session_id:"returned-session"
          ~cost:(cost 3 5 0.25)
          ())
  in
  register
    ~session_resume:true
    ~media_types:[Backend_types.Png]
    ~web:Backend_types.Web_search_and_fetch
    "rich-mapping"
    backend ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let complete =
    get_rich
      ~sw
      ~env
      ~limits
      ~backend_name:"rich-mapping"
      ~working_dir:workspace
      ()
  in
  let resumed_request =
    Backend_completer.make_completion_request
      ~system_prompt:"system-token"
      ~prompt:"resumed-user-token"
      ~json_schema:object_schema
      ~resume_session_id:"resume-token"
      ~attachments:[attachment]
      ~web_access:Backend_types.Web_search_and_fetch
      ~timeout:12.5
      ~max_turns:7
      ()
  in
  let response =
    match complete resumed_request with
    | Ok response -> response
    | Error error ->
        Alcotest.fail (Backend_completer.render_rich_completion_error error)
  in
  Alcotest.(check string) "normalized text" valid_json response.text ;
  Alcotest.(check (option string))
    "final session retained"
    (Some "returned-session")
    response.execution.final_session_id ;
  Alcotest.(check bool)
    "cost retained"
    true
    (response.execution.total_cost = Some (cost 3 5 0.25)) ;
  Alcotest.(check int) "one detailed attempt" 1 (List.length response.execution.attempts) ;
  Alcotest.(check bool) "event order" true (strictly_increasing response.event_trace.events) ;
  Alcotest.(check bool) "terminal retained" true (terminal_is_last response.event_trace.events) ;
  (match List.rev !(observation.specs) with
  | [spec] ->
      Alcotest.(check string)
        "resume maps user prompt without system replay"
        "resumed-user-token"
        spec.prompt ;
      Alcotest.(check (option yojson))
        "schema mapped"
        (Some object_schema)
        spec.json_schema ;
      Alcotest.(check (option string))
        "resume mapped"
        (Some "resume-token")
        spec.resume_session_id ;
      Alcotest.(check bool) "attachments mapped" true (spec.attachments = [attachment]) ;
      Alcotest.(check bool)
        "web mapped"
        true
        (spec.web_access = Backend_types.Web_search_and_fetch) ;
      Alcotest.(check (float 0.0)) "timeout mapped" 12.5 spec.timeout ;
      Alcotest.(check (option int)) "max turns mapped" (Some 7) spec.max_turns
  | specs -> Alcotest.failf "expected one captured spec, got %d" (List.length specs)) ;
  let plain_request =
    Backend_completer.make_completion_request
      ~system_prompt:"system-token"
      ~prompt:"plain-user-token"
      ()
  in
  (match complete plain_request with
  | Ok _ -> ()
  | Error error ->
      Alcotest.fail (Backend_completer.render_rich_completion_error error)) ;
  match List.rev !(observation.specs) with
  | [_; spec] ->
      Alcotest.(check string)
        "system and prompt use legacy composition"
        "SYSTEM INSTRUCTIONS:\nsystem-token\n\n---\n\nUSER REQUEST:\nplain-user-token"
        spec.prompt
  | specs -> Alcotest.failf "expected two captured specs, got %d" (List.length specs)

let test_central_preflight_rejects_before_backend_side_effects () =
  with_registry @@ fun () ->
  with_workspace @@ fun workspace ->
  let attachment = make_attachment workspace in
  let backend, observation =
    make_backend ~id:"rich-preflight" (fun ~env:_ ~context:_ ~call:_ _ -> success ())
  in
  register "rich-preflight" backend ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let complete =
    get_rich
      ~sw
      ~env
      ~limits
      ~backend_name:"rich-preflight"
      ~working_dir:workspace
      ()
  in
  let check request expected =
    match complete request with
    | Error
        {
          cause =
            Runtime_dispatch.Dispatch_failure
              (Runtime_dispatch.Preflight_failed
                (Task_preflight.Capability actual));
          _;
        }
      when expected actual ->
        ()
    | Error error ->
        Alcotest.failf
          "unexpected preflight error: %s"
          (Backend_completer.render_rich_completion_error error)
    | Ok _ -> Alcotest.fail "unsupported request passed central preflight"
  in
  check
    (Backend_completer.make_completion_request
       ~system_prompt:"system"
       ~prompt:"prompt"
       ~web_access:Backend_types.Web_search
       ())
    (function Task_preflight.Unsupported_web_access _ -> true | _ -> false) ;
  check
    (Backend_completer.make_completion_request
       ~system_prompt:"system"
       ~prompt:"prompt"
       ~resume_session_id:"unsupported-session"
       ())
    (function Task_preflight.Session_resume_unsupported -> true | _ -> false) ;
  check
    (Backend_completer.make_completion_request
       ~system_prompt:"system"
       ~prompt:"prompt"
       ~attachments:[attachment]
       ())
    (function Task_preflight.Unsupported_media_type Png -> true | _ -> false) ;
  Alcotest.(check int) "availability not called" 0 !(observation.availability_calls) ;
  Alcotest.(check int) "backend not called" 0 !(observation.calls)

let test_rich_text_and_events_never_promote_raw_output () =
  with_registry @@ fun () ->
  let backend, _ =
    make_backend ~id:"rich-private-output"
      (fun ~env:_ ~context:_ ~call:_ _ ->
        Backend_types.make_task_result
          ~status:Backend_types.Success
          ~stdout:"private-raw-line"
          ~agent_text:""
          ())
  in
  register "rich-private-output" backend ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let complete =
    get_rich
      ~sw
      ~env
      ~limits:no_attachment_limits
      ~backend_name:"rich-private-output"
      ~working_dir:"/tmp"
      ()
  in
  let response =
    match
      complete
        (Backend_completer.make_completion_request
           ~system_prompt:"system"
           ~prompt:"prompt"
           ())
    with
    | Ok response -> response
    | Error error ->
        Alcotest.fail (Backend_completer.render_rich_completion_error error)
  in
  Alcotest.(check string) "rich text is normalized only" "" response.text ;
  let promoted =
    List.exists
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Agent_text_delta text -> text = "private-raw-line"
        | _ -> false)
      response.event_trace.events
  in
  Alcotest.(check bool) "raw stdout absent from events" false promoted

let test_prepared_snapshot_and_two_attempt_detail () =
  with_registry @@ fun () ->
  let replacement_calls = ref 0 in
  let replacement, _ =
    make_backend ~id:"rich-snapshot" (fun ~env:_ ~context:_ ~call:_ _ ->
        incr replacement_calls ;
        success ~text:{|{"replacement":true}|} ())
  in
  let original, observation =
    make_backend ~id:"rich-snapshot" (fun ~env:_ ~context ~call _ ->
        Option.iter
          (fun context ->
            Task_execution_context.emit
              context
              (Task_event.Tool_started {id = Some "tool"; name = "read"}))
          context ;
        if call = 1 then begin
          Registry.register replacement ;
          success
            ~text:"not-json"
            ~session_id:"first-session"
            ~cost:(cost 2 0 0.1)
            ()
        end
        else
          success
            ~session_id:"second-session"
            ~cost:(cost 3 5 0.2)
            ())
  in
  register "rich-snapshot" original ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let complete =
    get_rich
      ~sw
      ~env
      ~limits:no_attachment_limits
      ~backend_name:"rich-snapshot"
      ~working_dir:"/tmp"
      ()
  in
  let request =
    Backend_completer.make_completion_request
      ~system_prompt:"system"
      ~prompt:"prompt"
      ~json_schema:object_schema
      ()
  in
  let response =
    match complete request with
    | Ok response -> response
    | Error error ->
        Alcotest.fail (Backend_completer.render_rich_completion_error error)
  in
  Alcotest.(check int) "prepared backend owns both attempts" 2 !(observation.calls) ;
  Alcotest.(check int) "replacement never runs" 0 !replacement_calls ;
  (match response.execution.attempts with
  | [first; second] ->
      Alcotest.(check bool)
        "attempt kinds retained"
        true
        (first.kind = Backend_types.Initial_attempt
        && second.kind = Backend_types.Fresh_attempt) ;
      Alcotest.(check bool)
        "first validation error retained"
        true
        (Option.is_some first.schema_validation_error) ;
      Alcotest.(check string) "first output retained" "not-json" first.result.agent_text ;
      Alcotest.(check string) "second output retained" valid_json second.result.agent_text
  | attempts -> Alcotest.failf "expected two attempts, got %d" (List.length attempts)) ;
  Alcotest.(check (option string))
    "last session retained"
    (Some "second-session")
    response.execution.final_session_id ;
  (match response.execution.total_cost with
  | Some total ->
      Alcotest.(check (option int)) "input cost aggregated" (Some 5) total.tokens_input ;
      Alcotest.(check (option int)) "output cost aggregated" (Some 5) total.tokens_output ;
      Alcotest.(check (option (float 0.000001))) "USD aggregated" (Some 0.3) total.cost_usd
  | None -> Alcotest.fail "aggregate cost missing") ;
  Alcotest.(check bool) "events strictly ordered" true (strictly_increasing response.event_trace.events) ;
  Alcotest.(check bool) "terminal delivered before return" true (terminal_is_last response.event_trace.events)

let test_structured_error_and_legacy_projection () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let install () =
    let backend, observation =
      make_backend ~id:"rich-error" (fun ~env:_ ~context:_ ~call _ ->
          if call = 1 then success ~text:"not-json" ()
          else success ~text:"[]" ())
    in
    register "rich-error" backend ;
    observation
  in
  let legacy_observation = install () in
  let legacy =
    get_legacy ~sw ~env ~backend_name:"rich-error" ~working_dir:"/tmp" ()
  in
  let legacy_error =
    match
      legacy
        ~system_prompt:"system"
        ~prompt:"prompt"
        ~json_schema:(Some object_schema)
        ~resume_session_id:None
    with
    | Error error -> error
    | Ok _ -> Alcotest.fail "legacy double validation unexpectedly succeeded"
  in
  Alcotest.(check int) "legacy has two calls" 2 !(legacy_observation.calls) ;
  Registry.clear () ;
  let rich_observation = install () in
  let rich =
    get_rich
      ~sw
      ~env
      ~limits:no_attachment_limits
      ~backend_name:"rich-error"
      ~working_dir:"/tmp"
      ()
  in
  let request =
    Backend_completer.make_completion_request
      ~system_prompt:"system"
      ~prompt:"prompt"
      ~json_schema:object_schema
      ()
  in
  (match rich request with
  | Error
      ({
         cause =
           Runtime_dispatch.Execution_failure
             (Backend_types.Schema_retry_failed {execution; _});
         event_trace;
       } as error) ->
      Alcotest.(check int) "rich has two calls" 2 !(rich_observation.calls) ;
      Alcotest.(check int) "error retains both attempts" 2 (List.length execution.attempts) ;
      Alcotest.(check bool) "error event order" true (strictly_increasing event_trace.events) ;
      Alcotest.(check bool) "error terminal delivered" true (terminal_is_last event_trace.events) ;
      Alcotest.(check string)
        "legacy renderer is byte-compatible"
        legacy_error
        (Backend_completer.render_rich_completion_error error)
  | Error error ->
      Alcotest.failf
        "wrong rich error: %s"
        (Backend_completer.render_rich_completion_error error)
  | Ok _ -> Alcotest.fail "rich double validation unexpectedly succeeded")

let test_event_capture_is_bounded_and_reports_omission () =
  with_registry @@ fun () ->
  let usage = cost 1 1 0.0 in
  let backend, _ =
    make_backend ~id:"rich-events" (fun ~env:_ ~context ~call:_ _ ->
        Option.iter
          (fun context ->
            for _ = 1 to 512 do
              Task_execution_context.emit context (Task_event.Token_usage usage) ;
              Eio.Fiber.yield ()
            done)
          context ;
        success ())
  in
  register "rich-events" backend ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let complete =
    get_rich
      ~sw
      ~env
      ~limits:no_attachment_limits
      ~backend_name:"rich-events"
      ~working_dir:"/tmp"
      ()
  in
  let response =
    match
      complete
        (Backend_completer.make_completion_request
           ~system_prompt:"system"
           ~prompt:"prompt"
           ())
    with
    | Ok response -> response
    | Error error ->
        Alcotest.fail (Backend_completer.render_rich_completion_error error)
  in
  let events = response.event_trace.events in
  let queue_truncated =
    List.exists
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Event_delivery_truncated _ -> true
        | _ -> false)
      events
  in
  Alcotest.(check bool)
    "capture length is bounded"
    true
    (List.length events <= Backend_completer.max_captured_events) ;
  Alcotest.(check bool)
    "collector or delivery reports truncation"
    true
    (response.event_trace.omitted_events > 0 || queue_truncated) ;
  Alcotest.(check bool) "retained events stay ordered" true (strictly_increasing events) ;
  Alcotest.(check bool) "terminal is retained" true (terminal_is_last events)

let test_validator_uses_effective_central_read_only_gate () =
  with_registry @@ fun () ->
  let backend, observation =
    make_backend ~id:"claude-code" (fun ~env:_ ~context:_ ~call:_ _ -> success ())
  in
  register ~read_only:false "claude-code" backend ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let validator =
    match
      Backend_completer.make_validator_by_name
        ~sw
        ~env
        ~backend_name:"claude-code"
        ~working_dir:"/tmp"
        ()
    with
    | Ok completer -> completer
    | Error error -> Alcotest.fail error
  in
  (match
     validator
       ~system_prompt:"system"
       ~prompt:"prompt"
       ~json_schema:None
       ~resume_session_id:None
   with
  | Error error ->
      Alcotest.(check bool)
        "effective preflight diagnostic"
        true
        (error = "backend does not support the requested read-only mode")
  | Ok _ -> Alcotest.fail "static read-only claim bypassed effective descriptor") ;
  Alcotest.(check int) "availability not reached" 0 !(observation.availability_calls) ;
  Alcotest.(check int) "backend not reached" 0 !(observation.calls)

let () =
  Alcotest.run
    "CBL-06 rich completer"
    [
      ( "request",
        [
          Alcotest.test_case "defaults" `Quick test_request_defaults;
          Alcotest.test_case
            "all fields map through task_spec"
            `Quick
            test_all_request_fields_map_through_task_spec;
        ] );
      ( "central dispatch",
        [
          Alcotest.test_case
            "preflight before side effects"
            `Quick
            test_central_preflight_rejects_before_backend_side_effects;
          Alcotest.test_case
            "prepared snapshot and two-attempt detail"
            `Quick
            test_prepared_snapshot_and_two_attempt_detail;
          Alcotest.test_case
            "effective read-only gate"
            `Quick
            test_validator_uses_effective_central_read_only_gate;
        ] );
      ( "response",
        [
          Alcotest.test_case
            "structured error and legacy projection"
            `Quick
            test_structured_error_and_legacy_projection;
          Alcotest.test_case
            "bounded event capture"
            `Quick
            test_event_capture_is_bounded_and_reports_omission;
          Alcotest.test_case
            "raw output remains private"
            `Quick
            test_rich_text_and_events_never_promote_raw_output;
        ] );
    ]
