(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

let object_schema = `Assoc [("type", `String "object")]

let valid_text = {|{"result":"ok"}|}

let invalid_text_1 = "not-json-at-all"

let invalid_text_2 = "[]"

let contains text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  needle_length = 0 || search 0

let attachment : Backend_types.media_attachment =
  {
    id = "cover";
    path = "media/cover.png";
    media_type = Png;
    sha256 = String.make 64 'a';
    size_bytes = 42;
  }

let cost ?tokens_input ?tokens_output ?cost_usd ?cache_creation_input_tokens
    ?cache_read_input_tokens () : Backend_types.cost =
  {
    tokens_input;
    tokens_output;
    cost_usd;
    cache_creation_input_tokens;
    cache_read_input_tokens;
  }

let result ?(status = Backend_types.Success) ?(text = valid_text) ?session_id
    ?(elapsed = 0.0) ?cost ?(stderr = "") () =
  Backend_types.make_task_result
    ~status
    ~agent_text:text
    ?session_id
    ~elapsed
    ?cost
    ~stderr
    ()

let make_mock ?(native = false) ?(supports_resume = false)
    ?(resume_failure = fun _ -> false) ?(on_context = fun _ -> ())
    ?(delay = 0.0) responses =
  let responses = Array.of_list responses in
  let calls = ref 0 in
  let captured_specs = ref [] in
  let module Backend = struct
    let id = "task-execution-mock"

    let name = "Task execution mock"

    let models = []

    let models_probe = None

    let available ~sw:_ ~env:_ = true

    let supports_session_resume = supports_resume

    let native_json_schema_output = native

    let is_resume_failure = resume_failure

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "mock"

    let run_task ~sw:_ ~env ?context ?on_raw_line:_ spec =
      let index = !calls in
      incr calls ;
      captured_specs := spec :: !captured_specs ;
      Option.iter on_context context ;
      if delay > 0.0 then Eio.Time.sleep (Eio.Stdenv.clock env) delay ;
      if index < Array.length responses then responses.(index)
      else
        result
          ~status:(Backend_types.Failed "unexpected extra backend call")
          ()
  end in
  ( (module Backend : Agentic_backend.S),
    calls,
    fun () -> List.rev !captured_specs )

let make_spec ?(prompt = "identify book") ?timeout ?json_schema
    ?(attachments = []) ?(web_access = Backend_types.Web_disabled) () =
  Backend_types.make_task_spec
    ~prompt
    ~working_dir:"/tmp"
    ?timeout
    ?json_schema
    ~attachments
    ~web_access
    ()

let run_detailed ?context ~backend spec =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Json_schema_enforcer.run_task_detailed
    ~sw
    ~env
    ?context:(Option.map (fun build -> build sw) context)
    ~backend
    spec

let run_legacy ~backend spec =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Json_schema_enforcer.run_task ~sw ~env ~backend spec

let get_execution = function
  | Ok execution -> execution
  | Error error ->
      Alcotest.failf
        "expected detailed success, got: %s"
        (Json_schema_enforcer.render_error error)

let kinds execution =
  List.map
    (fun (attempt : Backend_types.task_attempt) -> attempt.kind)
    execution.Backend_types.attempts

let check_single_initial execution expected_result =
  match execution.Backend_types.attempts with
  | [attempt] ->
      Alcotest.(check int) "attempt number" 1 attempt.number ;
      Alcotest.(check bool)
        "initial kind"
        true
        (attempt.kind = Backend_types.Initial_attempt) ;
      Alcotest.(check bool) "complete result retained" true (attempt.result = expected_result)
  | attempts -> Alcotest.failf "expected one attempt, got %d" (List.length attempts)

let test_no_schema_detailed () =
  let first =
    result
      ~session_id:"session-1"
      ~elapsed:1.25
      ~cost:(cost ~tokens_input:3 ())
      ()
  in
  let backend, calls, _ = make_mock [first] in
  let execution = get_execution (run_detailed ~backend (make_spec ())) in
  Alcotest.(check int) "one call" 1 !calls ;
  check_single_initial execution first ;
  Alcotest.(check bool) "final result" true (execution.final_result = first) ;
  Alcotest.(check (option string))
    "final session"
    (Some "session-1")
    execution.final_session_id

let test_native_success_and_rejection () =
  let success = result ~text:"native output is not validated" () in
  let success_backend, success_calls, _ = make_mock ~native:true [success] in
  let success_execution =
    get_execution
      (run_detailed
         ~backend:success_backend
         (make_spec ~json_schema:object_schema ()))
  in
  Alcotest.(check int) "native success one call" 1 !success_calls ;
  check_single_initial success_execution success ;
  let rejected =
    result
      ~status:(Backend_types.Failed "schema rejected")
      ~text:""
      ~elapsed:2.0
      ~stderr:"native detail"
      ()
  in
  let rejected_backend, rejected_calls, _ = make_mock ~native:true [rejected] in
  match
    run_detailed
      ~backend:rejected_backend
      (make_spec ~json_schema:object_schema ())
  with
  | Error
      (Backend_types.Native_backend_failure_with_schema {execution; message}) ->
      Alcotest.(check int) "native failure one call" 1 !rejected_calls ;
      Alcotest.(check string) "native message" "schema rejected" message ;
      check_single_initial execution rejected ;
      let legacy_backend, _, _ = make_mock ~native:true [rejected] in
      (match
         run_legacy
           ~backend:legacy_backend
           (make_spec ~json_schema:object_schema ())
       with
      | Error rendered ->
          Alcotest.(check string)
            "native exact compatibility renderer"
            (Json_schema_enforcer.render_error
               (Backend_types.Native_backend_failure_with_schema
                  {execution; message}))
            rendered ;
          Alcotest.(check string)
            "native pinned text"
            "native-backend call failed with a schema in force: schema \
             rejected\nbackend stderr: native detail"
            rendered
      | Ok _ -> Alcotest.fail "legacy native failure unexpectedly succeeded")
  | Error error ->
      Alcotest.failf
        "wrong native error: %s"
        (Json_schema_enforcer.render_error error)
  | Ok _ -> Alcotest.fail "native failure unexpectedly succeeded"

let test_first_validation_success () =
  let first = result ~session_id:"valid-first" () in
  let backend, calls, _ = make_mock [first] in
  let execution =
    get_execution
      (run_detailed ~backend (make_spec ~json_schema:object_schema ()))
  in
  Alcotest.(check int) "one call" 1 !calls ;
  check_single_initial execution first ;
  match execution.attempts with
  | [attempt] ->
      Alcotest.(check (option string))
        "no validation error"
        None
        attempt.schema_validation_error
  | _ -> Alcotest.fail "attempt shape changed"

let test_fresh_retry_success_preserves_complete_results_and_cost () =
  let first =
    result
      ~text:invalid_text_1
      ~session_id:"first-session"
      ~elapsed:10.0
      ~cost:(cost ~tokens_input:2 ~cost_usd:0.25 ())
      ()
  in
  let second =
    result
      ~session_id:"second-session"
      ~elapsed:20.0
      ~cost:(cost ~tokens_input:3 ~tokens_output:7 ())
      ()
  in
  let backend, calls, _ = make_mock ~delay:0.01 [first; second] in
  let execution =
    get_execution
      (run_detailed ~backend (make_spec ~json_schema:object_schema ()))
  in
  Alcotest.(check int) "two calls" 2 !calls ;
  Alcotest.(check bool)
    "ordered kinds"
    true
    (kinds execution = [Initial_attempt; Fresh_attempt]) ;
  (match execution.attempts with
  | [attempt1; attempt2] ->
      Alcotest.(check bool) "first complete result" true (attempt1.result = first) ;
      Alcotest.(check bool) "second complete result" true (attempt2.result = second) ;
      Alcotest.(check bool)
        "first validation error"
        true
        (Option.is_some attempt1.schema_validation_error) ;
      Alcotest.(check (option string))
        "second validation valid"
        None
        attempt2.schema_validation_error ;
      Alcotest.(check bool)
        "attempt 1 boundary elapsed"
        true
        (attempt1.attempt_elapsed >= 0.005) ;
      Alcotest.(check bool)
        "attempt 2 boundary elapsed"
        true
        (attempt2.attempt_elapsed >= 0.005)
  | _ -> Alcotest.fail "expected two detailed attempts") ;
  Alcotest.(check bool) "final is retry result" true (execution.final_result = second) ;
  Alcotest.(check (option string))
    "successful retry session selected"
    (Some "second-session")
    execution.final_session_id ;
  Alcotest.(check bool)
    "wall elapsed measured, not task_result sum"
    true
    (execution.total_elapsed >= 0.015 && execution.total_elapsed < 1.0) ;
  match execution.total_cost with
  | None -> Alcotest.fail "aggregated cost missing"
  | Some total ->
      Alcotest.(check (option int)) "input tokens summed" (Some 5) total.tokens_input ;
      Alcotest.(check (option int)) "known output retained" (Some 7) total.tokens_output ;
      Alcotest.(check (option (float 0.000001))) "known USD retained" (Some 0.25) total.cost_usd

let test_resume_retry_and_final_non_empty_session () =
  let first = result ~text:invalid_text_1 ~session_id:"session-kept" () in
  let second = result ~session_id:"" () in
  let backend, calls, _ = make_mock ~supports_resume:true [first; second] in
  let execution =
    get_execution
      (run_detailed ~backend (make_spec ~json_schema:object_schema ()))
  in
  Alcotest.(check int) "two calls" 2 !calls ;
  Alcotest.(check bool)
    "resume kind"
    true
    (kinds execution = [Initial_attempt; Resumed_attempt]) ;
  Alcotest.(check (option string))
    "last non-empty session selected"
    (Some "session-kept")
    execution.final_session_id

let test_blank_session_ids_choose_fresh_retry () =
  List.iter
    (fun blank_session_id ->
      let first =
        result ~text:invalid_text_1 ~session_id:blank_session_id ()
      in
      let second = result ~session_id:"fresh-final-session" () in
      let backend, calls, captured =
        make_mock ~supports_resume:true [first; second]
      in
      let execution =
        get_execution
          (run_detailed ~backend (make_spec ~json_schema:object_schema ()))
      in
      Alcotest.(check int) "blank session still uses two calls" 2 !calls ;
      Alcotest.(check bool)
        "blank session chooses fresh kind"
        true
        (kinds execution = [Initial_attempt; Fresh_attempt]) ;
      (match captured () with
      | [_; retry_spec] ->
          Alcotest.(check (option string))
            "fresh retry clears resume id"
            None
            retry_spec.Backend_types.resume_session_id
      | _ -> Alcotest.fail "expected initial and fresh specs") ;
      (match execution.attempts with
      | [_; retry] ->
          Alcotest.(check bool)
            "blank-session retry uploads attachments"
            true
            (retry.delivery.attachment_delivery = Upload_attachments)
      | _ -> Alcotest.fail "expected two attempts") ;
      Alcotest.(check (option string))
        "last nonblank session remains final"
        (Some "fresh-final-session")
        execution.final_session_id)
    [""; "   \t"]

let test_double_invalid_structured_error () =
  let first = result ~text:invalid_text_1 ~elapsed:1.0 () in
  let second = result ~text:invalid_text_2 ~elapsed:2.0 () in
  let backend, calls, _ = make_mock [first; second] in
  match
    run_detailed ~backend (make_spec ~json_schema:object_schema ())
  with
  | Error
      (Backend_types.Schema_retry_failed
        {
          execution;
          attempt_1_validation_error;
          attempt_2_failure = Schema_validation_failure attempt_2_validation_error;
        }) ->
      Alcotest.(check int) "two calls" 2 !calls ;
      Alcotest.(check bool)
        "distinct validation strings retained"
        true
        (attempt_1_validation_error <> attempt_2_validation_error) ;
      (match execution.attempts with
      | [attempt1; attempt2] ->
          Alcotest.(check bool) "first result retained" true (attempt1.result = first) ;
          Alcotest.(check bool) "second result retained" true (attempt2.result = second) ;
          Alcotest.(check (option string))
            "attempt 1 error"
            (Some attempt_1_validation_error)
            attempt1.schema_validation_error ;
          Alcotest.(check (option string))
            "attempt 2 error"
            (Some attempt_2_validation_error)
            attempt2.schema_validation_error
      | _ -> Alcotest.fail "expected both completed attempts")
  | Error error ->
      Alcotest.failf
        "wrong double-invalid error: %s"
        (Json_schema_enforcer.render_error error)
  | Ok _ -> Alcotest.fail "double invalid unexpectedly succeeded"

let test_invoked_resume_failure_stops_at_two_calls () =
  let first = result ~text:invalid_text_1 ~session_id:"expired-session" () in
  let second =
    result ~status:(Backend_types.Failed "resume session not found") ~text:"" ()
  in
  let backend, calls, _ =
    make_mock
      ~supports_resume:true
      ~resume_failure:(fun candidate -> candidate = second)
      [first; second]
  in
  match
    run_detailed ~backend (make_spec ~json_schema:object_schema ())
  with
  | Error
      (Backend_types.Schema_retry_failed
        {execution; attempt_2_failure = Resume_failure (Failed message); _}) ->
      Alcotest.(check string) "resume failure retained" "resume session not found" message ;
      Alcotest.(check int) "hard cap remains two" 2 !calls ;
      Alcotest.(check int) "two attempts" 2 (List.length execution.attempts)
  | Error error ->
      Alcotest.failf
        "wrong resume failure: %s"
        (Json_schema_enforcer.render_error error)
  | Ok _ -> Alcotest.fail "resume failure unexpectedly succeeded"

let validator_error document =
  match Json_schema_validator.validate ~schema:object_schema ~document with
  | Error message -> message
  | Ok () -> Alcotest.fail "expected validator error"

let test_throwing_resume_classifier_is_conservative () =
  let first = result ~text:invalid_text_1 ~session_id:"classifier-session" () in
  let second = result ~status:(Backend_types.Failed "retry transport failed") () in
  let make () =
    make_mock
      ~supports_resume:true
      ~resume_failure:(fun _ -> failwith "private classifier exception")
      [first; second]
  in
  let diagnostics = ref [] in
  let backend, calls, _ = make () in
  let detailed =
    Fun.protect
      ~finally:Diagnostics.reset_handler
      (fun () ->
        Diagnostics.set_handler (fun event -> diagnostics := event :: !diagnostics) ;
        run_detailed ~backend (make_spec ~json_schema:object_schema ()))
  in
  match detailed with
  | Error
      (Backend_types.Schema_retry_failed
        {
          execution;
          attempt_2_failure = Transport_failure (Failed message);
          _;
        }) ->
      Alcotest.(check int) "throwing classifier keeps two calls" 2 !calls ;
      Alcotest.(check string)
        "actual transport result classifies failure"
        "retry transport failed"
        message ;
      Alcotest.(check bool)
        "both complete results survive classifier exception"
        true
        (match execution.attempts with
        | [attempt1; attempt2] ->
            attempt1.result = first && attempt2.result = second
        | _ -> false) ;
      Alcotest.(check bool)
        "diagnostic is emitted"
        true
        (!diagnostics <> []) ;
      Alcotest.(check bool)
        "diagnostic omits private exception"
        false
        (List.exists
         (function
             | Diagnostics.Log (_, text) | Diagnostics.User_warning text ->
                 contains text "private classifier exception")
           !diagnostics) ;
      let rendered =
        "Both schema enforcement attempts failed.\nAttempt 1: "
        ^ validator_error invalid_text_1
        ^ "\nAttempt 2: Failed: retry transport failed"
      in
      Alcotest.(check string)
        "compatibility renderer remains exact"
        rendered
        (Json_schema_enforcer.render_error
           (Backend_types.Schema_retry_failed
              {
                execution;
                attempt_1_validation_error = validator_error invalid_text_1;
                attempt_2_failure = Transport_failure second.status;
              })) ;
      let legacy_backend, legacy_calls, _ = make () in
      (match
         run_legacy
           ~backend:legacy_backend
           (make_spec ~json_schema:object_schema ())
       with
      | Error legacy_rendered ->
          Alcotest.(check string)
            "legacy projection uses compatibility renderer"
            rendered
            legacy_rendered ;
          Alcotest.(check int) "legacy remains two calls" 2 !legacy_calls
      | Ok _ -> Alcotest.fail "throwing classifier legacy call unexpectedly succeeded")
  | Error error ->
      Alcotest.failf
        "throwing classifier produced wrong error: %s"
        (Json_schema_enforcer.render_error error)
  | Ok _ -> Alcotest.fail "throwing classifier unexpectedly succeeded"

let test_classifier_time_is_in_total_elapsed () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let first = result ~text:invalid_text_1 ~session_id:"slow-classifier" () in
  let second = result ~status:(Backend_types.Failed "resume unavailable") () in
  let backend, _, _ =
    make_mock
      ~supports_resume:true
      ~resume_failure:(fun _ ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.02 ;
        true)
      [first; second]
  in
  match
    Json_schema_enforcer.run_task_detailed
      ~sw
      ~env
      ~backend
      (make_spec ~json_schema:object_schema ())
  with
  | Error
      (Backend_types.Schema_retry_failed
        {execution; attempt_2_failure = Resume_failure _; _}) ->
      Alcotest.(check bool)
        "classifier delay is inside enforcer elapsed"
        true
        (execution.total_elapsed >= 0.015)
  | Error error ->
      Alcotest.failf
        "slow classifier produced wrong error: %s"
        (Json_schema_enforcer.render_error error)
  | Ok _ -> Alcotest.fail "slow resume failure unexpectedly succeeded"

let test_classifier_preserves_cancellation_and_fatals () =
  List.iter
    (fun fatal ->
      let first = result ~text:invalid_text_1 ~session_id:"fatal-session" () in
      let second = result ~status:(Backend_types.Failed "resume failed") () in
      let backend, _, _ =
        make_mock
          ~supports_resume:true
          ~resume_failure:(fun _ -> raise fatal)
          [first; second]
      in
      let propagated =
        try
          ignore
            (run_detailed ~backend (make_spec ~json_schema:object_schema ())) ;
          false
        with
        | Out_of_memory -> fatal = Out_of_memory
        | Stack_overflow -> fatal = Stack_overflow
        | Sys.Break -> fatal = Sys.Break
        | _ -> false
      in
      Alcotest.(check bool) "fatal classifier exception propagates" true propagated)
    [Out_of_memory; Stack_overflow; Sys.Break] ;
  let first = result ~text:invalid_text_1 ~session_id:"cancel-classifier" () in
  let second = result ~status:(Backend_types.Failed "resume failed") () in
  let backend, _, _ =
    make_mock
      ~supports_resume:true
      ~resume_failure:(fun _ ->
        raise (Eio.Cancel.Cancelled (Failure "classifier cancellation")))
      [first; second]
  in
  let propagated =
    try
      ignore (run_detailed ~backend (make_spec ~json_schema:object_schema ())) ;
      false
    with Eio.Cancel.Cancelled _ -> true
  in
  Alcotest.(check bool) "classifier cancellation propagates" true propagated

let test_fresh_transport_failure_is_structured () =
  let first = result ~text:invalid_text_1 () in
  let second = result ~status:(Backend_types.Failed "fresh transport failed") () in
  let backend, calls, _ = make_mock [first; second] in
  match run_detailed ~backend (make_spec ~json_schema:object_schema ()) with
  | Error
      (Backend_types.Schema_retry_failed
        {
          execution;
          attempt_2_failure = Transport_failure (Failed message);
          _;
        }) ->
      Alcotest.(check string) "transport message retained" "fresh transport failed" message ;
      Alcotest.(check int) "transport failure two calls" 2 !calls ;
      Alcotest.(check int) "transport result retained" 2 (List.length execution.attempts)
  | Error error ->
      Alcotest.failf
        "wrong transport failure: %s"
        (Json_schema_enforcer.render_error error)
  | Ok _ -> Alcotest.fail "fresh transport failure unexpectedly succeeded"

let test_timeout_and_cancellation_results_do_not_retry () =
  List.iter
    (fun status ->
      let first = result ~status ~text:"" () in
      let backend, calls, _ = make_mock [first] in
      let execution =
        get_execution
          (run_detailed ~backend (make_spec ~json_schema:object_schema ()))
      in
      Alcotest.(check int) "non-success one call" 1 !calls ;
      check_single_initial execution first)
    [Backend_types.Timeout; Backend_types.Cancelled]

let test_deadline_expiry_before_retry () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let first = result ~text:invalid_text_1 ~session_id:"deadline-session" () in
  let backend, calls, _ = make_mock [first] in
  let sink = Task_event.create_sink ~sw ~now:(fun () -> 0.0) () in
  let context =
    Task_execution_context.create ~remaining_time:(fun () -> Some 0.0) sink
  in
  let execution =
    get_execution
      (Json_schema_enforcer.run_task_detailed
         ~sw
         ~env
         ~context
         ~backend
         (make_spec ~json_schema:object_schema ()))
  in
  Alcotest.(check int) "expired deadline one call" 1 !calls ;
  Alcotest.(check int) "only completed call retained" 1 (List.length execution.attempts) ;
  Alcotest.(check bool)
    "deadline result is timeout"
    true
    (execution.final_result.status = Backend_types.Timeout) ;
  Alcotest.(check (option string))
    "completed session retained"
    (Some "deadline-session")
    execution.final_session_id

let test_cancellation_between_attempts_does_not_start_retry () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let first = result ~text:invalid_text_1 () in
  let backend, calls, _ = make_mock [first] in
  let events = ref [] in
  let cancellation = ref None in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () -> 0.0)
      ~on_event:(fun event ->
        events := event :: !events ;
        match event.Task_event.payload, !cancellation with
        | Attempt_finished _, Some token ->
            Eio.Cancel.cancel token (Failure "cancel between attempts")
        | _ -> ())
      ()
  in
  let context =
    Task_execution_context.create ~remaining_time:(fun () -> None) sink
  in
  let cancelled =
    try
      Eio.Cancel.sub (fun token ->
          cancellation := Some token ;
          ignore
            (Json_schema_enforcer.run_task_detailed
               ~sw
               ~env
               ~context
               ~backend
               (make_spec ~json_schema:object_schema ())) ;
          false)
    with Eio.Cancel.Cancelled _ -> true
  in
  Eio.Cancel.protect (fun () ->
      Task_event.emit_terminal sink Task_event.Cancelled ;
      Eio.Promise.await (Task_event.Private.delivery_complete sink)) ;
  Alcotest.(check bool) "cancellation observed" true cancelled ;
  Alcotest.(check int) "retry not invoked" 1 !calls ;
  let payloads = List.rev_map (fun event -> event.Task_event.payload) !events in
  Alcotest.(check bool)
    "retry not announced"
    false
    (List.exists (function Task_event.Retry_transition _ -> true | _ -> false) payloads)

let event_outcome_of_status = function
  | Backend_types.Success -> Task_event.Attempt_succeeded
  | Backend_types.Failed _ -> Task_event.Attempt_failed
  | Backend_types.Timeout -> Task_event.Attempt_timed_out
  | Backend_types.Cancelled -> Task_event.Attempt_cancelled

let execution_of_outcome = function
  | Ok execution -> execution
  | Error (Backend_types.Native_backend_failure_with_schema {execution; _})
  | Error (Backend_types.Schema_retry_failed {execution; _}) ->
      execution

type attempt_lifecycle =
  | Started of int * Backend_types.attempt_kind
  | Finished of int * Task_event.attempt_outcome

let attempt_lifecycle events =
  List.filter_map
    (fun event ->
      match event.Task_event.payload with
      | Attempt_started kind -> Some (Started (event.attempt, kind))
      | Attempt_finished outcome -> Some (Finished (event.attempt, outcome))
      | _ -> None)
    events

let expected_lifecycle execution =
  List.concat_map
    (fun (attempt : Backend_types.task_attempt) ->
      [
        Started (attempt.number, attempt.kind);
        Finished (attempt.number, event_outcome_of_status attempt.result.status);
      ])
    execution.Backend_types.attempts

let run_detailed_with_events ~env ~sw ~backend spec =
  let events = ref [] in
  let sink =
    Task_event.create_sink
      ~sw
      ~now:(fun () -> 0.0)
      ~on_event:(fun event -> events := event :: !events)
      ()
  in
  let context =
    Task_execution_context.create ~remaining_time:(fun () -> None) sink
  in
  let outcome =
    Json_schema_enforcer.run_task_detailed
      ~sw
      ~env
      ~context
      ~backend
      spec
  in
  Task_event.emit_terminal sink Task_event.Succeeded ;
  Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
  (outcome, List.rev !events)

let check_exact_attempt_events label outcome events =
  let execution = execution_of_outcome outcome in
  Alcotest.(check bool)
    (label ^ ": exact start/finish lifecycle")
    true
    (attempt_lifecycle events = expected_lifecycle execution)

let status_cases =
  [
    ("success", Backend_types.Success);
    ("failed", Backend_types.Failed "matrix failure");
    ("timeout", Backend_types.Timeout);
    ("cancelled", Backend_types.Cancelled);
  ]

let result_for_status status =
  match status with
  | Backend_types.Success -> result ()
  | Backend_types.Failed _ | Backend_types.Timeout | Backend_types.Cancelled ->
      result ~status ~text:"" ()

let test_attempt_event_agreement_matrix () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (status_name, status) ->
      let backend, calls, _ = make_mock [result_for_status status] in
      let outcome, events =
        run_detailed_with_events ~env ~sw ~backend (make_spec ())
      in
      Alcotest.(check int) (status_name ^ ": initial call count") 1 !calls ;
      check_exact_attempt_events ("initial " ^ status_name) outcome events)
    status_cases ;
  List.iter
    (fun (supports_resume, retry_name, expected_kind) ->
      List.iter
        (fun (status_name, status) ->
          let first_session =
            if supports_resume then Some "matrix-session" else None
          in
          let first =
            result ~text:invalid_text_1 ?session_id:first_session ()
          in
          let backend, calls, _ =
            make_mock
              ~supports_resume
              [first; result_for_status status]
          in
          let outcome, events =
            run_detailed_with_events
              ~env
              ~sw
              ~backend
              (make_spec ~json_schema:object_schema ())
          in
          let execution = execution_of_outcome outcome in
          Alcotest.(check int)
            (retry_name ^ " " ^ status_name ^ ": call count")
            2
            !calls ;
          Alcotest.(check bool)
            (retry_name ^ " " ^ status_name ^ ": exact kinds")
            true
            (kinds execution = [Initial_attempt; expected_kind]) ;
          check_exact_attempt_events
            (retry_name ^ " " ^ status_name)
            outcome
            events)
        status_cases)
    [
      (false, "fresh", Backend_types.Fresh_attempt);
      (true, "resume", Backend_types.Resumed_attempt);
    ]

let test_context_timeout_cancel_compatibility_projection () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (supports_resume, expected_kind) ->
      List.iter
        (fun status ->
          let first =
            result
              ~text:invalid_text_1
              ?session_id:
                (if supports_resume then Some "projection-session" else None)
              ()
          in
          let backend, calls, _ =
            make_mock ~supports_resume [first; result_for_status status]
          in
          let events = ref [] in
          let sink =
            Task_event.create_sink
              ~sw
              ~now:(fun () -> 0.0)
              ~on_event:(fun event -> events := event :: !events)
              ()
          in
          let context =
            Task_execution_context.create
              ~remaining_time:(fun () -> None)
              sink
          in
          let projected =
            Json_schema_enforcer.run_task
              ~sw
              ~env
              ~context
              ~backend
              (make_spec ~json_schema:object_schema ())
          in
          Task_event.emit_terminal sink Task_event.Succeeded ;
          Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
          Alcotest.(check int) "projection uses two calls" 2 !calls ;
          Alcotest.(check bool)
            "context timeout/cancel projects final task_result"
            true
            (match projected with
            | Ok result -> result.Backend_types.status = status
            | Error _ -> false) ;
          let expected =
            [
              Started (1, Initial_attempt);
              Finished (1, Attempt_succeeded);
              Started (2, expected_kind);
              Finished (2, event_outcome_of_status status);
            ]
          in
          Alcotest.(check bool)
            "projection exact start/finish lifecycle"
            true
            (attempt_lifecycle (List.rev !events) = expected))
        [Backend_types.Timeout; Backend_types.Cancelled])
    [
      (false, Backend_types.Fresh_attempt);
      (true, Backend_types.Resumed_attempt);
    ]

let test_cost_aggregation_is_field_wise () =
  let total =
    Backend_types.aggregate_costs
      [
        None;
        Some
          (cost
             ~tokens_input:2
             ~cost_usd:0.5
             ~cache_creation_input_tokens:11
             ());
        Some (cost ~tokens_input:3 ~tokens_output:7 ~cost_usd:0.25 ());
      ]
  in
  (match total with
  | None -> Alcotest.fail "partial costs disappeared"
  | Some total ->
      Alcotest.(check (option int)) "input sum" (Some 5) total.tokens_input ;
      Alcotest.(check (option int)) "output known" (Some 7) total.tokens_output ;
      Alcotest.(check (option (float 0.000001))) "USD sum" (Some 0.75) total.cost_usd ;
      Alcotest.(check (option int))
        "cache creation known"
        (Some 11)
        total.cache_creation_input_tokens ;
      Alcotest.(check (option int))
        "all-unknown field remains None"
        None
        total.cache_read_input_tokens) ;
  Alcotest.(check (option bool))
    "no cost records remains None"
    None
    (Option.map (fun _ -> true) (Backend_types.aggregate_costs [None; None])) ;
  match Backend_types.aggregate_costs [Some Backend_types.empty_cost] with
  | Some total ->
      Alcotest.(check (option int))
        "present all-unknown record keeps unknown field"
        None
        total.tokens_input ;
      let saturated =
        Backend_types.aggregate_costs
          [
            Some (cost ~tokens_input:max_int ~cost_usd:max_float ());
            Some (cost ~tokens_input:1 ~cost_usd:max_float ());
          ]
      in
      (match saturated with
      | Some saturated ->
          Alcotest.(check (option int))
            "integer overflow saturates"
            (Some max_int)
            saturated.tokens_input ;
          Alcotest.(check (option (float 0.0)))
            "finite float overflow saturates"
            (Some max_float)
            saturated.cost_usd
      | None -> Alcotest.fail "saturating aggregate disappeared") ;
      let invalid =
        Backend_types.aggregate_costs
          [
            Some
              (cost
                 ~tokens_input:(-1)
                 ~tokens_output:9
                 ~cost_usd:Float.nan
                 ~cache_creation_input_tokens:4
                 ());
            Some
              (cost
                 ~tokens_input:3
                 ~tokens_output:2
                 ~cost_usd:1.0
                 ~cache_creation_input_tokens:5
                 ());
          ]
      in
      (match invalid with
      | Some invalid ->
          Alcotest.(check (option int))
            "negative token invalidates only its field"
            None
            invalid.tokens_input ;
          Alcotest.(check (option int))
            "independent token field still sums"
            (Some 11)
            invalid.tokens_output ;
          Alcotest.(check (option (float 0.0)))
            "NaN invalidates only cost field"
            None
            invalid.cost_usd ;
          Alcotest.(check (option int))
            "independent cache field still sums"
            (Some 9)
          invalid.cache_creation_input_tokens
      | None -> Alcotest.fail "invalid field discarded complete cost record") ;
      List.iter
        (fun (label, invalid_cost) ->
          match
            Backend_types.aggregate_costs
              [Some (cost ~tokens_input:7 ~cost_usd:invalid_cost ())]
          with
          | Some aggregate ->
              Alcotest.(check (option (float 0.0)))
                (label ^ " invalidates cost only")
                None
                aggregate.cost_usd ;
              Alcotest.(check (option int))
                (label ^ " preserves token field")
                (Some 7)
                aggregate.tokens_input
          | None -> Alcotest.fail (label ^ " discarded complete cost record"))
        [
          ("positive infinity", Float.infinity);
          ("negative infinity", Float.neg_infinity);
          ("negative finite", -0.01);
        ]
  | None -> Alcotest.fail "present cost record disappeared"

let check_retry_input_policy ~supports_resume ~expected_kind ~expected_delivery () =
  let first = result ~text:invalid_text_1 ~session_id:"media-session" () in
  let observed_deliveries = ref [] in
  let backend, _, captured =
    make_mock
      ~supports_resume
      ~on_context:(fun context ->
        observed_deliveries :=
          Task_execution_context.requested_delivery context
          :: !observed_deliveries)
      [first; result ()]
  in
  let spec =
    make_spec
      ~timeout:10.0
      ~json_schema:object_schema
      ~attachments:[attachment]
      ~web_access:Backend_types.Web_search_and_fetch
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sink = Task_event.create_sink ~sw ~now:(fun () -> 0.0) () in
  let context =
    Task_execution_context.create ~remaining_time:(fun () -> Some 0.25) sink
  in
  let execution =
    get_execution
      (Json_schema_enforcer.run_task_detailed
         ~sw
         ~env
         ~context
         ~backend
         spec)
  in
  (match captured () with
  | [_; retry_spec] ->
      Alcotest.(check bool)
        "attachment references preserved"
        true
        (retry_spec.Backend_types.attachments = [attachment]) ;
      Alcotest.(check bool)
        "web policy preserved"
        true
        (retry_spec.web_access = Web_search_and_fetch) ;
      Alcotest.(check (float 0.000001))
        "retry bounded by remaining time"
        0.25
        retry_spec.timeout
  | specs -> Alcotest.failf "expected two captured specs, got %d" (List.length specs)) ;
  match execution.attempts with
  | [initial; retry] ->
      Alcotest.(check bool)
        "initial delivery uploads"
        true
        (initial.delivery.attachment_delivery = Upload_attachments) ;
      Alcotest.(check bool) "retry kind" true (retry.kind = expected_kind) ;
      Alcotest.(check bool)
        "delivery policy"
        true
        (retry.delivery.attachment_delivery = expected_delivery) ;
      Alcotest.(check bool)
        "telemetry attachment refs"
        true
        (retry.delivery.attachment_references = [attachment]) ;
      Alcotest.(check bool)
        "telemetry web policy"
        true
        (retry.delivery.web_access_policy = Web_search_and_fetch) ;
      Alcotest.(check bool)
        "backend observes the exact recorded delivery intent"
        true
        (List.rev !observed_deliveries
        = [Some initial.delivery; Some retry.delivery])
  | _ -> Alcotest.fail "expected retry telemetry"

let test_fresh_and_resume_attachment_policy () =
  check_retry_input_policy
    ~supports_resume:false
    ~expected_kind:Backend_types.Fresh_attempt
    ~expected_delivery:Backend_types.Upload_attachments
    () ;
  check_retry_input_policy
    ~supports_resume:true
    ~expected_kind:Backend_types.Resumed_attempt
    ~expected_delivery:Backend_types.Reuse_session_attachments
    ()

let test_compatibility_projection_and_renderer () =
  let first = result ~text:invalid_text_1 () in
  let second = result ~text:invalid_text_2 () in
  let detailed_backend, _, _ = make_mock [first; second] in
  let legacy_backend, _, _ = make_mock [first; second] in
  let spec = make_spec ~json_schema:object_schema () in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let detailed =
    Json_schema_enforcer.run_task_detailed
      ~sw
      ~env
      ~backend:detailed_backend
      spec
  in
  let legacy = Json_schema_enforcer.run_task ~sw ~env ~backend:legacy_backend spec in
  match detailed, legacy with
  | Error structured, Error rendered ->
      Alcotest.(check string)
        "legacy is exact structured renderer"
        (Json_schema_enforcer.render_error structured)
        rendered ;
      let err1 =
        Result.get_error
          (Json_schema_validator.validate
             ~schema:object_schema
             ~document:invalid_text_1)
      in
      let err2 =
        Result.get_error
          (Json_schema_validator.validate
             ~schema:object_schema
             ~document:invalid_text_2)
      in
      Alcotest.(check string)
        "pinned compatibility text"
        ("Both schema enforcement attempts failed.\nAttempt 1: " ^ err1
       ^ "\nAttempt 2: " ^ err2)
        rendered
  | Error _, Ok _ -> Alcotest.fail "legacy projection lost structured error"
  | Ok _, Error _ -> Alcotest.fail "detailed/legacy result disagreement"
  | Ok _, Ok _ -> Alcotest.fail "invalid responses unexpectedly succeeded"

let test_timeout_default_and_actual_passthrough () =
  let backend, calls, captured = make_mock [result ()] in
  let spec = make_spec () in
  Alcotest.(check (float 0.0)) "documented default" max_float spec.timeout ;
  let execution = get_execution (run_detailed ~backend spec) in
  Alcotest.(check int) "default invokes backend" 1 !calls ;
  Alcotest.(check bool)
    "default execution succeeds"
    true
    (execution.final_result.status = Backend_types.Success) ;
  match captured () with
  | [received] ->
      Alcotest.(check (float 0.0))
        "backend receives documented default"
        max_float
        received.timeout
  | _ -> Alcotest.fail "expected one captured spec"

let () =
  Alcotest.run
    "CBL-05 detailed task execution"
    [
      ( "single attempt paths",
        [
          Alcotest.test_case "no schema" `Quick test_no_schema_detailed;
          Alcotest.test_case
            "native success/rejection"
            `Quick
            test_native_success_and_rejection;
          Alcotest.test_case
            "first validation succeeds"
            `Quick
            test_first_validation_success;
          Alcotest.test_case
            "timeout/cancellation do not retry"
            `Quick
            test_timeout_and_cancellation_results_do_not_retry;
        ] );
      ( "retry detail",
        [
          Alcotest.test_case
            "fresh success preserves complete results"
            `Quick
            test_fresh_retry_success_preserves_complete_results_and_cost;
          Alcotest.test_case
            "resume and final session"
            `Quick
            test_resume_retry_and_final_non_empty_session;
          Alcotest.test_case
            "blank sessions select fresh retry"
            `Quick
            test_blank_session_ids_choose_fresh_retry;
          Alcotest.test_case
            "double invalid structured error"
            `Quick
            test_double_invalid_structured_error;
          Alcotest.test_case
            "resume failure keeps two-call cap"
            `Quick
            test_invoked_resume_failure_stops_at_two_calls;
          Alcotest.test_case
            "fresh transport failure is classified"
            `Quick
            test_fresh_transport_failure_is_structured;
          Alcotest.test_case
            "throwing resume classifier is conservative"
            `Quick
            test_throwing_resume_classifier_is_conservative;
          Alcotest.test_case
            "classifier time is included"
            `Quick
            test_classifier_time_is_in_total_elapsed;
          Alcotest.test_case
            "classifier cancellation/fatals propagate"
            `Quick
            test_classifier_preserves_cancellation_and_fatals;
        ] );
      ( "deadline and events",
        [
          Alcotest.test_case
            "deadline expires before retry"
            `Quick
            test_deadline_expiry_before_retry;
          Alcotest.test_case
            "cancellation between attempts"
            `Quick
            test_cancellation_between_attempts_does_not_start_retry;
          Alcotest.test_case
            "attempt/event agreement matrix"
            `Quick
            test_attempt_event_agreement_matrix;
          Alcotest.test_case
            "context timeout/cancel projection"
            `Quick
            test_context_timeout_cancel_compatibility_projection;
        ] );
      ( "aggregation and policy",
        [
          Alcotest.test_case
            "field-wise optional cost aggregation"
            `Quick
            test_cost_aggregation_is_field_wise;
          Alcotest.test_case
            "fresh/resume media and web policy"
            `Quick
            test_fresh_and_resume_attachment_policy;
        ] );
      ( "compatibility",
        [
          Alcotest.test_case
            "exact renderer/projection"
            `Quick
            test_compatibility_projection_and_renderer;
          Alcotest.test_case
            "timeout default and execution"
            `Quick
            test_timeout_default_and_actual_passthrough;
        ] );
    ]
