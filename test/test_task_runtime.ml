(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

exception Parent_cancelled

let () = Process_test_helper.run_if_requested ()

let limits : Task_preflight.limits =
  {max_attachments = 0; max_file_size_bytes = 0; max_total_size_bytes = 0}

let descriptor_for ?(session_resume = false) id =
  {
    Backend_registry.id;
    display_name = "Task runtime mock";
    binary_name = "task-runtime-mock";
    baseline_version = "1.0.0";
    capabilities =
      {
        structured_output = true;
        streaming_output = true;
        session_resume;
        mcp_support = Backend_registry.Mcp_none;
        read_only_support = false;
        project_config_surface = Backend_registry.Config_none;
        precedence_confidence = Backend_registry.Low;
        generated_lsp_config = false;
        file_reading = false;
        media_support = {media_types = []; evidence = None};
        web_support =
          {maximum = Backend_types.Web_disabled; evidence = None};
        native_json_schema_output = false;
        native_json_schema_output_evidence = None;
      };
  }

let register_backend ?(session_resume = false)
    ?(version_policy = Runtime_entry.No_version_gate) ~id ~available run =
  let module Backend = struct
    let id = id
    let name = "Task runtime mock"
    let models = []
    let models_probe = None
    let available = available
    let supports_session_resume = session_resume
    let native_json_schema_output = false
    let is_resume_failure _ = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "mock"

    let run_task ~sw ~env ?context ?on_raw_line spec =
      run ~sw ~env ?context ?on_raw_line spec
  end in
  let descriptor = descriptor_for ~session_resume id in
  let backend = (module Backend : Agentic_backend.S) in
  let entry =
    match
      Runtime_entry.create
        ~backend
        ~descriptor
        ~runtime_capabilities:descriptor.capabilities
        ~origin:Runtime_entry.Custom
        ~version_policy
    with
    | Ok entry -> entry
    | Error error ->
        Alcotest.fail (Runtime_entry.render_validation_error error)
  in
  Registry.register_validated entry

let success ?(text = "ok") ?session_id ?cost () =
  Backend_types.make_task_result
    ~status:Backend_types.Success
    ~agent_text:text
    ?session_id
    ?cost
    ()

let spec ?(prompt = "task") ?timeout ?json_schema () =
  Backend_types.make_task_spec
    ~prompt
    ~working_dir:"/tmp"
    ?timeout
    ?json_schema
    ()

let with_registry f =
  Registry.clear () ;
  Fun.protect ~finally:Registry.clear f

let terminal_count events =
  List.fold_left
    (fun count event ->
      match event.Task_event.payload with
      | Task_event.Terminal _ -> count + 1
      | _ -> count)
    0
    events

let status_of_result = function
  | Ok result -> result.Backend_types.status
  | Error error -> Alcotest.fail (Runtime_dispatch.render_error error)

let execution_of_detailed = function
  | Ok execution -> execution
  | Error error ->
      Alcotest.fail (Runtime_dispatch.render_detailed_error error)

let status_of_detailed outcome =
  (execution_of_detailed outcome).Backend_types.final_result.status

let object_schema = `Assoc [("type", `String "object")]

let cost input output usd : Backend_types.cost =
  {
    tokens_input = Some input;
    tokens_output = Some output;
    cost_usd = Some usd;
    cache_creation_input_tokens = None;
    cache_read_input_tokens = None;
  }

let check_single_completed_attempt execution expected =
  match execution.Backend_types.attempts with
  | [attempt] ->
      Alcotest.(check int) "completed attempt number" 1 attempt.number ;
      Alcotest.(check bool)
        "completed attempt result"
        true
        (attempt.result = expected)
  | attempts ->
      Alcotest.failf
        "expected one completed attempt, got %d"
        (List.length attempts)

let test_success_repeatable_await_and_event_dedup () =
  with_registry @@ fun () ->
  let calls = ref 0 in
  register_backend
    ~id:"success"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ ->
      incr calls ;
      success
        ~text:"final answer"
        ~session_id:"session-1"
        ~cost:
          {
            Backend_types.tokens_input = Some 2;
            tokens_output = Some 3;
            cost_usd = None;
            cache_creation_input_tokens = None;
            cache_read_input_tokens = None;
          }
        ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"success"
      ~on_event:(fun event -> events := event :: !events)
      (spec ())
  in
  let first = Task_runtime.await handle in
  let second = Task_runtime.await handle in
  Task_runtime.await_event_delivery handle ;
  Alcotest.(check bool) "repeatable await" true (first = second) ;
  Alcotest.(check int) "one backend call" 1 !calls ;
  let events = List.rev !events in
  Alcotest.(check int) "one terminal" 1 (terminal_count events) ;
  Alcotest.(check bool)
    "terminal is last"
    true
    (match List.rev events with
    | {Task_event.payload = Terminal Succeeded; _} :: _ -> true
    | _ -> false) ;
  let texts =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Agent_text_delta text -> Some text
        | _ -> None)
      events
  in
  Alcotest.(check (list string))
    "final public text emitted once"
    ["final answer"]
    texts

let test_cancel_is_idempotent_and_sibling_isolated () =
  with_registry @@ fun () ->
  register_backend
    ~id:"slow"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env ?context:_ ?on_raw_line:_ _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 30.0 ;
      success ()) ;
  register_backend
    ~id:"fast"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ -> success ~text:"sibling" ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let slow =
    Task_runtime.start_task ~sw ~env ~limits ~backend_id:"slow" (spec ())
  in
  let fast =
    Task_runtime.start_task ~sw ~env ~limits ~backend_id:"fast" (spec ())
  in
  Eio.Fiber.all (List.init 10 (fun _ () -> Task_runtime.cancel slow)) ;
  Alcotest.(check bool)
    "cancel normalized"
    true
    (status_of_result (Task_runtime.await slow) = Backend_types.Cancelled) ;
  Alcotest.(check bool)
    "sibling succeeds"
    true
    (status_of_result (Task_runtime.await fast) = Backend_types.Success)

let test_concurrent_await_is_repeatable () =
  with_registry @@ fun () ->
  register_backend
    ~id:"await"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env ?context:_ ?on_raw_line:_ _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01 ;
      success ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let handle =
    Task_runtime.start_task ~sw ~env ~limits ~backend_id:"await" (spec ())
  in
  let first = Eio.Fiber.fork_promise ~sw (fun () -> Task_runtime.await handle) in
  let second = Eio.Fiber.fork_promise ~sw (fun () -> Task_runtime.await handle) in
  Alcotest.(check bool)
    "concurrent await agrees"
    true
    (Eio.Promise.await_exn first = Eio.Promise.await_exn second)

let test_detailed_await_is_repeatable_and_concurrent () =
  with_registry @@ fun () ->
  let calls = ref 0 in
  register_backend
    ~id:"detailed-await"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env ?context:_ ?on_raw_line:_ _ ->
      incr calls ;
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01 ;
      success ~session_id:"memoized-session" ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"detailed-await"
      (spec ())
  in
  let first =
    Eio.Fiber.fork_promise ~sw (fun () -> Task_runtime.await_detailed handle)
  in
  let second =
    Eio.Fiber.fork_promise ~sw (fun () -> Task_runtime.await_detailed handle)
  in
  let first = Eio.Promise.await_exn first in
  let second = Eio.Promise.await_exn second in
  let repeated = Task_runtime.await_detailed handle in
  Alcotest.(check bool) "concurrent detailed awaits agree" true (first = second) ;
  Alcotest.(check bool) "repeated detailed await agrees" true (first = repeated) ;
  Alcotest.(check int) "detailed await invokes once" 1 !calls

let test_await_is_independent_from_callback_completion () =
  with_registry @@ fun () ->
  register_backend
    ~id:"callback-independent"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ -> success ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let terminal_callback_started, resolve_terminal_callback_started =
    Eio.Promise.create ()
  in
  let release_terminal_callback, resolve_release_terminal_callback =
    Eio.Promise.create ()
  in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"callback-independent"
      ~on_event:(fun event ->
        match event.Task_event.payload with
        | Task_event.Terminal _ ->
            Eio.Promise.resolve resolve_terminal_callback_started () ;
            Eio.Promise.await release_terminal_callback
        | _ -> ())
      (spec ())
  in
  Eio.Promise.await terminal_callback_started ;
  Alcotest.(check bool)
    "detailed await completes while terminal callback is blocked"
    true
    (status_of_detailed (Task_runtime.await_detailed handle)
    = Backend_types.Success) ;
  Eio.Promise.resolve resolve_release_terminal_callback () ;
  Task_runtime.await_event_delivery handle

let test_process_callback_can_await_same_handle () =
  with_registry @@ fun () ->
  register_backend
    ~id:"callback-self-await"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context ?on_raw_line:_ _ ->
      Option.iter
        (fun context ->
          Task_execution_context.emit
            context
            (Task_event.Process_started {pid = None}))
        context ;
      success ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let handle_ready, resolve_handle_ready = Eio.Promise.create () in
  let terminal_callback_done, resolve_terminal_callback_done =
    Eio.Promise.create ()
  in
  let callback_statuses = ref [] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"callback-self-await"
      ~on_event:(fun event ->
        match event.Task_event.payload with
        | Task_event.Process_started _ | Task_event.Terminal _ ->
            let handle = Eio.Promise.await handle_ready in
            let status =
              status_of_detailed (Task_runtime.await_detailed handle)
            in
            callback_statuses := status :: !callback_statuses ;
            (match event.payload with
            | Task_event.Terminal _ ->
                Eio.Promise.resolve resolve_terminal_callback_done ()
            | _ -> ())
        | _ -> ())
      (spec ())
  in
  Eio.Promise.resolve resolve_handle_ready handle ;
  Alcotest.(check bool)
    "owner completes"
    true
    (status_of_detailed (Task_runtime.await_detailed handle)
    = Backend_types.Success) ;
  Eio.Promise.await terminal_callback_done ;
  Task_runtime.await_event_delivery handle ;
  Alcotest.(check int)
    "process and terminal callbacks both await"
    2
    (List.length !callback_statuses) ;
  Alcotest.(check bool)
    "same-handle callback awaits complete"
    true
    (List.for_all (( = ) Backend_types.Success) !callback_statuses)

let test_timeout_and_invalid_timeout_before_call () =
  with_registry @@ fun () ->
  let calls = ref 0 in
  register_backend
    ~id:"timeout"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env ?context:_ ?on_raw_line:_ _ ->
      incr calls ;
      Eio.Time.sleep (Eio.Stdenv.clock env) 30.0 ;
      success ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let timed =
    Task_runtime.run_task
      ~sw
      ~env
      ~limits
      ~backend_id:"timeout"
      (spec ~timeout:0.02 ())
  in
  Alcotest.(check bool)
    "whole-task timeout"
    true
    (status_of_result timed = Backend_types.Timeout) ;
  let calls_after_timeout = !calls in
  List.iter
    (fun timeout ->
      match
        Task_runtime.run_task
          ~sw
          ~env
          ~limits
          ~backend_id:"timeout"
          (spec ~timeout ())
      with
      | Error Runtime_dispatch.Invalid_timeout -> ()
      | Error error ->
          Alcotest.failf
            "unexpected timeout error: %s"
            (Runtime_dispatch.render_error error)
      | Ok _ -> Alcotest.fail "invalid timeout was accepted")
    [-1.0; Float.nan] ;
  Alcotest.(check int)
    "invalid timeout has no backend call"
    calls_after_timeout
    !calls

let test_unbounded_default_and_callback_exception () =
  with_registry @@ fun () ->
  register_backend
    ~id:"unbounded"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ -> success ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"unbounded"
      ~on_event:(fun _ -> failwith "event callback secret")
      (spec ())
  in
  let result = Task_runtime.await handle in
  Task_runtime.await_event_delivery handle ;
  Alcotest.(check bool)
    "max_float default remains unbounded"
    true
    (status_of_result result = Backend_types.Success)

let test_raw_reasoning_is_not_promoted () =
  with_registry @@ fun () ->
  register_backend
    ~id:"raw"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line _ ->
      Option.iter (fun callback -> callback "private-chain-of-thought") on_raw_line ;
      success ~text:"public answer" ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let raw = ref [] in
  let events = ref [] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"raw"
      ~on_raw_line:(fun line -> raw := line :: !raw)
      ~on_event:(fun event -> events := event :: !events)
      (spec ())
  in
  let result = Task_runtime.await handle in
  Task_runtime.await_event_delivery handle ;
  ignore (status_of_result result) ;
  Alcotest.(check (list string))
    "raw callback unchanged"
    ["private-chain-of-thought"]
    (List.rev !raw) ;
  let normalized_text =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Agent_text_delta text -> Some text
        | _ -> None)
      (List.rev !events)
  in
  Alcotest.(check (list string))
    "only proven final text normalized"
    ["public answer"]
    normalized_text

let test_structured_final_text_is_emitted_without_stream_delta () =
  with_registry @@ fun () ->
  register_backend
    ~id:"structured-final"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context ?on_raw_line:_ _ ->
      Option.iter
        (fun context ->
          Task_execution_context.claim_structured_text context ;
          Task_execution_context.mark_final_public_text context)
        context ;
      success ~text:"proven final answer" ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"structured-final"
      ~on_event:(fun event -> events := event :: !events)
      (spec ())
  in
  ignore (status_of_result (Task_runtime.await handle)) ;
  Task_runtime.await_event_delivery handle ;
  let texts =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Agent_text_delta text -> Some text
        | _ -> None)
      (List.rev !events)
  in
  Alcotest.(check (list string))
    "strict final public text is emitted once"
    ["proven final answer"]
    texts

let test_unproven_structured_final_text_is_suppressed () =
  with_registry @@ fun () ->
  register_backend
    ~id:"structured-unproven"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context ?on_raw_line:_ _ ->
      Option.iter Task_execution_context.claim_structured_text context ;
      success ~text:"private malformed fallback" ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"structured-unproven"
      ~on_event:(fun event -> events := event :: !events)
      (spec ())
  in
  ignore (status_of_result (Task_runtime.await handle)) ;
  Task_runtime.await_event_delivery handle ;
  let texts =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Agent_text_delta text -> Some text
        | _ -> None)
      !events
  in
  Alcotest.(check (list string))
    "unproven strict result text is suppressed"
    []
    texts

let test_retry_attempts_share_deadline () =
  with_registry @@ fun () ->
  let calls = ref 0 in
  register_backend
    ~id:"retry"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env ?context:_ ?on_raw_line:_ _ ->
      incr calls ;
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.035 ;
      if !calls = 1 then success ~text:"not-json" ()
      else success ~text:{|{"ok":true}|} ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let schema = `Assoc [("type", `String "object")] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"retry"
      ~on_event:(fun event -> events := event :: !events)
      (spec ~timeout:0.05 ~json_schema:schema ())
  in
  let result = Task_runtime.await handle in
  Task_runtime.await_event_delivery handle ;
  Alcotest.(check bool)
    "retry cannot reset deadline"
    true
    (status_of_result result = Backend_types.Timeout) ;
  Alcotest.(check int) "retry began" 2 !calls ;
  let events = List.rev !events in
  let attempts =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Attempt_started kind -> Some (event.attempt, kind)
        | _ -> None)
      events
  in
  Alcotest.(check int) "two attempt events" 2 (List.length attempts) ;
  let finishes =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Attempt_finished outcome -> Some (event.attempt, outcome)
        | _ -> None)
      events
  in
  Alcotest.(check int) "two attempt finishes" 2 (List.length finishes) ;
  Alcotest.(check bool)
    "second attempt records timeout"
    true
    (match List.rev finishes with
    | (_, Task_event.Attempt_timed_out) :: _ -> true
    | _ -> false) ;
  Alcotest.(check int) "one terminal after retry timeout" 1 (terminal_count events)

let test_detailed_deadline_during_retry_retains_completed_progress () =
  with_registry @@ fun () ->
  let calls = ref 0 in
  let first_cost = cost 7 3 0.25 in
  let first =
    success
      ~text:"not-json"
      ~session_id:"deadline-session"
      ~cost:first_cost
      ()
  in
  register_backend
    ~id:"detailed-retry-timeout"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env ?context:_ ?on_raw_line:_ _ ->
      incr calls ;
      if !calls = 1 then first
      else begin
        Eio.Time.sleep (Eio.Stdenv.clock env) 30.0 ;
        success ()
      end) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"detailed-retry-timeout"
      ~on_event:(fun event -> events := event :: !events)
      (spec ~timeout:0.03 ~json_schema:object_schema ())
  in
  let execution =
    execution_of_detailed (Task_runtime.await_detailed handle)
  in
  Task_runtime.await_event_delivery handle ;
  Alcotest.(check int) "retry was invoked" 2 !calls ;
  Alcotest.(check bool)
    "outer deadline is final timeout"
    true
    (execution.final_result.status = Backend_types.Timeout) ;
  check_single_completed_attempt execution first ;
  Alcotest.(check bool)
    "completed cost retained"
    true
    (execution.total_cost = Some first_cost) ;
  Alcotest.(check (option string))
    "completed session retained"
    (Some "deadline-session")
    execution.final_session_id ;
  Alcotest.(check bool)
    "outer elapsed retained"
    true
    (execution.total_elapsed >= 0.02) ;
  let events = List.rev !events in
  let first_finish =
    List.exists
      (fun event ->
        event.Task_event.attempt = 1
        && event.payload
           = Task_event.Attempt_finished Task_event.Attempt_succeeded)
      events
  in
  let second_timeout =
    List.exists
      (fun event ->
        event.Task_event.attempt = 2
        && event.payload
           = Task_event.Attempt_finished Task_event.Attempt_timed_out)
      events
  in
  Alcotest.(check bool) "completed detail has matching event" true first_finish ;
  Alcotest.(check bool)
    "interrupted retry has timeout event but no fabricated detail"
    true
    second_timeout

let test_detailed_cancellation_between_attempts_retains_progress () =
  with_registry @@ fun () ->
  let calls = ref 0 in
  let first = success ~text:"not-json" ~session_id:"between-session" () in
  register_backend
    ~id:"detailed-cancel-between"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ ->
      incr calls ;
      if !calls = 1 then first else success ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let handle_ready, resolve_handle_ready = Eio.Promise.create () in
  let events = ref [] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"detailed-cancel-between"
      ~on_event:(fun event ->
        events := event :: !events ;
        match event.Task_event.attempt, event.payload with
        | 1, Task_event.Attempt_finished Task_event.Attempt_succeeded ->
            Task_runtime.cancel (Eio.Promise.await handle_ready)
        | _ -> ())
      (spec ~json_schema:object_schema ())
  in
  Eio.Promise.resolve resolve_handle_ready handle ;
  let execution =
    execution_of_detailed (Task_runtime.await_detailed handle)
  in
  Task_runtime.await_event_delivery handle ;
  Alcotest.(check int) "retry was not invoked" 1 !calls ;
  Alcotest.(check bool)
    "between-attempt cancellation normalized"
    true
    (execution.final_result.status = Backend_types.Cancelled) ;
  check_single_completed_attempt execution first ;
  Alcotest.(check bool)
    "retry transition absent"
    false
    (List.exists
       (fun event ->
         match event.Task_event.payload with
         | Task_event.Retry_transition _ -> true
         | _ -> false)
       !events)

let test_detailed_cancellation_during_retry_retains_completed_progress () =
  with_registry @@ fun () ->
  let calls = ref 0 in
  let first = success ~text:"not-json" ~session_id:"during-session" () in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let retry_entered, resolve_retry_entered = Eio.Promise.create () in
  register_backend
    ~id:"detailed-cancel-during"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env ?context:_ ?on_raw_line:_ _ ->
      incr calls ;
      if !calls = 1 then first
      else begin
        Eio.Promise.resolve resolve_retry_entered () ;
        Eio.Time.sleep (Eio.Stdenv.clock env) 30.0 ;
        success ()
      end) ;
  let events = ref [] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"detailed-cancel-during"
      ~on_event:(fun event -> events := event :: !events)
      (spec ~json_schema:object_schema ())
  in
  Eio.Promise.await retry_entered ;
  Task_runtime.cancel handle ;
  let execution =
    execution_of_detailed (Task_runtime.await_detailed handle)
  in
  Task_runtime.await_event_delivery handle ;
  Alcotest.(check int) "retry entered" 2 !calls ;
  Alcotest.(check bool)
    "during-retry cancellation normalized"
    true
    (execution.final_result.status = Backend_types.Cancelled) ;
  check_single_completed_attempt execution first ;
  Alcotest.(check bool)
    "interrupted retry emitted cancellation finish"
    true
    (List.exists
       (fun event ->
         event.Task_event.attempt = 2
         && event.payload
            = Task_event.Attempt_finished Task_event.Attempt_cancelled)
       !events)

let test_detailed_cancellation_before_first_attempt_has_empty_progress () =
  with_registry @@ fun () ->
  let calls = ref 0 in
  register_backend
    ~id:"detailed-cancel-before"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ ->
      incr calls ;
      success ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"detailed-cancel-before"
      (spec ())
  in
  Task_runtime.cancel handle ;
  let execution =
    execution_of_detailed (Task_runtime.await_detailed handle)
  in
  Alcotest.(check int) "backend not invoked" 0 !calls ;
  Alcotest.(check int) "no attempt fabricated" 0 (List.length execution.attempts) ;
  Alcotest.(check bool)
    "pre-attempt cancellation normalized"
    true
    (execution.final_result.status = Backend_types.Cancelled) ;
  Alcotest.(check (option bool))
    "no aggregate fabricated"
    None
    (Option.map (fun _ -> true) execution.total_cost) ;
  Alcotest.(check (option string))
    "no session fabricated"
    None
    execution.final_session_id

let test_detailed_ordinary_exception_retains_completed_progress () =
  with_registry @@ fun () ->
  let calls = ref 0 in
  let first_cost = cost 2 4 0.5 in
  let first =
    success
      ~text:"not-json"
      ~session_id:"exception-session"
      ~cost:first_cost
      ()
  in
  register_backend
    ~id:"detailed-progress-exception"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ ->
      incr calls ;
      if !calls = 1 then first
      else failwith "private ordinary exception /secret/path") ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"detailed-progress-exception"
      (spec ~json_schema:object_schema ())
  in
  match Task_runtime.await_detailed handle with
  | Error
      ((Runtime_dispatch.Dispatch_failure_with_execution
          {failure = Runtime_dispatch.Backend_execution_failed; execution}) as
       error) ->
      Alcotest.(check int) "retry raised after first result" 2 !calls ;
      check_single_completed_attempt execution first ;
      Alcotest.(check bool)
        "completed aggregate retained"
        true
        (execution.total_cost = Some first_cost) ;
      Alcotest.(check (option string))
        "completed session survives ordinary exception"
        (Some "exception-session")
        execution.final_session_id ;
      let rendered = Runtime_dispatch.render_detailed_error error in
      Alcotest.(check string)
        "ordinary error stays generic"
        "backend execution failed"
        rendered
  | Error error ->
      Alcotest.failf
        "unexpected detailed error: %s"
        (Runtime_dispatch.render_detailed_error error)
  | Ok _ -> Alcotest.fail "ordinary retry exception unexpectedly succeeded"

let test_non_success_stops_retry_and_emits_one_terminal () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (id, status) ->
      let calls = ref 0 in
      register_backend
        ~id
        ~available:(fun ~sw:_ ~env:_ -> true)
        (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ ->
          incr calls ;
          Backend_types.make_task_result ~status ()) ;
      let events = ref [] in
      let handle =
        Task_runtime.start_task
          ~sw
          ~env
          ~limits
          ~backend_id:id
          ~on_event:(fun event -> events := event :: !events)
          (spec
             ~json_schema:
               (`Assoc
                 [
                   ("type", `String "object");
                   ("required", `List [`String "ok"]);
                 ])
             ())
      in
      let result = Task_runtime.await handle in
      Task_runtime.await_event_delivery handle ;
      Alcotest.(check bool)
        (id ^ " status propagated")
        true
        (status_of_result result = status) ;
      Alcotest.(check int) (id ^ " has no retry") 1 !calls ;
      Alcotest.(check int)
        (id ^ " one terminal")
        1
        (terminal_count !events) ;
      let finishes =
        List.filter_map
          (fun event ->
            match event.Task_event.payload with
            | Task_event.Attempt_finished outcome -> Some outcome
            | _ -> None)
          !events
      in
      let expected_outcome =
        match status with
        | Backend_types.Success -> Task_event.Attempt_succeeded
        | Backend_types.Failed _ -> Task_event.Attempt_failed
        | Backend_types.Timeout -> Task_event.Attempt_timed_out
        | Backend_types.Cancelled -> Task_event.Attempt_cancelled
      in
      Alcotest.(check bool)
        (id ^ " has one matching attempt finish")
        true
        (finishes = [expected_outcome]))
    [
      ("first-failed", Backend_types.Failed "secret /private/failure");
      ("first-timeout", Backend_types.Timeout);
      ("first-cancelled", Backend_types.Cancelled);
    ]

let test_parent_switch_cancellation () =
  with_registry @@ fun () ->
  register_backend
    ~id:"parent-cancel"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw:_ ~env ?context:_ ?on_raw_line:_ _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 30.0 ;
      success ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun outer_sw ->
  let handle = ref None in
  let events = ref [] in
  (match
     Eio.Switch.run (fun parent_sw ->
         handle :=
           Some
             (Task_runtime.start_task
                ~sw:parent_sw
                ~env
                 ~limits
                 ~backend_id:"parent-cancel"
                 ~on_event:(fun event -> events := event :: !events)
                 (spec ())) ;
         Eio.Switch.fail parent_sw Parent_cancelled)
   with
  | exception Parent_cancelled -> ()
  | exception error -> raise error
  | () -> Alcotest.fail "parent switch failure did not propagate") ;
  let handle =
    match !handle with Some handle -> handle | None -> Alcotest.fail "no handle"
  in
  let result = Eio.Cancel.protect (fun () -> Task_runtime.await handle) in
  Eio.Cancel.protect (fun () -> Task_runtime.await_event_delivery handle) ;
  Alcotest.(check bool)
    "parent cancellation normalizes"
    true
    (status_of_result result = Backend_types.Cancelled) ;
  Alcotest.(check int) "parent cancellation terminal delivered" 1 (terminal_count !events) ;
  ignore outer_sw

let test_cancellation_checkpoints_before_preflight_version_and_availability () =
  let run_case ~id ~version_policy ~cancel_on expected_absent =
    with_registry @@ fun () ->
    let available_calls = ref 0 in
    let backend_calls = ref 0 in
    register_backend
      ~id
      ~version_policy
      ~available:(fun ~sw:_ ~env:_ ->
        incr available_calls ;
        true)
      (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ ->
        incr backend_calls ;
        success ()) ;
    Eio_posix.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let handle_ref = ref None in
    let payloads = ref [] in
    let on_event event =
      payloads := event.Task_event.payload :: !payloads ;
      if cancel_on event.Task_event.payload then
        Option.iter Task_runtime.cancel !handle_ref
    in
    let handle =
      Task_runtime.start_task
        ~sw
        ~env
        ~limits
        ~backend_id:id
        ~on_event
        (spec ())
    in
    handle_ref := Some handle ;
    if expected_absent = Task_event.Preflight_started then Task_runtime.cancel handle ;
    Alcotest.(check bool)
      "checkpoint cancellation normalized"
      true
      (status_of_result (Task_runtime.await handle) = Backend_types.Cancelled) ;
    Task_runtime.await_event_delivery handle ;
    Alcotest.(check bool)
      "later lifecycle phase absent"
      false
      (List.mem expected_absent !payloads) ;
    Alcotest.(check int) "backend not called" 0 !backend_calls ;
    if expected_absent = Task_event.Availability_check_started then
      Alcotest.(check int) "availability not called" 0 !available_calls
  in
  run_case
    ~id:"cancel-before-preflight"
    ~version_policy:Runtime_entry.No_version_gate
    ~cancel_on:(fun _ -> false)
    Task_event.Preflight_started ;
  run_case
    ~id:"cancel-before-version"
    ~version_policy:Runtime_entry.Enforce_baseline
    ~cancel_on:(function Task_event.Preflight_completed -> true | _ -> false)
    Task_event.Version_probe_started ;
  run_case
    ~id:"cancel-before-availability"
    ~version_policy:Runtime_entry.No_version_gate
    ~cancel_on:(function Task_event.Preflight_completed -> true | _ -> false)
    Task_event.Availability_check_started

let test_prepared_dispatch_snapshot_ignores_registry_replacement () =
  with_registry @@ fun () ->
  let original_calls = ref 0 in
  let replacement_calls = ref 0 in
  let install calls text =
    register_backend
      ~id:"snapshot"
      ~available:(fun ~sw:_ ~env:_ -> true)
      (fun ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _ ->
        incr calls ;
        success ~text ())
  in
  install original_calls "original" ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let replaced = ref false in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"snapshot"
      ~on_event:(fun event ->
        match event.Task_event.payload with
        | Task_event.Backend_selected _ when not !replaced ->
            replaced := true ;
            install replacement_calls "replacement"
        | _ -> ())
      (spec ())
  in
  let result = Task_runtime.await handle in
  Task_runtime.await_event_delivery handle ;
  let result =
    match result with Ok result -> result | Error error -> Alcotest.fail (Runtime_dispatch.render_error error)
  in
  Alcotest.(check string) "resolved backend retained" "original" result.agent_text ;
  Alcotest.(check int) "original called once" 1 !original_calls ;
  Alcotest.(check int) "replacement not called" 0 !replacement_calls

let test_real_process_cancellation_reaps_before_terminal () =
  Process_test_helper.install_launcher () ;
  with_registry @@ fun () ->
  register_backend
    ~id:"real-process"
    ~version_policy:Runtime_entry.Enforce_baseline
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw ~env ?context ?on_raw_line:_ spec ->
      let process =
        Backend_process.run_process
          ~sw
          ~env
          ~cmd:
            [
              Unix.realpath Sys.executable_name;
              "--process-descendant-helper";
              "sleep";
            ]
          ~working_dir:spec.Backend_types.working_dir
          ~timeout_seconds:30.0
          ?context
          ()
      in
      Backend_types.make_task_result
        ~status:process.Backend_process.status
        ~stdout:process.stdout
        ~stderr:process.stderr
        ~exit_code:process.exit_code
        ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let handle_ref = ref None in
  let pid = ref None in
  let payloads = ref [] in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"real-process"
      ~on_event:(fun event ->
        payloads := event.Task_event.payload :: !payloads ;
        match event.payload with
        | Task_event.Process_started {pid = Some process_pid} ->
            pid := Some process_pid ;
            Option.iter Task_runtime.cancel !handle_ref
        | _ -> ())
      (spec ())
  in
  handle_ref := Some handle ;
  let result = Task_runtime.await handle in
  Task_runtime.await_event_delivery handle ;
  Alcotest.(check bool)
    "real process cancellation normalized"
    true
    (status_of_result result = Backend_types.Cancelled) ;
  let pid = match !pid with Some pid -> pid | None -> Alcotest.fail "no process pid" in
  let process_exists =
    try
      Unix.kill pid 0 ;
      true
    with Unix.Unix_error (Unix.ESRCH, _, _) -> false
  in
  Alcotest.(check bool) "direct process reaped before await" false process_exists ;
  let payloads = List.rev !payloads in
  let index predicate =
    let rec loop offset = function
      | [] -> Alcotest.fail "expected lifecycle event"
      | payload :: rest -> if predicate payload then offset else loop (offset + 1) rest
    in
    loop 0 payloads
  in
  let ordered_lifecycle =
    [ index (function Task_event.Backend_selected _ -> true | _ -> false);
      index (function Task_event.Preflight_started -> true | _ -> false);
      index (function Task_event.Preflight_completed -> true | _ -> false);
      index (function Task_event.Version_probe_started -> true | _ -> false);
      index (function Task_event.Version_probe_completed -> true | _ -> false);
      index (function Task_event.Availability_check_started -> true | _ -> false);
      index (function Task_event.Availability_check_completed -> true | _ -> false);
      index (function Task_event.Attempt_started _ -> true | _ -> false);
      index (function Task_event.Process_started _ -> true | _ -> false);
      index (function Task_event.Process_exited _ -> true | _ -> false);
      index (function Task_event.Attempt_finished _ -> true | _ -> false);
      index (function Task_event.Terminal _ -> true | _ -> false);
    ]
  in
  Alcotest.(check (list int))
    "validated lifecycle ordering"
    (List.sort_uniq compare ordered_lifecycle)
    ordered_lifecycle ;
  let exited = index (function Task_event.Process_exited _ -> true | _ -> false) in
  let terminal = index (function Task_event.Terminal _ -> true | _ -> false) in
  Alcotest.(check bool) "cleanup event precedes terminal" true (exited < terminal)

let test_fatal_await_preserves_exception_after_process_cleanup () =
  Process_test_helper.install_launcher () ;
  with_registry @@ fun () ->
  register_backend
    ~id:"fatal-after-process"
    ~available:(fun ~sw:_ ~env:_ -> true)
    (fun ~sw ~env ?context ?on_raw_line:_ spec ->
      ignore
        (Backend_process.run_process
           ~sw
           ~env
           ~cmd:
             [
               Unix.realpath Sys.executable_name;
               "--process-descendant-helper";
               "success";
             ]
           ~working_dir:spec.Backend_types.working_dir
           ~timeout_seconds:3.0
           ?context
           ()) ;
      raise Out_of_memory) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let pid = ref None in
  let handle =
    Task_runtime.start_task
      ~sw
      ~env
      ~limits
      ~backend_id:"fatal-after-process"
      ~on_event:(fun event ->
        events := event :: !events ;
        match event.Task_event.payload with
        | Task_event.Process_started {pid = Some value} -> pid := Some value
        | _ -> ())
      (spec ())
  in
  (match Task_runtime.await_detailed handle with
  | exception Out_of_memory -> ()
  | exception error ->
      Alcotest.failf "wrong fatal exception: %s" (Printexc.to_string error)
  | Ok _ | Error _ -> Alcotest.fail "fatal exception was converted to a result") ;
  Task_runtime.await_event_delivery handle ;
  let pid = match !pid with Some value -> value | None -> Alcotest.fail "no pid" in
  let process_exists =
    try
      Unix.kill pid 0 ;
      true
    with Unix.Unix_error (Unix.ESRCH, _, _) -> false
  in
  Alcotest.(check bool) "fatal child reaped before await" false process_exists ;
  let payloads = List.rev_map (fun event -> event.Task_event.payload) !events in
  let index predicate =
    let rec loop offset = function
      | [] -> Alcotest.fail "missing fatal lifecycle event"
      | payload :: rest ->
          if predicate payload then offset else loop (offset + 1) rest
    in
    loop 0 payloads
  in
  let exited = index (function Task_event.Process_exited _ -> true | _ -> false) in
  let finished =
    index (function Task_event.Attempt_finished _ -> true | _ -> false)
  in
  let terminal = index (function Task_event.Terminal _ -> true | _ -> false) in
  Alcotest.(check bool)
    "cleanup and attempt finish precede fatal terminal"
    true
    (exited < finished && finished < terminal)

let () =
  Alcotest.run
    "Task_runtime"
    [
      ( "completion",
        [
          Alcotest.test_case
            "success and repeatable await"
            `Quick
            test_success_repeatable_await_and_event_dedup;
          Alcotest.test_case
            "concurrent await"
            `Quick
            test_concurrent_await_is_repeatable;
          Alcotest.test_case
            "detailed await is repeatable and concurrent"
            `Quick
            test_detailed_await_is_repeatable_and_concurrent;
          Alcotest.test_case
            "await independent from callback"
            `Quick
            test_await_is_independent_from_callback_completion;
          Alcotest.test_case
            "process callback can await same handle"
            `Quick
            test_process_callback_can_await_same_handle;
          Alcotest.test_case
            "unbounded and callback exception"
            `Quick
            test_unbounded_default_and_callback_exception;
        ] );
      ( "cancellation",
        [
          Alcotest.test_case
            "idempotent cancel and sibling isolation"
            `Quick
            test_cancel_is_idempotent_and_sibling_isolated;
          Alcotest.test_case
            "parent switch cancellation"
            `Quick
            test_parent_switch_cancellation;
          Alcotest.test_case
            "phase cancellation checkpoints"
            `Quick
            test_cancellation_checkpoints_before_preflight_version_and_availability;
          Alcotest.test_case
            "real process is reaped before terminal"
            `Quick
            test_real_process_cancellation_reaps_before_terminal;
          Alcotest.test_case
            "fatal await follows process cleanup"
            `Quick
            test_fatal_await_preserves_exception_after_process_cleanup;
          Alcotest.test_case
            "detailed cancellation before first attempt"
            `Quick
            test_detailed_cancellation_before_first_attempt_has_empty_progress;
          Alcotest.test_case
            "detailed cancellation between attempts"
            `Quick
            test_detailed_cancellation_between_attempts_retains_progress;
          Alcotest.test_case
            "detailed cancellation during retry"
            `Quick
            test_detailed_cancellation_during_retry_retains_completed_progress;
        ] );
      ( "deadline",
        [
          Alcotest.test_case
            "timeout and invalid timeout"
            `Quick
            test_timeout_and_invalid_timeout_before_call;
          Alcotest.test_case
            "retry shares deadline"
            `Quick
            test_retry_attempts_share_deadline;
          Alcotest.test_case
            "detailed retry timeout retains progress"
            `Quick
            test_detailed_deadline_during_retry_retains_completed_progress;
          Alcotest.test_case
            "non-success stops retry"
            `Quick
            test_non_success_stops_retry_and_emits_one_terminal;
          Alcotest.test_case
            "detailed ordinary exception retains progress"
            `Quick
            test_detailed_ordinary_exception_retains_completed_progress;
        ] );
      ( "privacy",
        [
          Alcotest.test_case
            "raw reasoning remains raw-only"
            `Quick
            test_raw_reasoning_is_not_promoted;
          Alcotest.test_case
            "structured final text fallback"
            `Quick
            test_structured_final_text_is_emitted_without_stream_delta;
          Alcotest.test_case
            "unproven structured text suppressed"
            `Quick
            test_unproven_structured_final_text_is_suppressed;
        ] );
      ( "dispatch snapshot",
        [
          Alcotest.test_case
            "registry replacement cannot split prepared dispatch"
            `Quick
            test_prepared_dispatch_snapshot_ignores_registry_replacement;
        ] );
    ]
