(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Focused tests for Backend_completer JSON Schema plumbing.

    These tests pin the seam that unblocks Agent.complete-based call sites:
    - the completer surface accepts an optional [json_schema]
    - the schema is forwarded into [Backend_types.task_spec]
    - execution routes through [Json_schema_enforcer.run_task]
    - native backends fail closed without retry *)

open Cabal

let yojson = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

let contains s needle =
  let sl = String.length s and nl = String.length needle in
  if nl = 0 then true
  else if sl < nl then false
  else
    let rec loop i =
      if i + nl > sl then false
      else if String.sub s i nl = needle then true
      else loop (i + 1)
    in
    loop 0

let make_mock ~supports_resume ~native ~responses =
  let call_count = ref 0 in
  let captured_prompts = ref [] in
  let captured_schemas = ref [] in
  let module M = struct
    let id = "test-backend-completer-json-schema"

    let name = "Test Backend Completer JSON Schema"

    let models = []

    let models_probe = None

    let available ~sw:_ ~env:_ = true

    let supports_session_resume = supports_resume

    let native_json_schema_output = native

    let is_resume_failure _ = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "test mock has no config"

    let run_task ~sw:_ ~env:_ ?on_raw_line:_ (spec : Backend_types.task_spec) =
      let i = !call_count in
      incr call_count ;
      captured_prompts := spec.Backend_types.prompt :: !captured_prompts ;
      captured_schemas := spec.Backend_types.json_schema :: !captured_schemas ;
      if i < List.length responses then List.nth responses i
      else
        Backend_types.make_task_result
          ~status:(Backend_types.Failed "no more scripted responses")
          ()
  end in
  ( (module M : Agentic_backend.S),
    call_count,
    captured_prompts,
    captured_schemas )

let object_schema = `Assoc [("type", `String "object")]

let valid_json_response = {|{"result":"ok"}|}

let invalid_response = "not-json-at-all"

let make_valid_result ?(session_id = "test-session-1") () =
  Backend_types.make_task_result
    ~status:Backend_types.Success
    ~agent_text:valid_json_response
    ~session_id
    ()

let make_invalid_result ?(session_id = "test-session-1") () =
  Backend_types.make_task_result
    ~status:Backend_types.Success
    ~agent_text:invalid_response
    ~session_id
    ()

let make_failed_result msg =
  Backend_types.make_task_result ~status:(Backend_types.Failed msg) ()

let test_no_schema_pass_through () =
  let backend, call_count, _, captured_schemas =
    make_mock
      ~supports_resume:false
      ~native:false
      ~responses:[make_valid_result ()]
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let completer =
    Backend_completer.make ~sw ~env ~backend ~working_dir:"/tmp" ()
  in
  match
    completer
      ~system_prompt:"sys"
      ~prompt:"user"
      ~json_schema:None
      ~resume_session_id:None
  with
  | Error e -> Alcotest.failf "expected Ok but got Error: %s" e
  | Ok result ->
      Alcotest.(check int) "one backend call" 1 !call_count ;
      Alcotest.(check string)
        "returns agent text"
        valid_json_response
        result.text ;
      Alcotest.(check (option yojson))
        "schema omitted from task spec"
        None
        (List.hd (List.rev !captured_schemas))

let test_schema_routes_through_enforcer_retry () =
  let backend, call_count, captured_prompts, captured_schemas =
    make_mock
      ~supports_resume:false
      ~native:false
      ~responses:[make_invalid_result (); make_valid_result ()]
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let completer =
    Backend_completer.make ~sw ~env ~backend ~working_dir:"/tmp" ()
  in
  match
    completer
      ~system_prompt:"sys"
      ~prompt:"original-prompt-token"
      ~json_schema:(Some object_schema)
      ~resume_session_id:None
  with
  | Error e -> Alcotest.failf "expected Ok but got Error: %s" e
  | Ok _ ->
      Alcotest.(check int) "retry made two backend calls" 2 !call_count ;
      let schemas = List.rev !captured_schemas in
      Alcotest.(check (option yojson))
        "first attempt carries schema"
        (Some object_schema)
        (List.nth schemas 0) ;
      Alcotest.(check (option yojson))
        "retry clears schema in corrective prompt"
        None
        (List.nth schemas 1) ;
      let retry_prompt = List.hd !captured_prompts in
      Alcotest.(check bool)
        "retry prompt contains schema heading"
        true
        (contains retry_prompt "## Required output schema")

let test_native_schema_path_is_single_call () =
  let backend, call_count, _, captured_schemas =
    make_mock
      ~supports_resume:false
      ~native:true
      ~responses:[make_valid_result ()]
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let completer =
    Backend_completer.make ~sw ~env ~backend ~working_dir:"/tmp" ()
  in
  match
    completer
      ~system_prompt:"sys"
      ~prompt:"user"
      ~json_schema:(Some object_schema)
      ~resume_session_id:None
  with
  | Error e -> Alcotest.failf "expected Ok but got Error: %s" e
  | Ok _ ->
      Alcotest.(check int) "native path is one call" 1 !call_count ;
      Alcotest.(check (option yojson))
        "native path keeps schema on task spec"
        (Some object_schema)
        (List.hd (List.rev !captured_schemas))

let test_native_schema_rejection_returns_error () =
  let backend, call_count, _, _ =
    make_mock
      ~supports_resume:false
      ~native:true
      ~responses:[make_failed_result "schema rejected by native backend"]
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let completer =
    Backend_completer.make ~sw ~env ~backend ~working_dir:"/tmp" ()
  in
  match
    completer
      ~system_prompt:"sys"
      ~prompt:"user"
      ~json_schema:(Some object_schema)
      ~resume_session_id:None
  with
  | Ok _ -> Alcotest.fail "expected Error on native schema rejection"
  | Error msg ->
      Alcotest.(check int) "native rejection is single-call" 1 !call_count ;
      Alcotest.(check bool)
        "error surfaces native rejection"
        true
        (contains msg "native-backend schema rejection")

let () =
  Alcotest.run
    "backend_completer_json_schema"
    [
      ( "seam",
        [
          Alcotest.test_case
            "no schema pass-through"
            `Quick
            test_no_schema_pass_through;
          Alcotest.test_case
            "schema routes through enforcer retry"
            `Quick
            test_schema_routes_through_enforcer_retry;
          Alcotest.test_case
            "native schema path is single call"
            `Quick
            test_native_schema_path_is_single_call;
          Alcotest.test_case
            "native schema rejection returns error"
            `Quick
            test_native_schema_rejection_returns_error;
        ] );
    ]
