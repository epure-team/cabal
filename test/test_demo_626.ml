(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #626 — Json_schema_enforcer unit tests with mock backends.

    Covers:
    - AC1: json_schema = None → pass-through, exactly one backend call
    - AC2: json_schema = Some, first response valid → exactly one backend call
    - AC3: first invalid + session_resume → 2 calls; second prompt has schema
           but NOT original prompt
    - AC4: first invalid + no session_resume → 2 calls; second prompt has both
           original prompt and schema
    - AC5: both invalid → Failed result surfacing both error messages
    - AC6: resumed response succeeds → session_id from second result propagated
    - AC7: Failed/Timeout/Cancelled first result propagated without retry
           (all three status constructors exercised separately)
    - AC8: resume retry prompt matches resume_retry_template with substitutions
    - AC9: fresh retry prompt matches fresh_retry_template with substitutions
    - AC10: first invalid + second Failed/Timeout/Cancelled reports the real
            second-attempt backend status *)

open Cabal

(** Substring search helper. *)
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

(** Build a minimal inline mock backend.

    Returns [(backend, call_count_ref, captured_prompts_ref)].
    [call_count_ref] counts how many times [run_task] was invoked.
    [captured_prompts_ref] accumulates prompts in reverse call order
    (most-recent-first), so [List.hd !captured_prompts_ref] is the last call. *)
let make_mock ~supports_resume ~responses =
  let call_count = ref 0 in
  let captured_prompts = ref [] in
  let module M = struct
    let id = "test-mock"

    let name = "Test Mock Backend"

    let models = []

    let models_probe = None

    let available ~sw:_ ~env:_ = true

    let supports_session_resume = supports_resume

    let native_json_schema_output = false

    let is_resume_failure _ = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "test mock has no config"

    let run_task ~sw:_ ~env:_ ?on_raw_line:_ (spec : Backend_types.task_spec) =
      let i = !call_count in
      incr call_count ;
      captured_prompts := spec.Backend_types.prompt :: !captured_prompts ;
      if i < List.length responses then List.nth responses i
      else
        Backend_types.make_task_result
          ~status:(Backend_types.Failed "no more scripted responses")
          ()
  end in
  ((module M : Agentic_backend.S), call_count, captured_prompts)

(** Object schema and response fixtures. *)
let object_schema = `Assoc [("type", `String "object")]

let valid_json_response = {|{"result":"ok"}|}

let invalid_response = "not-json-at-all"

(** A valid task_result with agent_text = valid JSON object and a session id. *)
let make_valid_result ?(session_id = "test-session-1") () =
  Backend_types.make_task_result
    ~status:Backend_types.Success
    ~agent_text:valid_json_response
    ~session_id
    ()

(** A task_result with agent_text that fails schema validation. *)
let make_invalid_result ?(session_id = "test-session-1") () =
  Backend_types.make_task_result
    ~status:Backend_types.Success
    ~agent_text:invalid_response
    ~session_id
    ()

(** A failed second-attempt result with invalid text that must not be schema
    validated. *)
let make_failed_retry_result msg =
  Backend_types.make_task_result
    ~status:(Backend_types.Failed msg)
    ~agent_text:"retry-failed-not-json"
    ()

(** A timed-out second-attempt result with invalid text that must not be schema
    validated. *)
let make_timeout_retry_result () =
  Backend_types.make_task_result
    ~status:Backend_types.Timeout
    ~agent_text:"retry-timeout-not-json"
    ()

(** A cancelled second-attempt result with invalid text that must not be schema
    validated. *)
let make_cancelled_retry_result () =
  Backend_types.make_task_result
    ~status:Backend_types.Cancelled
    ~agent_text:"retry-cancelled-not-json"
    ()

(** {1 AC1 — pass-through when json_schema = None} *)

let test_pass_through () =
  let backend, call_count, _ =
    make_mock ~supports_resume:false ~responses:[make_valid_result ()]
  in
  let spec =
    Backend_types.make_task_spec ~prompt:"original" ~working_dir:"/tmp" ()
  in
  (* json_schema = None → pass-through *)
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int) "AC1: exactly one backend call" 1 !call_count ;
  Alcotest.(check bool) "AC1: result is Ok" true (Result.is_ok result)

(** {1 AC2 — exactly one call when first response is valid} *)

let test_valid_first_response () =
  let backend, call_count, _ =
    make_mock ~supports_resume:false ~responses:[make_valid_result ()]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"original"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int) "AC2: exactly one backend call" 1 !call_count ;
  Alcotest.(check bool) "AC2: result is Ok" true (Result.is_ok result)

(** {1 AC3 — invalid + session_resume → 2 calls; second prompt has schema but
    NOT original prompt} *)

let test_invalid_with_session_resume () =
  let second_session_id = "test-session-2" in
  let backend, call_count, captured_prompts =
    make_mock
      ~supports_resume:true
      ~responses:
        [
          make_invalid_result ~session_id:"test-session-1" ();
          make_valid_result ~session_id:second_session_id ();
        ]
  in
  let original_prompt = "original-prompt-unique-token-AC3" in
  let spec =
    Backend_types.make_task_spec
      ~prompt:original_prompt
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int) "AC3: exactly two backend calls" 2 !call_count ;
  Alcotest.(check bool) "AC3: result is Ok" true (Result.is_ok result) ;
  (* The second (most-recent) call prompt is at the head of the list. *)
  let second_prompt = List.hd !captured_prompts in
  Alcotest.(check bool)
    "AC3: second prompt contains schema heading"
    true
    (contains second_prompt "## Required output schema") ;
  Alcotest.(check bool)
    "AC3: second prompt does NOT contain original prompt"
    false
    (contains second_prompt original_prompt)

(** {1 AC4 — invalid + no session_resume → 2 calls; second prompt has both
    original prompt and schema} *)

let test_invalid_without_session_resume () =
  let backend, call_count, captured_prompts =
    make_mock
      ~supports_resume:false
      ~responses:[make_invalid_result (); make_valid_result ()]
  in
  let original_prompt = "original-prompt-unique-token-AC4" in
  let spec =
    Backend_types.make_task_spec
      ~prompt:original_prompt
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int) "AC4: exactly two backend calls" 2 !call_count ;
  Alcotest.(check bool) "AC4: result is Ok" true (Result.is_ok result) ;
  let second_prompt = List.hd !captured_prompts in
  Alcotest.(check bool)
    "AC4: second prompt contains original prompt"
    true
    (contains second_prompt original_prompt) ;
  Alcotest.(check bool)
    "AC4: second prompt contains schema heading"
    true
    (contains second_prompt "## Required output schema")

(** {1 AC5 — both invalid → Error carrying both validation error messages} *)

let test_both_invalid () =
  let backend, call_count, _ =
    make_mock
      ~supports_resume:false
      ~responses:[make_invalid_result (); make_invalid_result ()]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"original"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int) "AC5: exactly two backend calls" 2 !call_count ;
  match result with
  | Ok _ -> Alcotest.fail "AC5: expected Error but got Ok"
  | Error msg ->
      Alcotest.(check bool)
        "AC5: error message is non-empty"
        true
        (String.length msg > 0) ;
      (* Both attempt errors must be surfaced — check the message contains
         two distinct error references. *)
      Alcotest.(check bool)
        "AC5: error references attempt 1"
        true
        (contains msg "Attempt 1") ;
      Alcotest.(check bool)
        "AC5: error references attempt 2"
        true
        (contains msg "Attempt 2")

(** {1 AC6 — session_id from the resumed result is propagated} *)

let test_session_id_propagated () =
  let second_session_id = "propagated-session-99" in
  let backend, _, _ =
    make_mock
      ~supports_resume:true
      ~responses:
        [
          make_invalid_result ~session_id:"first-session" ();
          make_valid_result ~session_id:second_session_id ();
        ]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"original"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  match result with
  | Error msg -> Alcotest.failf "AC6: expected Ok but got Error: %s" msg
  | Ok task_result ->
      Alcotest.(check (option string))
        "AC6: session_id propagated from resumed result"
        (Some second_session_id)
        task_result.Backend_types.session_id

(** {1 AC7 — Failed/Timeout/Cancelled backend result is propagated without
    schema validation or retry.

    Regression guard: a non-Success [result1.status] must NOT trigger
    schema validation (which would misdiagnose the failure as schema
    non-conformance and waste a backend call). *)

let test_failed_backend_result_is_propagated () =
  let failed_result =
    Backend_types.make_task_result
      ~status:(Backend_types.Failed "backend process crashed")
      ()
  in
  let backend, call_count, _ =
    make_mock ~supports_resume:false ~responses:[failed_result]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"original"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int) "AC7: exactly one backend call (no retry)" 1 !call_count ;
  match result with
  | Error msg ->
      Alcotest.failf
        "AC7: expected Ok (propagated Failed result) but got Error: %s"
        msg
  | Ok task_result ->
      Alcotest.(check bool)
        "AC7: returned status is Failed"
        true
        (match task_result.Backend_types.status with
        | Backend_types.Failed _ -> true
        | _ -> false)

(** {1 AC7b — Timeout backend result is propagated without retry} *)

let test_timeout_result_is_propagated () =
  let timeout_result =
    Backend_types.make_task_result ~status:Backend_types.Timeout ()
  in
  let backend, call_count, _ =
    make_mock ~supports_resume:false ~responses:[timeout_result]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"original"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int) "AC7b: exactly one backend call (no retry)" 1 !call_count ;
  match result with
  | Error msg ->
      Alcotest.failf
        "AC7b: expected Ok (propagated Timeout result) but got Error: %s"
        msg
  | Ok task_result ->
      Alcotest.(check bool)
        "AC7b: returned status is Timeout"
        true
        (match task_result.Backend_types.status with
        | Backend_types.Timeout -> true
        | _ -> false)

(** {1 AC7c — Cancelled backend result is propagated without retry} *)

let test_cancelled_result_is_propagated () =
  let cancelled_result =
    Backend_types.make_task_result ~status:Backend_types.Cancelled ()
  in
  let backend, call_count, _ =
    make_mock ~supports_resume:false ~responses:[cancelled_result]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"original"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int) "AC7c: exactly one backend call (no retry)" 1 !call_count ;
  match result with
  | Error msg ->
      Alcotest.failf
        "AC7c: expected Ok (propagated Cancelled result) but got Error: %s"
        msg
  | Ok task_result ->
      Alcotest.(check bool)
        "AC7c: returned status is Cancelled"
        true
        (match task_result.Backend_types.status with
        | Backend_types.Cancelled -> true
        | _ -> false)

(** {1 AC10 — Failed/Timeout/Cancelled second attempt reports backend status}

    Regression guard: when attempt 1 succeeds at the transport layer but fails
    schema validation, the retry result must have [status] checked before
    validating [agent_text]. Otherwise Failed/Timeout/Cancelled retries are
    misreported as schema-validation failures for their placeholder text. *)

let check_second_attempt_backend_status_is_reported ~case_label ~second_result
    ~expected_status_text () =
  let backend, call_count, _ =
    make_mock
      ~supports_resume:false
      ~responses:[make_invalid_result (); second_result]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"original"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int)
    (case_label ^ ": exactly two backend calls")
    2
    !call_count ;
  match result with
  | Ok _ -> Alcotest.failf "%s: expected Error but got Ok" case_label
  | Error msg ->
      Alcotest.(check bool)
        (case_label ^ ": error references attempt 1")
        true
        (contains msg "Attempt 1") ;
      Alcotest.(check bool)
        (case_label ^ ": error references attempt 2")
        true
        (contains msg "Attempt 2") ;
      Alcotest.(check bool)
        (case_label ^ ": error preserves retry backend status")
        true
        (contains msg expected_status_text)

let test_second_attempt_failed_status_is_reported () =
  check_second_attempt_backend_status_is_reported
    ~case_label:"AC10a Failed retry"
    ~second_result:(make_failed_retry_result "backend died on retry")
    ~expected_status_text:"Failed: backend died on retry"
    ()

let test_second_attempt_timeout_status_is_reported () =
  check_second_attempt_backend_status_is_reported
    ~case_label:"AC10b Timeout retry"
    ~second_result:(make_timeout_retry_result ())
    ~expected_status_text:"Timeout"
    ()

let test_second_attempt_cancelled_status_is_reported () =
  check_second_attempt_backend_status_is_reported
    ~case_label:"AC10c Cancelled retry"
    ~second_result:(make_cancelled_retry_result ())
    ~expected_status_text:"Cancelled"
    ()

(** {1 Template helpers}

    Substitute named placeholders of the form [{key}] in a template string.
    Replacements are applied in-order; first match wins for each placeholder. *)

let replace_first s ~from ~to_ =
  let sl = String.length s and fl = String.length from in
  let rec find i =
    if i + fl > sl then s
    else if String.sub s i fl = from then
      String.sub s 0 i ^ to_ ^ String.sub s (i + fl) (sl - i - fl)
    else find (i + 1)
  in
  find 0

let expand_template template substs =
  List.fold_left
    (fun s (placeholder, value) -> replace_first s ~from:placeholder ~to_:value)
    template
    substs

(** {1 AC8 — resume retry prompt matches resume_retry_template} *)

let test_resume_prompt_matches_template () =
  let schema = object_schema in
  let schema_json = Yojson.Safe.to_string ~std:true schema in
  (* Compute the validation error that the enforcer will see for invalid_response. *)
  let err =
    match Json_schema_validator.validate ~schema ~document:invalid_response with
    | Ok () ->
        Alcotest.fail
          "AC8 setup: expected validation error for invalid_response but got Ok"
    | Error e -> e
  in
  let second_session_id = "template-resume-session-2" in
  let backend, _, captured_prompts =
    make_mock
      ~supports_resume:true
      ~responses:
        [
          make_invalid_result ~session_id:"template-resume-session-1" ();
          make_valid_result ~session_id:second_session_id ();
        ]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"template-test-original-prompt"
      ~working_dir:"/tmp"
      ~json_schema:schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let _result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  (* The second (most-recent) call prompt is at the head of the captured list. *)
  let second_prompt = List.hd !captured_prompts in
  let expected =
    expand_template
      Json_schema_enforcer.resume_retry_template
      [("{schema}", schema_json); ("{error}", err)]
  in
  Alcotest.(check string)
    "AC8: resume prompt equals template with {schema} and {error} substituted"
    expected
    second_prompt

(** {1 AC9 — fresh retry prompt matches fresh_retry_template} *)

let test_fresh_prompt_matches_template () =
  let schema = object_schema in
  let schema_json = Yojson.Safe.to_string ~std:true schema in
  let err =
    match Json_schema_validator.validate ~schema ~document:invalid_response with
    | Ok () ->
        Alcotest.fail
          "AC9 setup: expected validation error for invalid_response but got Ok"
    | Error e -> e
  in
  let original_prompt = "fresh-template-original-prompt-unique-token" in
  let backend, _, captured_prompts =
    make_mock
      ~supports_resume:false
      ~responses:[make_invalid_result (); make_valid_result ()]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:original_prompt
      ~working_dir:"/tmp"
      ~json_schema:schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let _result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  let second_prompt = List.hd !captured_prompts in
  let expected =
    expand_template
      Json_schema_enforcer.fresh_retry_template
      [
        ("{original_prompt}", original_prompt);
        ("{schema}", schema_json);
        ("{error}", err);
      ]
  in
  Alcotest.(check string)
    "AC9: fresh prompt equals template with {original_prompt}, {schema}, and \
     {error} substituted"
    expected
    second_prompt

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story_626_enforcer_unit_tests"
    [
      ( "AC1 pass-through when json_schema = None",
        [Alcotest.test_case "exactly one backend call" `Quick test_pass_through]
      );
      ( "AC2 valid first response — no spurious retry",
        [
          Alcotest.test_case
            "exactly one backend call"
            `Quick
            test_valid_first_response;
        ] );
      ( "AC3 invalid + session_resume → resume path",
        [
          Alcotest.test_case
            "two calls; second prompt has schema not original"
            `Quick
            test_invalid_with_session_resume;
        ] );
      ( "AC4 invalid + no session_resume → fresh retry path",
        [
          Alcotest.test_case
            "two calls; second prompt has both original and schema"
            `Quick
            test_invalid_without_session_resume;
        ] );
      ( "AC5 both invalid → Error with both messages",
        [
          Alcotest.test_case
            "Error carries both attempt errors"
            `Quick
            test_both_invalid;
        ] );
      ( "AC6 session_id propagation from resumed result",
        [
          Alcotest.test_case
            "session_id from second result is returned"
            `Quick
            test_session_id_propagated;
        ] );
      ( "AC7a Failed backend result propagated without retry",
        [
          Alcotest.test_case
            "exactly one call; Ok with Failed status"
            `Quick
            test_failed_backend_result_is_propagated;
        ] );
      ( "AC7b Timeout backend result propagated without retry",
        [
          Alcotest.test_case
            "exactly one call; Ok with Timeout status"
            `Quick
            test_timeout_result_is_propagated;
        ] );
      ( "AC7c Cancelled backend result propagated without retry",
        [
          Alcotest.test_case
            "exactly one call; Ok with Cancelled status"
            `Quick
            test_cancelled_result_is_propagated;
        ] );
      ( "AC10a invalid first + Failed retry reports backend status",
        [
          Alcotest.test_case
            "Error includes retry Failed status"
            `Quick
            test_second_attempt_failed_status_is_reported;
        ] );
      ( "AC10b invalid first + Timeout retry reports backend status",
        [
          Alcotest.test_case
            "Error includes retry Timeout status"
            `Quick
            test_second_attempt_timeout_status_is_reported;
        ] );
      ( "AC10c invalid first + Cancelled retry reports backend status",
        [
          Alcotest.test_case
            "Error includes retry Cancelled status"
            `Quick
            test_second_attempt_cancelled_status_is_reported;
        ] );
      ( "AC8 resume retry prompt matches resume_retry_template",
        [
          Alcotest.test_case
            "prompt equals template with substitutions"
            `Quick
            test_resume_prompt_matches_template;
        ] );
      ( "AC9 fresh retry prompt matches fresh_retry_template",
        [
          Alcotest.test_case
            "prompt equals template with substitutions"
            `Quick
            test_fresh_prompt_matches_template;
        ] );
    ]
