(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #625 — Native JSON schema wiring for the first supporting
    backend.

    Covers:
    - AC-N1: native backend + json_schema = Some + non-JSON success response
             → Ok result, exactly 1 backend call (validate-and-retry NOT
             executed; no schema validation on native path).
    - AC-N2: native backend + json_schema = Some + Failed result
             → Error immediately with "native" in message, exactly 1 call
             (fail-fast; no fallback to validate-and-retry).
    - AC-N3: native backend + json_schema = None
             → Ok result, exactly 1 backend call (pass-through unchanged).
    - AC-N4: native backend + json_schema = Some + Success result
             → Ok result, session_id propagated correctly. *)

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

(** Build a minimal inline mock backend that declares
    [native_json_schema_output = true].

    Returns [(backend, call_count_ref, captured_specs_ref)].
    [call_count_ref] counts how many times [run_task] was invoked.
    [captured_specs_ref] accumulates specs in reverse call order. *)
let make_native_mock ?(supports_resume = false) ~responses () =
  let call_count = ref 0 in
  let captured_specs = ref [] in
  let module M = struct
    let id = "test-native-mock"

    let name = "Test Native Mock Backend"

    let models = []

    let models_probe = None

    let available ~sw:_ ~env:_ = true

    let supports_session_resume = supports_resume

    let native_json_schema_output = true

    let is_resume_failure _ = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "test native mock has no config"

    let run_task ~sw:_ ~env:_ ?on_raw_line:_ (spec : Backend_types.task_spec) =
      let i = !call_count in
      incr call_count ;
      captured_specs := spec :: !captured_specs ;
      if i < List.length responses then List.nth responses i
      else
        Backend_types.make_task_result
          ~status:(Backend_types.Failed "no more scripted responses")
          ()
  end in
  ((module M : Agentic_backend.S), call_count, captured_specs)

(** Object schema fixture. *)
let object_schema = `Assoc [("type", `String "object")]

(** A success result whose [agent_text] is NOT valid JSON (would fail
    validate-and-retry validation, but native path skips validation). *)
let non_json_success_result () =
  Backend_types.make_task_result
    ~agent_text:"this is not valid json at all"
    ~status:Backend_types.Success
    ()

(** A success result whose [agent_text] IS valid JSON conforming to
    [object_schema]. *)
let valid_json_result ?(session_id = "sess-42") () =
  Backend_types.make_task_result
    ~agent_text:{|{"result":"ok"}|}
    ~status:Backend_types.Success
    ~session_id
    ()

(** A failed result simulating native schema rejection. *)
let native_rejection_result () =
  Backend_types.make_task_result
    ~status:
      (Backend_types.Failed "unsupported JSON Schema keywords: $defs, if, then")
    ()

(** {1 AC-N1 — validate-and-retry NOT executed on native path}

    The enforcer must NOT run schema validation or issue a retry when the
    backend declares [native_json_schema_output = true].  Even a non-JSON
    response must be returned as [Ok] after exactly one backend call. *)

let test_native_no_validation_no_retry () =
  let backend, call_count, _ =
    make_native_mock ~responses:[non_json_success_result ()] ()
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"give me json"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int)
    "AC-N1: exactly one backend call (no retry)"
    1
    !call_count ;
  Alcotest.(check bool) "AC-N1: result is Ok" true (Result.is_ok result)

(** {1 AC-N2 — native rejection is returned as Error immediately}

    When the native backend returns [Failed], the enforcer must return
    [Error] containing the word "native" and must NOT make a second call. *)

let test_native_rejection_fail_fast () =
  let backend, call_count, _ =
    make_native_mock ~responses:[native_rejection_result ()] ()
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"give me json"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int)
    "AC-N2: exactly one backend call (no fallback retry)"
    1
    !call_count ;
  match result with
  | Ok _ -> Alcotest.fail "AC-N2: expected Error (native rejection) but got Ok"
  | Error msg ->
      Alcotest.(check bool)
        "AC-N2: error message identifies native rejection"
        true
        (contains msg "native")

(** {1 AC-N3 — json_schema = None is still a plain pass-through}

    Native capability does not change the pass-through behaviour when no
    schema is requested. *)

let test_native_passthrough_no_schema () =
  let backend, call_count, _ =
    make_native_mock ~responses:[valid_json_result ()] ()
  in
  let spec =
    Backend_types.make_task_spec ~prompt:"do something" ~working_dir:"/tmp" ()
    (* no json_schema *)
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  Alcotest.(check int) "AC-N3: exactly one backend call" 1 !call_count ;
  Alcotest.(check bool) "AC-N3: result is Ok" true (Result.is_ok result)

(** {1 AC-N4 — session_id is propagated from the native result}

    On the native path the [task_result] is returned as-is; session_id must
    be the one from the backend call. *)

let test_native_session_id_propagated () =
  let sid = "native-session-xyz" in
  let backend, _, _ =
    make_native_mock ~responses:[valid_json_result ~session_id:sid ()] ()
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"give me json"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  match result with
  | Error msg -> Alcotest.failf "AC-N4: expected Ok but got Error: %s" msg
  | Ok task_result ->
      Alcotest.(check (option string))
        "AC-N4: session_id propagated from native result"
        (Some sid)
        task_result.Backend_types.session_id

(** {1 AC-N5 — schema is present in the spec passed to the native backend}

    The [task_spec] given to the native backend must still carry
    [json_schema = Some _] so the backend implementation can wire it to its
    CLI flag. *)

let test_native_schema_in_spec () =
  let backend, _, captured_specs =
    make_native_mock ~responses:[valid_json_result ()] ()
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"give me json"
      ~working_dir:"/tmp"
      ~json_schema:object_schema
      ()
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let _ = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
  match !captured_specs with
  | [] -> Alcotest.fail "AC-N5: no spec was captured"
  | received_spec :: _ ->
      Alcotest.(check bool)
        "AC-N5: json_schema is Some in spec passed to native backend"
        true
        (Option.is_some received_spec.Backend_types.json_schema)

let () =
  Alcotest.run
    "Story_625_native_json_schema_wiring"
    [
      ( "AC-N1 native path — no validation, no retry",
        [
          Alcotest.test_case
            "non-JSON success response returns Ok after 1 call"
            `Quick
            test_native_no_validation_no_retry;
        ] );
      ( "AC-N2 native rejection — fail-fast, no fallback",
        [
          Alcotest.test_case
            "Failed result returns Error with native message, 1 call"
            `Quick
            test_native_rejection_fail_fast;
        ] );
      ( "AC-N3 no schema — native pass-through unchanged",
        [
          Alcotest.test_case
            "json_schema = None still gives exactly 1 call"
            `Quick
            test_native_passthrough_no_schema;
        ] );
      ( "AC-N4 session_id propagated from native result",
        [
          Alcotest.test_case
            "session_id from backend result is returned"
            `Quick
            test_native_session_id_propagated;
        ] );
      ( "AC-N5 schema present in spec passed to native backend",
        [
          Alcotest.test_case
            "json_schema = Some in spec received by native backend run_task"
            `Quick
            test_native_schema_in_spec;
        ] );
    ]
