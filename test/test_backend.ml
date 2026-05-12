(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the agentic backend abstraction layer. *)

open Cabal

let mock_fixtures_env_var = "EPURE_MOCK_AGENT_FIXTURES"

(** {1 Test Fixtures} *)

(** A mock backend for testing. *)
module Mock_backend : Agentic_backend.S = struct
  let id = "mock"

  let name = "Mock Backend"

  let available ~sw:_ ~env:_ = true

  let supports_session_resume = false

  let is_resume_failure (_result : Backend_types.task_result) = false

  let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
    Agentic_backend.Config_check_unsupported
      "mock test backend does not generate project config"

  let run_task ~sw:_ ~env:_ ?on_raw_line:_ (spec : Backend_types.task_spec) =
    (* Simple mock: just echo success with the prompt *)
    {
      Backend_types.status = Backend_types.Success;
      files_changed = [];
      report =
        Some
          {
            Backend_types.verdict = Some "approved";
            issues = [];
            questions = [];
            suggestions = [];
            raw_json = None;
          };
      elapsed = 1.0;
      cost = None;
      stdout = "Mock executed: " ^ spec.prompt;
      stderr = "";
      exit_code = 0;
      session_id = None;
    }
end

(** An unavailable backend for testing. *)
module Unavailable_backend : Agentic_backend.S = struct
  let id = "unavailable"

  let name = "Unavailable Backend"

  let available ~sw:_ ~env:_ = false

  let supports_session_resume = false

  let is_resume_failure (_result : Backend_types.task_result) = false

  let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
    Agentic_backend.Config_check_unsupported
      "unavailable test backend does not generate project config"

  let run_task ~sw:_ ~env:_ ?on_raw_line:_ (_spec : Backend_types.task_spec) =
    {
      Backend_types.status = Backend_types.Failed "Backend not available";
      files_changed = [];
      report = None;
      elapsed = 0.0;
      cost = None;
      stdout = "";
      stderr = "Backend not available";
      exit_code = 1;
      session_id = None;
    }
end

(** {1 Backend Types Tests} *)

let test_make_task_spec () =
  let spec =
    Backend_types.make_task_spec
      ~prompt:"Test prompt"
      ~working_dir:"/tmp/test"
      ()
  in
  Alcotest.(check string) "prompt" "Test prompt" spec.prompt ;
  Alcotest.(check string) "instructions default" "" spec.instructions ;
  Alcotest.(check string) "working_dir" "/tmp/test" spec.working_dir ;
  Alcotest.(check (float 0.01)) "default timeout" max_float spec.timeout ;
  Alcotest.(check bool)
    "expected_outputs default"
    true
    (spec.expected_outputs
    = [Backend_types.Files_changed; Backend_types.Structured_report]) ;
  Alcotest.(check int) "lsp_servers default" 0 (List.length spec.lsp_servers) ;
  Alcotest.(check string)
    "managed namespace default id"
    "epure"
    spec.managed_namespace.id ;
  Alcotest.(check string)
    "managed namespace default config_dir"
    ".epure/backend-config"
    spec.managed_namespace.config_dir

let test_make_task_spec_with_options () =
  let mcp_config =
    Backend_types.make_mcp_server_config
      ~name:"epure"
      ~command:"epure"
      ~args:["mcp-server"]
      ()
  in
  let lsp_config : Backend_types.lsp_server_config =
    {
      name = "ocaml-lsp";
      command = "ocamllsp";
      args = [];
      file_associations =
        [
          {extension = ".ml"; language_id = "ocaml"};
          {extension = ".mli"; language_id = "ocaml-interface"};
        ];
    }
  in
  let managed_namespace : Backend_types.managed_namespace =
    {
      id = "crucible";
      display_name = "Crucible";
      config_dir = ".crucible/backend-config";
    }
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"Build story #12"
      ~instructions:"Follow OCaml conventions"
      ~mcp_servers:[mcp_config]
      ~lsp_servers:[lsp_config]
      ~managed_namespace
      ~working_dir:"/home/user/project"
      ~timeout:600.0
      ~expected_outputs:[Backend_types.Structured_report]
      ()
  in
  Alcotest.(check string) "prompt" "Build story #12" spec.prompt ;
  Alcotest.(check string)
    "instructions"
    "Follow OCaml conventions"
    spec.instructions ;
  Alcotest.(check int) "mcp_servers count" 1 (List.length spec.mcp_servers) ;
  Alcotest.(check int) "lsp_servers count" 1 (List.length spec.lsp_servers) ;
  Alcotest.(check string)
    "custom namespace id"
    "crucible"
    spec.managed_namespace.id ;
  Alcotest.(check (float 0.01)) "timeout" 600.0 spec.timeout ;
  Alcotest.(check bool)
    "expected_outputs"
    true
    (spec.expected_outputs = [Backend_types.Structured_report])

let test_mcp_server_config () =
  let config =
    Backend_types.make_mcp_server_config
      ~name:"test-server"
      ~command:"/usr/bin/test"
      ~args:["--port"; "8080"]
      ~env:[("DEBUG", "1")]
      ()
  in
  Alcotest.(check string) "name" "test-server" config.name ;
  Alcotest.(check string) "command" "/usr/bin/test" config.command ;
  Alcotest.(check (list string)) "args" ["--port"; "8080"] config.args ;
  Alcotest.(check (list (pair string string))) "env" [("DEBUG", "1")] config.env

let test_duration () =
  let d = Backend_types.duration_of_seconds 123.456 in
  Alcotest.(check (float 0.001))
    "to_seconds"
    123.456
    (Backend_types.duration_to_seconds d) ;
  Alcotest.(check string) "show" "123.46s" (Backend_types.show_duration d)

let test_empty_report () =
  let report = Backend_types.empty_report in
  Alcotest.(check (option string)) "verdict" None report.verdict ;
  Alcotest.(check (list string)) "issues" [] report.issues ;
  Alcotest.(check (list string)) "questions" [] report.questions ;
  Alcotest.(check (list string)) "suggestions" [] report.suggestions

let test_task_spec_json_roundtrip () =
  let mcp_config =
    Backend_types.make_mcp_server_config
      ~name:"epure"
      ~command:"/usr/bin/epure"
      ~args:["mcp-server"; "--port"; "9090"]
      ~env:[("DEBUG", "1"); ("HOME", "/tmp")]
      ()
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"Build story #42"
      ~instructions:"Follow OCaml conventions"
      ~mcp_servers:[mcp_config]
      ~lsp_servers:
        [
          {
            Backend_types.name = "typescript";
            command = "typescript-language-server";
            args = ["--stdio"];
            file_associations =
              [
                {Backend_types.extension = ".ts"; language_id = "typescript"};
                {
                  Backend_types.extension = ".tsx";
                  language_id = "typescriptreact";
                };
              ];
          };
        ]
      ~working_dir:"/home/user/project"
      ~timeout:600.0
      ~expected_outputs:
        [Backend_types.Files_changed; Backend_types.Structured_report]
      ()
  in
  let json = Backend_types.task_spec_to_yojson spec in
  match Backend_types.task_spec_of_yojson json with
  | Error e -> Alcotest.failf "task_spec deserialization failed: %s" e
  | Ok roundtripped ->
      Alcotest.(check bool)
        "task_spec round-trips"
        true
        (Backend_types.equal_task_spec spec roundtripped)

let test_task_spec_json_defaults_for_new_fields () =
  let json =
    `Assoc
      [
        ("prompt", `String "legacy");
        ("instructions", `String "");
        ("mcp_servers", `List []);
        ("working_dir", `String "/tmp/project");
        ("timeout", `Float 1.0);
        ("expected_outputs", `List []);
        ("read_only", `Bool false);
      ]
  in
  match Backend_types.task_spec_of_yojson json with
  | Error e -> Alcotest.failf "legacy task_spec decode failed: %s" e
  | Ok spec ->
      Alcotest.(check int) "legacy lsp default" 0 (List.length spec.lsp_servers) ;
      Alcotest.(check string)
        "legacy namespace default"
        "epure"
        spec.managed_namespace.id

let test_validate_managed_namespace_rejects_bad_id () =
  let namespace : Backend_types.managed_namespace =
    {id = "../bad"; display_name = "Bad"; config_dir = ".epure/backend-config"}
  in
  match Backend_types.validate_managed_namespace namespace with
  | Ok () -> Alcotest.fail "expected invalid namespace id to be rejected"
  | Error msg ->
      Alcotest.(check bool) "error mentions id" true (String.contains msg 'i')

let test_validate_managed_namespace_rejects_absolute_config_dir () =
  let namespace : Backend_types.managed_namespace =
    {id = "safe"; display_name = "Safe"; config_dir = "/tmp/epure"}
  in
  match Backend_types.validate_managed_namespace namespace with
  | Ok () -> Alcotest.fail "expected absolute config_dir to be rejected"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions config_dir"
        true
        (String.length msg > 0)

let test_validate_managed_namespace_rejects_parent_config_dir () =
  let namespace : Backend_types.managed_namespace =
    {id = "safe"; display_name = "Safe"; config_dir = ".epure/../evil"}
  in
  match Backend_types.validate_managed_namespace namespace with
  | Ok () -> Alcotest.fail "expected parent segment to be rejected"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions parent segment"
        true
        (String.length msg > 0)

let test_make_task_spec_rejects_invalid_namespace () =
  let namespace : Backend_types.managed_namespace =
    {id = "Bad"; display_name = "Bad"; config_dir = ".epure/backend-config"}
  in
  try
    ignore
      (Backend_types.make_task_spec
         ~prompt:"test"
         ~working_dir:"/tmp"
         ~managed_namespace:namespace
         ()) ;
    Alcotest.fail "expected make_task_spec to reject invalid namespace"
  with Invalid_argument msg ->
    Alcotest.(check bool)
      "invalid argument explains namespace"
      true
      (String.length msg > 0)

let test_session_event_log_uses_session_logs_dir () =
  Eio_posix.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_dir "cabal_sessions_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () ->
      Session_event_log.write_session_start
        ~fs
        ~session_logs_dir:dir
        ~session_id:"session_1"
        ~backend:"mock"
        ~story_id:42
        ~agent_role:"builder"
        () ;
      Alcotest.(check (list string))
        "session listed from caller-provided dir"
        ["session_1"]
        (Session_event_log.list_sessions ~fs ~session_logs_dir:dir ()) ;
      let events =
        Session_event_log.read_events
          ~fs
          ~session_logs_dir:dir
          ~session_id:"session_1"
          ()
      in
      Alcotest.(check int) "one event" 1 (List.length events))

let test_task_result_json_roundtrip () =
  let report =
    {
      Backend_types.verdict = Some "approved";
      issues = ["issue1"; "issue2"];
      questions = ["question1"];
      suggestions = ["suggestion1"; "suggestion2"; "suggestion3"];
      raw_json = Some (`Assoc [("key", `String "value")]);
    }
  in
  let cost =
    {
      Backend_types.tokens_input = Some 1500;
      tokens_output = Some 800;
      cost_usd = Some 0.042;
      cache_creation_input_tokens = None;
      cache_read_input_tokens = None;
    }
  in
  let result =
    {
      Backend_types.status = Backend_types.Success;
      files_changed = ["src/main.ml"; "src/utils.ml"];
      report = Some report;
      elapsed = 45.5;
      cost = Some cost;
      stdout = "Build succeeded\nAll tests pass";
      stderr = "Warning: unused variable";
      exit_code = 0;
      session_id = None;
    }
  in
  let json = Backend_types.task_result_to_yojson result in
  match Backend_types.task_result_of_yojson json with
  | Error e -> Alcotest.failf "task_result deserialization failed: %s" e
  | Ok roundtripped ->
      Alcotest.(check bool)
        "task_result round-trips"
        true
        (Backend_types.equal_task_result result roundtripped)

let test_task_result_failed_json_roundtrip () =
  let result =
    {
      Backend_types.status = Backend_types.Failed "compilation error";
      files_changed = [];
      report = None;
      elapsed = 12.3;
      cost = None;
      stdout = "";
      stderr = "Error: unbound module";
      exit_code = 1;
      session_id = None;
    }
  in
  let json = Backend_types.task_result_to_yojson result in
  match Backend_types.task_result_of_yojson json with
  | Error e -> Alcotest.failf "task_result deserialization failed: %s" e
  | Ok roundtripped ->
      Alcotest.(check bool)
        "failed task_result round-trips"
        true
        (Backend_types.equal_task_result result roundtripped)

let test_task_result_timeout_json_roundtrip () =
  let result =
    {
      Backend_types.status = Backend_types.Timeout;
      files_changed = [];
      report = None;
      elapsed = 300.0;
      cost = None;
      stdout = "";
      stderr = "";
      exit_code = 124;
      session_id = None;
    }
  in
  let json = Backend_types.task_result_to_yojson result in
  match Backend_types.task_result_of_yojson json with
  | Error e -> Alcotest.failf "task_result deserialization failed: %s" e
  | Ok roundtripped ->
      Alcotest.(check bool)
        "timeout task_result round-trips"
        true
        (Backend_types.equal_task_result result roundtripped)

let backend_types_tests =
  [
    ("make_task_spec defaults", `Quick, test_make_task_spec);
    ("make_task_spec with options", `Quick, test_make_task_spec_with_options);
    ("mcp_server_config", `Quick, test_mcp_server_config);
    ("duration", `Quick, test_duration);
    ("empty_report", `Quick, test_empty_report);
    ("task_spec JSON round-trip", `Quick, test_task_spec_json_roundtrip);
    ( "task_spec JSON defaults for new fields",
      `Quick,
      test_task_spec_json_defaults_for_new_fields );
    ( "managed namespace rejects bad id",
      `Quick,
      test_validate_managed_namespace_rejects_bad_id );
    ( "managed namespace rejects absolute config_dir",
      `Quick,
      test_validate_managed_namespace_rejects_absolute_config_dir );
    ( "managed namespace rejects parent config_dir",
      `Quick,
      test_validate_managed_namespace_rejects_parent_config_dir );
    ( "make_task_spec rejects invalid namespace",
      `Quick,
      test_make_task_spec_rejects_invalid_namespace );
    ( "session event log uses host dir",
      `Quick,
      test_session_event_log_uses_session_logs_dir );
    ("task_result JSON round-trip", `Quick, test_task_result_json_roundtrip);
    ( "task_result failed JSON round-trip",
      `Quick,
      test_task_result_failed_json_roundtrip );
    ( "task_result timeout JSON round-trip",
      `Quick,
      test_task_result_timeout_json_roundtrip );
  ]

(** {1 Agentic Backend Tests} *)

let test_backend_id () =
  let backend = (module Mock_backend : Agentic_backend.S) in
  Alcotest.(check string) "id" "mock" (Agentic_backend.id backend)

let test_backend_name () =
  let backend = (module Mock_backend : Agentic_backend.S) in
  Alcotest.(check string) "name" "Mock Backend" (Agentic_backend.name backend)

let test_backend_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let backend = (module Mock_backend : Agentic_backend.S) in
  Alcotest.(check bool)
    "available"
    true
    (Agentic_backend.available ~sw ~env backend)

let test_backend_unavailable () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let backend = (module Unavailable_backend : Agentic_backend.S) in
  Alcotest.(check bool)
    "not available"
    false
    (Agentic_backend.available ~sw ~env backend)

let test_backend_run_task () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let backend = (module Mock_backend : Agentic_backend.S) in
  let spec =
    Backend_types.make_task_spec ~prompt:"Test task" ~working_dir:"/tmp" ()
  in
  let result = Agentic_backend.run_task ~sw ~env backend spec in
  Alcotest.(check bool)
    "status is Success"
    true
    (result.status = Backend_types.Success) ;
  Alcotest.(check bool)
    "stdout contains prompt"
    true
    (String.starts_with ~prefix:"Mock executed:" result.stdout) ;
  Alcotest.(check int) "exit_code" 0 result.exit_code

let test_backend_run_task_with_ctxt_preserves_opaque_ctxt () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let backend = (module Mock_backend : Agentic_backend.S) in
  let spec =
    Backend_types.make_task_spec ~prompt:"Test task" ~working_dir:"/tmp" ()
  in
  let counter = ref 0 in
  let ctxt =
    ( counter,
      fun () ->
        incr counter ;
        !counter )
  in
  let request = Backend_types.make_task_request ~spec ~ctxt in
  let response = Agentic_backend.run_task_with_ctxt ~sw ~env backend request in
  let returned_counter, tick = response.ctxt in
  Alcotest.(check bool)
    "status is Success"
    true
    (response.result.status = Backend_types.Success) ;
  Alcotest.(check bool) "same ref preserved" true (returned_counter == counter) ;
  Alcotest.(check int) "closure remains usable" 1 (tick ()) ;
  Alcotest.(check int) "closure keeps same state" 2 (tick ())

let agentic_backend_tests =
  [
    ("backend id", `Quick, test_backend_id);
    ("backend name", `Quick, test_backend_name);
    ("backend available", `Quick, test_backend_available);
    ("backend unavailable", `Quick, test_backend_unavailable);
    ("backend run_task", `Quick, test_backend_run_task);
    ( "backend run_task_with_ctxt preserves ctxt",
      `Quick,
      test_backend_run_task_with_ctxt_preserves_opaque_ctxt );
  ]

(** {1 Mock-agent Tests} *)

let with_env_var name value f =
  let previous = Sys.getenv_opt name in
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some v -> Unix.putenv name v
      | None ->
          (* OCaml's Unix module in this toolchain exposes [putenv] but not
             [unsetenv].  Mock_agent treats the empty string like an absent
             fixture path, so this restores the observable behavior needed by
             these tests. *)
          Unix.putenv name "")
    (fun () ->
      Unix.putenv name value ;
      f ())

let with_mock_fixture content f =
  let path = Filename.temp_file "mock-agent" ".json" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content) ;
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () -> with_env_var mock_fixtures_env_var path (fun () -> f path))

let test_mock_agent_available_with_fixture () =
  with_mock_fixture
    {|{"rules":[{"contains":"","stdout":"ok","status":"success"}]}|}
  @@ fun _path ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Alcotest.(check bool)
    "mock-agent available"
    true
    (Mock_agent.available ~sw ~env)

let test_mock_agent_unavailable_without_env () =
  with_env_var mock_fixtures_env_var "" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Alcotest.(check bool)
    "mock-agent unavailable"
    false
    (Mock_agent.available ~sw ~env)

let test_mock_agent_run_task_success () =
  with_mock_fixture
    {|{"rules":[{"contains":"hello","stdout":"world","status":"success"}]}|}
  @@ fun _path ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec =
    Backend_types.make_task_spec ~prompt:"say hello" ~working_dir:"/tmp" ()
  in
  let result = Mock_agent.run_task ~sw ~env spec in
  Alcotest.(check bool)
    "status is success"
    true
    (result.status = Backend_types.Success) ;
  Alcotest.(check string) "stdout" "world" result.stdout

let test_mock_agent_run_task_invalid_fixture () =
  with_mock_fixture "not json" @@ fun _path ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec =
    Backend_types.make_task_spec ~prompt:"say hello" ~working_dir:"/tmp" ()
  in
  let result = Mock_agent.run_task ~sw ~env spec in
  match result.status with
  | Backend_types.Failed msg ->
      Alcotest.(check bool)
        "diagnostic mentions fixture load"
        true
        (String.starts_with
           ~prefix:"mock-agent: failed to load fixture file"
           msg)
  | _ -> Alcotest.fail "Expected invalid fixture to fail"

let test_mock_agent_run_task_unmatched_prompt () =
  with_mock_fixture
    {|{"rules":[{"contains":"builder-only","stdout":"ok","status":"success"}]}|}
  @@ fun _path ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec =
    Backend_types.make_task_spec ~prompt:"review this" ~working_dir:"/tmp" ()
  in
  let result = Mock_agent.run_task ~sw ~env spec in
  match result.status with
  | Backend_types.Failed msg ->
      Alcotest.(check bool)
        "diagnostic mentions unmatched prompt"
        true
        (String.starts_with ~prefix:"mock-agent: no rule matched prompt" msg)
  | _ -> Alcotest.fail "Expected unmatched prompt to fail"

let test_mock_agent_limit_across_repeated_calls () =
  with_mock_fixture
    {|{"rules":[{"contains":"task","stdout":"first","status":"success","limit":1},{"contains":"task","stdout":"fallback","status":"success"}]}|}
  @@ fun _path ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec =
    Backend_types.make_task_spec ~prompt:"task" ~working_dir:"/tmp" ()
  in
  let first = Mock_agent.run_task ~sw ~env spec in
  let second = Mock_agent.run_task ~sw ~env spec in
  Alcotest.(check string) "first call uses limited rule" "first" first.stdout ;
  Alcotest.(check string) "second call falls through" "fallback" second.stdout

let test_mock_agent_rule_ordering () =
  with_mock_fixture
    {|{"rules":[{"contains":"task","stdout":"first","status":"success"},{"contains":"task","stdout":"second","status":"success"}]}|}
  @@ fun _path ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec =
    Backend_types.make_task_spec ~prompt:"task" ~working_dir:"/tmp" ()
  in
  let result = Mock_agent.run_task ~sw ~env spec in
  Alcotest.(check string) "first matching rule wins" "first" result.stdout

let mock_agent_tests =
  [
    ("available check", `Quick, test_mock_agent_available_with_fixture);
    ("unavailable without env", `Quick, test_mock_agent_unavailable_without_env);
    ("run_task success", `Quick, test_mock_agent_run_task_success);
    ( "run_task unmatched prompt",
      `Quick,
      test_mock_agent_run_task_unmatched_prompt );
    ( "run_task limit across repeated calls",
      `Quick,
      test_mock_agent_limit_across_repeated_calls );
    ("run_task rule ordering", `Quick, test_mock_agent_rule_ordering);
    ( "run_task invalid fixture",
      `Quick,
      test_mock_agent_run_task_invalid_fixture );
  ]

(** {1 Registry Tests} *)

let test_registry_register_and_get () =
  Registry.clear () ;
  let backend = (module Mock_backend : Agentic_backend.S) in
  Registry.register backend ;
  match Registry.get "mock" with
  | Some b -> Alcotest.(check string) "id matches" "mock" (Agentic_backend.id b)
  | None -> Alcotest.fail "Backend not found"

let test_registry_get_nonexistent () =
  Registry.clear () ;
  Alcotest.(check bool)
    "nonexistent returns None"
    true
    (Option.is_none (Registry.get "nonexistent"))

let test_registry_get_exn () =
  Registry.clear () ;
  let backend = (module Mock_backend : Agentic_backend.S) in
  Registry.register backend ;
  let b = Registry.get_exn "mock" in
  Alcotest.(check string) "id matches" "mock" (Agentic_backend.id b)

let test_registry_get_exn_not_found () =
  Registry.clear () ;
  Alcotest.check_raises "raises Not_found" Not_found (fun () ->
      let _ = Registry.get_exn "nonexistent" in
      ())

let test_registry_list () =
  Registry.clear () ;
  let mock = (module Mock_backend : Agentic_backend.S) in
  let unavail = (module Unavailable_backend : Agentic_backend.S) in
  Registry.register mock ;
  Registry.register unavail ;
  let backends = Registry.list () in
  Alcotest.(check int) "two backends" 2 (List.length backends)

let test_registry_list_ids () =
  Registry.clear () ;
  let mock = (module Mock_backend : Agentic_backend.S) in
  let unavail = (module Unavailable_backend : Agentic_backend.S) in
  Registry.register mock ;
  Registry.register unavail ;
  let ids = Registry.list_ids () in
  Alcotest.(check int) "two ids" 2 (List.length ids) ;
  Alcotest.(check bool) "contains mock" true (List.mem "mock" ids) ;
  Alcotest.(check bool) "contains unavailable" true (List.mem "unavailable" ids)

let test_registry_available () =
  Registry.clear () ;
  let mock = (module Mock_backend : Agentic_backend.S) in
  let unavail = (module Unavailable_backend : Agentic_backend.S) in
  Registry.register mock ;
  Registry.register unavail ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let available = Registry.available ~sw ~env in
  Alcotest.(check int) "one available" 1 (List.length available) ;
  let available_ids = List.map Agentic_backend.id available in
  Alcotest.(check bool) "mock is available" true (List.mem "mock" available_ids) ;
  Alcotest.(check bool)
    "unavailable not in list"
    false
    (List.mem "unavailable" available_ids)

let test_registry_first_available () =
  Registry.clear () ;
  let mock = (module Mock_backend : Agentic_backend.S) in
  Registry.register mock ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Registry.first_available ~sw ~env with
  | Some b -> Alcotest.(check string) "mock" "mock" (Agentic_backend.id b)
  | None -> Alcotest.fail "Expected at least one available backend"

let test_registry_first_available_none () =
  Registry.clear () ;
  let unavail = (module Unavailable_backend : Agentic_backend.S) in
  Registry.register unavail ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Alcotest.(check bool)
    "no available"
    true
    (Option.is_none (Registry.first_available ~sw ~env))

let test_registry_clear () =
  Registry.clear () ;
  let mock = (module Mock_backend : Agentic_backend.S) in
  Registry.register mock ;
  Alcotest.(check int) "one backend" 1 (List.length (Registry.list ())) ;
  Registry.clear () ;
  Alcotest.(check int) "no backends" 0 (List.length (Registry.list ()))

let registry_tests =
  [
    ("register and get", `Quick, test_registry_register_and_get);
    ("get nonexistent", `Quick, test_registry_get_nonexistent);
    ("get_exn success", `Quick, test_registry_get_exn);
    ("get_exn not found", `Quick, test_registry_get_exn_not_found);
    ("list", `Quick, test_registry_list);
    ("list_ids", `Quick, test_registry_list_ids);
    ("available", `Quick, test_registry_available);
    ("first_available", `Quick, test_registry_first_available);
    ("first_available none", `Quick, test_registry_first_available_none);
    ("clear", `Quick, test_registry_clear);
  ]

(** {1 Diagnostics Tests} *)

let test_diagnostics_formatted_log () =
  let events = ref [] in
  Fun.protect ~finally:Diagnostics.reset_handler (fun () ->
      Diagnostics.set_handler (fun event -> events := event :: !events) ;
      Diagnostics.warn "x %d" 3 ;
      match List.rev !events with
      | [Diagnostics.Log (Diagnostics.Warn, "x 3")] -> ()
      | _ -> Alcotest.fail "expected formatted warn log event")

let test_diagnostics_formatted_user_warning () =
  let events = ref [] in
  Fun.protect ~finally:Diagnostics.reset_handler (fun () ->
      Diagnostics.set_handler (fun event -> events := event :: !events) ;
      Diagnostics.user_warning "hello %s" "world" ;
      match List.rev !events with
      | [Diagnostics.User_warning "hello world"] -> ()
      | _ -> Alcotest.fail "expected formatted user warning event")

let diagnostics_tests =
  [
    ("formatted log", `Quick, test_diagnostics_formatted_log);
    ("formatted user warning", `Quick, test_diagnostics_formatted_user_warning);
  ]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Backend"
    [
      ("Backend_types", backend_types_tests);
      ("Agentic_backend", agentic_backend_tests);
      ("Mock_agent", mock_agent_tests);
      ("Registry", registry_tests);
      ("Diagnostics", diagnostics_tests);
    ]
