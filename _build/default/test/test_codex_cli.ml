(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the Codex CLI backend. *)

open Cabal

(** {1 Module Identity Tests} *)

let test_id () = Alcotest.(check string) "id" "codex" Codex_cli.id

let test_name () = Alcotest.(check string) "name" "OpenAI Codex" Codex_cli.name

let identity_tests =
  [
    ("id is codex", `Quick, test_id); ("name is OpenAI Codex", `Quick, test_name);
  ]

(** {1 JSONL Output Parsing Tests} *)

let test_parse_jsonl_with_message_and_usage () =
  let input =
    {|{"type":"thread.started","thread_id":"abc"}
{"type":"item.completed","item":{"type":"agent_message","text":"First message"}}
{"type":"item.completed","item":{"type":"agent_message","text":"Final result"}}
{"type":"turn.completed","usage":{"input_tokens":200,"output_tokens":75}}|}
  in
  let text, cost = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string) "last message" "Final result" text ;
  Alcotest.(check bool) "cost present" true (Option.is_some cost) ;
  match cost with
  | Some c ->
      Alcotest.(check (option int)) "input_tokens" (Some 200) c.tokens_input ;
      Alcotest.(check (option int)) "output_tokens" (Some 75) c.tokens_output
  | None -> Alcotest.fail "Expected cost to be Some"

let test_parse_jsonl_no_usage () =
  let input =
    {|{"type":"item.completed","item":{"type":"agent_message","text":"Hello world"}}|}
  in
  let text, cost = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string) "message text" "Hello world" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_jsonl_empty () =
  let text, cost = Codex_cli.parse_jsonl_output "" in
  (* Falls back to raw stdout *)
  Alcotest.(check string) "empty fallback" "" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_jsonl_malformed () =
  let input = "not json at all\n{invalid" in
  let text, cost = Codex_cli.parse_jsonl_output input in
  (* Falls back to raw stdout *)
  Alcotest.(check string) "malformed fallback" input text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let jsonl_output_tests =
  [
    ( "parse with message and usage",
      `Quick,
      test_parse_jsonl_with_message_and_usage );
    ("parse without usage", `Quick, test_parse_jsonl_no_usage);
    ("parse empty output", `Quick, test_parse_jsonl_empty);
    ("parse malformed output", `Quick, test_parse_jsonl_malformed);
  ]

(** {1 Command Construction Tests} *)

let sample_output_schema : Yojson.Safe.t =
  `Assoc
    [
      ("$schema", `String "https://json-schema.org/draft/2020-12/schema");
      ("type", `String "object");
      ("properties", `Assoc [("answer", `Assoc [("type", `String "string")])]);
      ("required", `List [`String "answer"]);
      ("additionalProperties", `Bool false);
    ]

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let remove_if_exists path =
  try if Sys.file_exists path then Sys.remove path with _ -> ()

let rec find_flag_value_index flag index = function
  | candidate :: value :: _ when String.equal candidate flag ->
      Some (index, value)
  | _ :: rest -> find_flag_value_index flag (index + 1) rest
  | [] -> None

let rec command_contains value = function
  | [] -> false
  | candidate :: rest ->
      String.equal candidate value || command_contains value rest

let rec command_last = function
  | [] -> Alcotest.fail "empty command"
  | [last] -> last
  | _ :: rest -> command_last rest

let assert_schema_file_wiring ?resume_session_id () =
  let spec =
    Backend_types.make_task_spec
      ?resume_session_id
      ~prompt:"Return a JSON object."
      ~working_dir:"/tmp/test"
      ~json_schema:sample_output_schema
      ()
  in
  let cmd, _stdin = Codex_cli.build_command ~mcp_config_path:None spec in
  (match resume_session_id with
  | Some sid ->
      Alcotest.(check bool)
        "resume command includes session id"
        true
        (command_contains sid cmd)
  | None ->
      Alcotest.(check bool)
        "normal command does not resume"
        false
        (command_contains "resume" cmd)) ;
  Alcotest.(check string) "trailing stdin marker" "-" (command_last cmd) ;
  match find_flag_value_index "--output-schema" 0 cmd with
  | None -> Alcotest.fail "--output-schema not found in command"
  | Some (schema_flag_index, schema_path) ->
      Fun.protect
        ~finally:(fun () -> remove_if_exists schema_path)
        (fun () ->
          let expected_json =
            Yojson.Safe.to_string ~std:true sample_output_schema
          in
          Alcotest.(check bool)
            "schema value is not inline JSON"
            false
            (String.equal expected_json schema_path) ;
          Alcotest.(check bool)
            "schema path exists"
            true
            (Sys.file_exists schema_path) ;
          Alcotest.(check string)
            "schema file contents"
            expected_json
            (read_file schema_path) ;
          Alcotest.(check bool)
            "schema flag precedes stdin marker"
            true
            (schema_flag_index < List.length cmd - 1))

let test_build_command_with_output_schema_normal () =
  assert_schema_file_wiring ()

let test_build_command_with_output_schema_resume () =
  assert_schema_file_wiring ~resume_session_id:"sess-abc-123" ()

let test_build_command_without_output_schema () =
  let specs =
    [
      Backend_types.make_task_spec
        ~prompt:"No schema"
        ~working_dir:"/tmp/test"
        ();
      Backend_types.make_task_spec
        ~prompt:"No schema resume"
        ~working_dir:"/tmp/test"
        ~resume_session_id:"sess-no-schema"
        ();
    ]
  in
  List.iter
    (fun spec ->
      let cmd, _stdin = Codex_cli.build_command ~mcp_config_path:None spec in
      Alcotest.(check bool)
        "no --output-schema flag"
        false
        (command_contains "--output-schema" cmd))
    specs

let command_construction_tests =
  [
    ( "build_command with output schema",
      `Quick,
      test_build_command_with_output_schema_normal );
    ( "build_command resume with output schema",
      `Quick,
      test_build_command_with_output_schema_resume );
    ( "build_command without output schema",
      `Quick,
      test_build_command_without_output_schema );
  ]

(** {1 Availability Tests} *)

let test_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Just verify available runs without raising; check logic tested in
     test_backend_process.ml via check_available *)
  let (_ : bool) = Codex_cli.available ~sw ~env in
  ()

let availability_tests = [("available check", `Quick, test_available)]

(** {1 E2E Harness Model Contract Tests} *)

let project_root () =
  match Sys.getenv_opt "PROJECT_ROOT" with
  | Some r -> r
  | None -> (
      let rec find dir =
        if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
        else
          let parent = Filename.dirname dir in
          if parent = dir then None else find parent
      in
      let rec up_past_build dir =
        let base = Filename.basename dir in
        let parent = Filename.dirname dir in
        if parent = dir then None
        else if base = "_build" then Some parent
        else up_past_build parent
      in
      let resolve dir =
        if not (List.mem "_build" (String.split_on_char '/' dir)) then Some dir
        else
          match up_past_build dir with
          | Some src when Sys.file_exists (Filename.concat src "dune-project")
            ->
              Some src
          | _ -> None
      in
      let starts =
        [
          Filename.dirname __FILE__;
          Sys.getcwd ();
          Filename.dirname (Sys.getcwd ());
        ]
      in
      match
        List.find_map
          (fun start ->
            match find start with None -> None | Some d -> resolve d)
          starts
      with
      | Some dir -> dir
      | None -> Sys.getcwd ())

let contains_substring haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  if nlen = 0 then true
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

let e2e_harness_source rel_path standalone_path =
  let root = project_root () in
  let candidates =
    [Filename.concat root rel_path; Filename.concat root standalone_path]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> read_file path
  | None -> Alcotest.fail (rel_path ^ " not found")

let e2e_config_source () =
  e2e_harness_source
    "libs/cabal/test/e2e_harness_config.ml"
    "test/e2e_harness_config.ml"

let demo_627_source () =
  e2e_harness_source "libs/cabal/test/test_demo_627.ml" "test/test_demo_627.ml"

let native_e2e_source () =
  e2e_harness_source
    "libs/cabal/test/test_native_json_schema_backends.ml"
    "test/test_native_json_schema_backends.ml"

let test_e2e_harness_removes_shared_model_env_var () =
  List.iter
    (fun (label, source) ->
      Alcotest.(check bool)
        (label ^ " does not read shared CABAL_E2E_MODEL")
        false
        (contains_substring source "Sys.getenv_opt \"CABAL_E2E_MODEL\"") ;
      Alcotest.(check bool)
        (label ^ " does not skip on shared CABAL_E2E_MODEL")
        false
        (contains_substring source "SKIPPED: CABAL_E2E_MODEL"))
    [
      ("test_demo_627", demo_627_source ()); ("native E2E", native_e2e_source ());
    ]

let test_e2e_harness_declares_per_backend_model_env_vars () =
  let source = e2e_config_source () in
  List.iter
    (fun expected ->
      Alcotest.(check bool)
        ("per-backend model env var declared: " ^ expected)
        true
        (contains_substring source expected))
    [
      "CABAL_E2E_MODEL_CLAUDE_CODE";
      "CABAL_E2E_MODEL_CODEX";
      "CABAL_E2E_MODEL_COPILOT_CLI";
      "CABAL_E2E_MODEL_OPENCODE";
      "CABAL_E2E_MODEL_GEMINI_CLI";
    ]

let test_e2e_harness_declares_backend_specific_defaults () =
  let source = e2e_config_source () in
  List.iter
    (fun expected ->
      Alcotest.(check bool)
        ("backend default declared: " ^ expected)
        true
        (contains_substring source expected))
    [
      "| \"claude-code\" -> Some \"haiku\"";
      "| \"codex\" -> None";
      "| \"copilot-cli\" -> Some \"claude-haiku-4.5\"";
      "| \"opencode\" -> Some \"openai/gpt-5.4-mini\"";
      "| \"gemini-cli\" -> Some \"gemini-3-flash-preview\"";
    ]

let test_demo_627_defaults_to_multi_backend_run () =
  let source = demo_627_source () in
  Alcotest.(check bool)
    "optional backend filter is allowed"
    true
    (contains_substring source "CABAL_E2E_BACKEND") ;
  Alcotest.(check bool)
    "missing backend filter is no longer a skip"
    false
    (contains_substring source "SKIPPED: CABAL_E2E_BACKEND not set") ;
  Alcotest.(check bool)
    "all_backend_ids drives default run"
    true
    (contains_substring source "all_backend_ids")

let test_e2e_harness_uses_test_managed_namespace () =
  let ns = E2e_harness_config.managed_namespace in
  Alcotest.(check string) "namespace id" "cabal-tests" ns.id ;
  Alcotest.(check string) "namespace display" "Cabal tests" ns.display_name ;
  Alcotest.(check string)
    "namespace config dir"
    ".cabal-tests/backend-config"
    ns.config_dir ;
  List.iter
    (fun (label, source) ->
      Alcotest.(check bool)
        (label ^ " threads test managed namespace")
        true
        (contains_substring
           source
           "~managed_namespace:E2e_harness_config.managed_namespace"))
    [
      ("test_demo_627", demo_627_source ()); ("native E2E", native_e2e_source ());
    ]

let e2e_harness_model_contract_tests =
  [
    ( "E2E harness removes shared model env var",
      `Quick,
      test_e2e_harness_removes_shared_model_env_var );
    ( "E2E harness declares per-backend model env vars",
      `Quick,
      test_e2e_harness_declares_per_backend_model_env_vars );
    ( "E2E harness declares backend-specific defaults",
      `Quick,
      test_e2e_harness_declares_backend_specific_defaults );
    ( "test_demo_627 defaults to multi-backend run",
      `Quick,
      test_demo_627_defaults_to_multi_backend_run );
    ( "E2E harness uses test managed namespace",
      `Quick,
      test_e2e_harness_uses_test_managed_namespace );
  ]

(** {1 Backend Interface Compliance Tests} *)

let test_implements_agentic_backend () =
  let backend = (module Codex_cli : Agentic_backend.S) in
  Alcotest.(check string)
    "id via interface"
    "codex"
    (Agentic_backend.id backend) ;
  Alcotest.(check string)
    "name via interface"
    "OpenAI Codex"
    (Agentic_backend.name backend)

let interface_tests =
  [("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend)]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Codex_cli"
    [
      ("Identity", identity_tests);
      ("JSONL Output", jsonl_output_tests);
      ("Command Construction", command_construction_tests);
      ("Availability", availability_tests);
      ("E2E Harness Model Contract", e2e_harness_model_contract_tests);
      ("Interface", interface_tests);
    ]
