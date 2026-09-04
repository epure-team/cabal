(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the Copilot CLI backend. *)

open Cabal

(** {1 Module Identity Tests} *)

let test_id () = Alcotest.(check string) "id" "copilot-cli" Copilot_cli.id

let test_name () =
  Alcotest.(check string) "name" "GitHub Copilot" Copilot_cli.name

let identity_tests =
  [
    ("id is copilot-cli", `Quick, test_id);
    ("name is GitHub Copilot", `Quick, test_name);
  ]

(** {1 Availability Tests} *)

let test_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Just verify available runs without raising; check logic tested in
     test_backend_process.ml via check_available *)
  let (_ : bool) = Copilot_cli.available ~sw ~env in
  ()

let availability_tests = [("available check", `Quick, test_available)]

(** {1 Command Construction Tests} *)

let minimal_spec () : Backend_types.task_spec =
  {
    prompt = "test";
    instructions = "";
    mcp_servers = [];
    managed_namespace = Backend_types.default_managed_namespace;
    lsp_servers = [];
    working_dir = "/tmp";
    timeout = 60.0;
    expected_outputs = [];
    attachments = [];
    web_access = Backend_types.Web_disabled;
    model = None;
    resume_session_id = None;
    max_turns = None;
    read_only = false;
    json_schema = None;
  }

let valid_session_id = "123e4567-e89b-12d3-a456-426614174000"

let json_line json = Yojson.Safe.to_string ~std:true json

let event ~id ~type_ data =
  `Assoc
    [
      ("type", `String type_);
      ("id", `String id);
      ("parentId", `String "00000000-0000-0000-0000-000000000000");
      ("timestamp", `String "2026-05-24T12:00:00.000Z");
      ("data", data);
    ]

let successful_jsonl ?prior_text ?(text = "public answer") ?(output_tokens = 7) () =
  let user =
    event ~id:"10000000-0000-0000-0000-000000000001" ~type_:"user.message"
      (`Assoc
        [
          ("attachments", `List []);
          ("content", `String "private prompt");
          ("interactionId", `String "20000000-0000-0000-0000-000000000001");
        ])
  in
  let turn_start =
    event ~id:"10000000-0000-0000-0000-000000000002"
      ~type_:"assistant.turn_start"
      (`Assoc
        [
          ("interactionId", `String "20000000-0000-0000-0000-000000000001");
          ("turnId", `String "turn-1");
        ])
  in
  let assistant ~event_id ~message_id ~text ~output_tokens =
    event ~id:event_id
      ~type_:"assistant.message"
      (`Assoc
        [
          ("content", `String text);
          ("interactionId", `String "20000000-0000-0000-0000-000000000001");
          ("messageId", `String message_id);
          ("outputTokens", `Int output_tokens);
          ("toolRequests", `List []);
          ("turnId", `String "turn-1");
        ])
  in
  let turn_end =
    event ~id:"10000000-0000-0000-0000-000000000004"
      ~type_:"assistant.turn_end" (`Assoc [("turnId", `String "turn-1")])
  in
  let result =
    `Assoc
      [
        ("type", `String "result");
        ("timestamp", `String "2026-05-24T12:00:01.000Z");
        ("exitCode", `Int 0);
        ("sessionId", `String valid_session_id);
        ( "usage",
          `Assoc
            [
              ( "codeChanges",
                `Assoc
                  [
                    ("filesModified", `List []);
                    ("linesAdded", `Int 0);
                    ("linesRemoved", `Int 0);
                  ] );
              ("premiumRequests", `Float 1.0);
              ("sessionDurationMs", `Int 1000);
              ("totalApiDurationMs", `Int 500);
            ] );
      ]
  in
  let assistants =
    (match prior_text with
    | None -> []
    | Some prior_text ->
        [
          assistant ~event_id:"event-prior" ~message_id:"message-prior"
            ~text:prior_text ~output_tokens:1;
        ])
    @ [
        assistant ~event_id:"10000000-0000-0000-0000-000000000003"
          ~message_id:"30000000-0000-0000-0000-000000000001" ~text
          ~output_tokens;
      ]
  in
  String.concat "\n"
    (List.map json_line ([user; turn_start] @ assistants @ [turn_end; result]))
  ^ "\n"

let successful_tool_jsonl ?(tool_success = true) () =
  let interaction_id = "20000000-0000-0000-0000-000000000001" in
  let turn_id = "turn-1" in
  let call_id = "call-1" in
  let message id content output_tokens tool_requests =
    event ~id ~type_:"assistant.message"
      (`Assoc
        [
          ("content", `String content);
          ("interactionId", `String interaction_id);
          ("messageId", `String ("message-" ^ id));
          ("outputTokens", `Int output_tokens);
          ("toolRequests", `List tool_requests);
          ("turnId", `String turn_id);
        ])
  in
  let records =
    [
      event ~id:"event-1" ~type_:"user.message"
        (`Assoc
          [
            ("attachments", `List []);
            ("content", `String "private prompt");
            ("interactionId", `String interaction_id);
          ]);
      event ~id:"event-2" ~type_:"assistant.turn_start"
        (`Assoc
          [("interactionId", `String interaction_id); ("turnId", `String turn_id)]);
      message "1" "" 1
        [
          `Assoc
            [
              ("toolCallId", `String call_id);
              ("name", `String "view");
              ("arguments", `Assoc [("path", `String "/private/image.png")]);
            ];
        ];
      event ~id:"event-4" ~type_:"tool.execution_start"
        (`Assoc
          [
            ("turnId", `String turn_id);
            ("toolCallId", `String call_id);
            ("toolName", `String "view");
          ]);
      event ~id:"event-5" ~type_:"tool.execution_complete"
        (`Assoc
          [
            ("turnId", `String turn_id);
            ("toolCallId", `String call_id);
            ("success", `Bool tool_success);
          ]);
      message "2" "public answer" 7 [];
      event ~id:"event-7" ~type_:"assistant.turn_end"
        (`Assoc [("turnId", `String turn_id)]);
      `Assoc
        [
          ("type", `String "result");
          ("timestamp", `String "2026-05-24T12:00:01.000Z");
          ("exitCode", `Int 0);
          ("sessionId", `String valid_session_id);
          ( "usage",
            `Assoc
              [
                ( "codeChanges",
                  `Assoc
                    [
                      ("filesModified", `List []);
                      ("linesAdded", `Int 0);
                      ("linesRemoved", `Int 0);
                    ] );
                ("premiumRequests", `Float 1.0);
                ("sessionDurationMs", `Int 1000);
                ("totalApiDurationMs", `Int 500);
              ] );
        ];
    ]
  in
  String.concat "\n" (List.map json_line records) ^ "\n"

let test_build_command_allows_project_custom_instructions () =
  let cmd, _stdin =
    Copilot_cli.build_command ~mcp_config_path:None (minimal_spec ())
  in
  Alcotest.(check bool)
    "does not disable generated .github/copilot-instructions.md"
    false
    (List.mem "--no-custom-instructions" cmd)

let has_adjacent_args flag value cmd =
  let rec loop = function
    | first :: second :: _ when first = flag && second = value -> true
    | _ :: rest -> loop rest
    | [] -> false
  in
  loop cmd

let contains_substr s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let test_build_command_includes_model_flag () =
  let spec =
    {(minimal_spec ()) with Backend_types.model = Some "gpt-5.4-mini"}
  in
  let cmd, _stdin = Copilot_cli.build_command ~mcp_config_path:None spec in
  Alcotest.(check bool)
    "includes --model"
    true
    (has_adjacent_args "--model" "gpt-5.4-mini" cmd)

let test_build_command_passes_prompt_as_argument () =
  let spec =
    {
      (minimal_spec ()) with
      Backend_types.prompt = "Reply exactly: EPURE_SMOKE_OK";
      instructions = "Project instruction";
    }
  in
  let expected_prompt =
    "Reply exactly: EPURE_SMOKE_OK\n\n\
     ---\n\
     Project Instructions:\n\
     Project instruction"
  in
  let cmd, stdin = Copilot_cli.build_command ~mcp_config_path:None spec in
  Alcotest.(check bool)
    "passes prompt to -p/--prompt"
    true
    (has_adjacent_args "-p" expected_prompt cmd
    || has_adjacent_args "--prompt" expected_prompt cmd) ;
  Alcotest.(check string) "stdin is empty" "" stdin

let media_attachment ?(id = "image") ?(path = "media/source image.png")
    media_type : Backend_types.media_attachment =
  {
    id;
    path;
    media_type;
    sha256 = String.make 64 'a';
    size_bytes = 128;
  }

let test_build_invocation_uses_exact_hardened_media_argv () =
  let png = media_attachment Backend_types.Png in
  let jpeg =
    media_attachment ~id:"second" ~path:"media/second.jpg" Backend_types.Jpeg
  in
  let spec =
    {
      (minimal_spec ()) with
      prompt = "private prompt";
      instructions = "private instructions";
      model = Some "private/model";
      attachments = [png; jpeg];
    }
  in
  let first = "/sealed inputs/attachment one.png" in
  let second = "/sealed inputs/attachment-two.jpg" in
  match
    Copilot_cli.build_invocation ~config_home:"/private/config"
      ~attachment_paths:[first; second] ~mcp_config_path:None spec
  with
  | Error message -> Alcotest.fail message
  | Ok invocation ->
      Alcotest.(check (list string))
        "exact hardened argv"
        [
          "env";
          "-u";
          "COPILOT_ALLOW_ALL";
          "-u";
          "COPILOT_ALLOW_ALL_PATHS";
          "-u";
          "COPILOT_ALLOW_ALL_URLS";
          "COPILOT_HOME=/private/config";
          "NO_COLOR=1";
          "COPILOT_DISABLE_TERMINAL_TITLE=1";
          "copilot";
          "--prefer-version";
          "1.0.54";
          "--no-auto-update";
          "--no-remote";
          "--no-experimental";
          "--no-ask-user";
          "--disable-builtin-mcps";
          "--output-format";
          "json";
          "--stream";
          "off";
          "--available-tools=view,grep,glob";
          "--allow-all-tools";
          "--deny-tool=shell";
          "--deny-tool=write";
          "--deny-tool=url";
          "--deny-tool=memory";
          "--disallow-temp-dir";
          "--add-dir";
          "/sealed inputs";
          "--model";
          "private/model";
          "--attachment";
          first;
          "--attachment";
          second;
          "-p";
          "private prompt\n\n---\nProject Instructions:\nprivate instructions";
        ]
        invocation.argv;
      Alcotest.(check (option string)) "no stdin" None invocation.stdin;
      let rendered = String.concat " " invocation.redacted_argv in
      List.iter
        (fun sensitive ->
          Alcotest.(check bool)
            "redacted argv omits sensitive value" false
            (contains_substr rendered sensitive))
        [first; second; "/private/config"; "private/model"; "private prompt"];
      Alcotest.(check bool) "no yolo" false (List.mem "--yolo" invocation.argv);
      Alcotest.(check bool)
        "no broad path grant" false
        (List.mem "--allow-all-paths" invocation.argv);
      Alcotest.(check bool)
        "no broad URL grant" false
        (List.mem "--allow-all-urls" invocation.argv)

let test_build_invocation_rejects_unsupported_requests () =
  let attachment = media_attachment Backend_types.Png in
  let rejected spec ?(paths = []) ?(delivery = Backend_types.Upload_attachments)
      () =
    Result.is_error
      (Copilot_cli.build_invocation ~config_home:"/private/config"
         ~attachment_paths:paths ~attachment_delivery:delivery
         ~mcp_config_path:None spec)
  in
  Alcotest.(check bool)
    "positive web rejected" true
    (rejected
       {(minimal_spec ()) with web_access = Backend_types.Web_search}
       ());
  Alcotest.(check bool)
    "read-only rejected" true
    (rejected {(minimal_spec ()) with read_only = true} ());
  Alcotest.(check bool)
    "resume rejected" true
    (rejected
       {(minimal_spec ()) with resume_session_id = Some valid_session_id}
       ());
  Alcotest.(check bool)
    "unsealed attachment rejected" true
    (rejected {(minimal_spec ()) with attachments = [attachment]} ());
  Alcotest.(check bool)
    "attachment reuse rejected" true
    (rejected {(minimal_spec ()) with attachments = [attachment]}
       ~delivery:Backend_types.Reuse_session_attachments ()) ;
  Alcotest.(check bool)
    "relative config home rejected" true
    (Result.is_error
       (Copilot_cli.build_invocation ~config_home:"relative/config"
          ~mcp_config_path:None (minimal_spec ())))

let test_verify_terminal_stdout_extracts_exact_public_records () =
  match Copilot_cli.verify_terminal_stdout (successful_jsonl ()) with
  | Error message -> Alcotest.fail message
  | Ok terminal ->
      Alcotest.(check string) "text" "public answer" terminal.text;
      Alcotest.(check (option string))
        "session" (Some valid_session_id) terminal.session_id;
      (match terminal.cost with
      | None -> Alcotest.fail "public output token usage was discarded"
      | Some cost ->
          Alcotest.(check (option int))
            "output tokens" (Some 7) cost.tokens_output)

let test_verify_terminal_stdout_rejects_malformed_and_nonterminal_streams () =
  let private_payload = "/private/token=must-not-escape" in
  let valid = successful_jsonl () in
  let cases =
    [
      "not-json " ^ private_payload;
      "";
      valid ^ valid;
      valid ^ json_line (`Assoc [("type", `String "after.result")]);
      String.concat "\n"
        [
          json_line
            (`Assoc
              [
                ("type", `String "session.error");
                ("message", `String private_payload);
              ]);
          valid;
        ];
    ]
  in
  List.iter
    (fun stream ->
      match Copilot_cli.verify_terminal_stdout stream with
      | Ok _ -> Alcotest.fail "invalid Copilot stream was accepted"
      | Error message ->
          Alcotest.(check bool)
            "sanitized parser error" false
            (contains_substr message private_payload))
    cases

let test_normalized_events_require_paired_successful_tools () =
  let private_path = "/private/image.png" in
  (match Copilot_cli.normalized_events_of_stdout (successful_tool_jsonl ()) with
  | Error message -> Alcotest.fail message
  | Ok events ->
      Alcotest.(check bool)
        "paired tool start" true
        (List.exists
           (function
             | Task_event.Tool_started {id = Some "call-1"; name = "view"} ->
                 true
             | _ -> false)
           events) ;
      Alcotest.(check bool)
        "paired tool finish" true
        (List.exists
           (function
             | Task_event.Tool_finished
                 {id = Some "call-1"; name = Some "view"} ->
                 true
             | _ -> false)
           events) ;
      Alcotest.(check bool)
        "public agent text" true
        (List.exists
           (function Task_event.Agent_text_delta "public answer" -> true | _ -> false)
           events)) ;
  match
    Copilot_cli.verify_terminal_stdout
      (successful_tool_jsonl ~tool_success:false ())
  with
  | Ok _ -> Alcotest.fail "failed tool execution was accepted"
  | Error message ->
      Alcotest.(check bool)
        "failed tool diagnostics omit arguments" false
        (contains_substr message private_path)

let test_sensitive_request_rejection_precedes_project_writes () =
  let workspace = Filename.temp_dir "cabal-copilot-reject-" "" in
  Fun.protect
    ~finally:(fun () -> Unix.rmdir workspace)
    (fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let attachment = media_attachment Backend_types.Png in
      let result =
        Copilot_cli.run_task ~sw ~env
          {(minimal_spec ()) with working_dir = workspace; attachments = [attachment]}
      in
      (match result.status with
      | Backend_types.Failed message ->
          Alcotest.(check bool)
            "central authorization error" true
            (contains_substr message "authorization")
      | _ -> Alcotest.fail "unsealed media request did not fail") ;
      Alcotest.(check bool)
        "no project config mutation" false
        (Sys.file_exists (Filename.concat workspace ".github")))

let rec ensure_dir path =
  if path <> "." && path <> Filename.dirname path then begin
    ensure_dir (Filename.dirname path) ;
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path content =
  ensure_dir (Filename.dirname path) ;
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let with_tmpdir f =
  let dir = Filename.temp_dir "epure_test_copilot_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value ;
  Fun.protect
    ~finally:(fun () -> Unix.putenv name (Option.value previous ~default:""))
    f

let test_run_task_verifies_protocol_and_removes_isolated_config () =
  Process_test_helper.install_launcher () ;
  with_tmpdir @@ fun project_dir ->
  let executable = Filename.concat project_dir "copilot" in
  let marker = Filename.concat project_dir "config-home.marker" in
  let stream = successful_jsonl ~prior_text:"discarded draft" () in
  write_file executable
    ("#!/bin/sh\n"
    ^ "printf '%s' \"$COPILOT_HOME\" > \"$COPILOT_TEST_MARKER\"\n"
    ^ "printf '%s' " ^ Filename.quote stream ^ "\n") ;
  Unix.chmod executable 0o700 ;
  let path = project_dir ^ ":" ^ Option.value (Sys.getenv_opt "PATH") ~default:"" in
  with_env "PATH" path @@ fun () ->
  with_env "COPILOT_TEST_MARKER" marker @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let callbacks = ref [] in
  let result =
    Copilot_cli.run_task ~sw ~env
      ~on_raw_line:(fun line -> callbacks := line :: !callbacks)
      {(minimal_spec ()) with working_dir = project_dir}
  in
  Alcotest.(check bool) "successful verified result" true
    (result.status = Backend_types.Success) ;
  Alcotest.(check string) "public text" "public answer" result.agent_text ;
  Alcotest.(check string) "raw stdout withheld" "" result.stdout ;
  Alcotest.(check string) "raw stderr withheld" "" result.stderr ;
  Alcotest.(check int) "only verified public callback lines" 2
    (List.length !callbacks) ;
  let config_home =
    let channel = open_in_bin marker in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  Alcotest.(check bool) "isolated config removed" false
    (Sys.file_exists config_home) ;
  let callback_text = String.concat "\n" !callbacks in
  Alcotest.(check bool)
    "callback withholds superseded assistant text" false
    (contains_substr callback_text "discarded draft") ;
  List.iter
    (fun private_value ->
      Alcotest.(check bool)
        "callback omits private input" false
        (contains_substr callback_text private_value))
    ["private prompt"; config_home]

let test_project_config_validation_prioritizes_invalid_generated_config () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun project_dir ->
      write_file
        (Filename.concat project_dir ".github/copilot-instructions.md")
        "user-owned instructions\n" ;
      let setup_result =
        Backend_config_writer.setup_artifacts
          ~project_dir
          ~force:false
          (Copilot_cli.project_config_artifacts
             ~managed_namespace:Backend_types.default_managed_namespace
             ~mcp_servers:[]
             ~lsp_servers:[])
      in
      write_file
        (Filename.concat project_dir ".github/mcp.json")
        "{ invalid json\n" ;
      match
        Copilot_cli.check_project_config ~sw ~env ~project_dir ~setup_result
      with
      | Agentic_backend.Config_invalid msg ->
          Alcotest.(check bool)
            "reports invalid generated MCP config"
            true
            (String.starts_with
               ~prefix:"Copilot MCP config is not strict JSON:"
               msg)
      | Agentic_backend.Config_check_unsupported reason ->
          Alcotest.failf
            "unsupported user artifact masked invalid generated MCP config: %s"
            reason
      | Agentic_backend.Config_valid ->
          Alcotest.fail "invalid generated MCP config should fail validation")

let test_project_config_validation_accepts_custom_namespace_marker () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun project_dir ->
      let managed_namespace : Backend_types.managed_namespace =
        {
          id = "crucible";
          display_name = "Crucible";
          config_dir = ".crucible/backend-config";
        }
      in
      let setup_result =
        Backend_config_writer.setup_artifacts
          ~project_dir
          ~force:false
          (Copilot_cli.project_config_artifacts
             ~managed_namespace
             ~mcp_servers:[]
             ~lsp_servers:[])
      in
      let instructions =
        let ic =
          open_in_bin
            (Filename.concat project_dir ".github/copilot-instructions.md")
        in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> really_input_string ic (in_channel_length ic))
      in
      Alcotest.(check bool)
        "generated custom marker exists"
        true
        (contains_substr instructions "crucible-managed") ;
      match
        Copilot_cli.check_project_config ~sw ~env ~project_dir ~setup_result
      with
      | Agentic_backend.Config_valid -> ()
      | Agentic_backend.Config_check_unsupported reason ->
          Alcotest.failf
            "expected custom namespace config to validate: %s"
            reason
      | Agentic_backend.Config_invalid msg ->
          Alcotest.failf "expected custom namespace config to validate: %s" msg)

let test_run_task_rejects_invalid_namespace_before_write () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun project_dir ->
      let invalid_namespace : Backend_types.managed_namespace =
        {
          id = "../bad";
          display_name = "Bad";
          config_dir = ".epure/backend-config";
        }
      in
      let spec =
        {
          (Backend_types.make_task_spec
             ~prompt:"test"
             ~working_dir:project_dir
             ())
          with
          Backend_types.managed_namespace = invalid_namespace;
        }
      in
      let result = Copilot_cli.run_task ~sw ~env spec in
      (match result.Backend_types.status with
      | Backend_types.Failed msg ->
          Alcotest.(check bool)
            "invalid namespace failure"
            true
            (String.length msg > 0)
      | _ -> Alcotest.fail "expected Failed status for invalid namespace") ;
      Alcotest.(check bool)
        "no project config directory written"
        false
        (Sys.file_exists (Filename.concat project_dir ".github")))

let command_tests =
  [
    ( "build_command allows project custom instructions",
      `Quick,
      test_build_command_allows_project_custom_instructions );
    ( "build_command includes model flag",
      `Quick,
      test_build_command_includes_model_flag );
    ( "build_command passes prompt as argument",
      `Quick,
      test_build_command_passes_prompt_as_argument );
    ( "hardened invocation uses sealed repeated attachments",
      `Quick,
      test_build_invocation_uses_exact_hardened_media_argv );
    ( "hardened invocation rejects unsupported requests",
      `Quick,
      test_build_invocation_rejects_unsupported_requests );
    ( "terminal JSONL extracts exact public records",
      `Quick,
      test_verify_terminal_stdout_extracts_exact_public_records );
    ( "malformed and nonterminal JSONL fail sanitized",
      `Quick,
      test_verify_terminal_stdout_rejects_malformed_and_nonterminal_streams );
    ( "normalized events require paired successful tools",
      `Quick,
      test_normalized_events_require_paired_successful_tools );
    ( "sensitive request rejection precedes project writes",
      `Quick,
      test_sensitive_request_rejection_precedes_project_writes );
    ( "run_task verifies protocol and removes isolated config",
      `Quick,
      test_run_task_verifies_protocol_and_removes_isolated_config );
    ( "project config validation prioritizes invalid generated config",
      `Quick,
      test_project_config_validation_prioritizes_invalid_generated_config );
    ( "project config validation accepts custom namespace marker",
      `Quick,
      test_project_config_validation_accepts_custom_namespace_marker );
    ( "run_task rejects invalid namespace before write",
      `Quick,
      test_run_task_rejects_invalid_namespace_before_write );
  ]

(** {1 Backend Interface Compliance Tests} *)

let test_implements_agentic_backend () =
  let backend = (module Copilot_cli : Agentic_backend.S) in
  Alcotest.(check string)
    "id via interface"
    "copilot-cli"
    (Agentic_backend.id backend) ;
  Alcotest.(check string)
    "name via interface"
    "GitHub Copilot"
    (Agentic_backend.name backend)

let interface_tests =
  [("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend)]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Copilot_cli"
    [
      ("Identity", identity_tests);
      ("Availability", availability_tests);
      ("Command", command_tests);
      ("Interface", interface_tests);
    ]
