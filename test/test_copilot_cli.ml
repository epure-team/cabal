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

let jsonl_records stream =
  stream |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         if String.trim line = "" then None
         else Some (Yojson.Safe.from_string line))

let jsonl_of_records records =
  String.concat "\n" (List.map json_line records) ^ "\n"

let map_record_data record_type transform = function
  | `Assoc fields as record -> (
      match List.assoc_opt "type" fields with
      | Some (`String value) when value = record_type -> (
          match List.assoc_opt "data" fields with
          | Some (`Assoc data) ->
              `Assoc
                (List.map
                   (fun (name, value) ->
                     if name = "data" then (name, `Assoc (transform data))
                     else (name, value))
                   fields)
          | _ -> record)
      | _ -> record)
  | record -> record

let replace_field name replacement fields =
  List.map
    (fun (field_name, value) ->
      if field_name = name then (field_name, replacement) else (field_name, value))
    fields

let replace_tool_name name stream =
  let replace_request = function
    | `Assoc fields -> `Assoc (replace_field "name" (`String name) fields)
    | value -> value
  in
  jsonl_records stream
  |> List.map (fun record ->
         record
         |> map_record_data "assistant.message" (fun fields ->
                List.map
                  (fun (field_name, value) ->
                    if field_name = "toolRequests" then
                      match value with
                      | `List requests ->
                          (field_name, `List (List.map replace_request requests))
                      | _ -> (field_name, value)
                    else (field_name, value))
                  fields)
         |> map_record_data "tool.execution_start" (fun fields ->
                replace_field "toolName" (`String name) fields))
  |> jsonl_of_records

let expect_protocol_rejection label stream =
  match Copilot_cli.Private.verify_terminal_stdout stream with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (label ^ " protocol stream was accepted")

let test_build_command_allows_project_custom_instructions () =
  let cmd, _stdin =
    Copilot_cli.Private.build_command ~mcp_config_path:None (minimal_spec ())
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
  let cmd, _stdin = Copilot_cli.Private.build_command ~mcp_config_path:None spec in
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
  let cmd, stdin = Copilot_cli.Private.build_command ~mcp_config_path:None spec in
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
    Copilot_cli.Private.build_invocation ~config_home:"/private/config"
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
      (Copilot_cli.Private.build_invocation ~config_home:"/private/config"
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
       (Copilot_cli.Private.build_invocation ~config_home:"relative/config"
          ~mcp_config_path:None (minimal_spec ())))

let test_verify_terminal_stdout_extracts_exact_public_records () =
  match Copilot_cli.Private.verify_terminal_stdout (successful_jsonl ()) with
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
      match Copilot_cli.Private.verify_terminal_stdout stream with
      | Ok _ -> Alcotest.fail "invalid Copilot stream was accepted"
      | Error message ->
          Alcotest.(check bool)
            "sanitized parser error" false
            (contains_substr message private_payload))
    cases

let test_normalized_events_require_paired_successful_tools () =
  let private_path = "/private/image.png" in
  (match
     Copilot_cli.Private.normalized_events_of_stdout (successful_tool_jsonl ())
   with
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
    Copilot_cli.Private.verify_terminal_stdout
      (successful_tool_jsonl ~tool_success:false ())
  with
  | Ok _ -> Alcotest.fail "failed tool execution was accepted"
  | Error message ->
      Alcotest.(check bool)
        "failed tool diagnostics omit arguments" false
        (contains_substr message private_path)

let test_protocol_tool_allowlist_and_turn_ownership () =
  List.iter
    (fun allowed ->
      match
        Copilot_cli.Private.verify_terminal_stdout
          (replace_tool_name allowed (successful_tool_jsonl ()))
      with
      | Ok _ -> ()
      | Error message -> Alcotest.fail message)
    ["view"; "grep"; "glob"] ;
  List.iter
    (fun forbidden ->
      expect_protocol_rejection ("forbidden " ^ forbidden)
        (replace_tool_name forbidden (successful_tool_jsonl ())))
    ["shell"; "write"; "url"; "web_fetch"] ;
  let records = jsonl_records (successful_tool_jsonl ()) in
  let cross_turn =
    match records with
    | [user; turn_one; request; start; complete; answer; turn_end; result] ->
        let interaction_id = "20000000-0000-0000-0000-000000000001" in
        let turn_two =
          event ~id:"event-turn-two" ~type_:"assistant.turn_start"
            (`Assoc
              [
                ("interactionId", `String interaction_id);
                ("turnId", `String "turn-2");
              ])
        in
        let first_end =
          event ~id:"event-turn-one-end" ~type_:"assistant.turn_end"
            (`Assoc [("turnId", `String "turn-1")])
        in
        let move_to_turn_two record =
          record
          |> map_record_data "tool.execution_start" (fun fields ->
                 replace_field "turnId" (`String "turn-2") fields)
          |> map_record_data "tool.execution_complete" (fun fields ->
                 replace_field "turnId" (`String "turn-2") fields)
          |> map_record_data "assistant.message" (fun fields ->
                 replace_field "turnId" (`String "turn-2") fields)
          |> map_record_data "assistant.turn_end" (fun fields ->
                 replace_field "turnId" (`String "turn-2") fields)
        in
        jsonl_of_records
          [
            user;
            turn_one;
            request;
            first_end;
            turn_two;
            move_to_turn_two start;
            move_to_turn_two complete;
            move_to_turn_two answer;
            move_to_turn_two turn_end;
            result;
          ]
    | _ -> Alcotest.fail "unexpected synthetic Copilot stream shape"
  in
  expect_protocol_rejection "cross-turn tool" cross_turn ;
  let outstanding =
    records
    |> List.filter (function
         | `Assoc fields -> (
            match List.assoc_opt "type" fields with
            | Some
                (`String
                  ("tool.execution_start" | "tool.execution_complete")) ->
                 false
             | _ -> true)
         | _ -> true)
    |> jsonl_of_records
  in
  match Copilot_cli.Private.verify_terminal_stdout outstanding with
  | Error message ->
      Alcotest.(check bool)
        "outstanding request rejected at turn end" true
        (contains_substr message "turn end")
  | Ok _ -> Alcotest.fail "turn ended with an outstanding tool"

let test_protocol_rejects_mcp_updates_and_workspace_changes () =
  let base = jsonl_records (successful_jsonl ()) in
  let prepend type_ data records =
    event ~id:("event-" ^ type_) ~type_ data :: records |> jsonl_of_records
  in
  expect_protocol_rejection "nonempty MCP discovery"
    (prepend "session.mcp_servers_loaded"
       (`Assoc [("servers", `List [`Assoc [("name", `String "hostile")]])])
       base) ;
  expect_protocol_rejection "effective forbidden tool update"
    (prepend "session.tools_updated"
       (`Assoc
         [
           ("model", `String "model");
           ("tools", `List [`String "shell"]);
         ])
       base) ;
  let changed_usage =
    List.map
      (function
        | `Assoc fields as record -> (
            match List.assoc_opt "type" fields with
            | Some (`String "result") ->
                let changed =
                  `Assoc
                    [
                      ("filesModified", `List [`String "/private/path"]);
                      ("linesAdded", `Int 1);
                      ("linesRemoved", `Int 0);
                    ]
                in
                `Assoc
                  (List.map
                     (fun (name, value) ->
                       if name = "usage" then
                         match value with
                         | `Assoc usage ->
                             (name, `Assoc (replace_field "codeChanges" changed usage))
                         | _ -> (name, value)
                       else (name, value))
                     fields)
            | _ -> record)
        | record -> record)
      base
    |> jsonl_of_records
  in
  expect_protocol_rejection "workspace changes in terminal usage" changed_usage

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

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

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

let test_project_mcp_config_rejected_before_spawn () =
  List.iter
    (fun project_path ->
      with_tmpdir @@ fun project_dir ->
      let executable = Filename.concat project_dir "copilot" in
      let marker = Filename.concat project_dir "spawned.marker" in
      let mcp_path = Filename.concat project_dir project_path in
      let hostile =
        {|{"mcpServers":{"hostile":{"type":"local","command":"must-not-run","args":[],"env":{},"tools":["*"]}}}|}
      in
      write_file mcp_path hostile ;
      write_file executable
        ("#!/bin/sh\n"
        ^ "printf 'spawned' > \"$COPILOT_TEST_MARKER\"\n"
        ^ "printf '%s' " ^ Filename.quote (successful_jsonl ()) ^ "\n") ;
      Unix.chmod executable 0o700 ;
      let path =
        project_dir ^ ":" ^ Option.value (Sys.getenv_opt "PATH") ~default:""
      in
      with_env "PATH" path @@ fun () ->
      with_env "COPILOT_TEST_MARKER" marker @@ fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let result =
        Copilot_cli.run_task ~sw ~env
          {(minimal_spec ()) with working_dir = project_dir}
      in
      Alcotest.(check bool)
        (project_path ^ " rejected") true
        (match result.status with Backend_types.Failed _ -> true | _ -> false) ;
      Alcotest.(check bool)
        (project_path ^ " starts no CLI") false (Sys.file_exists marker) ;
      Alcotest.(check string)
        (project_path ^ " user content unchanged") hostile (read_file mcp_path))
    [".mcp.json"; ".github/mcp.json"]

let test_isolated_config_cleanup_is_structural_for_all_results () =
  let statuses =
    [
      Backend_types.Success;
      Backend_types.Failed "backend failed";
      Backend_types.Timeout;
      Backend_types.Cancelled;
    ]
  in
  Eio_posix.run @@ fun _env ->
  List.iter
    (fun status ->
      let observed = ref None in
      let result =
        Copilot_cli.Private.with_isolated_config_home_for_test (fun directory ->
            observed := Some directory ;
            write_file (Filename.concat directory "logs/session.log")
              "private prompt and session" ;
            Backend_types.make_task_result ~status ())
      in
      Alcotest.(check bool)
        "successful cleanup preserves task status" true (result.status = status) ;
      match !observed with
      | Some directory ->
          Alcotest.(check bool)
            "successful cleanup removes prompt/session/log residue" false
            (Sys.file_exists directory)
      | None -> Alcotest.fail "cleanup test observed no private directory")
    statuses ;
  List.iter
    (fun status ->
      let attempts = ref 0 in
      let leaked = ref None in
      let result =
        Copilot_cli.Private.with_isolated_config_home_for_test
          ~on_cleanup_attempt:(fun () ->
            incr attempts ;
            raise (Sys_error "private cleanup fault"))
          (fun directory ->
            leaked := Some directory ;
            write_file (Filename.concat directory "logs/session.log")
              "private prompt and session" ;
            Backend_types.make_task_result ~status ())
      in
      Alcotest.(check int)
        "cleanup failure retries are bounded"
        Copilot_cli.Private.cleanup_retry_limit !attempts ;
      Alcotest.(check bool)
        "cleanup failure structurally fails every terminal status" true
        (match result.status with
        | Backend_types.Failed message ->
            message = "Copilot config isolation cleanup failed"
        | _ -> false) ;
      Option.iter
        (fun directory ->
          ignore (Sys.command ("rm -rf " ^ Filename.quote directory)))
        !leaked)
    statuses ;
  let interrupted_home = ref None in
  let interruption_propagated =
    try
      ignore
        (Copilot_cli.Private.with_isolated_config_home_for_test
           (fun directory ->
             interrupted_home := Some directory ;
             write_file (Filename.concat directory "logs/session.log")
               "private prompt and session" ;
             raise (Eio.Cancel.Cancelled (Failure "private interruption")))) ;
      false
    with Eio.Cancel.Cancelled _ -> true
  in
  Alcotest.(check bool)
    "interruption is propagated after protected cleanup" true
    interruption_propagated ;
  Option.iter
    (fun directory ->
      Alcotest.(check bool)
        "interruption leaves no prompt/session/log residue" false
        (Sys.file_exists directory))
    !interrupted_home

let test_run_task_fails_before_spawn_without_complete_mcp_isolation () =
  Process_test_helper.install_launcher () ;
  with_tmpdir @@ fun project_dir ->
  let executable = Filename.concat project_dir "copilot" in
  let marker = Filename.concat project_dir "spawn.marker" in
  write_file executable
    ("#!/bin/sh\n"
    ^ "printf spawned > " ^ Filename.quote marker ^ "\n"
    ^ "exit 91\n") ;
  Unix.chmod executable 0o700 ;
  let path = project_dir ^ ":" ^ Option.value (Sys.getenv_opt "PATH") ~default:"" in
  with_env "PATH" path @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let callbacks = ref [] in
  let result =
    Copilot_cli.run_task ~sw ~env
      ~on_raw_line:(fun line -> callbacks := line :: !callbacks)
      {(minimal_spec ()) with working_dir = project_dir}
  in
  (match result.status with
  | Backend_types.Failed message ->
      Alcotest.(check string)
        "fixed MCP isolation failure"
        "Copilot invocation rejected: Copilot CLI 1.0.54 cannot disable all MCP discovery"
        message
  | _ -> Alcotest.fail "expected structural MCP isolation failure") ;
  Alcotest.(check bool) "backend process was not spawned" false
    (Sys.file_exists marker) ;
  Alcotest.(check bool) "project config was not written" false
    (Sys.file_exists (Filename.concat project_dir ".github")) ;
  Alcotest.(check string) "raw stdout withheld" "" result.stdout ;
  Alcotest.(check int) "no callback was released" 0 (List.length !callbacks)

let test_project_config_generation_omits_mcp_artifact () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun project_dir ->
      let artifacts =
        Copilot_cli.project_config_artifacts
          ~managed_namespace:Backend_types.default_managed_namespace
          ~mcp_servers:
            [
              {
                Backend_types.name = "must-not-render";
                command = "must-not-run";
                args = [];
                env = [];
              };
            ]
          ~lsp_servers:[]
      in
      Alcotest.(check bool)
        "Copilot provider emits no project MCP artifact" false
        (List.exists
           (fun artifact ->
             artifact.Backend_config_writer.project_relative_path
             = ".github/mcp.json")
           artifacts) ;
      let setup_result =
        Backend_config_writer.setup_artifacts
          ~project_dir ~force:false artifacts
      in
      match
        Copilot_cli.check_project_config ~sw ~env ~project_dir ~setup_result
      with
      | Agentic_backend.Config_valid -> ()
      | Agentic_backend.Config_check_unsupported reason
      | Agentic_backend.Config_invalid reason -> Alcotest.fail reason)

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

let test_media_web_probe_offline_self_test () =
  let relative = "tools/probe_copilot_media_web.py" in
  let candidates =
    [
      relative;
      Filename.concat "../../.." relative;
      Filename.concat (Filename.dirname __FILE__) ("../" ^ relative);
    ]
  in
  let path =
    match List.find_opt Sys.file_exists candidates with
    | Some path -> path
    | None -> Alcotest.fail "Copilot media/web probe artifact not found"
  in
  Alcotest.(check bool)
    "probe artifact is executable" true
    ((Unix.stat path).st_perm land 0o111 <> 0) ;
  Alcotest.(check int)
    "offline probe self-test" 0
    (Sys.command (Filename.quote path ^ " --self-test"))

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
    ( "protocol tools are allowlisted and turn-bound",
      `Quick,
      test_protocol_tool_allowlist_and_turn_ownership );
    ( "protocol rejects MCP updates and workspace changes",
      `Quick,
      test_protocol_rejects_mcp_updates_and_workspace_changes );
    ( "sensitive request rejection precedes project writes",
      `Quick,
      test_sensitive_request_rejection_precedes_project_writes );
    ( "project MCP config is rejected before spawn",
      `Quick,
      test_project_mcp_config_rejected_before_spawn );
    ( "isolated config cleanup is structurally reported",
      `Quick,
      test_isolated_config_cleanup_is_structural_for_all_results );
    ( "run_task fails before spawn without complete MCP isolation",
      `Quick,
      test_run_task_fails_before_spawn_without_complete_mcp_isolation );
    ( "project config generation omits MCP artifacts",
      `Quick,
      test_project_config_generation_omits_mcp_artifact );
    ( "project config validation accepts custom namespace marker",
      `Quick,
      test_project_config_validation_accepts_custom_namespace_marker );
    ( "run_task rejects invalid namespace before write",
      `Quick,
      test_run_task_rejects_invalid_namespace_before_write );
    ( "media/web probe offline self-test",
      `Quick,
      test_media_web_probe_offline_self_test );
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
