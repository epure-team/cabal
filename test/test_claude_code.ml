(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the Claude Code backend. *)

open Cabal

(** {1 Module Identity Tests} *)

let test_id () = Alcotest.(check string) "id" "claude-code" Claude_code.id

let test_name () = Alcotest.(check string) "name" "Claude Code" Claude_code.name

let identity_tests =
  [
    ("id is claude-code", `Quick, test_id);
    ("name is Claude Code", `Quick, test_name);
  ]

(** {1 MCP Config Tests} *)

let test_write_mcp_config_single_server () =
  Eio_posix.run @@ fun env ->
  let tmp_path = "/tmp/epure-test-mcp-config.json" in
  let config =
    Backend_types.make_mcp_server_config
      ~name:"epure"
      ~command:"epure"
      ~args:["mcp-server"; "--db"; "test.db"]
      ~env:[("DEBUG", "1")]
      ()
  in
  Claude_code.write_mcp_config ~env ~path:tmp_path [config] ;
  (* Read and parse the file *)
  let fs = Eio.Stdenv.fs env in
  let content = Eio.Path.load Eio.Path.(fs / tmp_path) in
  let json = Yojson.Safe.from_string content in
  let open Yojson.Safe.Util in
  let servers = json |> member "mcpServers" in
  Alcotest.(check bool) "mcpServers exists" true (servers <> `Null) ;
  let epure_server = servers |> member "epure" in
  Alcotest.(check string)
    "command"
    "epure"
    (epure_server |> member "command" |> to_string) ;
  let args = epure_server |> member "args" |> to_list |> List.map to_string in
  Alcotest.(check (list string)) "args" ["mcp-server"; "--db"; "test.db"] args ;
  let env_obj = epure_server |> member "env" in
  Alcotest.(check string)
    "env DEBUG"
    "1"
    (env_obj |> member "DEBUG" |> to_string) ;
  (* Cleanup *)
  Eio.Path.unlink Eio.Path.(fs / tmp_path)

let test_write_mcp_config_multiple_servers () =
  Eio_posix.run @@ fun env ->
  let tmp_path = "/tmp/epure-test-mcp-config-multi.json" in
  let config1 =
    Backend_types.make_mcp_server_config
      ~name:"server1"
      ~command:"/usr/bin/server1"
      ()
  in
  let config2 =
    Backend_types.make_mcp_server_config
      ~name:"server2"
      ~command:"/usr/bin/server2"
      ~args:["--verbose"]
      ()
  in
  Claude_code.write_mcp_config ~env ~path:tmp_path [config1; config2] ;
  (* Read and parse the file *)
  let fs = Eio.Stdenv.fs env in
  let content = Eio.Path.load Eio.Path.(fs / tmp_path) in
  let json = Yojson.Safe.from_string content in
  let open Yojson.Safe.Util in
  let servers = json |> member "mcpServers" in
  let server1 = servers |> member "server1" in
  let server2 = servers |> member "server2" in
  Alcotest.(check bool) "server1 exists" true (server1 <> `Null) ;
  Alcotest.(check bool) "server2 exists" true (server2 <> `Null) ;
  Alcotest.(check string)
    "server1 command"
    "/usr/bin/server1"
    (server1 |> member "command" |> to_string) ;
  Alcotest.(check string)
    "server2 command"
    "/usr/bin/server2"
    (server2 |> member "command" |> to_string) ;
  (* Cleanup *)
  Eio.Path.unlink Eio.Path.(fs / tmp_path)

let mcp_config_tests =
  [
    ("write single server config", `Quick, test_write_mcp_config_single_server);
    ( "write multiple server configs",
      `Quick,
      test_write_mcp_config_multiple_servers );
  ]

(** {1 JSON Output Parsing Tests} *)

let test_parse_json_output_with_result () =
  let json =
    `Assoc
      [
        ("result", `String "Task completed successfully");
        ( "usage",
          `Assoc [("input_tokens", `Int 100); ("output_tokens", `Int 50)] );
      ]
  in
  let text, cost = Claude_code.parse_json_output json in
  Alcotest.(check string) "result text" "Task completed successfully" text ;
  Alcotest.(check bool) "cost present" true (Option.is_some cost) ;
  match cost with
  | Some c ->
      Alcotest.(check (option int)) "input_tokens" (Some 100) c.tokens_input ;
      Alcotest.(check (option int)) "output_tokens" (Some 50) c.tokens_output
  | None -> Alcotest.fail "Expected cost to be Some"

let test_parse_json_output_no_usage () =
  let json = `Assoc [("result", `String "Simple result")] in
  let text, cost = Claude_code.parse_json_output json in
  Alcotest.(check string) "result text" "Simple result" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_json_output_malformed () =
  let json = `Assoc [("foo", `String "bar")] in
  let text, cost = Claude_code.parse_json_output json in
  (* Fallback: returns the stringified JSON when result field is missing *)
  Alcotest.(check bool) "text is non-empty" true (String.length text > 0) ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let json_output_tests =
  [
    ("parse with result and usage", `Quick, test_parse_json_output_with_result);
    ("parse without usage", `Quick, test_parse_json_output_no_usage);
    ("parse malformed", `Quick, test_parse_json_output_malformed);
  ]

(** {1 Git Diff Tests} *)

let test_get_git_diff_in_git_repo () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Use the current project directory which is a git repo *)
  let working_dir = Sys.getcwd () in
  let files = Claude_code.get_git_diff ~sw ~env ~working_dir in
  (* We can't predict what files are changed, but we can check it returns a list *)
  Alcotest.(check bool)
    "returns a list"
    true
    (List.for_all (fun s -> String.length s > 0) files)

let test_get_git_diff_not_git_repo () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* /tmp is typically not a git repo *)
  let files = Claude_code.get_git_diff ~sw ~env ~working_dir:"/tmp" in
  (* Should return empty list, not crash *)
  Alcotest.(check (list string)) "empty for non-git dir" [] files

let git_diff_tests =
  [
    ("git diff in git repo", `Quick, test_get_git_diff_in_git_repo);
    ("git diff in non-git dir", `Quick, test_get_git_diff_not_git_repo);
  ]

(** {1 Availability Tests} *)

let test_available () =
  (* This test checks if Claude CLI is available on the system.
     The result depends on whether the test machine has claude installed. *)
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let is_available = Claude_code.available ~sw ~env in
  (* Just check it returns a bool without crashing *)
  Alcotest.(check bool)
    "returns true or false"
    true
    (is_available || not is_available)

let availability_tests = [("available check", `Quick, test_available)]

(** {1 Backend Interface Compliance Tests} *)

let test_implements_agentic_backend () =
  (* Verify Claude_code satisfies the AGENTIC_BACKEND.S signature *)
  let backend = (module Claude_code : Agentic_backend.S) in
  Alcotest.(check string)
    "id via interface"
    "claude-code"
    (Agentic_backend.id backend) ;
  Alcotest.(check string)
    "name via interface"
    "Claude Code"
    (Agentic_backend.name backend)

let interface_tests =
  [("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend)]

(** {1 Session Reuse Tests} *)

let test_build_command_no_resume () =
  let spec =
    Backend_types.make_task_spec
      ~prompt:"Test prompt"
      ~working_dir:"/tmp/test"
      ()
  in
  let cmd, stdin =
    Claude_code.build_command ~mcp_config_path:(Some "/tmp/mcp.json") spec
  in
  Alcotest.(check bool)
    "no --resume in cmd"
    true
    (not (List.mem "--resume" cmd)) ;
  Alcotest.(check bool) "prompt in stdin" true (String.length stdin > 0)

let test_build_command_with_resume () =
  let spec =
    Backend_types.make_task_spec
      ~prompt:"Continue work"
      ~working_dir:"/tmp/test"
      ~resume_session_id:"abc-123-def"
      ()
  in
  let cmd, _ =
    Claude_code.build_command ~mcp_config_path:(Some "/tmp/mcp.json") spec
  in
  (* Find --resume flag and its argument *)
  let rec find_resume = function
    | "--resume" :: sid :: _ -> Some sid
    | _ :: rest -> find_resume rest
    | [] -> None
  in
  match find_resume cmd with
  | Some sid -> Alcotest.(check string) "session_id" "abc-123-def" sid
  | None -> Alcotest.fail "--resume not found in command"

let rec find_flag_value flag = function
  | candidate :: value :: _ when candidate = flag -> Some value
  | _ :: rest -> find_flag_value flag rest
  | [] -> None

let test_build_command_with_output_schema () =
  let schema : Yojson.Safe.t =
    `Assoc
      [
        ("$schema", `String "https://json-schema.org/draft/2020-12/schema");
        ("type", `String "object");
        ("properties", `Assoc [("answer", `Assoc [("type", `String "string")])]);
        ("required", `List [`String "answer"]);
        ("additionalProperties", `Bool false);
      ]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"Return a JSON object."
      ~working_dir:"/tmp/test"
      ~json_schema:schema
      ()
  in
  let cmd, _stdin = Claude_code.build_command ~mcp_config_path:None spec in
  match find_flag_value "--output-schema" cmd with
  | Some actual ->
      Alcotest.(check string)
        "--output-schema inline JSON"
        (Yojson.Safe.to_string ~std:true schema)
        actual
  | None -> Alcotest.fail "--output-schema not found in command"

let test_parse_session_id_from_stdout () =
  let stdout =
    Yojson.Safe.to_string
      (`Assoc
         [("result", `String "done"); ("session_id", `String "sess-abc-123")])
  in
  let sid = Claude_code.parse_session_id_from_stdout stdout in
  Alcotest.(check (option string)) "session_id" (Some "sess-abc-123") sid

let test_parse_session_id_missing () =
  let stdout = Yojson.Safe.to_string (`Assoc [("result", `String "done")]) in
  let sid = Claude_code.parse_session_id_from_stdout stdout in
  Alcotest.(check (option string)) "no session_id" None sid

let test_parse_session_id_not_json () =
  let sid = Claude_code.parse_session_id_from_stdout "plain text output" in
  Alcotest.(check (option string)) "not json" None sid

let test_parse_session_id_from_jsonl () =
  (* stream-json mode: multiple JSON lines, session_id in init event *)
  let stdout =
    String.concat
      "\n"
      [
        Yojson.Safe.to_string
          (`Assoc
             [
               ("type", `String "system");
               ("subtype", `String "init");
               ("session_id", `String "70f62070-a552-4cc6-9ee2-b97cf02e3eda");
             ]);
        Yojson.Safe.to_string
          (`Assoc [("type", `String "assistant"); ("message", `String "hi")]);
        Yojson.Safe.to_string (`Assoc [("type", `String "result")]);
      ]
  in
  let sid = Claude_code.parse_session_id_from_stdout stdout in
  Alcotest.(check (option string))
    "session_id from JSONL"
    (Some "70f62070-a552-4cc6-9ee2-b97cf02e3eda")
    sid

let test_parse_cache_tokens () =
  let json =
    `Assoc
      [
        ("result", `String "done");
        ( "usage",
          `Assoc
            [
              ("input_tokens", `Int 1000);
              ("output_tokens", `Int 200);
              ("cache_creation_input_tokens", `Int 500);
              ("cache_read_input_tokens", `Int 300);
            ] );
      ]
  in
  let _, cost = Claude_code.parse_json_output json in
  match cost with
  | None -> Alcotest.fail "expected cost"
  | Some c ->
      Alcotest.(check (option int))
        "cache creation"
        (Some 500)
        c.cache_creation_input_tokens ;
      Alcotest.(check (option int))
        "cache read"
        (Some 300)
        c.cache_read_input_tokens

let session_reuse_tests =
  [
    ("build_command without resume", `Quick, test_build_command_no_resume);
    ("build_command with resume", `Quick, test_build_command_with_resume);
    ( "build_command with output schema",
      `Quick,
      test_build_command_with_output_schema );
    ("parse session_id from stdout", `Quick, test_parse_session_id_from_stdout);
    ("parse session_id missing", `Quick, test_parse_session_id_missing);
    ("parse session_id not json", `Quick, test_parse_session_id_not_json);
    ("parse session_id from JSONL", `Quick, test_parse_session_id_from_jsonl);
    ("parse cache tokens from usage", `Quick, test_parse_cache_tokens);
  ]

(** {1 Stream Event Parsing Tests} *)

let test_stream_event_tool_use_with_path () =
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "assistant");
           ( "message",
             `Assoc
               [
                 ( "content",
                   `List
                     [
                       `Assoc
                         [
                           ("type", `String "tool_use");
                           ("name", `String "Edit");
                           ( "input",
                             `Assoc [("file_path", `String "src/foo.ml")] );
                         ];
                     ] );
               ] );
         ])
  in
  let contains s sub =
    let slen = String.length s and sublen = String.length sub in
    let rec loop i =
      if i + sublen > slen then false
      else if String.sub s i sublen = sub then true
      else loop (i + 1)
    in
    loop 0
  in
  match Claude_code.parse_stream_event line with
  | Some text ->
      (* Should contain "→ Edit src/foo.ml", not "[Using tool: Edit]" *)
      Alcotest.(check bool)
        "no [Using tool:] noise"
        false
        (contains text "[Using tool:") ;
      Alcotest.(check bool)
        "contains file path"
        true
        (contains text "src/foo.ml")
  | None -> Alcotest.fail "expected Some for tool_use"

let test_stream_event_tool_result_skipped () =
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "user");
           ( "message",
             `Assoc
               [
                 ( "content",
                   `List
                     [
                       `Assoc
                         [
                           ("type", `String "tool_result");
                           ("tool_use_id", `String "toolu_01abc");
                         ];
                     ] );
               ] );
         ])
  in
  Alcotest.(check bool)
    "tool_result only returns None"
    true
    (Option.is_none (Claude_code.parse_stream_event line))

let test_stream_event_user_text_kept () =
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "user");
           ( "message",
             `Assoc
               [
                 ( "content",
                   `List
                     [
                       `Assoc
                         [
                           ("type", `String "tool_result");
                           ("tool_use_id", `String "toolu_01abc");
                         ];
                       `Assoc
                         [
                           ("type", `String "text");
                           ("text", `String "Here is the answer");
                         ];
                     ] );
               ] );
         ])
  in
  match Claude_code.parse_stream_event line with
  | Some text ->
      Alcotest.(check string) "text content kept" "Here is the answer" text
  | None -> Alcotest.fail "expected Some for user text"

let test_stream_event_system_init () =
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "system");
           ("subtype", `String "init");
           ("session_id", `String "abc123def456");
           ("cwd", `String "/tmp/test");
           ("tools", `List []);
         ])
  in
  match Claude_code.parse_stream_event line with
  | Some text ->
      Alcotest.(check string)
        "init shows short session ID"
        "[Session: abc123def456]"
        text
  | None -> Alcotest.fail "expected Some for system init"

let stream_event_tests =
  [
    ( "tool_use shows arrow and path",
      `Quick,
      test_stream_event_tool_use_with_path );
    ( "tool_result only returns None",
      `Quick,
      test_stream_event_tool_result_skipped );
    ( "user text kept alongside tool_result",
      `Quick,
      test_stream_event_user_text_kept );
    ("system init extracts session ID", `Quick, test_stream_event_system_init);
  ]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Claude_code"
    [
      ("Identity", identity_tests);
      ("MCP Config", mcp_config_tests);
      ("JSON Output", json_output_tests);
      ("Git Diff", git_diff_tests);
      ("Availability", availability_tests);
      ("Interface", interface_tests);
      ("Session Reuse", session_reuse_tests);
      ("Stream Events", stream_event_tests);
    ]
