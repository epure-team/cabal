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

let test_parse_json_output_with_structured_output () =
  let structured = `Assoc [("answer", `String "ok"); ("count", `Int 2)] in
  let json = `Assoc [("structured_output", structured)] in
  let text, cost = Claude_code.parse_json_output json in
  Alcotest.(check string)
    "structured_output JSON"
    (Yojson.Safe.to_string structured)
    text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_json_output_prefers_structured_output () =
  let structured = `Assoc [("answer", `String "structured")] in
  let json =
    `Assoc
      [
        ("structured_output", structured);
        ("result", `String "legacy result should not win");
      ]
  in
  let text, cost = Claude_code.parse_json_output json in
  Alcotest.(check string)
    "structured_output wins"
    (Yojson.Safe.to_string structured)
    text ;
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
    ( "parse structured_output as JSON",
      `Quick,
      test_parse_json_output_with_structured_output );
    ( "prefer structured_output over result",
      `Quick,
      test_parse_json_output_prefers_structured_output );
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
      ~resume_session_id:"70f62070-a552-4cc6-9ee2-b97cf02e3eda"
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
  | Some sid ->
      Alcotest.(check string)
        "session_id"
        "70f62070-a552-4cc6-9ee2-b97cf02e3eda"
        sid
  | None -> Alcotest.fail "--resume <id> not found in command"

let rec find_flag_value flag = function
  | candidate :: value :: _ when candidate = flag -> Some value
  | _ :: rest -> find_flag_value flag rest
  | [] -> None

let contains_substring haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    i + nlen <= hlen && (String.sub haystack i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let valid_session_id = "70f62070-a552-4cc6-9ee2-b97cf02e3eda"

let media_attachment ?(id = "image") ?(path = "media/front cover.png")
    ~size_bytes media_type : Backend_types.media_attachment =
  {id; path; media_type; sha256 = String.make 64 '0'; size_bytes}

let command_spec ?(attachments = []) ?(web_access = Backend_types.Web_disabled)
    ?resume_session_id ?json_schema ?model ?(read_only = false) () =
  Backend_types.make_task_spec
    ~prompt:"private prompt payload"
    ~instructions:"private project instructions"
    ~working_dir:"/private/workspace path"
    ~attachments
    ~web_access
    ?resume_session_id
    ?json_schema
    ?model
    ~read_only
    ()

let build_invocation ?attachment_paths
    ?(attachment_delivery = Backend_types.Upload_attachments)
    ?(project_config_path = None) ?(mcp_config_path = None) spec =
  match
    Claude_code.build_invocation ?attachment_paths ~attachment_delivery
      ~project_config_path ~mcp_config_path spec
  with
  | Ok invocation -> invocation
  | Error message -> Alcotest.fail message

let content_blocks stdin =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_string stdin |> member "message" |> member "content" |> to_list

let expected_tools ~read_only =
  if read_only then ["Read"; "Glob"; "Grep"]
  else ["Read"; "Glob"; "Grep"; "Bash"; "Edit"; "Write"; "Task"]

let expected_argv ?resume_session_id ?mcp_config_path ?project_config_path ?model
    ?schema ~read_only () =
  let tools = String.concat "," (expected_tools ~read_only) in
  [
    "claude";
    "--print";
    "--input-format";
    "stream-json";
    "--output-format";
    "stream-json";
    "--verbose";
  ]
  @
  (match resume_session_id with
  | Some id -> ["--resume"; id]
  | None -> [])
  @
  (if read_only then
     [
       "--dangerously-skip-permissions";
       "--tools";
       tools;
       "--disallowedTools";
       "Bash,Edit,Write,NotebookEdit,WebSearch,WebFetch";
     ]
   else
     [
       "--dangerously-skip-permissions";
       "--tools";
       tools;
       "--allowedTools";
       tools;
     ])
  @ ["--setting-sources"; "user"]
  @ (match mcp_config_path with
    | Some path -> ["--mcp-config"; path; "--strict-mcp-config"]
    | None -> ["--strict-mcp-config"])
  @ (match project_config_path with
    | Some path -> ["--settings"; path]
    | None -> [])
  @ (match model with Some value -> ["--model"; value] | None -> [])
  @ match schema with Some value -> ["--json-schema"; value] | None -> []

let test_build_invocation_text_schema_exact_protocol () =
  let schema : Yojson.Safe.t =
    `Assoc
      [
        ("$schema", `String "https://json-schema.org/draft/2020-12/schema");
        ("type", `String "object");
      ]
  in
  let stripped_schema = Yojson.Safe.to_string ~std:true (`Assoc [("type", `String "object")]) in
  let invocation =
    build_invocation ~project_config_path:(Some "/private/settings path.json")
      ~mcp_config_path:(Some "/private/mcp path.json")
      (command_spec ~json_schema:schema ~model:"private-model" ())
  in
  Alcotest.(check (list string))
    "exact text/schema argv"
    (expected_argv ~mcp_config_path:"/private/mcp path.json"
       ~project_config_path:"/private/settings path.json" ~model:"private-model"
       ~schema:stripped_schema ~read_only:false ())
    invocation.argv ;
  let blocks = content_blocks invocation.stdin in
  (match blocks with
  | [text] ->
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "composed prompt is first text block"
        "private prompt payload\n\n---\nProject Instructions:\nprivate project instructions"
        (text |> member "text" |> to_string)
  | _ -> Alcotest.fail "expected exactly one text block")

let test_build_invocation_web_disabled_and_read_only_exact_tool_sets () =
  List.iter
    (fun read_only ->
      let invocation = build_invocation (command_spec ~read_only ()) in
      Alcotest.(check (list string))
        "exact web-disabled tool policy"
        (expected_argv ~read_only ())
        invocation.argv ;
      Alcotest.(check bool)
        "WebSearch unavailable" false (List.mem "WebSearch" invocation.argv) ;
      Alcotest.(check bool)
        "WebFetch unavailable" false (List.mem "WebFetch" invocation.argv))
    [false; true]

let test_build_invocation_text_resume () =
  let reuse =
    build_invocation (command_spec ~resume_session_id:valid_session_id ())
  in
  Alcotest.(check int)
    "text resume carries one text block" 1
    (List.length (content_blocks reuse.stdin)) ;
  Alcotest.(check (option string))
    "resume uses the parity SDK flag/value shape"
    (Some valid_session_id)
    (find_flag_value "--resume" reuse.argv)

let test_build_invocation_redacts_every_sensitive_value () =
  let schema = `Assoc [("type", `String "object"); ("secret", `String "schema-secret")] in
  let invocation =
    build_invocation ~project_config_path:(Some "/private/settings path.json")
      ~mcp_config_path:(Some "/private/mcp path.json")
      (command_spec ~resume_session_id:valid_session_id
         ~json_schema:schema ~model:"private-model" ())
  in
  Alcotest.(check (list string))
    "exact redacted argv"
    (expected_argv ~resume_session_id:"<session-id>"
       ~mcp_config_path:"<mcp-config>" ~project_config_path:"<settings>"
       ~model:"<model>" ~schema:"<schema>" ~read_only:false ())
    invocation.redacted_argv ;
  Alcotest.(check string)
    "redacted stdin exposes format only"
    "<stream-json-input:text-only>" invocation.redacted_stdin ;
  let redacted =
    String.concat " " invocation.redacted_argv ^ invocation.redacted_stdin
  in
  List.iter
    (fun secret ->
      Alcotest.(check bool)
        "sensitive value omitted" false (contains_substring redacted secret))
    [
      valid_session_id;
      "private-model";
      "schema-secret";
      "settings path";
      "mcp path";
      "private prompt";
    ]

let test_build_invocation_keeps_media_and_web_fail_closed () =
  let attachments =
    [
      media_attachment ~id:"front" ~path:"media/front cover.png" ~size_bytes:11
        Backend_types.Png;
      media_attachment ~id:"back" ~path:"media/back cover.jpg" ~size_bytes:7
        Backend_types.Jpeg;
    ]
  in
  let assert_rejected label ?(attachment_paths = []) spec =
    match
      Claude_code.build_invocation ~attachment_paths
        ~attachment_delivery:Backend_types.Upload_attachments
        ~project_config_path:None ~mcp_config_path:None spec
    with
    | Ok _ -> Alcotest.fail (label ^ " was accepted")
    | Error message ->
        List.iter
          (fun private_value ->
            Alcotest.(check bool)
              (label ^ " omits private input") false
              (contains_substring message private_value))
          ["front cover.png"; "back cover.jpg"; "/private/sealed image.png"]
  in
  assert_rejected "media upload"
    ~attachment_paths:["/private/sealed image.png"; "/private/sealed image.jpg"]
    (command_spec ~attachments ()) ;
  assert_rejected "media session reuse"
    (command_spec ~attachments ~resume_session_id:valid_session_id ()) ;
  assert_rejected "web search"
    (command_spec ~web_access:Backend_types.Web_search ()) ;
  assert_rejected "web search/fetch"
    (command_spec ~web_access:Backend_types.Web_search_and_fetch ())

let test_build_invocation_rejects_reuse_without_resume () =
  let attachment = media_attachment ~size_bytes:11 Backend_types.Png in
  match
    Claude_code.build_invocation
      ~attachment_delivery:Backend_types.Reuse_session_attachments
      ~project_config_path:None ~mcp_config_path:None
      (command_spec ~attachments:[attachment] ())
  with
  | Ok _ -> Alcotest.fail "attachment reuse without resume was accepted"
  | Error _ -> ()

let test_build_invocation_rejects_invalid_resume_ids () =
  List.iter
    (fun invalid_id ->
      match
        Claude_code.build_invocation
          ~attachment_delivery:Backend_types.Upload_attachments
          ~project_config_path:None ~mcp_config_path:None
          (command_spec ~resume_session_id:invalid_id ())
      with
      | Ok _ -> Alcotest.fail "invalid resume id was accepted"
      | Error message ->
          Alcotest.(check bool)
            "resume error omits supplied value" false
            (invalid_id <> "" && contains_substring message invalid_id))
    [""; "--continue"; "not-a-uuid"; String.make 129 'a']

let rec remove_tree path =
  if Sys.file_exists path then
    match (Unix.lstat path).st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name -> remove_tree (Filename.concat path name)) ;
        Unix.rmdir path
    | _ -> Sys.remove path

let with_temp_dir label f =
  let path = Filename.temp_dir ("cabal-claude-" ^ label ^ "-") "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> f path)

let with_path_prefix path f =
  let previous = Sys.getenv_opt "PATH" in
  let next =
    match previous with
    | Some value when value <> "" -> path ^ ":" ^ value
    | Some _ | None -> path
  in
  Unix.putenv "PATH" next ;
  Fun.protect
    ~finally:(fun () -> Unix.putenv "PATH" (Option.value ~default:"" previous))
    f

let write_executable path contents =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents) ;
  Unix.chmod path 0o700

let test_sensitive_requests_fail_before_config_or_spawn () =
  with_temp_dir "fail-closed" @@ fun temp_dir ->
  let marker = Filename.concat temp_dir "claude-ran" in
  write_executable
    (Filename.concat temp_dir "claude")
    (Printf.sprintf "#!/bin/sh\nprintf ran > %s\nexit 99\n" (Filename.quote marker)) ;
  with_path_prefix temp_dir @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let attachment =
    media_attachment ~path:"media/private image.png" ~size_bytes:11
      Backend_types.Png
  in
  let specs =
    [
      command_spec ~attachments:[attachment] ();
      command_spec ~web_access:Backend_types.Web_search ();
      command_spec ~web_access:Backend_types.Web_search_and_fetch ();
    ]
  in
  List.iter
    (fun spec ->
      let spec = {spec with Backend_types.working_dir = temp_dir} in
      let result = Claude_code.run_task ~sw ~env spec in
      (match result.status with
      | Backend_types.Failed message ->
          Alcotest.(check bool)
            "fixed unsupported diagnostic" true
            (contains_substring message "not enabled without authenticated proof") ;
          Alcotest.(check bool)
            "caller path omitted" false
            (contains_substring message attachment.path)
      | _ -> Alcotest.fail "sensitive request did not fail closed") ;
      Alcotest.(check bool)
        "no project config side effect" false
        (Sys.file_exists (Filename.concat temp_dir ".cabal")) ;
      Alcotest.(check bool)
        "no backend process spawn" false (Sys.file_exists marker))
    specs

let test_missing_project_config_diagnostic_is_redacted () =
  with_temp_dir "config-redaction" @@ fun temp_dir ->
  let settings_path = Filename.concat temp_dir "private-settings.json" in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let setup_result : Backend_config_writer.setup_result =
    {
      project_config_path = Some settings_path;
      write_outcome = None;
      write_outcomes = [];
    }
  in
  match
    Claude_code.check_project_config ~sw ~env ~project_dir:temp_dir ~setup_result
  with
  | Agentic_backend.Config_invalid message ->
      List.iter
        (fun private_value ->
          Alcotest.(check bool)
            "config diagnostic omits private path" false
            (contains_substring message private_value))
        [settings_path; temp_dir] ;
      Alcotest.(check bool)
        "config diagnostic identifies redacted settings validation" true
        (contains_substring message "<settings>")
  | Agentic_backend.Config_valid ->
      Alcotest.fail "expected failure, but config validation succeeded"
  | Agentic_backend.Config_check_unsupported message ->
      if contains_substring message "not available" then
        Alcotest.fail "expected failure, but the fake CLI was unavailable"
      else Alcotest.fail "expected failure, but config validation raised"

let test_project_config_exception_diagnostic_is_redacted () =
  with_temp_dir "config-exception-redaction" @@ fun temp_dir ->
  let settings_path = Filename.concat temp_dir "private-settings.json" in
  let channel = open_out settings_path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel "{}") ;
  write_executable
    (Filename.concat temp_dir "claude")
    "#!/bin/sh\nexit 0\n" ;
  with_path_prefix temp_dir @@ fun () ->
  Eio_posix.run @@ fun env ->
  let setup_result : Backend_config_writer.setup_result =
    {
      project_config_path = Some settings_path;
      write_outcome = None;
      write_outcomes = [];
    }
  in
  let observed = ref None in
  (try
     Eio.Switch.run @@ fun sw ->
     observed :=
       Some
         (Claude_code.check_project_config ~sw ~env
            ~project_dir:(temp_dir ^ "\000private-workspace")
            ~setup_result)
   with Unix.Unix_error _ -> ()) ;
  match !observed with
  | None -> Alcotest.fail "config validation did not return a result"
  | Some (Agentic_backend.Config_check_unsupported message) ->
      Alcotest.(check string)
        "exception diagnostic is fixed"
        "Claude Code native config validation could not run"
        message
  | Some Agentic_backend.Config_valid ->
      Alcotest.fail "expected invalid-working-directory validation failure"
  | Some (Agentic_backend.Config_invalid message) ->
      Alcotest.fail ("expected unsupported validation result: " ^ message)

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
  (* The "$schema" key is deliberately absent from what reaches the CLI: the
     Claude Code binary cannot resolve the 2020-12 meta-schema URI and refuses
     the flag outright (epure #283).  Everything else must survive verbatim. *)
  let expected : Yojson.Safe.t =
    `Assoc
      [
        ("type", `String "object");
        ("properties", `Assoc [("answer", `Assoc [("type", `String "string")])]);
        ("required", `List [`String "answer"]);
        ("additionalProperties", `Bool false);
      ]
  in
  let cmd, _stdin = Claude_code.build_command ~mcp_config_path:None spec in
  match find_flag_value "--json-schema" cmd with
  | Some actual ->
      Alcotest.(check string)
        "--json-schema inline JSON, meta-schema stripped"
        (Yojson.Safe.to_string ~std:true expected)
        actual
  | None -> Alcotest.fail "--json-schema not found in command"

(* A schema that never carried "$schema" must pass through untouched — the
   stripping must not be an excuse to rewrite the caller's schema. *)
let test_build_command_schema_without_meta_is_verbatim () =
  let schema : Yojson.Safe.t =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "inner",
                `Assoc
                  [
                    ("type", `String "object");
                    ("oneOf", `List [`Assoc [("required", `List [`String "a"])]]);
                  ] );
            ] );
      ]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"x"
      ~working_dir:"/tmp/test"
      ~json_schema:schema
      ()
  in
  let cmd, _stdin = Claude_code.build_command ~mcp_config_path:None spec in
  match find_flag_value "--json-schema" cmd with
  | Some actual ->
      Alcotest.(check string)
        "schema without $schema is unchanged"
        (Yojson.Safe.to_string ~std:true schema)
        actual ;
      let contains hay needle =
        let nh = String.length hay and nn = String.length needle in
        let rec go i =
          i + nn <= nh && (String.sub hay i nn = needle || go (i + 1))
        in
        nn = 0 || go 0
      in
      Alcotest.(check bool)
        "no $schema key leaked in"
        false
        (contains actual "$schema")
  | None -> Alcotest.fail "--json-schema not found in command"

let test_parse_session_id_from_stdout () =
  let stdout =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "result");
           ("subtype", `String "success");
           ("is_error", `Bool false);
           ("result", `String "done");
           ("session_id", `String valid_session_id);
         ])
  in
  let sid = Claude_code.parse_session_id_from_stdout stdout in
  Alcotest.(check (option string)) "session_id" (Some valid_session_id) sid

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

let test_parse_session_id_rejects_nonpublic_or_noncanonical_records () =
  let stdout =
    String.concat
      "\n"
      [
        Printf.sprintf {|{"session_id":"%s"}|} valid_session_id;
        Printf.sprintf
          {|{"type":"system","subtype":"error","session_id":"%s"}|}
          valid_session_id;
        {|{"type":"result","subtype":"success","is_error":false,"session_id":"sess-private"}|};
      ]
  in
  Alcotest.(check (option string))
    "untrusted/noncanonical session ids ignored" None
    (Claude_code.parse_session_id_from_stdout stdout)

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
    ( "text input uses exact stream-json/schema protocol",
      `Quick,
      test_build_invocation_text_schema_exact_protocol );
    ( "web-disabled/read-only uses exact isolated tool sets",
      `Quick,
      test_build_invocation_web_disabled_and_read_only_exact_tool_sets );
    ( "text resume binds the canonical session id",
      `Quick,
      test_build_invocation_text_resume );
    ( "redacted invocation omits all sensitive values",
      `Quick,
      test_build_invocation_redacts_every_sensitive_value );
    ( "media and positive web remain fail closed",
      `Quick,
      test_build_invocation_keeps_media_and_web_fail_closed );
    ( "attachment reuse requires resume",
      `Quick,
      test_build_invocation_rejects_reuse_without_resume );
    ( "invalid resume ids are rejected",
      `Quick,
      test_build_invocation_rejects_invalid_resume_ids );
    ( "sensitive requests fail before config or spawn",
      `Quick,
      test_sensitive_requests_fail_before_config_or_spawn );
    ( "missing project config diagnostics are redacted",
      `Quick,
      test_missing_project_config_diagnostic_is_redacted );
    ( "project config exception diagnostics are redacted",
      `Quick,
      test_project_config_exception_diagnostic_is_redacted );
    ( "build_command leaves a meta-schema-free schema verbatim (#283)",
      `Quick,
      test_build_command_schema_without_meta_is_verbatim );
    ( "build_command with output schema, meta-schema stripped (#283)",
      `Quick,
      test_build_command_with_output_schema );
    ("parse session_id from stdout", `Quick, test_parse_session_id_from_stdout);
    ("parse session_id missing", `Quick, test_parse_session_id_missing);
    ("parse session_id not json", `Quick, test_parse_session_id_not_json);
    ("parse session_id from JSONL", `Quick, test_parse_session_id_from_jsonl);
    ( "reject nonpublic/noncanonical session ids",
      `Quick,
      test_parse_session_id_rejects_nonpublic_or_noncanonical_records );
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
                  ("role", `String "assistant");
                  ( "content",
                    `List
                      [
                        `Assoc
                          [
                            ("type", `String "tool_use");
                            ("id", `String "toolu_01-safe");
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
      Alcotest.(check bool)
        "no [Using tool:] noise"
        false
        (contains text "[Using tool:") ;
      Alcotest.(check bool)
        "tool argument path is private"
        false
        (contains text "src/foo.ml") ;
      Alcotest.(check bool) "contains fixed tool name" true (contains text "Edit")
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

let test_stream_event_user_text_and_tool_result_private () =
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
  let rendered = Claude_code.parse_stream_event line in
  Alcotest.(check bool)
    "input text is never rendered" true
    (match rendered with
    | None -> true
    | Some text -> not (contains_substring text "Here is the answer"))

let test_stream_event_system_init () =
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "system");
           ("subtype", `String "init");
           ("session_id", `String valid_session_id);
           ("cwd", `String "/tmp/test");
           ("tools", `List []);
         ])
  in
  match Claude_code.parse_stream_event line with
  | Some text ->
      Alcotest.(check string)
        "init diagnostic omits session ID"
        "[Session started]"
        text
  | None -> Alcotest.fail "expected Some for system init"

let payload_kind = function
  | Task_event.Session_id _ -> "session"
  | Agent_text_delta _ -> "agent"
  | Tool_started _ -> "tool-started"
  | Tool_finished _ -> "tool-finished"
  | Token_usage _ -> "usage"
  | _ -> "other"

let test_normalized_events_accept_only_public_bounded_structures () =
  let public_lines =
    [
      Printf.sprintf
        {|{"type":"system","subtype":"init","session_id":"%s","cwd":"/private/workspace","tools":["Read"]}|}
        valid_session_id;
      {|{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":"public answer"},{"type":"tool_use","id":"toolu_01-safe","name":"WebSearch","input":{"query":"private query"}}]}}|};
      {|{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_01-safe","content":"private result"}]}}|};
      Printf.sprintf
        {|{"type":"result","subtype":"success","is_error":false,"structured_output":{"answer":"done"},"session_id":"%s","usage":{"input_tokens":12,"output_tokens":3,"cache_creation_input_tokens":2,"cache_read_input_tokens":4},"total_cost_usd":0.001}|}
        valid_session_id;
    ]
  in
  let events =
    List.concat_map Claude_code.normalized_events_of_stream_line public_lines
  in
  Alcotest.(check (list string))
    "public event kinds"
    [
      "session";
      "agent";
      "tool-started";
      "agent";
      "session";
      "usage";
    ]
    (List.map payload_kind events) ;
  let rendered =
    public_lines |> List.filter_map Claude_code.parse_stream_event
    |> String.concat "\n"
  in
  List.iter
    (fun private_value ->
      Alcotest.(check bool)
        "rendered public stream omits private values" false
        (contains_substring rendered private_value))
    ["private query"; "private result"; "/private/workspace"; valid_session_id]

let test_normalized_events_reject_private_error_and_unsafe_values () =
  let private_lines =
    [
      {|{"type":"assistant","message":{"role":"assistant","content":[{"type":"thinking","thinking":"private reasoning"}]}}|};
      {|{"type":"assistant","message":{"role":"assistant","error":"private error","content":[{"type":"text","text":"tempting error text"}]}}|};
      {|{"type":"assistant","error":"authentication_failed","message":{"role":"assistant","content":[{"type":"text","text":"private top-level error text"}]}}|};
      {|{"type":"assistant","message":{"role":"assistant","content":[{"type":"tool_use","id":"/private/id","name":"Read\nprivate","input":{"file_path":"/private/file"}}]}}|};
      {|{"type":"user","message":{"role":"user","content":[{"type":"text","text":"private prompt"},{"type":"image","source":{"type":"base64","media_type":"image/png","data":"private-image-base64"}}]}}|};
      {|{"type":"result","subtype":"success","is_error":true,"result":"private failure","session_id":"70f62070-a552-4cc6-9ee2-b97cf02e3eda","usage":{"input_tokens":9}}|};
      {|{"type":"result","subtype":"unknown","is_error":false,"result":"private unknown result","session_id":"70f62070-a552-4cc6-9ee2-b97cf02e3eda","usage":{"input_tokens":9}}|};
      {|{"type":"system","subtype":"init","session_id":"--unsafe-session"}|};
      {|{"type":"system","subtype":"error","message":"private system error"}|};
      "not-json private raw fallback";
    ]
  in
  let events =
    List.concat_map Claude_code.normalized_events_of_stream_line private_lines
  in
  Alcotest.(check (list string))
    "private and unsafe structures normalize nothing" []
    (List.map payload_kind events) ;
  let public_text =
    String.concat "\n" private_lines |> Claude_code.parse_public_stdout_text
  in
  Alcotest.(check string) "no raw/private text fallback" "" public_text

let test_normalized_usage_rejects_invalid_fields_independently () =
  let line =
    {|{"type":"result","subtype":"success","is_error":false,"result":"ok","usage":{"input_tokens":-1,"output_tokens":3,"cache_creation_input_tokens":"2","cache_read_input_tokens":true},"total_cost_usd":-1.0}|}
  in
  match Claude_code.normalized_events_of_stream_line line with
  | [Task_event.Agent_text_delta "ok"; Token_usage usage] ->
      Alcotest.(check (option int))
        "negative input ignored" None usage.tokens_input ;
      Alcotest.(check (option int))
        "valid output retained" (Some 3) usage.tokens_output ;
      Alcotest.(check (option int))
        "string cache creation ignored" None
        usage.cache_creation_input_tokens ;
      Alcotest.(check (option int))
        "boolean cache read ignored" None usage.cache_read_input_tokens ;
      Alcotest.(check (option (float 0.0)))
        "negative cost ignored" None usage.cost_usd
  | _ -> Alcotest.fail "expected public text and independently valid usage"

let probe_path () =
  let relative = "tools/probe_claude_media_web.py" in
  let candidates =
    [
      relative;
      Filename.concat ".." relative;
      Filename.concat "../.." relative;
      Filename.concat "../../.." relative;
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> Alcotest.fail "Claude media/web probe artifact not found"

let test_media_web_probe_offline_self_test () =
  let path = probe_path () in
  Alcotest.(check bool)
    "probe artifact executable" true
    ((Unix.stat path).st_perm land 0o111 <> 0) ;
  Alcotest.(check int)
    "offline probe self-test" 0
    (Sys.command (Filename.quote path ^ " --self-test"))

let stream_event_tests =
  [
    ( "tool_use shows arrow without arguments",
      `Quick,
      test_stream_event_tool_use_with_path );
    ( "tool_result only returns None",
      `Quick,
      test_stream_event_tool_result_skipped );
    ( "user text and tool_result remain private",
      `Quick,
      test_stream_event_user_text_and_tool_result_private );
    ("system init uses a fixed marker", `Quick, test_stream_event_system_init);
    ( "normalize only public bounded structures",
      `Quick,
      test_normalized_events_accept_only_public_bounded_structures );
    ( "reject private/error/unsafe structures",
      `Quick,
      test_normalized_events_reject_private_error_and_unsafe_values );
    ( "usage fields validate independently",
      `Quick,
      test_normalized_usage_rejects_invalid_fields_independently );
    ( "media/web probe offline self-test",
      `Quick,
      test_media_web_probe_offline_self_test );
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
