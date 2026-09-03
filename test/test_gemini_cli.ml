(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the Gemini CLI backend. *)

open Cabal

let valid_session_id = "123e4567-e89b-12d3-a456-426614174000"

let contains_substring haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec loop index =
    index + nlen <= hlen
    && (String.sub haystack index nlen = needle || loop (index + 1))
  in
  nlen = 0 || loop 0

let command_contains value command = List.exists (String.equal value) command

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let rec remove_tree path =
  if Sys.file_exists path then
    match (Unix.lstat path).st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Sys.remove path

let with_temp_dir label f =
  let path = Filename.temp_dir ("cabal-gemini-" ^ label ^ "-") "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> f path)

let with_path_prefix path f =
  let previous = Sys.getenv_opt "PATH" in
  let next =
    match previous with
    | Some value when value <> "" -> path ^ ":" ^ value
    | Some _ | None -> path
  in
  Unix.putenv "PATH" next;
  Fun.protect
    ~finally:(fun () -> Unix.putenv "PATH" (Option.value ~default:"" previous))
    f

let write_executable path contents =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents);
  Unix.chmod path 0o700

(** {1 Module identity} *)

let test_id () = Alcotest.(check string) "id" "gemini-cli" Gemini_cli.id

let test_name () = Alcotest.(check string) "name" "Gemini CLI" Gemini_cli.name

let test_unproven_capabilities_remain_disabled () =
  let descriptor =
    match Backend_registry.find Gemini_cli.id with
    | Some descriptor -> descriptor
    | None -> Alcotest.fail "Gemini descriptor is missing"
  in
  let capabilities = descriptor.Backend_registry.capabilities in
  Alcotest.(check int)
    "no proven media types" 0
    (List.length capabilities.media_support.media_types);
  Alcotest.(check bool)
    "no media evidence" true
    (Option.is_none capabilities.media_support.evidence);
  Alcotest.(check bool)
    "web maximum remains disabled" true
    (capabilities.web_support.maximum = Backend_types.Web_disabled);
  Alcotest.(check bool)
    "no positive web evidence" true
    (Option.is_none capabilities.web_support.evidence)

let identity_tests =
  [
    ("id is gemini-cli", `Quick, test_id);
    ("name is Gemini CLI", `Quick, test_name);
    ( "unproven capabilities remain disabled",
      `Quick,
      test_unproven_capabilities_remain_disabled );
  ]

(** {1 Public output parsing} *)

let payload_kind = function
  | Task_event.Session_id _ -> "session"
  | Agent_text_delta _ -> "agent"
  | Tool_started _ -> "tool-started"
  | Tool_finished _ -> "tool-finished"
  | Token_usage _ -> "usage"
  | _ -> "other"

let test_normalized_events_accept_only_public_protocol_records () =
  let lines =
    [
      Printf.sprintf
        {|{"type":"init","timestamp":"ignored","session_id":"%s","model":"gemini"}|}
        valid_session_id;
      {|{"type":"message","role":"assistant","content":"public answer","delta":true}|};
      {|{"type":"tool_use","tool_name":"google_web_search","tool_id":"tool-1","parameters":{"query":"private query"}}|};
      {|{"type":"tool_result","tool_id":"tool-1","status":"success","output":"private result"}|};
      {|{"type":"result","status":"success","stats":{"input_tokens":12,"output_tokens":3,"cached":4,"total_tokens":19,"duration_ms":9,"tool_calls":1,"models":{"private-model":{"input_tokens":12}}}}|};
    ]
  in
  let events = List.concat_map Gemini_cli.normalized_events_of_line lines in
  Alcotest.(check (list string))
    "public event kinds"
    ["session"; "agent"; "tool-started"; "tool-finished"; "usage"]
    (List.map payload_kind events);
  match events with
  | [
   Task_event.Session_id session_id;
   Agent_text_delta "public answer";
   Tool_started {id = Some "tool-1"; name = "google_web_search"};
   Tool_finished {id = Some "tool-1"; name = None};
   Token_usage usage;
  ] ->
      Alcotest.(check string) "canonical session" valid_session_id session_id;
      Alcotest.(check (option int)) "input" (Some 12) usage.tokens_input;
      Alcotest.(check (option int)) "output" (Some 3) usage.tokens_output;
      Alcotest.(check (option int))
        "cached" (Some 4) usage.cache_read_input_tokens
  | _ -> Alcotest.fail "unexpected normalized Gemini payloads"

let test_normalized_events_drop_private_and_unsafe_records () =
  let private_path = "/private/sealed/image.png" in
  let lines =
    [
      Printf.sprintf
        {|{"type":"message","role":"user","content":"prompt echo %s"}|}
        private_path;
      {|{"type":"reasoning","content":"private reasoning"}|};
      Printf.sprintf
        {|{"type":"error","severity":"error","message":"private error %s"}|}
        private_path;
      Printf.sprintf
        {|{"type":"tool_use","tool_name":"web_fetch","tool_id":"%s","parameters":{"url":"https://secret.invalid"}}|}
        private_path;
      Printf.sprintf
        {|{"type":"tool_result","tool_id":"%s","status":"error","output":"private result","error":{"message":"private"}}|}
        private_path;
      {|{"type":"result","status":"error","error":{"message":"private"},"stats":{"input_tokens":99}}|};
      {|{"type":"result","status":"success","usageMetadata":{"promptTokenCount":99}}|};
      {|{"type":"message","role":"assistant","content":42}|};
      {|{"type":"init","session_id":"--resume latest"}|};
      "not-json raw fallback";
    ]
  in
  let events = List.concat_map Gemini_cli.normalized_events_of_line lines in
  Alcotest.(check (list string))
    "private and unsafe records are dropped" []
    (List.map payload_kind events);
  let stdout = String.concat "\n" lines in
  Alcotest.(check string)
    "private/raw records are not agent text" ""
    (Gemini_cli.parse_public_stdout_text stdout);
  Alcotest.(check (option string))
    "unsafe session is absent" None
    (Gemini_cli.parse_public_session_id stdout);
  Alcotest.(check bool)
    "error result does not expose usage" true
    (Option.is_none (Gemini_cli.parse_public_cost stdout))

let test_stream_parser_concatenates_assistant_deltas_only () =
  let input =
    Printf.sprintf
      {|{"type":"init","session_id":"%s","model":"gemini"}
{"type":"message","role":"user","content":"private prompt"}
{"type":"message","role":"assistant","content":"{\"answer\":"}
{"type":"tool_use","tool_name":"web_fetch","tool_id":"fetch-1","parameters":{"prompt":"private"}}
{"type":"tool_result","tool_id":"fetch-1","status":"success","output":"private"}
{"type":"message","role":"assistant","content":"\"ok\"}"}
{"type":"result","status":"success","stats":{"input_tokens":8,"output_tokens":2,"cached":1}}|}
      valid_session_id
  in
  let text, cost, session_id = Gemini_cli.parse_gemini_stream_json input in
  Alcotest.(check string) "assistant deltas" {|{"answer":"ok"}|} text;
  Alcotest.(check (option string))
    "public session" (Some valid_session_id) session_id;
  match cost with
  | Some usage ->
      Alcotest.(check (option int)) "input" (Some 8) usage.tokens_input;
      Alcotest.(check (option int)) "output" (Some 2) usage.tokens_output;
      Alcotest.(check (option int))
        "cached" (Some 1) usage.cache_read_input_tokens
  | None -> Alcotest.fail "public success usage was dropped"

let test_json_output_parser_is_strict () =
  let public =
    `Assoc
      [
        ("session_id", `String valid_session_id);
        ("response", `String "public response");
        ( "stats",
          `Assoc
            [
              ("input_tokens", `Int 5);
              ("output_tokens", `Int 2);
              ("cached", `Int 1);
            ] );
      ]
  in
  let text, cost = Gemini_cli.parse_gemini_json_output public in
  Alcotest.(check string) "documented response" "public response" text;
  Alcotest.(check bool) "documented stats" true (Option.is_some cost);
  let undocumented =
    `Assoc
      [
        ("result", `String "must not be promoted");
        ( "usageMetadata",
          `Assoc
            [("promptTokenCount", `Int 9); ("candidatesTokenCount", `Int 4)]
        );
      ]
  in
  let text, cost = Gemini_cli.parse_gemini_json_output undocumented in
  Alcotest.(check string) "undocumented text dropped" "" text;
  Alcotest.(check bool) "undocumented usage dropped" true (Option.is_none cost)

let test_session_parser_accepts_only_canonical_uuid () =
  let valid_line =
    Printf.sprintf {|{"type":"init","session_id":"%s"}|} valid_session_id
  in
  Alcotest.(check (option string))
    "canonical UUID" (Some valid_session_id)
    (Gemini_cli.parse_public_session_id valid_line);
  List.iter
    (fun invalid_id ->
      let line =
        `Assoc
          [("type", `String "init"); ("session_id", `String invalid_id)]
        |> Yojson.Safe.to_string
      in
      Alcotest.(check (option string))
        "invalid UUID" None
        (Gemini_cli.parse_public_session_id line))
    [
      "";
      "latest";
      "1";
      "123E4567-E89B-12D3-A456-426614174000";
      "123e4567-e89b-12d3-a456-42661417400";
      valid_session_id ^ "\n";
    ]

let test_usage_rejects_invalid_values_independently () =
  let input =
    {|{"type":"result","status":"success","stats":{"input_tokens":2,"output_tokens":"private","cached":-1}}|}
  in
  match Gemini_cli.parse_public_cost input with
  | Some usage ->
      Alcotest.(check (option int)) "valid input survives" (Some 2)
        usage.tokens_input;
      Alcotest.(check (option int)) "string output rejected" None
        usage.tokens_output;
      Alcotest.(check (option int)) "negative cache rejected" None
        usage.cache_read_input_tokens
  | None -> Alcotest.fail "independent valid usage was discarded"

let output_tests =
  [
    ( "normalize public protocol records",
      `Quick,
      test_normalized_events_accept_only_public_protocol_records );
    ( "drop private and unsafe records",
      `Quick,
      test_normalized_events_drop_private_and_unsafe_records );
    ( "stream parser uses assistant deltas only",
      `Quick,
      test_stream_parser_concatenates_assistant_deltas_only );
    ("JSON output parser is strict", `Quick, test_json_output_parser_is_strict);
    ( "session parser accepts canonical UUID only",
      `Quick,
      test_session_parser_accepts_only_canonical_uuid );
    ( "usage rejects invalid values independently",
      `Quick,
      test_usage_rejects_invalid_values_independently );
  ]

(** {1 Invocation construction} *)

let media_attachment ?(id = "image") ?(path = "media/original.png") media_type
    : Backend_types.media_attachment =
  {id; path; media_type; sha256 = String.make 64 '0'; size_bytes = 1}

let policy_path = "/private/cabal-gemini-web-policy.toml"
let sealed_png_path = "/private/sealed dir/asset,$(x);![]{}.png"

let command_spec ?(attachments = []) ?(web_access = Backend_types.Web_disabled)
    ?resume_session_id ?json_schema ?model () =
  Backend_types.make_task_spec
    ~prompt:"private prompt @/tmp/steal $(touch should-not-run)"
    ~instructions:"private instructions @../../secret"
    ~working_dir:"/private/workspace path" ~attachments ~web_access
    ?resume_session_id ?json_schema ?model ()

let build_invocation ?attachment_paths
    ?(attachment_delivery = Backend_types.Upload_attachments) spec =
  match
    Gemini_cli.build_invocation ?attachment_paths ~attachment_delivery
      ~web_policy_path:policy_path ~mcp_config_path:None spec
  with
  | Ok invocation -> invocation
  | Error message -> Alcotest.fail message

let base_argv ?model ?resume () =
  [
    "gemini";
    "--output-format";
    "stream-json";
    "-y";
    "--policy";
    policy_path;
    "--admin-policy";
    policy_path;
  ]
  @ (match model with Some value -> ["-m"; value] | None -> [])
  @ (match resume with Some value -> ["--resume"; value] | None -> [])
  @ ["-p"; ""]

let expected_plain_stdin =
  "private prompt \\@/tmp/steal $(touch should-not-run)\n\n---\nProject "
  ^ "Instructions:\nprivate instructions \\@../../secret"

let test_build_invocation_zero_images_exact_structure () =
  let invocation = build_invocation (command_spec ()) in
  Alcotest.(check (list string)) "argv" (base_argv ()) invocation.argv;
  Alcotest.(check (option string))
    "stdin" (Some expected_plain_stdin) invocation.stdin

let test_build_invocation_rejects_unproven_media () =
  let attachment = media_attachment Backend_types.Png in
  let cases =
    [
      ( Backend_types.Upload_attachments,
        [],
        command_spec ~attachments:[attachment] () );
      ( Backend_types.Upload_attachments,
        [sealed_png_path],
        command_spec ~attachments:[attachment] () );
      ( Backend_types.Reuse_session_attachments,
        [],
        command_spec ~attachments:[attachment]
          ~resume_session_id:valid_session_id () );
    ]
  in
  List.iter
    (fun (attachment_delivery, attachment_paths, spec) ->
      match
        Gemini_cli.build_invocation ~attachment_paths ~attachment_delivery
          ~web_policy_path:policy_path ~mcp_config_path:None spec
      with
      | Ok _ -> Alcotest.fail "unproven Gemini media transport was enabled"
      | Error message ->
          Alcotest.(check bool)
            "fixed unsupported result" true
            (contains_substring message "media attachments are unsupported");
          List.iter
            (fun private_value ->
              Alcotest.(check bool)
                "unsupported diagnostic omits private paths" false
                (contains_substring message private_value))
            [sealed_png_path; "media/original.png"])
    cases

let test_build_invocation_preserves_attachment_free_resume_reuse () =
  let invocation =
    build_invocation
      ~attachment_delivery:Backend_types.Reuse_session_attachments
      (command_spec ~resume_session_id:valid_session_id ())
  in
  Alcotest.(check (list string))
    "resume reuse argv" (base_argv ~resume:valid_session_id ()) invocation.argv;
  Alcotest.(check (option string))
    "stdin" (Some expected_plain_stdin) invocation.stdin

let test_build_invocation_preserves_non_native_schema_retry_prompt () =
  let schema = `Assoc [("type", `String "object")] in
  let retry_prompt =
    "## Required output schema\n{\"type\":\"object\"}\nRetry without @leak"
  in
  let spec =
    Backend_types.make_task_spec ~prompt:retry_prompt ~working_dir:"/private/ws"
      ~resume_session_id:valid_session_id ~json_schema:schema ()
  in
  let invocation = build_invocation spec in
  Alcotest.(check bool)
    "Gemini stays on non-native schema path" false
    (command_contains "--output-schema" invocation.argv);
  Alcotest.(check (option string))
    "retry prompt is preserved and literal at-sign is neutralized"
    (Some
       "## Required output schema\n{\"type\":\"object\"}\nRetry without \\@leak")
    invocation.stdin

let test_build_invocation_rejects_unproven_positive_web_levels () =
  List.iter
    (fun web_access ->
      match
        Gemini_cli.build_invocation
          ~attachment_delivery:Backend_types.Upload_attachments
          ~web_policy_path:policy_path ~mcp_config_path:None
          (command_spec ~web_access ())
      with
      | Ok _ -> Alcotest.fail "unproven positive web level was enabled"
      | Error message ->
          Alcotest.(check bool)
            "fixed unsupported result" true
            (contains_substring message "positive web access is unsupported"))
    [Backend_types.Web_search; Backend_types.Web_search_and_fetch]

let test_build_invocation_rejects_invalid_resume_ids () =
  List.iter
    (fun invalid_id ->
      match
        Gemini_cli.build_invocation
          ~attachment_delivery:Backend_types.Upload_attachments
          ~web_policy_path:policy_path ~mcp_config_path:None
          (command_spec ~resume_session_id:invalid_id ())
      with
      | Ok _ -> Alcotest.fail "invalid resume id was accepted"
      | Error message ->
          Alcotest.(check bool)
            "resume error is fixed" true
            (contains_substring message "resume session id is invalid");
          if invalid_id <> "" then
            Alcotest.(check bool)
              "resume error omits the value" false
              (contains_substring message invalid_id))
    [""; "latest"; "--resume"; valid_session_id ^ "\n"]

let test_build_invocation_rejects_invalid_policy_paths () =
  List.iter
    (fun invalid_path ->
      match
        Gemini_cli.build_invocation
          ~attachment_delivery:Backend_types.Upload_attachments
          ~web_policy_path:invalid_path ~mcp_config_path:None (command_spec ())
      with
      | Ok _ -> Alcotest.fail "invalid web policy path was accepted"
      | Error message ->
          Alcotest.(check bool)
            "policy error is fixed" true
            (contains_substring message "task web policy path is invalid");
          if invalid_path <> "" then
            Alcotest.(check bool)
              "policy error omits path" false
              (contains_substring message invalid_path))
    [""; "relative.toml"; "/private/comma,policy.toml"; "/private/bad\n.toml"]

let test_compatibility_command_matches_baseline_stdin_contract () =
  let argv, stdin =
    Gemini_cli.build_command ~mcp_config_path:None
      (command_spec ~model:"gemini-3-flash-preview" ())
  in
  Alcotest.(check (list string))
    "compatibility argv"
    [
      "gemini";
      "--output-format";
      "stream-json";
      "-y";
      "-m";
      "gemini-3-flash-preview";
      "-p";
      "";
    ]
    argv;
  Alcotest.(check bool)
    "baseline has no skip-trust" false
    (command_contains "--skip-trust" argv);
  Alcotest.(check string) "compatibility stdin" expected_plain_stdin stdin

let test_redacted_invocation_hides_private_values () =
  let invocation =
    build_invocation
      (command_spec ~model:"private-model"
         ~resume_session_id:valid_session_id ())
  in
  let rendered = String.concat " " invocation.redacted_argv in
  List.iter
    (fun secret ->
      Alcotest.(check bool)
        "diagnostic argv omits private value" false
        (contains_substring rendered secret))
    [
      policy_path;
      "private-model";
      valid_session_id;
      "private prompt";
      "workspace/original";
    ];
  Alcotest.(check (list string))
    "redacted argv keeps fixed structure"
    [
      "gemini";
      "--output-format";
      "stream-json";
      "-y";
      "--policy";
      "<web-policy>";
      "--admin-policy";
      "<web-policy>";
      "-m";
      "<model>";
      "--resume";
      "<session-id>";
      "-p";
      "";
    ]
    invocation.redacted_argv

let test_web_policy_file_is_fixed_private_and_scoped () =
  let observed_path = ref None in
  Gemini_cli.with_web_disabled_policy_file (fun path ->
      observed_path := Some path;
      Alcotest.(check bool) "policy exists" true (Sys.file_exists path);
      Alcotest.(check int)
        "policy is private" 0o600
        ((Unix.stat path).st_perm land 0o777);
      Alcotest.(check string)
        "exact deny policy"
        "[[rule]]\ntoolName = [\"google_web_search\", \"web_fetch\"]\ndecision = \
         \"deny\"\npriority = 999\n"
        (read_file path));
  match !observed_path with
  | Some path ->
      Alcotest.(check bool) "policy removed after scope" false
        (Sys.file_exists path)
  | None -> Alcotest.fail "policy callback did not run"

let test_web_policy_file_is_removed_after_exception () =
  let observed_path = ref None in
  (try
     Gemini_cli.with_web_disabled_policy_file (fun path ->
         observed_path := Some path;
         raise Exit)
   with Exit -> ());
  match !observed_path with
  | Some path ->
      Alcotest.(check bool)
        "policy removed after exception" false (Sys.file_exists path)
  | None -> Alcotest.fail "policy callback did not run"

let test_standard_admin_policy_conflict_detection () =
  with_temp_dir "admin-policy" @@ fun directory ->
  Alcotest.(check bool)
    "empty standard directory" false
    (Gemini_cli.standard_admin_policy_conflict ~directory);
  let unrelated = Filename.concat directory "README.txt" in
  let channel = open_out unrelated in
  close_out channel;
  Alcotest.(check bool)
    "non-policy file ignored" false
    (Gemini_cli.standard_admin_policy_conflict ~directory);
  let policy = Filename.concat directory "global.toml" in
  let channel = open_out policy in
  close_out channel;
  Alcotest.(check bool)
    "standard policy conflicts" true
    (Gemini_cli.standard_admin_policy_conflict ~directory)

let command_tests =
  [
    ( "zero images exact argv/stdin",
      `Quick,
      test_build_invocation_zero_images_exact_structure );
    ( "unproven media remains unsupported",
      `Quick,
      test_build_invocation_rejects_unproven_media );
    ( "attachment-free resume reuse",
      `Quick,
      test_build_invocation_preserves_attachment_free_resume_reuse );
    ( "non-native schema retry preserved",
      `Quick,
      test_build_invocation_preserves_non_native_schema_retry_prompt );
    ( "positive web levels remain unsupported",
      `Quick,
      test_build_invocation_rejects_unproven_positive_web_levels );
    ( "invalid resume IDs rejected",
      `Quick,
      test_build_invocation_rejects_invalid_resume_ids );
    ( "invalid policy paths rejected",
      `Quick,
      test_build_invocation_rejects_invalid_policy_paths );
    ( "compatibility command uses baseline stdin contract",
      `Quick,
      test_compatibility_command_matches_baseline_stdin_contract );
    ( "diagnostic argv is redacted",
      `Quick,
      test_redacted_invocation_hides_private_values );
    ( "web-disabled policy is private and scoped",
      `Quick,
      test_web_policy_file_is_fixed_private_and_scoped );
    ( "web-disabled policy cleans exceptional exits",
      `Quick,
      test_web_policy_file_is_removed_after_exception );
    ( "standard admin policy conflict is detected",
      `Quick,
      test_standard_admin_policy_conflict_detection );
  ]

(** {1 Fail-before-side-effect transport gate} *)

let test_sensitive_low_level_calls_fail_before_config_or_spawn () =
  with_temp_dir "authorization" @@ fun temp_dir ->
  let marker = Filename.concat temp_dir "gemini-ran" in
  write_executable
    (Filename.concat temp_dir "gemini")
    (Printf.sprintf "#!/bin/sh\nprintf ran > %s\nexit 99\n"
       (Filename.quote marker));
  with_path_prefix temp_dir @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let attachment = media_attachment Backend_types.Png in
  let run ?context spec = Gemini_cli.run_task ~sw ~env ?context spec in
  let assert_rejected label result =
    (match result.Backend_types.status with
    | Backend_types.Failed message ->
        Alcotest.(check bool)
          (label ^ " fixed authorization failure") true
          (contains_substring message
             "central prepared transport authorization is required");
        Alcotest.(check bool)
          (label ^ " omits attachment path") false
          (contains_substring message "media/original.png")
    | _ -> Alcotest.fail (label ^ " was not rejected"));
    Alcotest.(check bool)
      (label ^ " creates no config") false
      (Sys.file_exists (Filename.concat temp_dir ".gemini/settings.json"));
    Alcotest.(check bool)
      (label ^ " spawns no process") false (Sys.file_exists marker)
  in
  assert_rejected "attachment"
    (run
       (Backend_types.make_task_spec ~prompt:"must not run"
          ~working_dir:temp_dir ~attachments:[attachment] ()));
  assert_rejected "positive web"
    (run
       (Backend_types.make_task_spec ~prompt:"must not run"
          ~working_dir:temp_dir ~web_access:Backend_types.Web_search ()));
  let sink = Task_event.create_sink ~sw ~now:(fun () -> 0.0) () in
  let untrusted_context =
    Task_execution_context.create ~remaining_time:(fun () -> None) sink
  in
  assert_rejected "untrusted context"
    (run ~context:untrusted_context
       (Backend_types.make_task_spec ~prompt:"must not run"
          ~working_dir:temp_dir ~attachments:[attachment] ()))

let test_invalid_session_fails_before_config_or_spawn () =
  with_temp_dir "invalid-session" @@ fun temp_dir ->
  let marker = Filename.concat temp_dir "gemini-ran" in
  write_executable
    (Filename.concat temp_dir "gemini")
    (Printf.sprintf "#!/bin/sh\nprintf ran > %s\nexit 99\n"
       (Filename.quote marker));
  with_path_prefix temp_dir @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result =
    Gemini_cli.run_task ~sw ~env
      (Backend_types.make_task_spec ~prompt:"must not run"
         ~working_dir:temp_dir ~resume_session_id:"--latest" ())
  in
  (match result.status with
  | Backend_types.Failed message ->
      Alcotest.(check bool)
        "fixed session failure" true
        (contains_substring message "resume session id is invalid")
  | _ -> Alcotest.fail "invalid session was not rejected");
  Alcotest.(check bool)
    "no config" false
    (Sys.file_exists (Filename.concat temp_dir ".gemini/settings.json"));
  Alcotest.(check bool) "no spawn" false (Sys.file_exists marker)

let transport_gate_tests =
  [
    ( "sensitive low-level calls fail before side effects",
      `Quick,
      test_sensitive_low_level_calls_fail_before_config_or_spawn );
    ( "invalid session fails before side effects",
      `Quick,
      test_invalid_session_fails_before_config_or_spawn );
  ]

(** {1 Probe artifact} *)

let project_root () =
  match Sys.getenv_opt "PROJECT_ROOT" with
  | Some root -> root
  | None -> (
      let rec find directory =
        if Sys.file_exists (Filename.concat directory "dune-project") then
          Some directory
        else
          let parent = Filename.dirname directory in
          if parent = directory then None else find parent
      in
      let rec source_root directory =
        let parent = Filename.dirname directory in
        if parent = directory then None
        else if Filename.basename directory = "_build" then Some parent
        else source_root parent
      in
      let resolve directory =
        if not (List.mem "_build" (String.split_on_char '/' directory)) then
          Some directory
        else
          match source_root directory with
          | Some root
            when Sys.file_exists (Filename.concat root "dune-project") ->
              Some root
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
            match find start with None -> None | Some root -> resolve root)
          starts
      with
      | Some root -> root
      | None -> Sys.getcwd ())

let test_media_web_probe_offline_self_test () =
  let path =
    Filename.concat (project_root ()) "tools/probe_gemini_media_web.py"
  in
  Alcotest.(check bool) "probe exists" true (Sys.file_exists path);
  Alcotest.(check bool)
    "probe is executable" true
    ((Unix.stat path).st_perm land 0o111 <> 0);
  Alcotest.(check int)
    "offline self-test" 0
    (Sys.command (Printf.sprintf "%s --self-test" (Filename.quote path)))

let probe_tests =
  [
    ( "media/web probe offline self-test",
      `Quick,
      test_media_web_probe_offline_self_test );
  ]

(** {1 Availability and interface} *)

let test_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let (_ : bool) = Gemini_cli.available ~sw ~env in
  ()

let test_implements_agentic_backend () =
  let backend = (module Gemini_cli : Agentic_backend.S) in
  Alcotest.(check string)
    "id via interface" "gemini-cli" (Agentic_backend.id backend);
  Alcotest.(check string)
    "name via interface" "Gemini CLI" (Agentic_backend.name backend)

let () =
  Alcotest.run "Gemini_cli"
    [
      ("Identity", identity_tests);
      ("Public output", output_tests);
      ("Invocation", command_tests);
      ("Transport gate", transport_gate_tests);
      ("Probe", probe_tests);
      ("Availability", [("available check", `Quick, test_available)]);
      ( "Interface",
        [("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend)]
      );
    ]
