(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the OpenCode CLI backend. *)

open Cabal

(** {1 Helpers} *)

let with_tmpdir f =
  let dir = Filename.temp_dir "epure_test_oc_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let write_file path content =
  let oc = open_out path in
  output_string oc content ;
  close_out oc

let rec ensure_parent_dir path =
  let dir = Filename.dirname path in
  if dir = path || dir = "." then ()
  else begin
    ensure_parent_dir dir ;
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755
  end

let write_file_creating_dirs path content =
  ensure_parent_dir path ;
  write_file path content

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

let with_cwd path f =
  let previous = Sys.getcwd () in
  Unix.chdir path ;
  Fun.protect ~finally:(fun () -> Unix.chdir previous) f

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value ;
  Fun.protect
    ~finally:(fun () -> Unix.putenv name (Option.value ~default:"" previous))
    f

let rec with_env_bindings bindings f =
  match bindings with
  | [] -> f ()
  | (name, value) :: rest ->
      with_env name value (fun () -> with_env_bindings rest f)

let write_executable path contents =
  write_file path contents ;
  Unix.chmod path 0o700

let write_process_group_launcher path =
  write_executable path
    {|#!/usr/bin/env python3
import os
import subprocess
import sys

os.setsid()
handshake = os.fdopen(3, "w", buffering=1)
control = os.fdopen(4, "r", buffering=1)
status = os.fdopen(5, "w", buffering=1)
handshake.write(f"PGID {os.getpid()}\n")
if control.readline() != "ACK\n":
    handshake.write("ERROR missing ACK\n")
    sys.exit(1)
process = subprocess.Popen(sys.argv[2:])
os.close(0)
os.close(1)
os.close(2)
handshake.write("EXEC\n")
handshake.close()
returncode = process.wait()
if returncode >= 0:
    status.write(f"EXIT {returncode}\n")
else:
    status.write(f"SIGNAL {-returncode}\n")
status.close()
for command in control:
    if command == "RELEASE\n":
        break
|}

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n ;
  close_in ic ;
  Bytes.to_string buf

let contains_str s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let count_equal_line expected content =
  content |> String.split_on_char '\n'
  |> List.fold_left
       (fun count line -> if line = expected then count + 1 else count)
       0

let strip_line_comment_headers content =
  content |> String.split_on_char '\n'
  |> List.filter (fun line ->
      not (String.starts_with ~prefix:"//" (String.trim line)))
  |> String.concat "\n"

(** {1 Module Identity Tests} *)

let test_id () = Alcotest.(check string) "id" "opencode" Opencode_cli.id

let test_name () = Alcotest.(check string) "name" "OpenCode" Opencode_cli.name

let identity_tests =
  [("id is opencode", `Quick, test_id); ("name is OpenCode", `Quick, test_name)]

(** {1 JSON Events Parsing Tests} *)

let valid_session_id = "ses_123456789abcABCDEFGHIJKLMN"

let second_session_id = "ses_abcdef123456ABCDEFGHIJKLMN"

let valid_message_id = "msg_123456789abcABCDEFGHIJKLMN"

let user_message_id = "msg_abcdef123456ABCDEFGHIJKLMN"

let valid_part_id = "prt_123456789abcABCDEFGHIJKLMN"

let second_part_id = "prt_abcdef123456ABCDEFGHIJKLMN"

let event ?(timestamp = `Int 1) ?(session_id = valid_session_id) type_ part =
  `Assoc
    [
      ("type", `String type_);
      ("timestamp", timestamp);
      ("sessionID", `String session_id);
      ("part", part);
    ]
  |> Yojson.Safe.to_string

let part ?(part_id = valid_part_id) ?(session_id = valid_session_id)
    ?(message_id = valid_message_id) type_ fields =
  `Assoc
    (("id", `String part_id)
    :: ("sessionID", `String session_id)
    :: ("messageID", `String message_id)
    :: ("type", `String type_)
    :: fields)

let step_start ?timestamp ?session_id ?part_id ?message_id () =
  event ?timestamp ?session_id "step_start"
    (part ?part_id ?session_id ?message_id "step-start" [])

let completed_text ?timestamp ?session_id ?part_id ?message_id text =
  event ?timestamp ?session_id "text"
    (part ?part_id ?session_id ?message_id "text"
       [
         ("text", `String text);
         ("time", `Assoc [("start", `Int 1); ("end", `Int 2)]);
       ])

let step_finish ?timestamp ?session_id ?part_id ?message_id ?(input = `Int 3)
    ?(output = `Int 2) ?(reason = `String "stop") ?(cost = `Float 0.01) () =
  event ?timestamp ?session_id "step_finish"
    (part ?part_id ?session_id ?message_id "step-finish"
       [
         ("reason", reason);
         ("cost", cost);
         ( "tokens",
           `Assoc
             [
               ("total", `Int 5);
               ("input", input);
               ("output", output);
               ("reasoning", `Int 0);
               ("cache", `Assoc [("read", `Int 0); ("write", `Int 0)]);
             ] );
       ])

let test_parse_json_events_with_content () =
  let finish =
    event ~timestamp:(`Int 4) "step_finish"
      (part ~part_id:"prt_3456789abcdeABCDEFGHIJKLMN" "step-finish"
         [
           ("reason", `String "stop");
           ("cost", `Float 0.005);
           ( "tokens",
             `Assoc
               [
                 ("total", `Int 429);
                 ("input", `Int 300);
                 ("output", `Int 120);
                 ("reasoning", `Int 9);
                 ("cache", `Assoc [("read", `Int 40); ("write", `Int 10)]);
               ] );
         ])
  in
  let input =
    String.concat "\n"
      [
        step_start ();
        completed_text ~timestamp:(`Int 2) "First chunk";
        completed_text ~timestamp:(`Int 3)
          ~part_id:"prt_23456789abcdABCDEFGHIJKLMN" " second chunk";
        finish;
      ]
  in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "concatenated text" "First chunk second chunk" text ;
  Alcotest.(check bool) "cost present" true (Option.is_some cost) ;
  match cost with
  | Some c ->
      Alcotest.(check (option int)) "input_tokens" (Some 300) c.tokens_input ;
      Alcotest.(check (option int)) "output_tokens" (Some 120) c.tokens_output ;
      Alcotest.(check (option int))
        "cache read tokens"
        (Some 40)
        c.cache_read_input_tokens ;
      Alcotest.(check (option int))
        "cache write tokens"
        (Some 10)
        c.cache_creation_input_tokens ;
      Alcotest.(check bool) "cost_usd present" true (Option.is_some c.cost_usd)
  | None -> Alcotest.fail "Expected cost to be Some"

let test_parse_json_events_text_field () =
  let input =
    String.concat "\n" [step_start (); completed_text "Hello from OpenCode"]
  in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "text field" "Hello from OpenCode" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_json_events_no_usage () =
  let input = String.concat "\n" [step_start (); completed_text "Simple response"] in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "content text" "Simple response" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_json_events_empty () =
  let text, cost = Opencode_cli.parse_json_events "" in
  Alcotest.(check string) "empty fallback" "" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_json_events_malformed () =
  let input = "garbage data\n{not valid json" in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "malformed fallback" "" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let payload_kind = function
  | Task_event.Session_id _ -> "session"
  | Agent_text_delta _ -> "agent"
  | Tool_started _ -> "tool-started"
  | Tool_finished _ -> "tool-finished"
  | Token_usage _ -> "usage"
  | _ -> "other"

let test_normalized_events_public_protocol_records () =
  let lines =
    [
      step_start ();
      completed_text "public answer";
      event ~timestamp:(`Int 3) "tool_use"
        (part ~part_id:second_part_id "tool"
           [
             ("callID", `String "call-safe_1");
             ("tool", `String "webfetch");
             ( "state",
               `Assoc
                 [
                   ("status", `String "completed");
                   ("input", `Assoc [("url", `String "private input")]);
                   ("output", `String "private result");
                   ("title", `String "private title");
                   ("metadata", `Assoc []);
                   ("time", `Assoc [("start", `Int 1); ("end", `Int 2)]);
                 ] );
           ]);
      event ~timestamp:(`Int 4) "step_finish"
        (part ~part_id:"prt_3456789abcdeABCDEFGHIJKLMN" "step-finish"
           [
             ("reason", `String "stop");
             ("cost", `Float 0.01);
             ( "tokens",
               `Assoc
                 [
                   ("total", `Int 120);
                   ("input", `Int 12);
                   ("output", `Int 3);
                   ("reasoning", `Int 99);
                   ("cache", `Assoc [("read", `Int 4); ("write", `Int 2)]);
                 ] );
           ]);
    ]
  in
  let events = List.concat_map Opencode_cli.normalized_events_of_line lines in
  Alcotest.(check (list string))
    "normalized public record kinds"
    ["session"; "agent"; "tool-finished"; "usage"]
    (List.map payload_kind events) ;
  match events with
  | [
   Task_event.Session_id session_id;
   Agent_text_delta "public answer";
   Tool_finished {id = Some "call-safe_1"; name = Some "webfetch"};
   Token_usage usage;
  ] ->
      Alcotest.(check string) "canonical session id" valid_session_id session_id ;
      Alcotest.(check (option int)) "input usage" (Some 12) usage.tokens_input ;
      Alcotest.(check (option int)) "output usage" (Some 3) usage.tokens_output ;
      Alcotest.(check (option int))
        "cache read usage"
        (Some 4)
        usage.cache_read_input_tokens ;
      Alcotest.(check (option int))
        "cache write usage"
        (Some 2)
        usage.cache_creation_input_tokens
  | _ -> Alcotest.fail "unexpected normalized event payloads"

let test_normalized_events_drop_private_and_raw_records () =
  let lines =
    [
      Printf.sprintf
        {|{"type":"reasoning","sessionID":"%s","part":{"type":"reasoning","text":"private reasoning","time":{"end":2}}}|}
        valid_session_id;
      Printf.sprintf
        {|{"type":"text","sessionID":"%s","part":{"type":"text","text":"private user text"}}|}
        valid_session_id;
      Printf.sprintf
        {|{"type":"error","sessionID":"%s","error":{"message":"private error /private/image.png"}}|}
        valid_session_id;
      Printf.sprintf
        {|{"type":"tool_use","sessionID":"%s","part":{"type":"tool","callID":"call-error","tool":"webfetch","state":{"status":"error","input":{"url":"private input"},"error":"private tool error"}}}|}
        valid_session_id;
      Printf.sprintf
        {|{"type":"tool_use","sessionID":"%s","part":{"type":"tool","callID":"/private/call","tool":"private tool name","state":{"status":"completed","input":{},"output":"private result"}}}|}
        valid_session_id;
      Printf.sprintf
        {|{"type":"step_start","sessionID":"%s","part":{"type":"step-start"}}|}
        "/private/session";
      "raw fallback must stay private";
    ]
  in
  Alcotest.(check (list string))
    "private/error/raw records produce no normalized events"
    []
    (List.concat_map Opencode_cli.normalized_events_of_line lines
    |> List.map payload_kind) ;
  let stdout = String.concat "\n" lines in
  let text, cost = Opencode_cli.parse_json_events stdout in
  Alcotest.(check string) "private text is not promoted" "" text ;
  Alcotest.(check bool) "private usage is absent" true (Option.is_none cost) ;
  Alcotest.(check string)
    "runtime parser has no raw fallback"
    ""
    (Opencode_cli.parse_stdout_text stdout)

let test_strict_envelope_rejects_missing_or_invalid_fields () =
  let missing_part_identity =
    {|{"type":"text","timestamp":1,"sessionID":"ses_123456789abcABCDEFGHIJKLMN","part":{"type":"text","text":"must stay private","time":{"start":1,"end":2}}}|}
  in
  let cases =
    [
      missing_part_identity;
      completed_text ~timestamp:(`Int (-1)) "negative timestamp";
      completed_text ~timestamp:(`String "1") "string timestamp";
      event "text"
        (part "text"
           [
             ("text", `String "missing completion start");
             ("time", `Assoc [("end", `Int 2)]);
           ]);
      event "text"
        (part "text"
           [
             ("text", `String "reversed completion time");
             ("time", `Assoc [("start", `Int 3); ("end", `Int 2)]);
           ]);
      step_finish ~reason:`Null ();
      step_finish ~input:(`Int (-1)) ();
    ]
  in
  List.iter
    (fun line ->
      Alcotest.(check (list string))
        "invalid baseline envelope emits no public event" []
        (Opencode_cli.normalized_events_of_line line |> List.map payload_kind))
    cases

let test_strict_stream_rejects_mixed_sessions () =
  let input =
    String.concat "\n"
      [
        step_start ();
        completed_text ~timestamp:(`Int 2) ~session_id:second_session_id
          "cross-session text";
        step_finish ~timestamp:(`Int 3) ();
      ]
  in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "mixed-session text rejected" "" text ;
  Alcotest.(check bool) "mixed-session usage rejected" true (Option.is_none cost) ;
  Alcotest.(check (option string))
    "mixed session has no public identity" None
    (Opencode_cli.parse_public_session_id input)

let test_strict_stream_rejects_adversarial_user_text () =
  let input =
    String.concat "\n"
      [
        step_start ();
        completed_text ~timestamp:(`Int 2) ~message_id:user_message_id
          "forged completed user text";
        step_finish ~timestamp:(`Int 3) ();
      ]
  in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "user message is not assistant output" "" text ;
  Alcotest.(check bool) "invalid stream usage rejected" true (Option.is_none cost)

let test_strict_stream_accepts_sequential_assistant_messages () =
  let input =
    String.concat "\n"
      [
        step_start ();
        step_finish ~timestamp:(`Int 2) ();
        step_start ~timestamp:(`Int 3) ~message_id:user_message_id ();
        completed_text ~timestamp:(`Int 4) ~message_id:user_message_id
          "assistant after tool turn";
        step_finish ~timestamp:(`Int 5) ~message_id:user_message_id ();
      ]
  in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string)
    "later assistant text retained" "assistant after tool turn" text ;
  Alcotest.(check bool) "both assistant usages retained" true (Option.is_some cost) ;
  Alcotest.(check (option string))
    "session remains stable"
    (Some valid_session_id)
    (Opencode_cli.parse_public_session_id input)

let test_legacy_minimal_normalizer_is_not_runtime_public_text () =
  let line = {|{"type":"text","part":{"text":"legacy fixture"}}|} in
  Alcotest.(check bool)
    "legacy normalization API remains source-compatible"
    true
    (Opencode_cli.normalized_events_of_line line
    = [Task_event.Agent_text_delta "legacy fixture"]) ;
  let text, cost = Opencode_cli.parse_json_events line in
  Alcotest.(check string) "legacy shape is not runtime agent text" "" text ;
  Alcotest.(check bool) "legacy shape has no usage" true (Option.is_none cost)

let test_session_parser_accepts_only_canonical_opencode_ids () =
  let valid = step_start () in
  Alcotest.(check (option string))
    "canonical OpenCode id"
    (Some valid_session_id)
    (Opencode_cli.parse_public_session_id valid) ;
  List.iter
    (fun invalid_id ->
      let line =
        step_start ~session_id:invalid_id ()
      in
      Alcotest.(check (option string))
        "invalid OpenCode id is rejected"
        None
        (Opencode_cli.parse_public_session_id line))
    [
      "";
      "--continue";
      "ses_short";
      "SES_123456789abcABCDEFGHIJKLMN";
      "ses_123456789abzABCDEFGHIJKLMN";
      "ses_123456789abcABCDEFGHIJKLMN\n";
      String.make 129 'a';
    ]

let test_token_fields_are_bounded_and_aggregate_saturates () =
  let above_max_int = Int64.(to_string (add (of_int Stdlib.max_int) 1L)) in
  let invalid =
    [
      step_finish ~input:(`Int (-1)) ~output:(`Int (-2)) ~cost:(`Int (-1)) ();
      step_finish ~input:(`String "1") ~output:(`Float 1.5)
        ~cost:(`String "bad") ();
      Printf.sprintf
        {|{"type":"step_finish","timestamp":2,"sessionID":"%s","part":{"id":"%s","sessionID":"%s","messageID":"%s","type":"step-finish","reason":"stop","tokens":{"total":0,"input":%s,"output":%s,"reasoning":0,"cache":{"read":%s,"write":%s}},"cost":null}}|}
        valid_session_id valid_part_id valid_session_id valid_message_id
        above_max_int above_max_int above_max_int above_max_int;
    ]
  in
  List.iter
    (fun line ->
      Alcotest.(check (list string))
        "invalid usage emits no event"
        []
        (Opencode_cli.normalized_events_of_line line |> List.map payload_kind) ;
      let _, cost = Opencode_cli.parse_json_events line in
      Alcotest.(check bool) "invalid usage emits no cost" true (Option.is_none cost))
    invalid ;
  let usage value =
    event "step_finish"
      (part "step-finish"
         [
           ("reason", `String "stop");
           ("cost", `Int 0);
           ( "tokens",
             `Assoc
               [
                 ("total", `Int value);
                 ("input", `Int value);
                 ("output", `Int value);
                 ("reasoning", `Int 0);
                 ("cache", `Assoc [("read", `Int value); ("write", `Int value)]);
               ] );
         ])
  in
  let _, cost =
    Opencode_cli.parse_json_events
      (String.concat "\n"
         [step_start (); usage Stdlib.max_int; usage 1])
  in
  match cost with
  | Some cost ->
      Alcotest.(check (list (option int)))
        "token totals saturate without wrapping"
        [Some Stdlib.max_int; Some Stdlib.max_int; Some Stdlib.max_int; Some Stdlib.max_int]
        [
          cost.tokens_input;
          cost.tokens_output;
          cost.cache_read_input_tokens;
          cost.cache_creation_input_tokens;
        ]
  | None -> Alcotest.fail "saturated usage was discarded"

let json_events_tests =
  [
    ("parse with content and usage", `Quick, test_parse_json_events_with_content);
    ("parse text field", `Quick, test_parse_json_events_text_field);
    ("parse without usage", `Quick, test_parse_json_events_no_usage);
    ("parse empty output", `Quick, test_parse_json_events_empty);
    ("parse malformed output", `Quick, test_parse_json_events_malformed);
    ( "normalize public protocol records",
      `Quick,
      test_normalized_events_public_protocol_records );
    ( "drop private and raw records",
      `Quick,
      test_normalized_events_drop_private_and_raw_records );
    ( "reject missing and invalid baseline envelopes",
      `Quick,
      test_strict_envelope_rejects_missing_or_invalid_fields );
    ( "reject mixed-session streams",
      `Quick,
      test_strict_stream_rejects_mixed_sessions );
    ( "reject completed user text",
      `Quick,
      test_strict_stream_rejects_adversarial_user_text );
    ( "accept sequential assistant messages",
      `Quick,
      test_strict_stream_accepts_sequential_assistant_messages );
    ( "isolate legacy minimal normalized text",
      `Quick,
      test_legacy_minimal_normalizer_is_not_runtime_public_text );
    ( "accept canonical session identifiers only",
      `Quick,
      test_session_parser_accepts_only_canonical_opencode_ids );
    ( "bound and saturate token usage",
      `Quick,
      test_token_fields_are_bounded_and_aggregate_saturates );
  ]

(** {1 Availability Tests} *)

let test_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Just verify available runs without raising; check logic tested in
     test_backend_process.ml via check_available *)
  let (_ : bool) = Opencode_cli.available ~sw ~env in
  ()

let availability_tests = [("available check", `Quick, test_available)]

(** {1 Command Construction Tests} *)

let media_attachment ?(id = "image") ?(path = "media/cover.png") media_type :
    Backend_types.media_attachment =
  {id; path; media_type; sha256 = String.make 64 '0'; size_bytes = 1}

let minimal_spec ?model ?(attachments = []) ?(working_dir = "/tmp")
    ?(web_access = Backend_types.Web_disabled) ?resume_session_id ?json_schema () =
  Backend_types.make_task_spec ~prompt:"test" ~working_dir ?model
    ~attachments ~web_access ?resume_session_id ?json_schema ()

let has_adjacent_args flag value cmd =
  let rec loop = function
    | first :: second :: _ when first = flag && second = value -> true
    | _ :: rest -> loop rest
    | [] -> false
  in
  loop cmd

let test_build_command_includes_model_flag () =
  let cmd, _stdin =
    Opencode_cli.build_command
      ~mcp_config_path:None
      (minimal_spec ~model:"openai/gpt-4o-mini" ())
  in
  Alcotest.(check bool)
    "includes -m model"
    true
    (has_adjacent_args "-m" "openai/gpt-4o-mini" cmd)

let web_permission_json = function
  | Backend_types.Web_disabled ->
      {|{"websearch":"deny","webfetch":"deny","codesearch":"deny","task":"deny"}|}
  | Web_search ->
      {|{"websearch":"allow","webfetch":"deny","codesearch":"deny","task":"deny"}|}
  | Web_search_and_fetch ->
      {|{"websearch":"allow","webfetch":"allow","codesearch":"deny","task":"deny"}|}

let web_config_json policy =
  let permissions = web_permission_json policy in
  Printf.sprintf
    {|{"share":"disabled","permission":%s,"agent":{"build":{"mode":"primary","permission":%s}}}|}
    permissions permissions

let expected_root ?model ?(web_access = Backend_types.Web_disabled)
    ?(redacted = false) () =
  let project_config = if redacted then "<project-config>" else "/tmp/opencode.json" in
  [
    "env";
    "-u";
    "OPENCODE_DB";
    "OPENCODE_PERMISSION=" ^ web_permission_json web_access;
    "OPENCODE_CONFIG_CONTENT=" ^ web_config_json web_access;
    "OPENCODE_CONFIG=" ^ project_config;
    "OPENCODE_CONFIG_DIR=";
    "OPENCODE_DISABLE_PROJECT_CONFIG=1";
    "OPENCODE_EXPERIMENTAL=0";
    "OPENCODE_EXPERIMENTAL_EXA=0";
    ( "OPENCODE_ENABLE_EXA="
    ^ if web_access = Backend_types.Web_disabled then "0" else "1" );
    "OPENCODE_AUTO_SHARE=0";
    "OPENCODE_DISABLE_AUTOUPDATE=1";
    "OPENCODE_DISABLE_LSP_DOWNLOAD=1";
    "opencode";
    "run";
    "--pure";
    "--format";
    "json";
    "--agent";
    "build";
  ]
  @ match model with Some value -> ["-m"; value] | None -> []

let build_invocation ?attachment_paths
    ?(attachment_delivery = Backend_types.Upload_attachments) spec =
  match
    Opencode_cli.build_invocation ?attachment_paths ~attachment_delivery
      ~mcp_config_path:None spec
  with
  | Ok invocation -> invocation
  | Error message -> Alcotest.fail message

let sealed_png_path = "/private/cabal task inputs/attachment one.png"
let sealed_jpeg_path = "/private/cabal task inputs/attachment two.jpg"

let test_build_invocation_exact_repeated_file_argv () =
  let attachments =
    [
      media_attachment ~id:"front" ~path:"media/front cover.png"
        Backend_types.Png;
      media_attachment ~id:"back" ~path:"media/back cover.jpg"
        Backend_types.Jpeg;
    ]
  in
  let invocation =
    build_invocation ~attachment_paths:[sealed_png_path; sealed_jpeg_path]
      (minimal_spec ~model:"openai/gpt-5.4-mini" ~attachments ())
  in
  Alcotest.(check (list string))
    "repeated --file keeps each sealed path as one argv element"
    (expected_root ~model:"openai/gpt-5.4-mini" ()
    @ ["--file"; sealed_png_path; "--file"; sealed_jpeg_path; "-"])
    invocation.argv ;
  Alcotest.(check string) "prompt stays on stdin" "test"
    (Option.value ~default:"" invocation.stdin)

let test_build_invocation_exact_web_policy_argv () =
  List.iter
    (fun policy ->
      let invocation = build_invocation (minimal_spec ~web_access:policy ()) in
      Alcotest.(check (list string))
        "fixed web policy argv"
        (expected_root ~web_access:policy () @ ["-"])
        invocation.argv)
    [
      Backend_types.Web_disabled;
      Backend_types.Web_search;
      Backend_types.Web_search_and_fetch;
    ]

let json_member_string name json =
  match Yojson.Safe.Util.member name json with
  | `String value -> value
  | _ -> Alcotest.failf "missing string field %s" name

let required_assignment_json prefix argv =
  match
    List.find_map
      (fun arg ->
        if String.starts_with ~prefix arg then
          Some
            (Yojson.Safe.from_string
               (String.sub arg (String.length prefix)
                  (String.length arg - String.length prefix)))
        else None)
      argv
  with
  | Some value -> value
  | None -> Alcotest.failf "missing assignment %s" prefix

let test_fixed_policy_denies_hostile_subagent_delegation () =
  let hostile_source source =
    `Assoc
      [
        ( "source",
          `String source );
        ("permission", `Assoc [("task", `String "allow")]);
        ( "agent",
          `Assoc
            [
              ( "explore",
                `Assoc
                  [
                    ("mode", `String "subagent");
                    ( "permission",
                      `Assoc
                        [
                          ("websearch", `String "allow");
                          ("webfetch", `String "allow");
                          ("codesearch", `String "allow");
                          ("task", `String "allow");
                        ] );
                  ] );
            ] );
      ]
  in
  List.iter
    (fun source ->
      let hostile = hostile_source source in
      let explore =
        hostile |> Yojson.Safe.Util.member "agent"
        |> Yojson.Safe.Util.member "explore"
        |> Yojson.Safe.Util.member "permission"
      in
      Alcotest.(check string)
        (source ^ " fixture allows subagent fetch") "allow"
        (json_member_string "webfetch" explore))
    ["project"; "account"; "managed"] ;
  List.iter
    (fun policy ->
      let invocation = build_invocation (minimal_spec ~web_access:policy ()) in
      let top_level =
        required_assignment_json "OPENCODE_PERMISSION=" invocation.argv
      in
      let config =
        required_assignment_json "OPENCODE_CONFIG_CONTENT=" invocation.argv
      in
      let invocation_agent =
        config |> Yojson.Safe.Util.member "agent"
        |> Yojson.Safe.Util.member "build"
        |> Yojson.Safe.Util.member "permission"
      in
      Alcotest.(check string)
        "top-level delegation ceiling" "deny"
        (json_member_string "task" top_level) ;
      Alcotest.(check string)
        "invocation-agent delegation ceiling" "deny"
        (json_member_string "task" invocation_agent))
    [
      Backend_types.Web_disabled;
      Backend_types.Web_search;
      Backend_types.Web_search_and_fetch;
    ]

let test_build_invocation_absolutizes_nested_relative_config_path () =
  with_tmpdir (fun root ->
      let relative_working_dir = Filename.concat "project" "./nested" in
      with_cwd root @@ fun () ->
      let invocation =
        build_invocation (minimal_spec ~working_dir:relative_working_dir ())
      in
      let expected =
        Filename.concat
          (Filename.concat root (Filename.concat "project" "nested"))
          "opencode.json"
      in
      Alcotest.(check bool)
        "explicit config path is absolute" true
        (has_adjacent_args
           ("OPENCODE_CONFIG=" ^ expected)
           "OPENCODE_CONFIG_DIR=" invocation.argv) ;
      Alcotest.(check bool)
        "redacted argv omits absolute workspace" false
        (contains_str (String.concat " " invocation.redacted_argv) root))

let test_build_invocation_rejects_invalid_working_dir_without_disclosure () =
  let private_path = "/private/attacker\000path" in
  match
    Opencode_cli.build_invocation ~mcp_config_path:None
      (minimal_spec ~working_dir:private_path ())
  with
  | Ok _ -> Alcotest.fail "invalid working directory was accepted"
  | Error message ->
      Alcotest.(check string)
        "fixed working-directory failure"
        "OpenCode invocation rejected: OpenCode working directory is invalid"
        message ;
      Alcotest.(check bool)
        "failure does not disclose working directory" false
        (contains_str message private_path)

let test_build_invocation_redacts_sensitive_values () =
  let attachment = media_attachment Backend_types.Png in
  let invocation =
    build_invocation ~attachment_paths:[sealed_png_path]
      (minimal_spec ~model:"private/model" ~attachments:[attachment] ())
  in
  Alcotest.(check (list string))
    "redacted argv retains only fixed values and placeholders"
    (expected_root ~model:"<model>" ~redacted:true ()
    @ ["--file"; "<attachment-1>"; "-"])
    invocation.redacted_argv ;
  let rendered = String.concat " " invocation.redacted_argv in
  List.iter
    (fun sensitive ->
      Alcotest.(check bool)
        "sensitive value omitted"
        false
        (contains_str rendered sensitive))
    [sealed_png_path; "private/model"; "private prompt"; "/tmp"]

let test_build_invocation_rejects_unsupported_resume_and_reuse () =
  Alcotest.(check bool)
    "runtime resume capability remains false"
    false
    Opencode_cli.supports_session_resume ;
  let attachment = media_attachment Backend_types.Png in
  let cases =
    [
      ( minimal_spec ~resume_session_id:valid_session_id (),
        Backend_types.Upload_attachments,
        [] );
      ( minimal_spec ~attachments:[attachment] (),
        Backend_types.Reuse_session_attachments,
        [] );
    ]
  in
  List.iter
    (fun (spec, attachment_delivery, attachment_paths) ->
      match
        Opencode_cli.build_invocation ~attachment_delivery ~attachment_paths
          ~mcp_config_path:None spec
      with
      | Ok _ -> Alcotest.fail "unsupported OpenCode resume/reuse was accepted"
      | Error message ->
          Alcotest.(check bool)
            "unsupported gate is explicit"
            true
            (contains_str message "unsupported"))
    cases

let test_build_invocation_rejects_unsealed_or_mismatched_images () =
  let attachments =
    [
      media_attachment Backend_types.Png;
      media_attachment ~id:"second" ~path:"media/back.jpg" Backend_types.Jpeg;
    ]
  in
  let spec = minimal_spec ~attachments () in
  List.iter
    (fun attachment_paths ->
      match
        Opencode_cli.build_invocation ~attachment_paths ~mcp_config_path:None
          spec
      with
      | Ok _ -> Alcotest.fail "unsealed or mismatched image paths were accepted"
      | Error message ->
          Alcotest.(check bool)
            "sealed-path mismatch is explicit"
            true
            (contains_str message "sealed attachment set"))
    [
      ["media/source.png"; sealed_jpeg_path];
      [sealed_png_path];
      [sealed_png_path; "/private/cabal task inputs/attachment two.png"];
      [sealed_png_path; sealed_jpeg_path ^ "\000suffix"];
    ]

let assert_failed_before_config_or_spawn label spec =
  with_tmpdir (fun dir ->
      let marker = Filename.concat dir "opencode-ran" in
      let fake = Filename.concat dir "opencode" in
      write_executable fake
        (Printf.sprintf
           "#!/bin/sh\nprintf 'ran\\n' > %s\nexit 99\n"
           (Filename.quote marker)) ;
      with_path_prefix dir @@ fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let result = Opencode_cli.run_task ~sw ~env spec in
      (match result.status with
      | Backend_types.Failed message ->
          Alcotest.(check bool)
            (label ^ " central authorization error")
            true
            (contains_str message "central prepared transport authorization")
      | _ -> Alcotest.fail (label ^ " sensitive request was not rejected")) ;
      Alcotest.(check bool)
        (label ^ " creates no config")
        false
        (Sys.file_exists (Filename.concat spec.working_dir "opencode.json")) ;
      Alcotest.(check bool)
        (label ^ " spawns no process")
        false
        (Sys.file_exists marker))

let test_sensitive_low_level_calls_fail_before_config_or_spawn () =
  with_tmpdir (fun workspace ->
      let attachment =
        media_attachment ~path:"private image.png" Backend_types.Png
      in
      assert_failed_before_config_or_spawn "attachment-only"
        (Backend_types.make_task_spec ~prompt:"must not run"
           ~working_dir:workspace ~attachments:[attachment] ()) ;
      assert_failed_before_config_or_spawn "web-only"
        (Backend_types.make_task_spec ~prompt:"must not run"
           ~working_dir:workspace ~web_access:Backend_types.Web_search ()))

let test_schema_retry_reuploads_same_sealed_image_at_most_twice () =
  with_tmpdir (fun workspace ->
      let image_bytes = "\x89PNG\r\n\x1a\npayload" in
      let relative_path = "media/source image.png" in
      write_file_creating_dirs
        (Filename.concat workspace relative_path)
        image_bytes ;
      let attachment : Backend_types.media_attachment =
        {
          id = "source";
          path = relative_path;
          media_type = Backend_types.Png;
          sha256 =
            "399b7cc9a888d40c8a3d09a9cb47c5a8a20932bee65c45792a2e4f5513beb3b0";
          size_bytes = String.length image_bytes;
        }
      in
      let schema =
        `Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc
                [
                  ( "answer",
                    `Assoc
                      [("type", `String "string"); ("const", `String "ok")] );
                ] );
            ("required", `List [`String "answer"]);
            ("additionalProperties", `Bool false);
          ]
      in
      let spec =
        Backend_types.make_task_spec ~prompt:"inspect the image"
          ~working_dir:workspace ~attachments:[attachment] ~json_schema:schema ()
      in
      let limits : Task_preflight.limits =
        {
          max_attachments = 1;
          max_file_size_bytes = 1024;
          max_total_size_bytes = 1024;
        }
      in
      let prepared =
        match Task_preflight.prepare_inputs ~limits spec with
        | Ok prepared -> prepared
        | Error error ->
            Alcotest.failf "input preparation failed: %s"
              (Task_preflight.render_error error)
      in
      Fun.protect
        ~finally:(fun () -> ignore (Task_preflight.release_inputs prepared))
        (fun () ->
          let sealed_path =
            match Task_preflight.Private.staged_attachments prepared with
            | [(_, path)] -> path
            | _ -> Alcotest.fail "expected one sealed attachment"
          in
          let capture = Filename.concat workspace "captured-argv" in
          let count = Filename.concat workspace "call-count" in
          let fake_dir = Filename.concat workspace "fake-bin" in
          Unix.mkdir fake_dir 0o700 ;
          let fake = Filename.concat fake_dir "opencode" in
          let launcher = Filename.concat fake_dir "process-group-launcher" in
          write_process_group_launcher launcher ;
          write_executable fake
            (Printf.sprintf
               {|#!/bin/sh
printf 'CALL\n' >> %s
printf '%%s\n' "$@" >> %s
if [ -f %s ]; then
  SESSION='ses_abcdef123456ABCDEFGHIJKLMN'
  TEXT='{\"answer\":\"ok\"}'
else
  : > %s
  SESSION='ses_123456789abcABCDEFGHIJKLMN'
  TEXT='not-json'
fi
printf '%%s\n' "{\"type\":\"step_start\",\"timestamp\":1,\"sessionID\":\"$SESSION\",\"part\":{\"id\":\"prt_123456789abcABCDEFGHIJKLMN\",\"sessionID\":\"$SESSION\",\"messageID\":\"msg_123456789abcABCDEFGHIJKLMN\",\"type\":\"step-start\"}}"
printf '%%s\n' "{\"type\":\"text\",\"timestamp\":2,\"sessionID\":\"$SESSION\",\"part\":{\"id\":\"prt_23456789abcdABCDEFGHIJKLMN\",\"sessionID\":\"$SESSION\",\"messageID\":\"msg_123456789abcABCDEFGHIJKLMN\",\"type\":\"text\",\"text\":\"$TEXT\",\"time\":{\"start\":1,\"end\":2}}}"
printf '%%s\n' "{\"type\":\"step_finish\",\"timestamp\":3,\"sessionID\":\"$SESSION\",\"part\":{\"id\":\"prt_3456789abcdeABCDEFGHIJKLMN\",\"sessionID\":\"$SESSION\",\"messageID\":\"msg_123456789abcABCDEFGHIJKLMN\",\"type\":\"step-finish\",\"reason\":\"stop\",\"tokens\":{\"total\":2,\"input\":1,\"output\":1,\"reasoning\":0,\"cache\":{\"read\":0,\"write\":0}},\"cost\":0}}"
|}
               (Filename.quote capture)
               (Filename.quote capture)
               (Filename.quote count)
               (Filename.quote count)) ;
          with_env "CABAL_PROCESS_GROUP_LAUNCHER" launcher @@ fun () ->
          with_path_prefix fake_dir @@ fun () ->
          Eio_posix.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let sink = Task_event.create_sink ~sw ~now:(fun () -> 0.0) () in
          let context =
            Task_execution_context.create ~remaining_time:(fun () -> None) sink
          in
          (match
             Task_execution_context.Private.authorize_transport context
               ~backend_id:Opencode_cli.id
               ~attachment_references:spec.attachments
               ~web_access_policy:spec.web_access ~prepared_inputs:prepared
           with
          | Ok () -> ()
          | Error message -> Alcotest.fail message) ;
          let detailed =
            Json_schema_enforcer.run_task_detailed ~sw ~env ~context
              ~backend:(module Opencode_cli : Agentic_backend.S) spec
          in
          let execution =
            match detailed with
            | Ok execution -> execution
            | Error error ->
                Alcotest.failf "schema retry failed: %s"
                  (Json_schema_enforcer.render_error error)
          in
          Alcotest.(check int)
            "exactly two backend attempts"
            2
            (List.length execution.attempts) ;
          Alcotest.(check (list string))
            "initial and fresh attempts only"
            ["initial"; "fresh"]
            (List.map
               (fun (attempt : Backend_types.task_attempt) ->
                 match attempt.kind with
                 | Initial_attempt -> "initial"
                 | Fresh_attempt -> "fresh"
                 | Resumed_attempt -> "resumed")
               execution.attempts) ;
          List.iter
            (fun (attempt : Backend_types.task_attempt) ->
              Alcotest.(check bool)
                "both attempts request attachment upload"
                true
                (attempt.delivery.attachment_delivery =
                 Backend_types.Upload_attachments))
            execution.attempts ;
          Alcotest.(check string)
            "fresh retry returns schema-valid public text"
            {|{"answer":"ok"}|}
            execution.final_result.agent_text ;
          let captured = read_file capture in
          Alcotest.(check int)
            "process call count"
            2
            (count_equal_line "CALL" captured) ;
          Alcotest.(check int)
            "fresh retry receives the same sealed path"
            2
            (count_equal_line sealed_path captured) ;
          Alcotest.(check int)
            "each invocation uses one repeated-file flag"
            2
            (count_equal_line "--file" captured) ;
          Alcotest.(check bool)
            "workspace-relative source path is never passed"
           false
           (contains_str captured relative_path)))

let test_exit_zero_error_jsonl_becomes_sanitized_failure () =
  with_tmpdir (fun workspace ->
      let fake_dir = Filename.concat workspace "fake-bin" in
      Unix.mkdir fake_dir 0o700 ;
      let private_payload = "/private/auth-token=must-not-escape" in
      write_executable
        (Filename.concat fake_dir "opencode")
        (Printf.sprintf
           {|#!/bin/sh
printf '%%s\n' '{"type":"error","timestamp":1,"sessionID":"ses_123456789abcABCDEFGHIJKLMN","error":{"name":"ProviderAuthError","data":{"message":"%s"}}}'
exit 0
|}
           private_payload) ;
      let launcher = Filename.concat fake_dir "process-group-launcher" in
      write_process_group_launcher launcher ;
      with_env "CABAL_PROCESS_GROUP_LAUNCHER" launcher @@ fun () ->
      with_path_prefix fake_dir @@ fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let result =
        Opencode_cli.run_task ~sw ~env
          (Backend_types.make_task_spec ~prompt:"trigger error"
             ~working_dir:workspace ())
      in
      (match result.status with
      | Backend_types.Failed message ->
          Alcotest.(check bool)
            "fixed protocol failure" true
            (contains_str message "OpenCode" && contains_str message "error") ;
          Alcotest.(check bool)
            "status omits error payload" false
            (contains_str message private_payload)
      | _ -> Alcotest.fail "exit-zero error JSONL was returned as success") ;
      List.iter
        (fun public_field ->
          Alcotest.(check bool)
            "returned public fields omit error payload" false
            (contains_str public_field private_payload))
        [result.stdout; result.agent_text; result.stderr] ;
      Alcotest.(check string) "raw stdout discarded" "" result.stdout ;
      Alcotest.(check string) "agent text discarded" "" result.agent_text ;
      Alcotest.(check (option string))
        "session discarded" None result.session_id ;
      Alcotest.(check int) "real zero exit retained" 0 result.exit_code)

let test_run_isolates_mutable_config_and_neutralizes_inherited_flags () =
  with_tmpdir (fun workspace ->
      let hostile_subagent_config =
        {|{"permission":{"task":"allow"},"agent":{"explore":{"mode":"subagent","permission":{"websearch":"allow","webfetch":"allow","codesearch":"allow","task":"allow"}}}}|}
      in
      write_file
        (Filename.concat workspace "opencode.json")
        hostile_subagent_config ;
      let hostile_account_data = Filename.concat workspace "hostile-account" in
      let hostile_managed = Filename.concat workspace "hostile-managed" in
      Unix.mkdir hostile_account_data 0o700 ;
      Unix.mkdir hostile_managed 0o700 ;
      write_file
        (Filename.concat hostile_account_data "account-config.json")
        hostile_subagent_config ;
      write_file
        (Filename.concat hostile_managed "opencode.json")
        hostile_subagent_config ;
      let fake_dir = Filename.concat workspace "fake-bin" in
      Unix.mkdir fake_dir 0o700 ;
      let capture = Filename.concat workspace "captured-environment" in
      write_executable
        (Filename.concat fake_dir "opencode")
        (Printf.sprintf
           {|#!/bin/sh
MODE=$(stat -c '%%a' "$XDG_CONFIG_HOME" 2>/dev/null || printf missing)
AGENT=''
while [ "$#" -gt 0 ]; do
  if [ "$1" = '--agent' ]; then
    shift
    AGENT="$1"
  fi
  shift
done
{
  printf '%%s\n' "$XDG_CONFIG_HOME"
  printf '%%s\n' "$OPENCODE_TEST_HOME"
  printf '%%s\n' "$OPENCODE_DB"
  printf '%%s\n' "$OPENCODE_TEST_MANAGED_CONFIG_DIR"
  printf '%%s\n' "$XDG_DATA_HOME"
  printf '%%s\n' "$AGENT"
  printf '%%s\n' "$OPENCODE_PERMISSION"
  printf '%%s\n' "$OPENCODE_CONFIG_CONTENT"
  printf '%%s\n' "$MODE"
  printf '%%s\n' "$OPENCODE_EXPERIMENTAL"
  printf '%%s\n' "$OPENCODE_EXPERIMENTAL_EXA"
  printf '%%s\n' "$OPENCODE_ENABLE_EXA"
  printf '%%s\n' "$OPENCODE_DISABLE_PROJECT_CONFIG"
  printf '%%s\n' "$OPENCODE_CONFIG_DIR"
  printf '%%s\n' "$OPENCODE_CONFIG"
} > %s
SESSION='ses_123456789abcABCDEFGHIJKLMN'
printf '%%s\n' "{\"type\":\"step_start\",\"timestamp\":1,\"sessionID\":\"$SESSION\",\"part\":{\"id\":\"prt_123456789abcABCDEFGHIJKLMN\",\"sessionID\":\"$SESSION\",\"messageID\":\"msg_123456789abcABCDEFGHIJKLMN\",\"type\":\"step-start\"}}"
printf '%%s\n' "{\"type\":\"text\",\"timestamp\":2,\"sessionID\":\"$SESSION\",\"part\":{\"id\":\"prt_23456789abcdABCDEFGHIJKLMN\",\"sessionID\":\"$SESSION\",\"messageID\":\"msg_123456789abcABCDEFGHIJKLMN\",\"type\":\"text\",\"text\":\"ok\",\"time\":{\"start\":1,\"end\":2}}}"
printf '%%s\n' "{\"type\":\"step_finish\",\"timestamp\":3,\"sessionID\":\"$SESSION\",\"part\":{\"id\":\"prt_3456789abcdeABCDEFGHIJKLMN\",\"sessionID\":\"$SESSION\",\"messageID\":\"msg_123456789abcABCDEFGHIJKLMN\",\"type\":\"step-finish\",\"reason\":\"stop\",\"tokens\":{\"total\":2,\"input\":1,\"output\":1,\"reasoning\":0,\"cache\":{\"read\":0,\"write\":0}},\"cost\":0}}"
|}
           (Filename.quote capture)) ;
      let launcher = Filename.concat fake_dir "process-group-launcher" in
      write_process_group_launcher launcher ;
      with_env_bindings
        [
          ("CABAL_PROCESS_GROUP_LAUNCHER", launcher);
          ("OPENCODE_EXPERIMENTAL", "1");
          ("OPENCODE_EXPERIMENTAL_EXA", "1");
          ("OPENCODE_ENABLE_EXA", "1");
          ("OPENCODE_DISABLE_PROJECT_CONFIG", "0");
          ("OPENCODE_CONFIG", "/private/attacker-config.json");
          ("OPENCODE_CONFIG_DIR", "/private/attacker-config-dir");
          ("OPENCODE_CONFIG_CONTENT", hostile_subagent_config);
          ("OPENCODE_PERMISSION", hostile_subagent_config);
          ("OPENCODE_DB", "/private/attacker-state.db");
          ("OPENCODE_TEST_MANAGED_CONFIG_DIR", hostile_managed);
          ("XDG_CONFIG_HOME", "/private/attacker-xdg");
          ("XDG_DATA_HOME", hostile_account_data);
          ("OPENCODE_TEST_HOME", "/private/attacker-home");
        ]
        (fun () ->
          with_path_prefix fake_dir @@ fun () ->
          Eio_posix.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let result =
            Opencode_cli.run_task ~sw ~env
              (Backend_types.make_task_spec ~prompt:"config isolation"
                 ~working_dir:workspace ())
          in
          match result.status with
          | Backend_types.Success -> ()
          | Failed message -> Alcotest.fail message
          | Timeout -> Alcotest.fail "config isolation fake timed out"
          | Cancelled -> Alcotest.fail "config isolation fake was cancelled") ;
      let captured = String.split_on_char '\n' (read_file capture) in
      match captured with
      | isolation_home
        :: test_home
        :: isolated_db
        :: managed_config_dir
        :: data_home
        :: runtime_agent
        :: runtime_permission
        :: runtime_config
        :: mode
        :: experimental
        :: experimental_exa
        :: enable_exa
        :: disable_project
        :: config_dir
        :: config
        :: _ ->
          Alcotest.(check bool)
            "isolated config home is outside workspace" false
            (String.starts_with ~prefix:(workspace ^ Filename.dir_sep)
               isolation_home) ;
          Alcotest.(check string)
            "home config discovery uses same isolation" isolation_home test_home ;
          Alcotest.(check string)
            "inherited database override is removed" "" isolated_db ;
          Alcotest.(check string)
            "system managed config discovery is isolated"
            (Filename.concat isolation_home "managed")
            managed_config_dir ;
          Alcotest.(check string)
            "provider account data remains available" hostile_account_data
            data_home ;
          Alcotest.(check bool)
            "runtime uses a private primary agent" true
            (runtime_agent <> "build"
            && String.starts_with ~prefix:"cabal-" runtime_agent) ;
          Alcotest.(check bool)
            "fixed policy names only the private runtime agent" true
            (contains_str runtime_config (Printf.sprintf "\"%s\"" runtime_agent)
            && not (contains_str runtime_config {|"build"|})) ;
          let top_level = Yojson.Safe.from_string runtime_permission in
          let invocation_agent =
            Yojson.Safe.from_string runtime_config
            |> Yojson.Safe.Util.member "agent"
            |> Yojson.Safe.Util.member runtime_agent
            |> Yojson.Safe.Util.member "permission"
          in
          List.iter
            (fun permission ->
              Alcotest.(check string)
                ("top-level denies " ^ permission) "deny"
                (json_member_string permission top_level) ;
              Alcotest.(check string)
                ("invocation agent denies " ^ permission) "deny"
                (json_member_string permission invocation_agent))
            ["websearch"; "webfetch"; "codesearch"; "task"] ;
          List.iter
            (fun path ->
              Alcotest.(check bool)
                "hostile subagent fixture remains present" true
                (contains_str (read_file path) {|"explore"|}
                && contains_str (read_file path) {|"webfetch":"allow"|}))
            [
              Filename.concat workspace "opencode.json";
              Filename.concat hostile_account_data "account-config.json";
              Filename.concat hostile_managed "opencode.json";
            ] ;
          Alcotest.(check string) "private isolation mode" "700" mode ;
          Alcotest.(check (list string))
            "experimental network flags neutralized"
            ["0"; "0"; "0"]
            [experimental; experimental_exa; enable_exa] ;
          Alcotest.(check string)
            "project discovery disabled" "1" disable_project ;
          Alcotest.(check string) "inherited config dir cleared" "" config_dir ;
          Alcotest.(check string)
            "only exact project config remains"
            (Filename.concat workspace "opencode.json")
            config ;
          Alcotest.(check bool)
            "temporary config isolation removed" false
            (Sys.file_exists isolation_home)
      | _ -> Alcotest.fail "fake OpenCode captured incomplete environment")

let test_run_loads_mcp_lsp_config_from_nested_relative_working_dir () =
  with_tmpdir (fun root ->
      let relative_working_dir = Filename.concat "project" "nested" in
      let absolute_working_dir = Filename.concat root relative_working_dir in
      ensure_parent_dir (Filename.concat absolute_working_dir "placeholder") ;
      let fake_dir = Filename.concat root "fake-bin" in
      Unix.mkdir fake_dir 0o700 ;
      let capture = Filename.concat root "captured-relative-config" in
      write_executable
        (Filename.concat fake_dir "opencode")
        (Printf.sprintf
           {|#!/bin/sh
{
  printf '%%s\n' "$PWD"
  printf '%%s\n' "$OPENCODE_CONFIG"
  while IFS= read -r LINE; do printf '%%s\n' "$LINE"; done < "$OPENCODE_CONFIG"
} > %s
SESSION='ses_123456789abcABCDEFGHIJKLMN'
printf '%%s\n' "{\"type\":\"step_start\",\"timestamp\":1,\"sessionID\":\"$SESSION\",\"part\":{\"id\":\"prt_123456789abcABCDEFGHIJKLMN\",\"sessionID\":\"$SESSION\",\"messageID\":\"msg_123456789abcABCDEFGHIJKLMN\",\"type\":\"step-start\"}}"
printf '%%s\n' "{\"type\":\"text\",\"timestamp\":2,\"sessionID\":\"$SESSION\",\"part\":{\"id\":\"prt_23456789abcdABCDEFGHIJKLMN\",\"sessionID\":\"$SESSION\",\"messageID\":\"msg_123456789abcABCDEFGHIJKLMN\",\"type\":\"text\",\"text\":\"ok\",\"time\":{\"start\":1,\"end\":2}}}"
printf '%%s\n' "{\"type\":\"step_finish\",\"timestamp\":3,\"sessionID\":\"$SESSION\",\"part\":{\"id\":\"prt_3456789abcdeABCDEFGHIJKLMN\",\"sessionID\":\"$SESSION\",\"messageID\":\"msg_123456789abcABCDEFGHIJKLMN\",\"type\":\"step-finish\",\"reason\":\"stop\",\"tokens\":{\"total\":2,\"input\":1,\"output\":1,\"reasoning\":0,\"cache\":{\"read\":0,\"write\":0}},\"cost\":0}}"
|}
           (Filename.quote capture)) ;
      let launcher = Filename.concat fake_dir "process-group-launcher" in
      write_process_group_launcher launcher ;
      let lsp_server : Backend_types.lsp_server_config =
        {
          name = "test-lsp";
          command = "test-language-server";
          args = ["--stdio"];
          file_associations = [{extension = ".test"; language_id = "test"}];
        }
      in
      let spec =
        Backend_types.make_task_spec ~prompt:"relative config"
          ~working_dir:relative_working_dir
          ~mcp_servers:
            [
              Backend_types.make_mcp_server_config ~name:"epure"
                ~command:"epure" ~args:["mcp"] ();
            ]
          ~lsp_servers:[lsp_server] ()
      in
      with_cwd root @@ fun () ->
      with_env "CABAL_PROCESS_GROUP_LAUNCHER" launcher @@ fun () ->
      with_path_prefix fake_dir @@ fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let result = Opencode_cli.run_task ~sw ~env spec in
      (match result.status with
      | Backend_types.Success -> ()
      | Failed message -> Alcotest.fail message
      | Timeout -> Alcotest.fail "relative config fake timed out"
      | Cancelled -> Alcotest.fail "relative config fake was cancelled") ;
      let captured = read_file capture in
      let expected_config = Filename.concat absolute_working_dir "opencode.json" in
      Alcotest.(check bool)
        "child cwd is nested project" true
        (String.starts_with ~prefix:(absolute_working_dir ^ "\n") captured) ;
      Alcotest.(check bool)
        "child receives absolute config for its cwd" true
        (contains_str captured ("\n" ^ expected_config ^ "\n")) ;
      Alcotest.(check bool)
        "requested MCP config loaded" true
        (contains_str captured {|
    "epure": {|}) ;
      Alcotest.(check bool)
        "requested LSP config loaded" true
        (contains_str captured {|
    "test-lsp": {|}))

let rec project_root_from path =
  if Sys.file_exists (Filename.concat path "dune-project") then path
  else
    let parent = Filename.dirname path in
    if parent = path then Sys.getcwd () else project_root_from parent

let test_media_web_probe_offline_self_test () =
  let root = project_root_from (Sys.getcwd ()) in
  let path = Filename.concat root "tools/probe_opencode_media_web.py" in
  Alcotest.(check bool) "probe artifact exists" true (Sys.file_exists path) ;
  Alcotest.(check bool)
    "probe artifact is executable"
    true
    ((Unix.stat path).st_perm land 0o111 <> 0) ;
  Alcotest.(check int)
    "offline probe validators pass"
    0
    (Sys.command (Printf.sprintf "%s --self-test" (Filename.quote path)))

let test_media_web_addendum_records_non_evidence_provenance () =
  let root = project_root_from (Sys.getcwd ()) in
  let path =
    Filename.concat root "docs/native-json-schema-investigation/opencode.md"
  in
  let content = read_file path in
  List.iter
    (fun required ->
      Alcotest.(check bool)
        ("provenance note contains: " ^ required) true
        (contains_str content required))
    [
      "Authenticated observation version: OpenCode `1.2.24`";
      "below the enforced descriptor baseline `1.14.20`";
      "not capability evidence";
      "tested_at_version must be greater than or equal to baseline_version";
      "Web_disabled` with `evidence = None";
    ] ;
  List.iter
    (fun rejected ->
      Alcotest.(check bool)
        ("false run claim absent: " ^ rejected) false
        (contains_str content rejected))
    [
      "Content-dependent assertion at `1.14.20`";
      "run successfully as a forward-compatibility advisory";
      "installed OpenCode `1.18.25`";
    ]

let test_interface_documents_network_and_macos_trust_scope () =
  let root = project_root_from (Sys.getcwd ()) in
  let path = Filename.concat root "src/opencode_cli.mli" in
  let content = read_file path in
  List.iter
    (fun required ->
      Alcotest.(check bool)
        ("interface contains: " ^ required) true
        (contains_str content required))
    [
      "websearch";
      "webfetch";
      "codesearch";
      "task delegation";
      "/Library/Managed Preferences";
      "administrator-trusted";
      "OS shell network";
    ]

let test_opencode_capabilities_remain_unpromoted () =
  match Backend_registry.find "opencode" with
  | None -> Alcotest.fail "OpenCode descriptor missing"
  | Some descriptor ->
      let capabilities = descriptor.capabilities in
      Alcotest.(check bool)
        "no advertised media" true
        (capabilities.media_support.media_types = []) ;
      Alcotest.(check bool)
        "no media evidence" true
        (Option.is_none capabilities.media_support.evidence) ;
      Alcotest.(check bool)
        "web remains disabled" true
        (capabilities.web_support.maximum = Backend_types.Web_disabled) ;
      Alcotest.(check bool)
        "no web evidence" true
        (Option.is_none capabilities.web_support.evidence)

let command_tests =
  [
    ( "build_command includes model flag",
      `Quick,
      test_build_command_includes_model_flag );
    ( "build exact repeated file argv",
      `Quick,
      test_build_invocation_exact_repeated_file_argv );
    ( "build exact fixed web policy argv",
      `Quick,
      test_build_invocation_exact_web_policy_argv );
    ( "deny hostile subagent delegation",
      `Quick,
      test_fixed_policy_denies_hostile_subagent_delegation );
    ( "absolutize nested relative config path",
      `Quick,
      test_build_invocation_absolutizes_nested_relative_config_path );
    ( "reject invalid working directory without disclosure",
      `Quick,
      test_build_invocation_rejects_invalid_working_dir_without_disclosure );
    ( "redact sensitive argv values",
      `Quick,
      test_build_invocation_redacts_sensitive_values );
    ( "reject unsupported resume and reuse",
      `Quick,
      test_build_invocation_rejects_unsupported_resume_and_reuse );
    ( "reject unsealed or mismatched images",
      `Quick,
      test_build_invocation_rejects_unsealed_or_mismatched_images );
    ( "sensitive low-level calls fail before side effects",
      `Quick,
      test_sensitive_low_level_calls_fail_before_config_or_spawn );
    ( "schema retry reuploads one sealed image twice",
      `Quick,
      test_schema_retry_reuploads_same_sealed_image_at_most_twice );
    ( "exit-zero error JSONL is a sanitized failure",
      `Quick,
      test_exit_zero_error_jsonl_becomes_sanitized_failure );
    ( "isolate mutable config and inherited experimental flags",
      `Quick,
      test_run_isolates_mutable_config_and_neutralizes_inherited_flags );
    ( "load MCP/LSP config from nested relative working directory",
      `Quick,
      test_run_loads_mcp_lsp_config_from_nested_relative_working_dir );
    ( "media/web probe offline self-test",
      `Quick,
      test_media_web_probe_offline_self_test );
    ( "record below-baseline non-evidence provenance",
      `Quick,
      test_media_web_addendum_records_non_evidence_provenance );
    ( "document network and macOS trust scope",
      `Quick,
      test_interface_documents_network_and_macos_trust_scope );
    ( "capabilities remain unpromoted",
      `Quick,
      test_opencode_capabilities_remain_unpromoted );
  ]

(** {1 Backend Interface Compliance Tests} *)

let test_implements_agentic_backend () =
  let backend = (module Opencode_cli : Agentic_backend.S) in
  Alcotest.(check string)
    "id via interface"
    "opencode"
    (Agentic_backend.id backend) ;
  Alcotest.(check string)
    "name via interface"
    "OpenCode"
    (Agentic_backend.name backend)

let interface_tests =
  [("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend)]

(** {1 OpenCode config mutation helpers} *)

let epure_mcp_server () =
  Backend_types.make_mcp_server_config
    ~name:"epure"
    ~command:"epure"
    ~args:["mcp"]
    ()

let task_spec_with_mcp ?managed_namespace dir =
  Backend_types.make_task_spec
    ~prompt:"test"
    ~working_dir:dir
    ~mcp_servers:[epure_mcp_server ()]
    ?managed_namespace
    ()

let custom_managed_namespace =
  {
    Backend_types.id = "acme";
    display_name = "Acme";
    config_dir = ".acme/backend-config";
  }

let test_ensure_mcp_preserves_jsonc_managed_header () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"opencode"
           ~project_dir:dir
           ~force:false) ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let content = read_file (Filename.concat dir "opencode.json") in
      Alcotest.(check bool)
        "managed attribution comment preserved"
        true
        (contains_str content "Generated by Cabal"
        && contains_str content "cabal-managed"
        && contains_str content "cabal-hash") ;
      Alcotest.(check bool)
        "legacy _epure schema keys are absent"
        false
        (contains_str content "_epure_") ;
      let json = Yojson.Safe.from_string (strip_line_comment_headers content) in
      match json with
      | `Assoc fields ->
          Alcotest.(check bool)
            "mcp key present in schema-valid JSON body"
            true
            (List.mem_assoc "mcp" fields)
      | _ -> Alcotest.fail "expected opencode config object")

let test_ensure_mcp_removes_legacy_epure_keys () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      write_file
        path
        {|{
  "_epure_attribution": "Generated by Epure — do not edit manually",
  "_epure-managed": true,
  "_epure-hash": "d41d8cd98f00b204e9800998ecf8427e",
  "_epure_custom": "keep-me",
  "model": "anthropic/claude-sonnet-4.5"
}
|} ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let content = read_file path in
      List.iter
        (fun key ->
          Alcotest.(check bool)
            ("legacy schema key removed: " ^ key)
            false
            (contains_str content key))
        ["_epure_attribution"; "_epure-managed"; "_epure-hash"] ;
      Alcotest.(check bool)
        "non-legacy _epure-like field preserved"
        true
        (contains_str content "_epure_custom" && contains_str content "keep-me") ;
      Alcotest.(check bool)
        "user model field preserved"
        true
        (contains_str content "anthropic/claude-sonnet-4.5") ;
      let json = Yojson.Safe.from_string (strip_line_comment_headers content) in
      match json with
      | `Assoc fields ->
          Alcotest.(check bool)
            "mcp key present"
            true
            (List.mem_assoc "mcp" fields)
      | _ -> Alcotest.fail "expected opencode config object")

let test_ensure_mcp_parses_block_comments_and_trailing_commas () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      write_file
        path
        {|{
  /* user-authored JSONC comment */
  "model": "anthropic/claude-sonnet-4.5",
  "mcp": {
    "user-server": {
      "type": "local",
      "command": ["user-mcp"],
      "enabled": true,
      "environment": {},
    },
  },
}
|} ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let content = read_file path in
      Alcotest.(check bool)
        "user model preserved after JSONC parse"
        true
        (contains_str content "anthropic/claude-sonnet-4.5") ;
      Alcotest.(check bool)
        "user MCP server preserved after JSONC parse"
        true
        (contains_str content "user-server") ;
      Alcotest.(check bool)
        "epure MCP server added"
        true
        (contains_str content "\"epure\"") ;
      ignore (Yojson.Safe.from_string content : Yojson.Safe.t))

let test_ensure_mcp_parse_failure_preserves_file () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original =
        "{\n  \"model\": \"anthropic/claude-sonnet-4.5\",\n  /* unterminated"
      in
      write_file path original ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      Alcotest.(check string)
        "parse failure leaves opencode.json byte-for-byte unchanged"
        original
        (read_file path))

let test_ensure_mcp_runtime_entry_has_local_schema () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let content = read_file path in
      let json = Yojson.Safe.from_string (strip_line_comment_headers content) in
      let expected =
        `Assoc
          [
            ("type", `String "local");
            ("command", `List [`String "epure"; `String "mcp"]);
            ("enabled", `Bool true);
            ("environment", `Assoc []);
          ]
      in
      match json with
      | `Assoc fields -> (
          match List.assoc_opt "mcp" fields with
          | Some (`Assoc mcp_fields) -> (
              match List.assoc_opt "epure" mcp_fields with
              | Some actual ->
                  Alcotest.(check string)
                    "runtime MCP entry schema"
                    (Yojson.Safe.to_string expected)
                    (Yojson.Safe.to_string actual)
              | _ -> Alcotest.fail "expected epure MCP server")
          | _ -> Alcotest.fail "expected mcp object")
      | _ -> Alcotest.fail "expected opencode config object")

let test_ensure_mcp_after_legacy_migration_keeps_setup_idempotent () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let body_without_mcp =
        "{\n  \"model\": \"anthropic/claude-sonnet-4.5\"\n}"
      in
      let hash = Digest.to_hex (Digest.string body_without_mcp) in
      write_file
        path
        (Printf.sprintf
           {|{
  "_epure_attribution": "Generated by Epure — do not edit manually",
  "_epure-managed": true,
  "_epure-hash": "%s",
  "model": "anthropic/claude-sonnet-4.5"
}
|}
           hash) ;
      let first_setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      (match first_setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Written _) -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "legacy setup refused before MCP merge: %s" msg
      | Some _ -> Alcotest.fail "expected legacy setup to migrate"
      | None -> Alcotest.fail "expected opencode setup outcome") ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let second_setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      match second_setup.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "setup refused after MCP merge: %s" msg
      | Some _ ->
          Alcotest.fail "expected setup after MCP merge to be idempotent"
      | None -> Alcotest.fail "expected opencode setup outcome")

let test_ensure_mcp_custom_namespace_marks_and_stays_idempotent () =
  with_tmpdir (fun dir ->
      let ns = custom_managed_namespace in
      let path = Filename.concat dir "opencode.json" in
      let first_setup =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:ns
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
          ()
      in
      (match first_setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Written _) -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "custom setup refused before MCP merge: %s" msg
      | Some _ -> Alcotest.fail "expected custom setup to write opencode.json"
      | None -> Alcotest.fail "expected opencode setup outcome") ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json
        ~env
        (task_spec_with_mcp ~managed_namespace:ns dir) ;
      let content = read_file path in
      List.iter
        (fun marker ->
          Alcotest.(check bool)
            ("custom marker present: " ^ marker)
            true
            (contains_str content marker))
        ["Generated by Acme"; "acme-managed"; "acme-hash"] ;
      List.iter
        (fun marker ->
          Alcotest.(check bool)
            ("default-namespace marker absent: " ^ marker)
            false
            (contains_str content marker))
        [
          "Generated by Cabal";
          "cabal-managed";
          "cabal-hash";
          "Generated by Epure";
          "epure-managed";
          "epure-hash";
        ] ;
      let second_setup =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:ns
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
          ()
      in
      match second_setup.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "custom setup refused after MCP merge: %s" msg
      | Some _ -> Alcotest.fail "expected custom setup to remain idempotent"
      | None -> Alcotest.fail "expected opencode setup outcome")

let test_ensure_mcp_custom_namespace_marks_new_file () =
  with_tmpdir (fun dir ->
      let ns = custom_managed_namespace in
      let path = Filename.concat dir "opencode.json" in
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json
        ~env
        (task_spec_with_mcp ~managed_namespace:ns dir) ;
      let content = read_file path in
      Alcotest.(check bool)
        "new file uses custom managed header"
        true
        (contains_str content "Generated by Acme"
        && contains_str content "acme-managed"
        && contains_str content "acme-hash") ;
      Alcotest.(check bool)
        "new file does not use default-namespace or legacy Epure markers"
        false
        (contains_str content "Generated by Cabal"
        || contains_str content "cabal-managed"
        || contains_str content "cabal-hash"
        || contains_str content "Generated by Epure"
        || contains_str content "epure-managed"
        || contains_str content "epure-hash") ;
      let setup =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:ns
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
          ()
      in
      match setup.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "custom setup refused after direct MCP write: %s" msg
      | Some _ -> Alcotest.fail "expected direct MCP write to be idempotent"
      | None -> Alcotest.fail "expected opencode setup outcome")

let test_ensure_mcp_custom_namespace_migrates_legacy_epure_header () =
  with_tmpdir (fun dir ->
      let ns = custom_managed_namespace in
      let path = Filename.concat dir "opencode.json" in
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"opencode"
           ~project_dir:dir
           ~force:false) ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json
        ~env
        (task_spec_with_mcp ~managed_namespace:ns dir) ;
      let content = read_file path in
      Alcotest.(check bool)
        "default-namespace header migrated to custom marker"
        true
        (contains_str content "acme-managed" && contains_str content "acme-hash") ;
      Alcotest.(check bool)
        "default-namespace and legacy Epure markers no longer emitted"
        false
        (contains_str content "cabal-managed"
        || contains_str content "cabal-hash"
        || contains_str content "Generated by Cabal"
        || contains_str content "epure-managed"
        || contains_str content "epure-hash"
        || contains_str content "Generated by Epure") ;
      let setup =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:ns
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
          ()
      in
      match setup.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "custom setup refused migrated header: %s" msg
      | Some _ -> Alcotest.fail "expected migrated header to be idempotent"
      | None -> Alcotest.fail "expected opencode setup outcome")

let test_ensure_mcp_after_skipped_setup_preserves_user_file () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original =
        {|{
  // user comment must survive byte-for-byte
  "model": "anthropic/claude-sonnet-4.5"
}
|}
      in
      write_file_creating_dirs path original ;
      let setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      (match setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Skipped_user_content _) -> ()
      | _ -> Alcotest.fail "expected setup to skip user-authored opencode.json") ;
      Eio_posix.run @@ fun env ->
      match
        Opencode_cli.ensure_mcp_if_config_applied
          ~env
          ~setup_outcome:setup.Backend_config_gen.write_outcome
          (task_spec_with_mcp dir)
      with
      | Ok () -> Alcotest.fail "expected MCP merge to be blocked after skip"
      | Error msg ->
          Alcotest.(check bool)
            "error mentions opencode.json"
            true
            (contains_str msg "opencode.json") ;
          Alcotest.(check string)
            "user-authored opencode.json unchanged"
            original
            (read_file path))

let test_ensure_mcp_after_refused_setup_preserves_hash_mismatch_file () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"opencode"
           ~project_dir:dir
           ~force:false) ;
      let path = Filename.concat dir "opencode.json" in
      let modified = read_file path ^ "// user modification\n" in
      write_file path modified ;
      let setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      (match setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Refused_hash_mismatch _) -> ()
      | _ ->
          Alcotest.fail "expected setup to refuse hash-mismatched opencode.json") ;
      Eio_posix.run @@ fun env ->
      match
        Opencode_cli.ensure_mcp_if_config_applied
          ~env
          ~setup_outcome:setup.Backend_config_gen.write_outcome
          (task_spec_with_mcp dir)
      with
      | Ok () -> Alcotest.fail "expected MCP merge to be blocked after refusal"
      | Error msg ->
          Alcotest.(check bool)
            "error mentions hash mismatch"
            true
            (contains_str msg "hash" || contains_str msg "refused") ;
          Alcotest.(check string)
            "hash-mismatched opencode.json unchanged"
            modified
            (read_file path))

let config_update_tests =
  [
    ( "ensure_mcp preserves managed JSONC header",
      `Quick,
      test_ensure_mcp_preserves_jsonc_managed_header );
    ( "ensure_mcp removes legacy _epure keys",
      `Quick,
      test_ensure_mcp_removes_legacy_epure_keys );
    ( "ensure_mcp parses block comments and trailing commas",
      `Quick,
      test_ensure_mcp_parses_block_comments_and_trailing_commas );
    ( "ensure_mcp parse failure preserves file",
      `Quick,
      test_ensure_mcp_parse_failure_preserves_file );
    ( "ensure_mcp runtime entry has local schema",
      `Quick,
      test_ensure_mcp_runtime_entry_has_local_schema );
    ( "ensure_mcp after legacy migration keeps setup idempotent",
      `Quick,
      test_ensure_mcp_after_legacy_migration_keeps_setup_idempotent );
    ( "ensure_mcp custom namespace marks and stays idempotent",
      `Quick,
      test_ensure_mcp_custom_namespace_marks_and_stays_idempotent );
    ( "ensure_mcp custom namespace marks new file",
      `Quick,
      test_ensure_mcp_custom_namespace_marks_new_file );
    ( "ensure_mcp custom namespace migrates legacy Epure header",
      `Quick,
      test_ensure_mcp_custom_namespace_migrates_legacy_epure_header );
    ( "ensure_mcp blocked after skipped setup preserves user file",
      `Quick,
      test_ensure_mcp_after_skipped_setup_preserves_user_file );
    ( "ensure_mcp blocked after refused setup preserves file",
      `Quick,
      test_ensure_mcp_after_refused_setup_preserves_hash_mismatch_file );
  ]

(** {1 Test Runner} *)

(** {1 Mutation Guard Tests — Story #515} *)

(** {2 AC4/AC5: user_owned_backup_needed predicate} *)

let test_predicate_skipped_user_content () =
  Alcotest.(check bool)
    "Skipped_user_content arms guard"
    true
    (Opencode_cli.user_owned_backup_needed
       (Some (Backend_config_gen.Skipped_user_content "/some/path")))

let test_predicate_already_current () =
  Alcotest.(check bool)
    "Already_current skips guard (AC4)"
    false
    (Opencode_cli.user_owned_backup_needed
       (Some Backend_config_gen.Already_current))

let test_predicate_written () =
  Alcotest.(check bool)
    "Written skips guard (AC4)"
    false
    (Opencode_cli.user_owned_backup_needed
       (Some (Backend_config_gen.Written "/p")))

let test_predicate_refused_hash_mismatch () =
  Alcotest.(check bool)
    "Refused_hash_mismatch skips guard (AC5)"
    false
    (Opencode_cli.user_owned_backup_needed
       (Some (Backend_config_gen.Refused_hash_mismatch "msg")))

let test_predicate_backed_up () =
  Alcotest.(check bool)
    "Backed_up_and_written skips guard (AC4)"
    false
    (Opencode_cli.user_owned_backup_needed
       (Some
          (Backend_config_gen.Backed_up_and_written
             {path = "/p"; backup_path = "/b"})))

let test_predicate_none () =
  Alcotest.(check bool)
    "None skips guard"
    false
    (Opencode_cli.user_owned_backup_needed None)

let predicate_tests =
  [
    ( "Skipped_user_content arms guard",
      `Quick,
      test_predicate_skipped_user_content );
    ("Already_current skips guard (AC4)", `Quick, test_predicate_already_current);
    ("Written skips guard (AC4)", `Quick, test_predicate_written);
    ( "Refused_hash_mismatch skips guard (AC5)",
      `Quick,
      test_predicate_refused_hash_mismatch );
    ("Backed_up_and_written skips guard (AC4)", `Quick, test_predicate_backed_up);
    ("None skips guard", `Quick, test_predicate_none);
  ]

(** {2 AC1/AC3: read_opencode_backup} *)

let test_read_backup_success () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let content = {|{"model":"claude-sonnet","theme":"dark"}|} in
      write_file path content ;
      Eio_posix.run @@ fun env ->
      match Opencode_cli.read_opencode_backup ~env ~config_path:path with
      | Ok bytes ->
          Alcotest.(check string)
            "backup bytes match file content"
            content
            bytes
      | Error msg -> Alcotest.failf "Expected Ok but got Error: %s" msg)

let test_read_backup_failure_missing_file () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "nonexistent.json" in
      Eio_posix.run @@ fun env ->
      match Opencode_cli.read_opencode_backup ~env ~config_path:path with
      | Ok _ -> Alcotest.fail "Expected Error for missing file but got Ok"
      | Error _ -> ())

let backup_tests =
  [
    ("read backup success (AC1)", `Quick, test_read_backup_success);
    ( "read backup error on missing file (AC3)",
      `Quick,
      test_read_backup_failure_missing_file );
  ]

(** {2 AC2: check_opencode_mutation} *)

let test_check_no_mutation () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let content = {|{"model":"claude-sonnet"}|} in
      write_file path content ;
      Eio_posix.run @@ fun env ->
      let mock_result =
        Backend_types.make_task_result
          ~status:Backend_types.Success
          ~stdout:"some output"
          ~session_id:"sess-123"
          ()
      in
      let result =
        Opencode_cli.check_opencode_mutation
          ~env
          ~config_path:path
          ~backup:content
          mock_result
      in
      Alcotest.(check string)
        "stdout unchanged"
        "some output"
        result.Backend_types.stdout ;
      Alcotest.(check (option string))
        "session_id unchanged"
        (Some "sess-123")
        result.Backend_types.session_id)

let test_check_mutation_detected () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original = {|{"model":"claude-sonnet"}|} in
      let mutated = {|{"model":"gpt-4","theme":"dark","extra":"injected"}|} in
      write_file path original ;
      Eio_posix.run @@ fun env ->
      let mock_result =
        Backend_types.make_task_result
          ~status:Backend_types.Success
          ~stdout:"some output"
          ~session_id:"sess-abc"
          ()
      in
      write_file path mutated ;
      let result =
        Opencode_cli.check_opencode_mutation
          ~env
          ~config_path:path
          ~backup:original
          mock_result
      in
      (match result.Backend_types.status with
      | Backend_types.Failed msg ->
          Alcotest.(check bool)
            "failure message mentions mutation"
            true
            (let len = String.length msg and sub = "mutated" in
             let nlen = String.length sub in
             let rec f i =
               i + nlen <= len && (String.sub msg i nlen = sub || f (i + 1))
             in
             f 0)
      | _ -> Alcotest.fail "Expected Failed status") ;
      Alcotest.(check string) "stdout discarded" "" result.Backend_types.stdout ;
      Alcotest.(check (option string))
        "session_id discarded"
        None
        result.Backend_types.session_id ;
      let restored = read_file path in
      Alcotest.(check string)
        "file content restored to original"
        original
        restored)

let test_check_deletion_detected () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original = {|{"model":"claude-sonnet"}|} in
      write_file path original ;
      Eio_posix.run @@ fun env ->
      let mock_result =
        Backend_types.make_task_result
          ~status:Backend_types.Success
          ~stdout:"some output"
          ~session_id:"sess-del"
          ()
      in
      (* Simulate OpenCode deleting the file *)
      Sys.remove path ;
      let result =
        Opencode_cli.check_opencode_mutation
          ~env
          ~config_path:path
          ~backup:original
          mock_result
      in
      match result.Backend_types.status with
      | Backend_types.Failed _ -> ()
      | _ -> Alcotest.fail "Expected Failed status when file is deleted")

let test_check_mutation_restore_fails () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original = {|{"model":"claude-sonnet"}|} in
      let mutated = {|{"model":"gpt-4","injected":true}|} in
      write_file path original ;
      Eio_posix.run @@ fun env ->
      let mock_result =
        Backend_types.make_task_result
          ~status:Backend_types.Success
          ~stdout:"some output"
          ~session_id:"sess-fail"
          ()
      in
      (* Mutate the file, then make the file itself read-only so save fails *)
      write_file path mutated ;
      Unix.chmod path 0o444 ;
      let result =
        Fun.protect
          ~finally:(fun () -> Unix.chmod path 0o644)
          (fun () ->
            Opencode_cli.check_opencode_mutation
              ~env
              ~config_path:path
              ~backup:original
              mock_result)
      in
      let contains sub s =
        let sub_len = String.length sub and s_len = String.length s in
        let rec loop i =
          i + sub_len <= s_len && (String.sub s i sub_len = sub || loop (i + 1))
        in
        loop 0
      in
      (match result.Backend_types.status with
      | Backend_types.Failed msg ->
          Alcotest.(check bool)
            "failure message mentions restore failed (not falsely 'restored')"
            true
            (contains "restore failed" msg)
      | _ -> Alcotest.fail "Expected Failed status when mutation detected") ;
      Alcotest.(check string) "stdout discarded" "" result.Backend_types.stdout ;
      Alcotest.(check (option string))
        "session_id discarded"
        None
        result.Backend_types.session_id)

let mutation_tests =
  [
    ("no mutation passes result through (AC2)", `Quick, test_check_no_mutation);
    ( "mutation detected: restore and failure (AC2)",
      `Quick,
      test_check_mutation_detected );
    ( "deletion detected: returns Failed (AC2)",
      `Quick,
      test_check_deletion_detected );
    ( "mutation with restore failure: message says restore failed (AC2)",
      `Quick,
      test_check_mutation_restore_fails );
  ]

let () =
  Alcotest.run
    "Opencode_cli"
    [
      ("Identity", identity_tests);
      ("JSON Events", json_events_tests);
      ("Availability", availability_tests);
      ("Command", command_tests);
      ("Interface", interface_tests);
      ("Config Update", config_update_tests);
      ("Mutation Guard Predicate", predicate_tests);
      ("Backup Read", backup_tests);
      ("Mutation Detection", mutation_tests);
    ]
