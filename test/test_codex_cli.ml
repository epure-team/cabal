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
let valid_thread_id = "019c1f65-7f5a-7e13-9f8a-0c4f68d6a321"

let identity_tests =
  [
    ("id is codex", `Quick, test_id); ("name is OpenAI Codex", `Quick, test_name);
  ]

(** {1 JSONL Output Parsing Tests} *)

let test_parse_jsonl_with_message_and_usage () =
  let input =
    {|{"type":"thread.started","thread_id":"019c1f65-7f5a-7e13-9f8a-0c4f68d6a321"}
{"type":"item.completed","item":{"type":"agent_message","text":"First message"}}
{"type":"item.completed","item":{"type":"agent_message","text":"Final result"}}
{"type":"turn.completed","usage":{"input_tokens":200,"cached_input_tokens":50,"output_tokens":75}}|}
  in
  let text, cost = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string) "last message" "Final result" text;
  Alcotest.(check bool) "cost present" true (Option.is_some cost);
  match cost with
  | Some c ->
      Alcotest.(check (option int)) "input_tokens" (Some 200) c.tokens_input;
      Alcotest.(check (option int)) "output_tokens" (Some 75) c.tokens_output;
      Alcotest.(check (option int))
        "cached_input_tokens" (Some 50) c.cache_read_input_tokens
  | None -> Alcotest.fail "Expected cost to be Some"

let test_parse_jsonl_no_usage () =
  let input =
    {|{"type":"item.completed","item":{"type":"agent_message","text":"Hello world"}}|}
  in
  let text, cost = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string) "message text" "Hello world" text;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_jsonl_empty () =
  let text, cost = Codex_cli.parse_jsonl_output "" in
  (* Falls back to raw stdout *)
  Alcotest.(check string) "empty fallback" "" text;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_jsonl_malformed () =
  let input = "not json at all\n{invalid" in
  let text, cost = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string) "malformed output is not promoted" "" text;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_jsonl_ignores_non_public_records () =
  let input =
    {|{"type":"item.completed","item":{"type":"reasoning","text":"private reasoning"}}
{"type":"error","message":"private error"}
{"type":"item.started","item":{"type":"agent_message","text":"not completed"}}
{"type":"response.output_text.delta","delta":"raw fallback"}|}
  in
  let text, cost = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string) "no private/raw text promoted" "" text;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_stdout_text_retains_legacy_raw_fallback () =
  let input =
    Printf.sprintf {|{"type":"thread.started","thread_id":"%s"}|}
      valid_thread_id
  in
  Alcotest.(check string)
    "legacy helper retains raw fallback" input
    (Codex_cli.parse_stdout_text input);
  Alcotest.(check string)
    "runtime parser remains strict" ""
    (Codex_cli.parse_public_stdout_text input)

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
      Printf.sprintf {|{"type":"thread.started","thread_id":"%s"}|}
        valid_thread_id;
      {|{"type":"item.completed","item":{"id":"item-1","type":"agent_message","text":"public answer"}}|};
      {|{"type":"item.started","item":{"id":"tool-1","type":"web_search","query":"private query omitted"}}|};
      {|{"type":"item.completed","item":{"id":"tool-1","type":"web_search","result":"private result omitted"}}|};
      {|{"type":"turn.completed","usage":{"input_tokens":12,"cached_input_tokens":4,"output_tokens":3,"reasoning_output_tokens":99}}|};
    ]
  in
  let events = List.concat_map Codex_cli.normalized_events_of_line lines in
  Alcotest.(check (list string))
    "normalized public record kinds"
    [ "session"; "agent"; "tool-started"; "tool-finished"; "usage" ]
    (List.map payload_kind events);
  match events with
  | [
   Task_event.Session_id session_id;
   Agent_text_delta "public answer";
   Tool_started { id = Some "tool-1"; name = "web_search" };
   Tool_finished { id = Some "tool-1"; name = Some "web_search" };
   Token_usage usage;
  ] ->
      Alcotest.(check string) "canonical session id" valid_thread_id session_id;
      Alcotest.(check (option int)) "input usage" (Some 12) usage.tokens_input;
      Alcotest.(check (option int)) "output usage" (Some 3) usage.tokens_output;
      Alcotest.(check (option int))
        "cached input usage" (Some 4) usage.cache_read_input_tokens
  | _ -> Alcotest.fail "unexpected normalized event payloads"

let test_session_parser_accepts_only_canonical_codex_thread_ids () =
  let valid_line =
    Printf.sprintf {|{"type":"thread.started","thread_id":"%s"}|}
      valid_thread_id
  in
  Alcotest.(check (option string))
    "canonical UUID is accepted" (Some valid_thread_id)
    (Codex_cli.parse_public_session_id valid_line);
  let invalid_ids =
    [
      "";
      "--last";
      "thread-123";
      "019C1F65-7F5A-7E13-9F8A-0C4F68D6A321";
      "019c1f65-7f5a-7e13-9f8a-0c4f68d6a32";
      "019c1f65-7f5a-7e13-9f8a-0c4f68d6a321\n";
      String.make 129 'a';
    ]
  in
  List.iter
    (fun invalid_id ->
      let line =
        `Assoc
          [
            ("type", `String "thread.started"); ("thread_id", `String invalid_id);
          ]
        |> Yojson.Safe.to_string
      in
      Alcotest.(check (option string))
        "invalid thread id is rejected" None
        (Codex_cli.parse_public_session_id line))
    invalid_ids;
  let undocumented_fallback =
    Printf.sprintf {|{"type":"thread.started","session_id":"%s"}|}
      valid_thread_id
  in
  Alcotest.(check (option string))
    "session_id fallback is not accepted for thread.started" None
    (Codex_cli.parse_public_session_id undocumented_fallback)

let test_token_fields_reject_invalid_values () =
  let above_max_int = Int64.(to_string (add (of_int Stdlib.max_int) 1L)) in
  let invalid_lines =
    [
      {|{"type":"turn.completed","usage":{"input_tokens":-1,"output_tokens":-2,"cached_input_tokens":-3}}|};
      {|{"type":"turn.completed","usage":{"input_tokens":"1","output_tokens":1.5,"cached_input_tokens":true}}|};
      Printf.sprintf
        {|{"type":"turn.completed","usage":{"input_tokens":%s,"output_tokens":%s,"cached_input_tokens":%s}}|}
        above_max_int above_max_int above_max_int;
    ]
  in
  List.iter
    (fun line ->
      Alcotest.(check (list string))
        "invalid-only usage emits no token event" []
        (Codex_cli.normalized_events_of_line line |> List.map payload_kind);
      let _, cost = Codex_cli.parse_jsonl_output line in
      Alcotest.(check bool)
        "invalid-only usage emits no cost" true (Option.is_none cost))
    invalid_lines

let test_token_fields_preserve_zero_and_independent_valid_values () =
  let zero_line =
    {|{"type":"turn.completed","usage":{"input_tokens":0,"output_tokens":0,"cached_input_tokens":0}}|}
  in
  let _, zero_cost = Codex_cli.parse_jsonl_output zero_line in
  (match zero_cost with
  | Some cost ->
      Alcotest.(check (list (option int)))
        "zero token values are preserved" [ Some 0; Some 0; Some 0 ]
        [ cost.tokens_input; cost.tokens_output; cost.cache_read_input_tokens ]
  | None -> Alcotest.fail "zero usage was discarded");
  let independent =
    {|{"type":"turn.completed","usage":{"input_tokens":2,"output_tokens":"bad","cached_input_tokens":-1}}
{"type":"turn.completed","usage":{"input_tokens":null,"output_tokens":3,"cached_input_tokens":false}}
{"type":"turn.completed","usage":{"input_tokens":-4,"output_tokens":[],"cached_input_tokens":4}}|}
  in
  let _, cost = Codex_cli.parse_jsonl_output independent in
  match cost with
  | Some cost ->
      Alcotest.(check (list (option int)))
        "valid token fields survive invalid siblings" [ Some 2; Some 3; Some 4 ]
        [ cost.tokens_input; cost.tokens_output; cost.cache_read_input_tokens ]
  | None -> Alcotest.fail "independent valid token fields were discarded"

let test_token_aggregation_saturates_at_max_int () =
  let line value =
    Printf.sprintf
      {|{"type":"turn.completed","usage":{"input_tokens":%d,"output_tokens":%d,"cached_input_tokens":%d}}|}
      value value value
  in
  let input = String.concat "\n" [ line Stdlib.max_int; line 1 ] in
  let _, cost = Codex_cli.parse_jsonl_output input in
  match cost with
  | Some cost ->
      Alcotest.(check (list (option int)))
        "all token totals saturate without wrapping"
        [ Some Stdlib.max_int; Some Stdlib.max_int; Some Stdlib.max_int ]
        [ cost.tokens_input; cost.tokens_output; cost.cache_read_input_tokens ]
  | None -> Alcotest.fail "saturated usage was discarded"

let test_normalized_events_ignore_private_and_unsafe_records () =
  let lines =
    [
      {|{"type":"item.completed","item":{"type":"reasoning","text":"private reasoning"}}|};
      {|{"type":"error","message":"private error /private/image.png"}|};
      {|{"type":"item.started","item":{"id":"/private/image.png","type":"command_execution","command":"cat /private/image.png"}}|};
      {|{"type":"item.completed","item":{"type":"agent_message","text":42}}|};
      {|{"type":"thread.started","thread_id":"/private/session"}|};
      {|{"type":"turn.completed","usage":"private malformed usage"}|};
    ]
  in
  let events = List.concat_map Codex_cli.normalized_events_of_line lines in
  Alcotest.(check (list string))
    "only the safe fixed-name tool record remains" [ "tool-started" ]
    (List.map payload_kind events);
  match events with
  | [ Task_event.Tool_started { id = None; name = "command_execution" } ] -> ()
  | _ -> Alcotest.fail "unsafe protocol identifiers or private records leaked"

let jsonl_output_tests =
  [
    ( "parse with message and usage",
      `Quick,
      test_parse_jsonl_with_message_and_usage );
    ("parse without usage", `Quick, test_parse_jsonl_no_usage);
    ("parse empty output", `Quick, test_parse_jsonl_empty);
    ("parse malformed output", `Quick, test_parse_jsonl_malformed);
    ( "ignore non-public JSONL records",
      `Quick,
      test_parse_jsonl_ignores_non_public_records );
    ( "legacy raw fallback is isolated from runtime parsing",
      `Quick,
      test_parse_stdout_text_retains_legacy_raw_fallback );
    ( "normalize public protocol records",
      `Quick,
      test_normalized_events_public_protocol_records );
    ( "ignore private and unsafe protocol records",
      `Quick,
      test_normalized_events_ignore_private_and_unsafe_records );
    ( "session parser accepts canonical UUIDs only",
      `Quick,
      test_session_parser_accepts_only_canonical_codex_thread_ids );
    ( "invalid token fields are rejected",
      `Quick,
      test_token_fields_reject_invalid_values );
    ( "zero and independent token fields are preserved",
      `Quick,
      test_token_fields_preserve_zero_and_independent_valid_values );
    ( "token aggregation saturates at max_int",
      `Quick,
      test_token_aggregation_saturates_at_max_int );
  ]

(** {1 Command Construction Tests} *)

let sample_output_schema : Yojson.Safe.t =
  `Assoc
    [
      ("$schema", `String "https://json-schema.org/draft/2020-12/schema");
      ("type", `String "object");
      ( "properties",
        `Assoc [ ("answer", `Assoc [ ("type", `String "string") ]) ] );
      ("required", `List [ `String "answer" ]);
      ("additionalProperties", `Bool false);
    ]

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let remove_if_exists path =
  try if Sys.file_exists path then Sys.remove path with _ -> ()

let rec remove_tree path =
  if Sys.file_exists path then
    match (Unix.lstat path).st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Sys.remove path

let with_temp_dir label f =
  let path = Filename.temp_dir ("cabal-codex-" ^ label ^ "-") "" in
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
  | [ last ] -> last
  | _ :: rest -> command_last rest

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

let media_attachment ?(id = "image") ?(path = "media/cover.png") media_type :
    Backend_types.media_attachment =
  { id; path; media_type; sha256 = String.make 64 '0'; size_bytes = 1 }

let sealed_png_path = "/private/cabal-task-inputs-a1/attachment-a1.png"
let sealed_jpeg_path = "/private/cabal-task-inputs-a1/attachment-b2.jpg"

let command_spec ?(attachments = []) ?(web_access = Backend_types.Web_disabled)
    ?resume_session_id ?json_schema ?model ?(read_only = false) () =
  Backend_types.make_task_spec ~prompt:"private prompt payload"
    ~instructions:"private project instructions"
    ~working_dir:"/private/workspace path" ~attachments ~web_access
    ?resume_session_id ?json_schema ?model ~read_only ()

let build_invocation ?schema_path ?attachment_paths
    ?(attachment_delivery = Backend_types.Upload_attachments) spec =
  match
    Codex_cli.build_invocation ?schema_path ?attachment_paths
      ~attachment_delivery ~mcp_config_path:None spec
  with
  | Ok invocation -> invocation
  | Error msg -> Alcotest.fail msg

let expected_root ?(web = "disabled") ?(read_only = false) ?model ?schema () =
  [
    "codex";
    "exec";
    "--json";
    "--skip-git-repo-check";
    "--ignore-user-config";
    "-c";
    Printf.sprintf "web_search=\"%s\"" web;
  ]
  @ (if read_only then [ "-s"; "read-only" ] else [ "--full-auto" ])
  @ (match model with Some value -> [ "-m"; value ] | None -> [])
  @ match schema with Some path -> [ "--output-schema"; path ] | None -> []

let expected_resume ?(web = "disabled") ?(read_only = false) ?model ?schema
    session_id =
  [ "codex"; "exec" ]
  @ (match schema with Some path -> [ "--output-schema"; path ] | None -> [])
  @ (if read_only then [ "-s"; "read-only" ] else [ "--full-auto" ])
  @ [ "resume"; session_id ]
  @ [
      "--json";
      "--skip-git-repo-check";
      "--ignore-user-config";
      "-c";
      Printf.sprintf "web_search=\"%s\"" web;
    ]
  @ match model with Some value -> [ "-m"; value ] | None -> []

let test_build_invocation_zero_images_exact_argv () =
  let invocation = build_invocation (command_spec ()) in
  Alcotest.(check (list string))
    "zero image argv"
    (expected_root () @ [ "-" ])
    invocation.argv

let test_build_invocation_read_only_exact_argv () =
  let invocation = build_invocation (command_spec ~read_only:true ()) in
  Alcotest.(check (list string))
    "read-only argv"
    (expected_root ~read_only:true () @ [ "-" ])
    invocation.argv

let test_build_invocation_one_image_with_spaces_exact_argv () =
  let attachment =
    media_attachment ~path:"media/front cover.png" Backend_types.Png
  in
  let invocation =
    build_invocation ~attachment_paths:[ sealed_png_path ]
      (command_spec ~attachments:[ attachment ] ())
  in
  Alcotest.(check (list string))
    "one image argv preserves one path element"
    (expected_root () @ [ "-i"; sealed_png_path; "-" ])
    invocation.argv

let test_build_invocation_multiple_images_exact_argv () =
  let attachments =
    [
      media_attachment ~id:"front" ~path:"media/front cover.png"
        Backend_types.Png;
      media_attachment ~id:"back" ~path:"media/back cover.jpg"
        Backend_types.Jpeg;
    ]
  in
  let invocation =
    build_invocation
      ~attachment_paths:[ sealed_png_path; sealed_jpeg_path ]
      (command_spec ~attachments ())
  in
  Alcotest.(check (list string))
    "multiple images use repeated flags"
    (expected_root () @ [ "-i"; sealed_png_path; "-i"; sealed_jpeg_path; "-" ])
    invocation.argv

let test_build_invocation_resume_uploads_png_jpeg_with_schema () =
  let attachments =
    [
      media_attachment ~id:"front" ~path:"media/front.png" Backend_types.Png;
      media_attachment ~id:"back" ~path:"media/back.jpg" Backend_types.Jpeg;
    ]
  in
  let schema_path = "/private/schema path.json" in
  let invocation =
    build_invocation ~schema_path
      ~attachment_paths:[ sealed_png_path; sealed_jpeg_path ]
      ~attachment_delivery:Backend_types.Upload_attachments
      (command_spec ~attachments ~resume_session_id:valid_thread_id
         ~json_schema:sample_output_schema ~read_only:true ())
  in
  Alcotest.(check (list string))
    "resume root schema/sandbox precede subcommand and PNG/JPEG follow UUID"
    (expected_resume ~read_only:true ~schema:schema_path valid_thread_id
    @ [ "-i"; sealed_png_path; "-i"; sealed_jpeg_path; "-" ])
    invocation.argv

let test_build_invocation_resume_reuses_media_with_schema () =
  let attachments =
    [
      media_attachment ~id:"front" ~path:"media/front.png" Backend_types.Png;
      media_attachment ~id:"back" ~path:"media/back.jpg" Backend_types.Jpeg;
    ]
  in
  let schema_path = "/private/schema path.json" in
  let invocation =
    build_invocation ~schema_path
      ~attachment_delivery:Backend_types.Reuse_session_attachments
      (command_spec ~attachments ~resume_session_id:valid_thread_id
         ~json_schema:sample_output_schema ~read_only:true ())
  in
  Alcotest.(check (list string))
    "resume reuse composes root schema without duplicate image flags"
    (expected_resume ~read_only:true ~schema:schema_path valid_thread_id
    @ [ "-" ])
    invocation.argv

let test_build_invocation_image_schema_composition () =
  let attachment = media_attachment Backend_types.Png in
  let schema_path = "/private/schema path.json" in
  let invocation =
    build_invocation ~schema_path ~attachment_paths:[ sealed_png_path ]
      (command_spec ~attachments:[ attachment ]
         ~json_schema:sample_output_schema ())
  in
  Alcotest.(check (list string))
    "image and schema compose"
    (expected_root ~schema:schema_path () @ [ "-i"; sealed_png_path; "-" ])
    invocation.argv

let test_build_invocation_web_policies () =
  List.iter
    (fun (policy, mode) ->
      let invocation = build_invocation (command_spec ~web_access:policy ()) in
      Alcotest.(check (list string))
        ("web mode " ^ mode)
        (expected_root ~web:mode () @ [ "-" ])
        invocation.argv)
    [
      (Backend_types.Web_disabled, "disabled");
      (Backend_types.Web_search, "cached");
      (Backend_types.Web_search_and_fetch, "live");
    ]

let test_build_invocation_redacts_sensitive_argv () =
  let attachments =
    [
      media_attachment ~id:"front" ~path:"media/front cover.png"
        Backend_types.Png;
      media_attachment ~id:"back" ~path:"media/back cover.jpg"
        Backend_types.Jpeg;
    ]
  in
  let invocation =
    build_invocation ~schema_path:"/private/schema path.json"
      ~attachment_paths:[ sealed_png_path; sealed_jpeg_path ]
      (command_spec ~attachments ~resume_session_id:valid_thread_id
         ~json_schema:sample_output_schema ~model:"private-model" ())
  in
  Alcotest.(check (list string))
    "redacted argv keeps flags and counts only"
    (expected_resume ~model:"<model>" ~schema:"<schema>" "<session-id>"
    @ [ "-i"; "<attachment-1>"; "-i"; "<attachment-2>"; "-" ])
    invocation.redacted_argv;
  let rendered = String.concat " " invocation.redacted_argv in
  List.iter
    (fun secret ->
      Alcotest.(check bool)
        ("redacted argv omits " ^ secret)
        false
        (contains_substring rendered secret))
    [
      "front cover.png";
      "back cover.jpg";
      sealed_png_path;
      sealed_jpeg_path;
      "schema path.json";
      valid_thread_id;
      "private-model";
      "private prompt payload";
      "private/workspace";
    ]

let test_build_invocation_rejects_unsafe_staged_attachment_path () =
  let attachment =
    media_attachment ~path:"../private/cover.png" Backend_types.Png
  in
  match
    Codex_cli.build_invocation
      ~attachment_delivery:Backend_types.Upload_attachments
      ~attachment_paths:[ "../private/cover.png" ] ~mcp_config_path:None
      (command_spec ~attachments:[ attachment ] ())
  with
  | Ok _ -> Alcotest.fail "unsafe staged attachment path accepted"
  | Error msg ->
      Alcotest.(check bool)
        "error is sanitized" false
        (contains_substring msg "../private/cover.png")

let test_build_invocation_requires_one_staged_path_per_upload () =
  let attachments =
    [
      media_attachment ~id:"front" Backend_types.Png;
      media_attachment ~id:"back" Backend_types.Jpeg;
    ]
  in
  List.iter
    (fun attachment_paths ->
      match
        Codex_cli.build_invocation
          ~attachment_delivery:Backend_types.Upload_attachments
          ~attachment_paths ~mcp_config_path:None
          (command_spec ~attachments ())
      with
      | Ok _ -> Alcotest.fail "mismatched sealed attachment set accepted"
      | Error message ->
          Alcotest.(check bool)
            "mismatch is sanitized" true
            (contains_substring message "sealed attachment set"))
    [
      [];
      [ sealed_png_path ];
      [ sealed_png_path; sealed_jpeg_path; "/extra.png" ];
    ]

let test_build_invocation_rejects_reuse_without_resume () =
  let attachment = media_attachment Backend_types.Png in
  match
    Codex_cli.build_invocation
      ~attachment_delivery:Backend_types.Reuse_session_attachments
      ~mcp_config_path:None
      (command_spec ~attachments:[ attachment ] ())
  with
  | Ok _ -> Alcotest.fail "session attachment reuse accepted without resume"
  | Error _ -> ()

let invalid_resume_session_ids () =
  [
    "";
    " ";
    "--last";
    "thread-123";
    "019C1F65-7F5A-7E13-9F8A-0C4F68D6A321";
    "019c1f65-7f5a-7e13-9f8a-0c4f68d6a321\n";
    String.make 129 'a';
  ]

let test_build_invocation_rejects_invalid_resume_session_ids () =
  List.iter
    (fun invalid_id ->
      match
        Codex_cli.build_invocation
          ~attachment_delivery:Backend_types.Upload_attachments
          ~mcp_config_path:None
          (command_spec ~resume_session_id:invalid_id ())
      with
      | Ok _ -> Alcotest.fail "invalid resume session id accepted"
      | Error msg ->
          Alcotest.(check bool)
            "session rejection is sanitized" true
            (contains_substring msg "resume session id is invalid");
          if String.trim invalid_id <> "" then
            Alcotest.(check bool)
              "session rejection omits caller value" false
              (contains_substring msg invalid_id))
    (invalid_resume_session_ids ())

let test_invalid_resume_session_ids_fail_before_config_or_spawn () =
  with_temp_dir "invalid-session" @@ fun temp_dir ->
  let marker = Filename.concat temp_dir "codex-ran" in
  let fake_codex = Filename.concat temp_dir "codex" in
  write_executable fake_codex
    (Printf.sprintf "#!/bin/sh\nprintf 'ran\\n' > %s\nexit 99\n"
       (Filename.quote marker));
  with_path_prefix temp_dir @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun invalid_id ->
      let spec =
        Backend_types.make_task_spec ~prompt:"must not run"
          ~working_dir:temp_dir ~resume_session_id:invalid_id ()
      in
      let result = Codex_cli.run_task ~sw ~env spec in
      (match result.status with
      | Backend_types.Failed msg ->
          Alcotest.(check bool)
            "run_task returns sanitized session rejection" true
            (contains_substring msg "resume session id is invalid")
      | _ -> Alcotest.fail "invalid session did not fail");
      Alcotest.(check bool)
        "invalid session creates no Codex config" false
        (Sys.file_exists (Filename.concat temp_dir ".codex/config.toml"));
      Alcotest.(check bool)
        "invalid session spawns no Codex process" false (Sys.file_exists marker))
    (invalid_resume_session_ids ())

let test_invalid_transport_fails_before_filesystem_or_spawn () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let attachment =
    media_attachment ~path:"../private/cover.png" Backend_types.Png
  in
  let result =
    Codex_cli.run_task ~sw ~env
      (command_spec ~attachments:[ attachment ]
         ~web_access:Backend_types.Web_search_and_fetch ())
  in
  match result.status with
  | Backend_types.Failed msg ->
      Alcotest.(check bool)
        "sensitive low-level request requires central authorization" true
        (contains_substring msg "central prepared transport authorization");
      Alcotest.(check bool)
        "failure does not reveal attachment path" false
        (contains_substring msg attachment.path)
  | _ -> Alcotest.fail "invalid request did not fail before spawn"

let test_web_only_and_untrusted_context_fail_before_config_or_spawn () =
  with_temp_dir "central-authorization" @@ fun temp_dir ->
  let marker = Filename.concat temp_dir "codex-ran" in
  write_executable
    (Filename.concat temp_dir "codex")
    (Printf.sprintf "#!/bin/sh\nprintf 'ran\\n' > %s\nexit 99\n"
       (Filename.quote marker));
  with_path_prefix temp_dir @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let run ?context task = Codex_cli.run_task ~sw ~env ?context task in
  let assert_rejected label result =
    (match result.Backend_types.status with
    | Backend_types.Failed message ->
        Alcotest.(check bool)
          (label ^ " sanitized authorization error")
          true
          (contains_substring message
             "central prepared transport authorization is required")
    | _ -> Alcotest.fail (label ^ " sensitive request was not rejected"));
    Alcotest.(check bool)
      (label ^ " creates no project config")
      false
      (Sys.file_exists (Filename.concat temp_dir ".codex/config.toml"));
    Alcotest.(check bool)
      (label ^ " spawns no process")
      false (Sys.file_exists marker)
  in
  assert_rejected "web-only low-level call"
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
          ~working_dir:temp_dir ~web_access:Backend_types.Web_search_and_fetch
          ()))

let assert_schema_file_wiring ?resume_session_id () =
  let spec =
    Backend_types.make_task_spec ?resume_session_id
      ~prompt:"Return a JSON object." ~working_dir:"/tmp/test"
      ~json_schema:sample_output_schema ()
  in
  let cmd, _stdin = Codex_cli.build_command ~mcp_config_path:None spec in
  (match resume_session_id with
  | Some sid ->
      Alcotest.(check bool)
        "resume command includes session id" true (command_contains sid cmd)
  | None ->
      Alcotest.(check bool)
        "normal command does not resume" false
        (command_contains "resume" cmd));
  Alcotest.(check string) "trailing stdin marker" "-" (command_last cmd);
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
            "schema value is not inline JSON" false
            (String.equal expected_json schema_path);
          Alcotest.(check bool)
            "schema path exists" true
            (Sys.file_exists schema_path);
          Alcotest.(check string)
            "schema file contents" expected_json (read_file schema_path);
          Alcotest.(check bool)
            "schema flag precedes stdin marker" true
            (schema_flag_index < List.length cmd - 1))

let test_build_command_with_output_schema_normal () =
  assert_schema_file_wiring ()

let test_build_command_with_output_schema_resume () =
  assert_schema_file_wiring ~resume_session_id:valid_thread_id ()

let test_build_command_without_output_schema () =
  let specs =
    [
      Backend_types.make_task_spec ~prompt:"No schema" ~working_dir:"/tmp/test"
        ();
      Backend_types.make_task_spec ~prompt:"No schema resume"
        ~working_dir:"/tmp/test" ~resume_session_id:valid_thread_id ();
    ]
  in
  List.iter
    (fun spec ->
      let cmd, _stdin = Codex_cli.build_command ~mcp_config_path:None spec in
      Alcotest.(check bool)
        "no --output-schema flag" false
        (command_contains "--output-schema" cmd))
    specs

let test_output_schema_file_is_task_scoped () =
  let path = ref None in
  Codex_cli.with_output_schema_file sample_output_schema (fun schema_path ->
      path := Some schema_path;
      Alcotest.(check bool)
        "schema exists in scope" true
        (Sys.file_exists schema_path));
  let schema_path =
    match !path with
    | Some value -> value
    | None -> Alcotest.fail "no schema path"
  in
  Alcotest.(check bool)
    "schema removed after scope" false
    (Sys.file_exists schema_path);
  let exceptional_path = ref None in
  (try
     Codex_cli.with_output_schema_file sample_output_schema (fun schema_path ->
         exceptional_path := Some schema_path;
         failwith "cancelled task")
   with Failure _ -> ());
  match !exceptional_path with
  | Some schema_path ->
      Alcotest.(check bool)
        "schema removed on exception" false
        (Sys.file_exists schema_path)
  | None -> Alcotest.fail "no exceptional schema path"

let command_construction_tests =
  [
    ( "zero images exact argv",
      `Quick,
      test_build_invocation_zero_images_exact_argv );
    ("read-only exact argv", `Quick, test_build_invocation_read_only_exact_argv);
    ( "one image with spaces exact argv",
      `Quick,
      test_build_invocation_one_image_with_spaces_exact_argv );
    ( "multiple images exact argv",
      `Quick,
      test_build_invocation_multiple_images_exact_argv );
    ( "resume uploads images",
      `Quick,
      test_build_invocation_resume_uploads_png_jpeg_with_schema );
    ( "resume reuses session images",
      `Quick,
      test_build_invocation_resume_reuses_media_with_schema );
    ( "image and schema compose",
      `Quick,
      test_build_invocation_image_schema_composition );
    ("web policies exact argv", `Quick, test_build_invocation_web_policies);
    ( "redacted argv hides sensitive values",
      `Quick,
      test_build_invocation_redacts_sensitive_argv );
    ( "unsafe attachment path rejected",
      `Quick,
      test_build_invocation_rejects_unsafe_staged_attachment_path );
    ( "one staged path required per upload",
      `Quick,
      test_build_invocation_requires_one_staged_path_per_upload );
    ( "reuse without resume rejected",
      `Quick,
      test_build_invocation_rejects_reuse_without_resume );
    ( "invalid resume session IDs rejected",
      `Quick,
      test_build_invocation_rejects_invalid_resume_session_ids );
    ( "invalid resume IDs fail before config or spawn",
      `Quick,
      test_invalid_resume_session_ids_fail_before_config_or_spawn );
    ( "invalid transport fails before filesystem/spawn",
      `Quick,
      test_invalid_transport_fails_before_filesystem_or_spawn );
    ( "web-only and untrusted context fail before config/spawn",
      `Quick,
      test_web_only_and_untrusted_context_fail_before_config_or_spawn );
    ( "build_command with output schema",
      `Quick,
      test_build_command_with_output_schema_normal );
    ( "build_command resume with output schema",
      `Quick,
      test_build_command_with_output_schema_resume );
    ( "build_command without output schema",
      `Quick,
      test_build_command_without_output_schema );
    ( "output schema file is task scoped",
      `Quick,
      test_output_schema_file_is_task_scoped );
  ]

(** {1 Availability Tests} *)

let test_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Just verify available runs without raising; check logic tested in
     test_backend_process.ml via check_available *)
  let (_ : bool) = Codex_cli.available ~sw ~env in
  ()

let availability_tests = [ ("available check", `Quick, test_available) ]

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

let e2e_harness_source rel_path standalone_path =
  let root = project_root () in
  let candidates =
    [ Filename.concat root rel_path; Filename.concat root standalone_path ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> read_file path
  | None -> Alcotest.fail (rel_path ^ " not found")

let e2e_config_source () =
  e2e_harness_source "libs/cabal/test/e2e_harness_config.ml"
    "test/e2e_harness_config.ml"

let demo_627_source () =
  e2e_harness_source "libs/cabal/test/test_demo_627.ml" "test/test_demo_627.ml"

let native_e2e_source () =
  e2e_harness_source "libs/cabal/test/test_native_json_schema_backends.ml"
    "test/test_native_json_schema_backends.ml"

let test_media_web_probe_offline_self_test () =
  let path =
    Filename.concat (project_root ()) "tools/probe_codex_media_web.py"
  in
  Alcotest.(check bool) "probe artifact exists" true (Sys.file_exists path);
  let permissions = (Unix.stat path).st_perm in
  Alcotest.(check bool)
    "probe artifact is executable" true
    (permissions land 0o111 <> 0);
  let command = Printf.sprintf "%s --self-test" (Filename.quote path) in
  Alcotest.(check int)
    "offline self-test executes mode-specific validators" 0
    (Sys.command command)

let test_e2e_harness_removes_shared_model_env_var () =
  List.iter
    (fun (label, source) ->
      Alcotest.(check bool)
        (label ^ " does not read shared CABAL_E2E_MODEL")
        false
        (contains_substring source "Sys.getenv_opt \"CABAL_E2E_MODEL\"");
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
      "CABAL_E2E_MODEL_OPENCODE";
      "CABAL_E2E_MODEL_GEMINI_CLI";
    ] ;
  Alcotest.(check bool)
    "quarantined Copilot has no executable E2E model contract" false
    (contains_substring source "CABAL_E2E_MODEL_COPILOT_CLI")

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
      "| \"opencode\" -> Some \"openai/gpt-5.4-mini\"";
      "| \"gemini-cli\" -> Some \"gemini-3-flash-preview\"";
    ] ;
  Alcotest.(check bool)
    "quarantined Copilot has no executable E2E default" false
    (contains_substring source "claude-haiku-4.5")

let test_demo_627_defaults_to_multi_backend_run () =
  let source = demo_627_source () in
  Alcotest.(check bool)
    "optional backend filter is allowed" true
    (contains_substring source "CABAL_E2E_BACKEND");
  Alcotest.(check bool)
    "missing backend filter is no longer a skip" false
    (contains_substring source "SKIPPED: CABAL_E2E_BACKEND not set");
  Alcotest.(check bool)
    "all_backend_ids drives default run" true
    (contains_substring source "all_backend_ids")

let test_e2e_harness_uses_test_managed_namespace () =
  let ns = E2e_harness_config.managed_namespace in
  Alcotest.(check string) "namespace id" "cabal-tests" ns.id;
  Alcotest.(check string) "namespace display" "Cabal tests" ns.display_name;
  Alcotest.(check string)
    "namespace config dir" ".cabal-tests/backend-config" ns.config_dir;
  List.iter
    (fun (label, source) ->
      Alcotest.(check bool)
        (label ^ " threads test managed namespace")
        true
        (contains_substring source
           "~managed_namespace:E2e_harness_config.managed_namespace"))
    [
      ("test_demo_627", demo_627_source ()); ("native E2E", native_e2e_source ());
    ]

let e2e_harness_model_contract_tests =
  [
    ( "media/web probe offline self-test",
      `Quick,
      test_media_web_probe_offline_self_test );
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
    "id via interface" "codex"
    (Agentic_backend.id backend);
  Alcotest.(check string)
    "name via interface" "OpenAI Codex"
    (Agentic_backend.name backend)

let interface_tests =
  [ ("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend) ]

(** {1 Test Runner} *)

let () =
  Alcotest.run "Codex_cli"
    [
      ("Identity", identity_tests);
      ("JSONL Output", jsonl_output_tests);
      ("Command Construction", command_construction_tests);
      ("Availability", availability_tests);
      ("E2E Harness Model Contract", e2e_harness_model_contract_tests);
      ("Interface", interface_tests);
    ]
