(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the CMV-style session trimmer. *)

open Cabal

(** {1 Path encoding tests} *)

let test_encode_working_dir_basic () =
  (* Claude Code encodes /home/user/project as -home-user-project:
     every non-[a-zA-Z0-9-] char (including the leading /) maps to single -. *)
  let result = Session_trimmer.encode_working_dir "/home/user/project" in
  Alcotest.(check string) "basic path" "-home-user-project" result

let test_encode_working_dir_no_slash () =
  (* Relative paths: / → single - *)
  let result = Session_trimmer.encode_working_dir "relative/path" in
  Alcotest.(check string) "no leading slash" "relative-path" result

let test_encode_working_dir_deep () =
  (* Verified against actual ~/.claude/projects/ directory names. *)
  let result = Session_trimmer.encode_working_dir "/home/mathias/dev/epure" in
  Alcotest.(check string) "deep path" "-home-mathias-dev-epure" result

let test_encode_working_dir_root () =
  let result = Session_trimmer.encode_working_dir "/" in
  Alcotest.(check string) "root" "-" result

let test_encode_working_dir_hidden_dir () =
  (* A dot-prefixed component: /.hidden → -- (/ and . each become -) *)
  let result = Session_trimmer.encode_working_dir "/path/.hidden/sub" in
  Alcotest.(check string) "hidden dir" "-path--hidden-sub" result

let test_encode_working_dir_worktree () =
  (* Matches actual ~/.claude/projects/ name for a worktree path.
     /home/mathias/dev/epure/.claude/worktrees/resource-guardian
       → -home-mathias-dev-epure--claude-worktrees-resource-guardian *)
  let result =
    Session_trimmer.encode_working_dir
      "/home/mathias/dev/epure/.claude/worktrees/resource-guardian"
  in
  Alcotest.(check string)
    "worktree path"
    "-home-mathias-dev-epure--claude-worktrees-resource-guardian"
    result

let test_encode_working_dir_existing_hyphen () =
  (* Existing hyphens in path components must be preserved unchanged. *)
  let result = Session_trimmer.encode_working_dir "/home/user/my-project" in
  Alcotest.(check string) "existing hyphen" "-home-user-my-project" result

let path_encoding_tests =
  [
    ("basic path", `Quick, test_encode_working_dir_basic);
    ("no leading slash", `Quick, test_encode_working_dir_no_slash);
    ("deep path", `Quick, test_encode_working_dir_deep);
    ("root path", `Quick, test_encode_working_dir_root);
    ("hidden dir component", `Quick, test_encode_working_dir_hidden_dir);
    ("worktree path", `Quick, test_encode_working_dir_worktree);
    ( "existing hyphen preserved",
      `Quick,
      test_encode_working_dir_existing_hyphen );
  ]

(** {1 Session file location tests} *)

let test_find_session_nonexistent () =
  let result =
    Session_trimmer.find_session_file
      ~working_dir:"/nonexistent/path"
      ~session_id:"abc-123"
  in
  Alcotest.(check (option string)) "not found" None result

let session_file_tests =
  [("nonexistent session", `Quick, test_find_session_nonexistent)]

(** {1 trim_line tests} *)

let test_trim_empty_line () =
  let result = Session_trimmer.trim_line ~threshold:500 "" in
  Alcotest.(check bool) "empty line dropped" true (Option.is_none result)

let test_trim_whitespace_line () =
  let result = Session_trimmer.trim_line ~threshold:500 "   " in
  Alcotest.(check bool) "whitespace dropped" true (Option.is_none result)

let test_trim_non_json_line () =
  let result = Session_trimmer.trim_line ~threshold:500 "not json at all" in
  match result with
  | None -> Alcotest.fail "non-JSON should be preserved"
  | Some (line, _) -> Alcotest.(check string) "preserved" "not json at all" line

let test_trim_file_history_snapshot () =
  let line =
    Yojson.Safe.to_string (`Assoc [("type", `String "file-history-snapshot")])
  in
  let result = Session_trimmer.trim_line ~threshold:500 line in
  Alcotest.(check bool)
    "file-history-snapshot dropped"
    true
    (Option.is_none result)

let test_trim_queue_operation () =
  let line =
    Yojson.Safe.to_string (`Assoc [("type", `String "queue-operation")])
  in
  let result = Session_trimmer.trim_line ~threshold:500 line in
  Alcotest.(check bool) "queue-operation dropped" true (Option.is_none result)

let test_trim_user_message_small () =
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ( "message",
             `Assoc
               [
                 ("role", `String "user");
                 ( "content",
                   `List
                     [
                       `Assoc
                         [("type", `String "text"); ("text", `String "hello")];
                     ] );
               ] );
         ])
  in
  let result = Session_trimmer.trim_line ~threshold:500 line in
  match result with
  | None -> Alcotest.fail "user message should be preserved"
  | Some (_, metrics) ->
      Alcotest.(check int)
        "no tool results stubbed"
        0
        metrics.tool_results_stubbed

let test_trim_user_message_large_tool_result () =
  let large_text = String.make 1000 'x' in
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ( "message",
             `Assoc
               [
                 ("role", `String "user");
                 ( "content",
                   `List
                     [
                       `Assoc
                         [
                           ("type", `String "text"); ("text", `String large_text);
                         ];
                     ] );
               ] );
         ])
  in
  let result = Session_trimmer.trim_line ~threshold:500 line in
  match result with
  | None -> Alcotest.fail "user message should be preserved"
  | Some (trimmed, metrics) ->
      Alcotest.(check int)
        "one tool result stubbed"
        1
        metrics.tool_results_stubbed ;
      Alcotest.(check bool)
        "trimmed is shorter"
        true
        (String.length trimmed < String.length line)

let test_trim_user_message_strips_images () =
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ( "message",
             `Assoc
               [
                 ("role", `String "user");
                 ( "content",
                   `List
                     [
                       `Assoc
                         [("type", `String "text"); ("text", `String "hello")];
                       `Assoc
                         [
                           ("type", `String "image");
                           ("source", `String "base64data");
                         ];
                     ] );
               ] );
         ])
  in
  let result = Session_trimmer.trim_line ~threshold:500 line in
  match result with
  | None -> Alcotest.fail "user message should be preserved"
  | Some (_, metrics) ->
      Alcotest.(check int) "one image stripped" 1 metrics.images_stripped

let test_trim_assistant_thinking () =
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ( "message",
             `Assoc
               [
                 ("role", `String "assistant");
                 ( "content",
                   `List
                     [
                       `Assoc
                         [
                           ("type", `String "thinking");
                           ("thinking", `String "deep thoughts");
                         ];
                       `Assoc
                         [("type", `String "text"); ("text", `String "answer")];
                     ] );
               ] );
         ])
  in
  let result = Session_trimmer.trim_line ~threshold:500 line in
  match result with
  | None -> Alcotest.fail "assistant message should be preserved"
  | Some (_, metrics) ->
      Alcotest.(check int) "thinking removed" 1 metrics.thinking_blocks_removed

let test_trim_assistant_write_tool_input () =
  let large_input =
    `Assoc
      [
        ("file_path", `String "/tmp/test.ml");
        ("content", `String (String.make 1000 'y'));
      ]
  in
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
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
                           ("name", `String "Write");
                           ("input", large_input);
                         ];
                     ] );
               ] );
         ])
  in
  let result = Session_trimmer.trim_line ~threshold:500 line in
  match result with
  | None -> Alcotest.fail "assistant message should be preserved"
  | Some (_, metrics) ->
      Alcotest.(check int) "tool input stubbed" 1 metrics.tool_inputs_stubbed

let test_trim_strips_usage () =
  let line =
    Yojson.Safe.to_string
      (`Assoc
         [
           ( "message",
             `Assoc [("role", `String "assistant"); ("content", `String "hi")]
           );
           ("usage", `Assoc [("input_tokens", `Int 100)]);
         ])
  in
  let result = Session_trimmer.trim_line ~threshold:500 line in
  match result with
  | None -> Alcotest.fail "message should be preserved"
  | Some (trimmed, _) ->
      let json = Yojson.Safe.from_string trimmed in
      let has_usage =
        try
          let _ = Yojson.Safe.Util.member "usage" json in
          Yojson.Safe.Util.member "usage" json <> `Null
        with _ -> false
      in
      Alcotest.(check bool) "usage stripped" false has_usage

let trim_line_tests =
  [
    ("empty line", `Quick, test_trim_empty_line);
    ("whitespace line", `Quick, test_trim_whitespace_line);
    ("non-JSON line", `Quick, test_trim_non_json_line);
    ("file-history-snapshot dropped", `Quick, test_trim_file_history_snapshot);
    ("queue-operation dropped", `Quick, test_trim_queue_operation);
    ("user message small preserved", `Quick, test_trim_user_message_small);
    ( "user message large tool result stubbed",
      `Quick,
      test_trim_user_message_large_tool_result );
    ("user message strips images", `Quick, test_trim_user_message_strips_images);
    ("assistant thinking removed", `Quick, test_trim_assistant_thinking);
    ( "assistant write tool input stubbed",
      `Quick,
      test_trim_assistant_write_tool_input );
    ("usage metadata stripped", `Quick, test_trim_strips_usage);
  ]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Session_trimmer"
    [
      ("path_encoding", path_encoding_tests);
      ("session_file", session_file_tests);
      ("trim_line", trim_line_tests);
    ]
