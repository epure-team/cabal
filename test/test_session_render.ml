(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal
open Portable_session

let pair e = (e.role, e.text)

let test_roundtrip () =
  (* render → ingest preserves role and text for user/assistant events (FR-004). *)
  let evs =
    [
      make_event User "hello";
      make_event Assistant "world";
      make_event User "again";
    ]
  in
  let jsonl = Session_render.claude_code evs in
  let back = Session_ingest.claude_code jsonl in
  Alcotest.(check int) "count preserved" 3 (List.length back) ;
  Alcotest.(check bool)
    "roles and texts preserved"
    true
    (List.map pair evs = List.map pair back)

let test_conversation_only () =
  (* Tool/System events are dropped by render (conversation-only, FR-003). *)
  let evs =
    [
      make_event User "u";
      make_event
        ~tool:{name = "Bash"; input_summary = "ls"; output_summary = ""}
        Tool
        "";
      make_event System "sys";
      make_event Assistant "a";
    ]
  in
  let back = Session_ingest.claude_code (Session_render.claude_code evs) in
  Alcotest.(check (list string))
    "only user+assistant survive"
    ["u"; "a"]
    (List.map (fun e -> e.text) back)

let test_valid_jsonl () =
  let jsonl =
    Session_render.claude_code [make_event User "x"; make_event Assistant "y"]
  in
  List.iter
    (fun line ->
      if String.trim line <> "" then
        match Yojson.Safe.from_string line with
        | _ -> ()
        | exception _ -> Alcotest.failf "invalid JSON line: %s" line)
    (String.split_on_char '\n' jsonl)

let test_empty () =
  Alcotest.(check string)
    "empty input yields empty string"
    ""
    (Session_render.claude_code [])

let () =
  Alcotest.run
    "session_render"
    [
      ( "claude_code",
        [
          Alcotest.test_case "ingest∘render roundtrip" `Quick test_roundtrip;
          Alcotest.test_case "conversation-only" `Quick test_conversation_only;
          Alcotest.test_case "valid jsonl" `Quick test_valid_jsonl;
          Alcotest.test_case "empty" `Quick test_empty;
        ] );
    ]
