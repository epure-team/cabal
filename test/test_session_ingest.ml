(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal
open Portable_session

let fixture =
  String.concat
    "\n"
    [
      {|{"type":"summary","summary":"ignore me"}|};
      {|{"type":"user","sessionId":"s1","timestamp":"2026-01-01T00:00:00Z","message":{"role":"user","content":"hello there"}}|};
      {|{"type":"assistant","sessionId":"s1","timestamp":"2026-01-01T00:00:01Z","message":{"role":"assistant","model":"opus","content":[{"type":"thinking","thinking":"secret reasoning"},{"type":"text","text":"hi back"},{"type":"tool_use","id":"t1","name":"Bash","input":{"command":"ls"}}],"usage":{"input_tokens":10,"output_tokens":5}}}|};
      {|this is not json|};
      {|{"type":"user","sessionId":"s1","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"t1","content":"file.txt"}]}}|};
    ]

let test_ingest_shapes () =
  let evs = Session_ingest.claude_code fixture in
  Alcotest.(check int)
    "event count (summary+garbage skipped, thinking dropped)"
    4
    (List.length evs) ;
  Alcotest.(check bool)
    "thinking text never surfaces"
    true
    (not (List.exists (fun e -> e.text = "secret reasoning") evs)) ;
  match evs with
  | first :: _ ->
      Alcotest.(check bool) "first is user" true (first.role = User) ;
      Alcotest.(check string) "first text" "hello there" first.text ;
      Alcotest.(check (option string))
        "provenance client"
        (Some "claude-code")
        first.provenance.client ;
      Alcotest.(check (option string))
        "provenance session"
        (Some "s1")
        first.provenance.source_session
  | [] -> Alcotest.fail "no events"

let test_ingest_assistant_and_tools () =
  let evs = Session_ingest.claude_code fixture in
  let assistant = List.find (fun e -> e.role = Assistant) evs in
  Alcotest.(check string) "assistant text" "hi back" assistant.text ;
  Alcotest.(check (option string))
    "assistant model"
    (Some "opus")
    assistant.model ;
  (match assistant.tokens with
  | Some c ->
      Alcotest.(check (option int)) "input tokens" (Some 10) c.tokens_input
  | None -> Alcotest.fail "expected token usage") ;
  let tools = List.filter (fun e -> e.role = Tool) evs in
  Alcotest.(check int) "two tool events (use + result)" 2 (List.length tools) ;
  let tool_use =
    List.find
      (fun e -> match e.tool with Some t -> t.name = "Bash" | None -> false)
      evs
  in
  match tool_use.tool with
  | Some t ->
      Alcotest.(check bool)
        "input captured"
        true
        (String.length t.input_summary > 0)
  | None -> Alcotest.fail "expected tool ref"

let test_empty_and_blank () =
  Alcotest.(check int)
    "empty input"
    0
    (List.length (Session_ingest.claude_code "")) ;
  Alcotest.(check int)
    "blank lines"
    0
    (List.length (Session_ingest.claude_code "\n\n  \n"))

(* Regression: valid JSON that is not an object, or records with a non-object
   [message] / non-object content blocks, must be skipped, never raise
   (the ".mli" contract), including when mixed with good records. *)
let test_never_raises_on_odd_json () =
  let odd =
    String.concat
      "\n"
      [
        "42";
        {|"a bare string"|};
        "[1,2,3]";
        {|{"type":"user","message":"not-an-object"}|};
        {|{"type":"assistant","message":{"role":"assistant","content":["bare string block",{"type":"text","text":"good"}]}}|};
        {|{"type":"user","message":{"role":"user","content":"survivor"}}|};
      ]
  in
  let evs = Session_ingest.claude_code odd in
  (* The two good texts ("good" from a valid block beside a bad one, and
     "survivor") must survive; nothing raises. *)
  let ts = List.map (fun e -> e.text) evs in
  Alcotest.(check bool)
    "good block survived bad sibling"
    true
    (List.mem "good" ts) ;
  Alcotest.(check bool) "survivor record ingested" true (List.mem "survivor" ts)

let () =
  Alcotest.run
    "session_ingest"
    [
      ( "claude_code",
        [
          Alcotest.test_case "record shapes" `Quick test_ingest_shapes;
          Alcotest.test_case
            "assistant + tools"
            `Quick
            test_ingest_assistant_and_tools;
          Alcotest.test_case "empty/blank" `Quick test_empty_and_blank;
          Alcotest.test_case
            "never raises on odd json"
            `Quick
            test_never_raises_on_odd_json;
        ] );
    ]
