(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(* The Pi adapter's three helpers parse untrusted model output and decide
   whether to re-run a coding agent. Nothing pinned them, so this file does.
   Each case is written against a shape the stream can actually take, not
   against the implementation. *)

open Cabal.Yaml_adapter
open Cabal.Backend_types

let session_line id = Printf.sprintf {|{"type":"session","id":"%s"}|} id

let message_end blocks =
  Printf.sprintf
    {|{"type":"message_end","message":{"role":"assistant","content":[%s]}}|}
    (String.concat "," blocks)

let text_block s = Printf.sprintf {|{"type":"text","text":"%s"}|} s

let test_extracts_assistant_text () =
  let stdout =
    String.concat
      "\n"
      [session_line "s-1"; message_end [text_block "the answer"]]
  in
  Alcotest.(check string) "answer" "the answer" (parse_pi_json_events stdout)

let test_drops_non_text_blocks () =
  (* Reasoning must not reach the caller: the whole point of filtering on
     [type = "text"] is that a thinking block is not an answer. *)
  let thinking = {|{"type":"thinking","text":"internal deliberation"}|} in
  let stdout = message_end [thinking; text_block "public answer"] in
  let parsed = parse_pi_json_events stdout in
  Alcotest.(check string) "only the text block" "public answer" parsed

let test_ignores_malformed_and_foreign_lines () =
  (* A stray log line or a truncated record must not lose the answer that
     follows it. *)
  let stdout =
    String.concat
      "\n"
      [
        "not json at all";
        {|{"type":"message_end","message":{"role":"user","content":[]}}|};
        {|{"type":"message_end","message":|};
        message_end [text_block "survives"];
      ]
  in
  Alcotest.(check string) "answer survives" "survives" (parse_pi_json_events stdout)

let test_empty_when_nothing_parses () =
  (* Documented behaviour, and the reason the .mli says callers must treat ""
     as absence: a malformed stream is indistinguishable here from a model that
     answered with nothing. *)
  Alcotest.(check string) "garbage" "" (parse_pi_json_events "garbage\n{}\n") ;
  Alcotest.(check string) "empty input" "" (parse_pi_json_events "")

let test_session_id_found_and_absent () =
  let stdout =
    String.concat "\n" [session_line "abc-123"; message_end [text_block "x"]]
  in
  Alcotest.(check (option string))
    "found"
    (Some "abc-123")
    (parse_pi_session_id stdout) ;
  Alcotest.(check (option string))
    "absent"
    None
    (parse_pi_session_id (message_end [text_block "x"])) ;
  Alcotest.(check (option string))
    "malformed"
    None
    (parse_pi_session_id "{\"type\":\"session\"")

let result ~status ?(stdout = "") ?(stderr = "") () =
  make_task_result ~status ~stdout ~stderr ()

let test_retry_trigger_requires_failure () =
  let needle = "Error: stream ended without finish_reason" in
  Alcotest.(check bool)
    "failed with the marker"
    true
    (pi_stream_ended_without_finish_reason
       (result ~status:(Failed "x") ~stderr:needle ())) ;
  Alcotest.(check bool)
    "case-insensitive"
    true
    (pi_stream_ended_without_finish_reason
       (result ~status:(Failed "x") ~stderr:"STREAM ENDED WITHOUT FINISH_REASON" ())) ;
  (* A success is never retried, even if the phrase appears -- retrying a
     completed coding turn would re-apply its edits. *)
  Alcotest.(check bool)
    "success is never retried"
    false
    (pi_stream_ended_without_finish_reason
       (result ~status:Success ~stdout:needle ())) ;
  Alcotest.(check bool)
    "unrelated failure"
    false
    (pi_stream_ended_without_finish_reason
       (result ~status:(Failed "x") ~stderr:"compilation error" ()))

let () =
  Alcotest.run
    "pi_adapter"
    [
      ( "json_events",
        [
          ("assistant_text", `Quick, test_extracts_assistant_text);
          ("drops_non_text", `Quick, test_drops_non_text_blocks);
          ("malformed_lines", `Quick, test_ignores_malformed_and_foreign_lines);
          ("empty_on_garbage", `Quick, test_empty_when_nothing_parses);
        ] );
      ("session_id", [("found_absent", `Quick, test_session_id_found_and_absent)]);
      ( "retry_trigger",
        [("requires_failure", `Quick, test_retry_trigger_requires_failure)] );
    ]
