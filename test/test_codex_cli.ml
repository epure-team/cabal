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

let identity_tests =
  [
    ("id is codex", `Quick, test_id); ("name is OpenAI Codex", `Quick, test_name);
  ]

(** {1 JSONL Output Parsing Tests} *)

let test_parse_jsonl_with_message_and_usage () =
  let input =
    {|{"type":"thread.started","thread_id":"abc"}
{"type":"item.completed","item":{"type":"agent_message","text":"First message"}}
{"type":"item.completed","item":{"type":"agent_message","text":"Final result"}}
{"type":"turn.completed","usage":{"input_tokens":200,"output_tokens":75}}|}
  in
  let text, cost = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string) "last message" "Final result" text ;
  Alcotest.(check bool) "cost present" true (Option.is_some cost) ;
  match cost with
  | Some c ->
      Alcotest.(check (option int)) "input_tokens" (Some 200) c.tokens_input ;
      Alcotest.(check (option int)) "output_tokens" (Some 75) c.tokens_output
  | None -> Alcotest.fail "Expected cost to be Some"

let test_parse_jsonl_no_usage () =
  let input =
    {|{"type":"item.completed","item":{"type":"agent_message","text":"Hello world"}}|}
  in
  let text, cost = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string) "message text" "Hello world" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_jsonl_empty () =
  let text, cost = Codex_cli.parse_jsonl_output "" in
  (* Falls back to raw stdout *)
  Alcotest.(check string) "empty fallback" "" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_jsonl_malformed () =
  let input = "not json at all\n{invalid" in
  let text, cost = Codex_cli.parse_jsonl_output input in
  (* Falls back to raw stdout *)
  Alcotest.(check string) "malformed fallback" input text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let jsonl_output_tests =
  [
    ( "parse with message and usage",
      `Quick,
      test_parse_jsonl_with_message_and_usage );
    ("parse without usage", `Quick, test_parse_jsonl_no_usage);
    ("parse empty output", `Quick, test_parse_jsonl_empty);
    ("parse malformed output", `Quick, test_parse_jsonl_malformed);
  ]

(** {1 Availability Tests} *)

let test_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Just verify available runs without raising; check logic tested in
     test_backend_process.ml via check_available *)
  let (_ : bool) = Codex_cli.available ~sw ~env in
  ()

let availability_tests = [("available check", `Quick, test_available)]

(** {1 Backend Interface Compliance Tests} *)

let test_implements_agentic_backend () =
  let backend = (module Codex_cli : Agentic_backend.S) in
  Alcotest.(check string)
    "id via interface"
    "codex"
    (Agentic_backend.id backend) ;
  Alcotest.(check string)
    "name via interface"
    "OpenAI Codex"
    (Agentic_backend.name backend)

let interface_tests =
  [("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend)]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Codex_cli"
    [
      ("Identity", identity_tests);
      ("JSONL Output", jsonl_output_tests);
      ("Availability", availability_tests);
      ("Interface", interface_tests);
    ]
