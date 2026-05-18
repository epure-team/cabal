(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for each adapter's [parse_stdout_text] extractor.

    These tests verify the per-adapter normalisation that backs
    [Backend_types.task_result.agent_text]: each adapter is fed a sample of
    the raw stdout shape its CLI is documented to produce, and the test
    pins the extracted agent text to the expected value.  No live CLI
    invocations happen here. *)

open Cabal

(** {1 claude-code: single JSON envelope with a [result] field} *)

let test_claude_code_extracts_result_field () =
  let stdout =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "result");
           ("subtype", `String "success");
           ("result", `String "Hello from claude.");
           ("session_id", `String "sess-1");
         ])
  in
  Alcotest.(check string)
    "agent text is the [result] field"
    "Hello from claude."
    (Claude_code.parse_stdout_text stdout)

let test_claude_code_falls_back_to_raw_on_non_json () =
  let stdout = "not json at all" in
  Alcotest.(check string)
    "fallback preserves raw stdout"
    stdout
    (Claude_code.parse_stdout_text stdout)

(** {1 codex: JSONL stream, final [item.completed/agent_message] item} *)

let test_codex_extracts_last_agent_message () =
  (* Codex emits a stream of JSON events; the final assistant reply is the
     last [item.completed]/[agent_message] item, optionally followed by a
     [turn.completed] event with usage data. *)
  let stdout =
    {|{"type":"thread.started","thread_id":"t-1"}
{"type":"item.started","item":{"id":"item_0","type":"agent_message"}}
{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"partial reply"}}
{"type":"item.completed","item":{"id":"item_1","type":"agent_message","text":"Final agent reply."}}
{"type":"turn.completed","usage":{"input_tokens":42,"output_tokens":7}}|}
  in
  Alcotest.(check string)
    "agent text is the last agent_message item"
    "Final agent reply."
    (Codex_cli.parse_stdout_text stdout)

let test_codex_falls_back_to_raw_when_no_agent_message () =
  let stdout =
    {|{"type":"thread.started","thread_id":"t-1"}
{"type":"turn.completed","usage":{"input_tokens":1,"output_tokens":0}}|}
  in
  Alcotest.(check string)
    "fallback returns raw stdout when no agent_message"
    stdout
    (Codex_cli.parse_stdout_text stdout)

(** {1 gemini: stream-json NDJSON, [response]/[result] fields} *)

let test_gemini_extracts_response_field () =
  let stdout =
    String.concat
      "\n"
      [
        Yojson.Safe.to_string (`Assoc [("type", `String "init")]);
        Yojson.Safe.to_string
          (`Assoc
             [
               ("response", `String "Hello from gemini.");
               ( "usageMetadata",
                 `Assoc
                   [
                     ("promptTokenCount", `Int 12);
                     ("candidatesTokenCount", `Int 3);
                   ] );
             ]);
      ]
  in
  Alcotest.(check string)
    "agent text is the [response] field"
    "Hello from gemini."
    (Gemini_cli.parse_stdout_text stdout)

let test_gemini_falls_back_to_raw_on_non_json () =
  let stdout = "non-json output\n" in
  Alcotest.(check string)
    "fallback preserves raw stdout"
    stdout
    (Gemini_cli.parse_stdout_text stdout)

(** {1 opencode: JSON events stream} *)

let test_opencode_extracts_response_via_json_events () =
  (* Round-trip through [parse_json_events] to keep the test resilient to
     the precise event shape opencode uses internally: the contract here is
     [parse_stdout_text stdout = fst (parse_json_events stdout)]. *)
  let stdout =
    Yojson.Safe.to_string (`Assoc [("response", `String "from opencode")])
  in
  let expected, _ = Opencode_cli.parse_json_events stdout in
  Alcotest.(check string)
    "agent text matches parse_json_events"
    expected
    (Opencode_cli.parse_stdout_text stdout)

(** {1 copilot: plain text, no structured envelope} *)

let test_copilot_returns_stdout_verbatim () =
  let stdout = "Plain-text reply from copilot." in
  Alcotest.(check string)
    "agent text is stdout (whitespace-trimmed)"
    stdout
    (Copilot_cli.parse_stdout_text stdout)

let test_copilot_trims_trailing_whitespace () =
  let stdout = "Hello world\n\n  " in
  Alcotest.(check string)
    "trailing whitespace stripped"
    "Hello world"
    (Copilot_cli.parse_stdout_text stdout)

(** {1 task_result wiring}

    Smoke check that [make_task_result ~agent_text:...] is wired through and
    accessible via the record field. *)

let test_make_task_result_carries_agent_text () =
  let r =
    Backend_types.make_task_result
      ~status:Backend_types.Success
      ~stdout:"raw bytes here"
      ~agent_text:"normalised text"
      ()
  in
  Alcotest.(check string)
    "agent_text round-trip"
    "normalised text"
    r.Backend_types.agent_text ;
  Alcotest.(check string) "stdout untouched" "raw bytes here" r.stdout

let test_make_task_result_defaults_agent_text_empty () =
  let r = Backend_types.make_task_result ~status:Backend_types.Success () in
  Alcotest.(check string)
    "agent_text defaults to empty string"
    ""
    r.Backend_types.agent_text

let () =
  Alcotest.run
    "Agent_text_extraction"
    [
      ( "claude-code",
        [
          ( "extracts result field",
            `Quick,
            test_claude_code_extracts_result_field );
          ( "falls back to raw on non-JSON",
            `Quick,
            test_claude_code_falls_back_to_raw_on_non_json );
        ] );
      ( "codex",
        [
          ( "extracts last agent_message from JSONL",
            `Quick,
            test_codex_extracts_last_agent_message );
          ( "falls back to raw when no agent_message",
            `Quick,
            test_codex_falls_back_to_raw_when_no_agent_message );
        ] );
      ( "gemini",
        [
          ("extracts response field", `Quick, test_gemini_extracts_response_field);
          ( "falls back to raw on non-JSON",
            `Quick,
            test_gemini_falls_back_to_raw_on_non_json );
        ] );
      ( "opencode",
        [
          ( "extracts response via json events",
            `Quick,
            test_opencode_extracts_response_via_json_events );
        ] );
      ( "copilot",
        [
          ( "returns stdout verbatim",
            `Quick,
            test_copilot_returns_stdout_verbatim );
          ( "trims trailing whitespace",
            `Quick,
            test_copilot_trims_trailing_whitespace );
        ] );
      ( "task_result wiring",
        [
          ( "make_task_result carries agent_text",
            `Quick,
            test_make_task_result_carries_agent_text );
          ( "make_task_result defaults to empty",
            `Quick,
            test_make_task_result_defaults_agent_text_empty );
        ] );
    ]
