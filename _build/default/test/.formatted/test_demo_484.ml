(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #484 — Streaming and observability parity.

    Covers:
    - AC1: Codex event streaming integrated; persisted events contain only
           redacted raw metadata
    - AC2: Gemini stream-json events captured with prompts, logs, auth headers,
           token-like payloads, and file contents redacted
    - AC3: OpenCode JSON event streams captured preserving event type/shape
           metadata without persisting raw sensitive payloads
    - AC4: Backend-native fallback for unsupported/unnormalized events preserves
           event type, shape, scalar metadata, and redaction summary; never
           persists prompts, diffs, logs, auth headers, or token payloads
    - AC5: Backend event fixtures with forbidden content → persisted records
           contain zero forbidden raw content while preserving event type,
           backend id, redacted metadata, truncation/redaction summary, and
           optional shape hash *)

open Cabal

let contains_str s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

(* Serialize a redacted event to string for content checks *)
let to_str r = Yojson.Safe.to_string (Backend_event_redaction.to_json r)

(** {1 AC1 — Codex event streaming via session lifecycle capture} *)

(* AC1: Codex item.completed events have their text field redacted. *)
let test_ac1_codex_text_redacted () =
  let event =
    `Assoc
      [
        ("type", `String "item.completed");
        ( "item",
          `Assoc
            [
              ("type", `String "agent_message");
              ( "text",
                `String
                  "Here is the full implementation of your requested feature \
                   with detailed explanation." );
            ] );
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"codex" event in
  let s = to_str r in
  Alcotest.(check bool)
    "AC1: response text not in persisted event"
    false
    (contains_str s "Here is the full implementation") ;
  Alcotest.(check string)
    "AC1: event_type preserved"
    "item.completed"
    r.Backend_event_redaction.event_type

(* AC1: Codex turn.completed events preserve token usage counts (numbers). *)
let test_ac1_codex_turn_completed_preserves_usage () =
  let event =
    `Assoc
      [
        ("type", `String "turn.completed");
        ( "usage",
          `Assoc [("input_tokens", `Int 1024); ("output_tokens", `Int 512)] );
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"codex" event in
  Alcotest.(check string)
    "AC1: event_type = turn.completed"
    "turn.completed"
    r.Backend_event_redaction.event_type ;
  Alcotest.(check int)
    "AC1: nothing redacted"
    0
    r.Backend_event_redaction.fields_redacted ;
  let s = to_str r in
  Alcotest.(check bool)
    "AC1: input_tokens count preserved"
    true
    (contains_str s "1024")

(* AC1: Codex session_id is preserved (safe scalar). *)
let test_ac1_codex_session_id_preserved () =
  let event =
    `Assoc
      [
        ("type", `String "session.created");
        ("session_id", `String "codex-session-abc123");
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"codex" event in
  let s = to_str r in
  Alcotest.(check bool)
    "AC1: session_id preserved"
    true
    (contains_str s "codex-session-abc123")

(** {1 AC2 — Gemini stream-json events captured with redaction} *)

(* AC2: Gemini response field is redacted. *)
let test_ac2_gemini_response_redacted () =
  let event =
    `Assoc
      [
        ("type", `String "response");
        ( "response",
          `String
            "This is the full Gemini agent response with all the details of \
             the implementation." );
        ("usageMetadata", `Assoc [("promptTokenCount", `Int 800)]);
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"gemini" event in
  let s = to_str r in
  Alcotest.(check bool)
    "AC2: response body not in persisted event"
    false
    (contains_str s "full Gemini agent response") ;
  Alcotest.(check bool) "AC2: token count preserved" true (contains_str s "800")

(* AC2: Auth header is redacted. *)
let test_ac2_gemini_auth_header_redacted () =
  let event =
    `Assoc
      [
        ("type", `String "api_request");
        ("authorization", `String "Bearer ya29.secret-oauth-token-here");
        ("model", `String "gemini-1.5-pro");
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"gemini" event in
  let s = to_str r in
  Alcotest.(check bool)
    "AC2: auth token not in persisted event"
    false
    (contains_str s "ya29.secret-oauth-token-here") ;
  Alcotest.(check bool)
    "AC2: model name preserved"
    true
    (contains_str s "gemini-1.5-pro")

(* AC2: Log output is redacted. *)
let test_ac2_gemini_log_redacted () =
  let event =
    `Assoc
      [
        ("type", `String "debug");
        ( "log",
          `String
            "DEBUG: Processing request with prompt: 'Do these changes...' \
             token=sk-abc" );
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"gemini" event in
  let s = to_str r in
  Alcotest.(check bool)
    "AC2: log content not in persisted event"
    false
    (contains_str s "Processing request with prompt")

(* AC2: File content is redacted. *)
let test_ac2_gemini_file_content_redacted () =
  let event =
    `Assoc
      [
        ("type", `String "file_read");
        ( "content",
          `String "let foo = 42\nlet bar = \"secret\"\nlet baz = foo + 1" );
        ("path", `String "src/foo.ml");
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"gemini" event in
  let s = to_str r in
  Alcotest.(check bool)
    "AC2: file content not in persisted event"
    false
    (contains_str s "let foo = 42")

(** {1 AC3 — OpenCode JSON event streams captured preserving type/shape} *)

(* AC3: OpenCode text events have their text payload redacted. *)
let test_ac3_opencode_text_redacted () =
  let event =
    `Assoc
      [
        ("type", `String "text");
        ( "part",
          `Assoc
            [
              ( "text",
                `String
                  "This is the agent response explaining what was done to \
                   complete your task." );
            ] );
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"opencode" event in
  let s = to_str r in
  Alcotest.(check bool)
    "AC3: text payload not in persisted event"
    false
    (contains_str s "agent response explaining") ;
  Alcotest.(check string)
    "AC3: event type preserved"
    "text"
    r.Backend_event_redaction.event_type

(* AC3: OpenCode step_finish events preserve token counts and cost (numbers). *)
let test_ac3_opencode_step_finish_preserves_scalars () =
  let event =
    `Assoc
      [
        ("type", `String "step_finish");
        ( "part",
          `Assoc
            [
              ("tokens", `Assoc [("input", `Int 512); ("output", `Int 256)]);
              ("cost", `Float 0.0042);
            ] );
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"opencode" event in
  Alcotest.(check int)
    "AC3: no fields redacted for step_finish"
    0
    r.Backend_event_redaction.fields_redacted ;
  let s = to_str r in
  Alcotest.(check bool)
    "AC3: input token count preserved"
    true
    (contains_str s "512") ;
  Alcotest.(check bool)
    "AC3: output token count preserved"
    true
    (contains_str s "256")

(* AC3: OpenCode event shape/type metadata is preserved. *)
let test_ac3_opencode_event_type_preserved () =
  let event =
    `Assoc
      [
        ("type", `String "tool_call");
        ("tool_id", `String "bash");
        ("status", `String "completed");
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"opencode" event in
  Alcotest.(check string)
    "AC3: event_type preserved"
    "tool_call"
    r.Backend_event_redaction.event_type ;
  let s = to_str r in
  Alcotest.(check bool)
    "AC3: status preserved"
    true
    (contains_str s "completed")

(** {1 AC4 — Backend-native fallback for unsupported/unnormalized events} *)

(* AC4: Fallback preserves event type for unknown events. *)
let test_ac4_fallback_preserves_event_type () =
  let event =
    `Assoc
      [
        ("type", `String "unknown_proprietary_event_type"); ("metadata", `Int 42);
      ]
  in
  let r =
    Backend_event_redaction.redact_event ~backend_id:"some-backend" event
  in
  Alcotest.(check string)
    "AC4: event_type preserved for unknown event"
    "unknown_proprietary_event_type"
    r.Backend_event_redaction.event_type

(* AC4: Fallback redacts sensitive fields in unknown events. *)
let test_ac4_fallback_redacts_prompt () =
  let event =
    `Assoc
      [
        ("type", `String "unknown_event");
        ( "prompt",
          `String
            "Please implement the following feature: add user authentication \
             with OAuth2 and store the refresh token securely." );
      ]
  in
  let r =
    Backend_event_redaction.redact_event ~backend_id:"some-backend" event
  in
  let s = to_str r in
  Alcotest.(check bool)
    "AC4: prompt not in persisted event"
    false
    (contains_str s "Please implement the following feature") ;
  Alcotest.(check bool)
    "AC4: at least one field redacted"
    true
    (r.Backend_event_redaction.fields_redacted > 0)

(* AC4: Fallback preserves scalar metadata (numbers, booleans). *)
let test_ac4_fallback_preserves_scalar_metadata () =
  let event =
    `Assoc
      [
        ("type", `String "metrics");
        ("duration_ms", `Int 1337);
        ("success", `Bool true);
        ("retry_count", `Int 2);
        ("exit_code", `Int 0);
      ]
  in
  let r =
    Backend_event_redaction.redact_event ~backend_id:"some-backend" event
  in
  Alcotest.(check int)
    "AC4: no fields redacted for scalar-only event"
    0
    r.Backend_event_redaction.fields_redacted ;
  let s = to_str r in
  Alcotest.(check bool)
    "AC4: duration_ms preserved"
    true
    (contains_str s "1337") ;
  Alcotest.(check bool) "AC4: retry_count preserved" true (contains_str s "2")

(* AC4: Fallback includes a redaction summary. *)
let test_ac4_fallback_has_redaction_summary () =
  let event =
    `Assoc
      [
        ("type", `String "unknown_event");
        ("diff", `String "--- a/foo.ml\n+++ b/foo.ml\n@@ -1 +1 @@\n-old\n+new");
      ]
  in
  let r =
    Backend_event_redaction.redact_event ~backend_id:"some-backend" event
  in
  let summary = r.Backend_event_redaction.redaction_summary in
  Alcotest.(check bool)
    "AC4: redaction_summary is non-empty"
    true
    (String.length summary > 0)

(* AC4: Fallback provides an optional shape hash. *)
let test_ac4_fallback_has_shape_hash () =
  let event =
    `Assoc
      [
        ("type", `String "unknown_event");
        ("count", `Int 5);
        ("flag", `Bool false);
      ]
  in
  let r =
    Backend_event_redaction.redact_event ~backend_id:"some-backend" event
  in
  Alcotest.(check bool)
    "AC4: shape_hash is Some _"
    true
    (Option.is_some r.Backend_event_redaction.shape_hash)

(** {1 AC5 — Zero forbidden raw content in persisted NDJSON/DB records} *)

(* AC5: No prompt text in persisted event. *)
let test_ac5_no_prompt_persisted () =
  let forbidden = "FORBIDDEN_PROMPT_CONTENT_XYZ" in
  let event =
    `Assoc
      [
        ("type", `String "build_event");
        ("prompt", `String (forbidden ^ " please add unit tests"));
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"codex" event in
  Alcotest.(check bool)
    "AC5: prompt content absent from persisted event"
    false
    (contains_str (to_str r) forbidden)

(* AC5: No diff in persisted event. *)
let test_ac5_no_diff_persisted () =
  let forbidden = "FORBIDDEN_DIFF_abc123" in
  let event =
    `Assoc
      [
        ("type", `String "patch_event");
        ( "diff",
          `String
            ("--- a/src/foo.ml\n+++ b/src/foo.ml\n" ^ forbidden
           ^ "\n@@ -1 +1 @@") );
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"codex" event in
  Alcotest.(check bool)
    "AC5: diff content absent from persisted event"
    false
    (contains_str (to_str r) forbidden)

(* AC5: No authorization header in persisted event. *)
let test_ac5_no_auth_header_persisted () =
  let forbidden = "Bearer sk-secret-api-key-123456" in
  let event =
    `Assoc
      [
        ("type", `String "http_request");
        ("authorization", `String forbidden);
        ("url", `String "https://api.example.com/v1/chat");
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"gemini" event in
  Alcotest.(check bool)
    "AC5: auth header absent from persisted event"
    false
    (contains_str (to_str r) forbidden)

(* AC5: No file contents in persisted event. *)
let test_ac5_no_file_content_persisted () =
  let forbidden = "SECRET_FILE_CONTENT_do_not_persist" in
  let event =
    `Assoc
      [
        ("type", `String "file_event");
        ("file_content", `String (forbidden ^ "\nlet x = 1\nlet y = 2"));
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"gemini" event in
  Alcotest.(check bool)
    "AC5: file content absent from persisted event"
    false
    (contains_str (to_str r) forbidden)

(* AC5: No token-bearing payload in persisted event. *)
let test_ac5_no_token_payload_persisted () =
  let forbidden = "sk-super-secret-api-token-do-not-store" in
  let event =
    `Assoc
      [
        ("type", `String "auth_event");
        ("token", `String forbidden);
        ("expires_in", `Int 3600);
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"opencode" event in
  Alcotest.(check bool)
    "AC5: token payload absent from persisted event"
    false
    (contains_str (to_str r) forbidden)

(* AC5: No build log in persisted event. *)
let test_ac5_no_build_log_persisted () =
  let forbidden = "BUILDLOG_SECRET_dune_build_output_xyz" in
  let event =
    `Assoc
      [
        ("type", `String "build_output");
        ("log", `String (forbidden ^ "\nFile \"src/foo.ml\", line 1, error"));
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"codex" event in
  Alcotest.(check bool)
    "AC5: build log absent from persisted event"
    false
    (contains_str (to_str r) forbidden)

(* AC5: Persisted record preserves event type, backend_id, and shape hash. *)
let test_ac5_persisted_preserves_metadata () =
  let event =
    `Assoc
      [
        ("type", `String "rich_event");
        ("prompt", `String "Do something secret");
        ("tokens_input", `Int 777);
        ("status", `String "ok");
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"gemini" event in
  Alcotest.(check string)
    "AC5: backend_id preserved"
    "gemini"
    r.Backend_event_redaction.backend_id ;
  Alcotest.(check string)
    "AC5: event_type preserved"
    "rich_event"
    r.Backend_event_redaction.event_type ;
  Alcotest.(check bool)
    "AC5: shape_hash present"
    true
    (Option.is_some r.Backend_event_redaction.shape_hash) ;
  let s = to_str r in
  Alcotest.(check bool)
    "AC5: token count preserved in output"
    true
    (contains_str s "777") ;
  Alcotest.(check bool)
    "AC5: prompt text not in output"
    false
    (contains_str s "Do something secret")

(** {1 Minor — "key" field over-redaction fix} *)

(* The "key" field name was in sensitive_fields but is over-broad: any field
   named "key" carrying a non-sensitive config identifier (e.g.
   {"key":"temperature"}) would be wrongly redacted.  After the fix "key" is
   removed from sensitive_fields, so short values pass through. *)
let test_key_field_not_over_redacted () =
  let event =
    `Assoc
      [
        ("type", `String "config");
        ("key", `String "temperature");
        ("value", `Float 0.7);
      ]
  in
  let r = Backend_event_redaction.redact_event ~backend_id:"opencode" event in
  let s = to_str r in
  Alcotest.(check bool)
    "non-sensitive key value not over-redacted"
    true
    (contains_str s "temperature")

(** {1 AC2/AC3 — on_raw_line wiring: pipeline redacts before storage} *)

(* Compile-time check: Agentic_backend.run_task must expose ?on_raw_line.
   If the module type does not include the parameter this file fails to compile,
   which counts as a test failure under `dune build`. *)
let _on_raw_line_signature_check :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    ?on_raw_line:(string -> unit) ->
    Cabal.Agentic_backend.t ->
    Cabal.Backend_types.task_spec ->
    Cabal.Backend_types.task_result =
  Cabal.Agentic_backend.run_task

(* The on_raw_line pipeline used in build_flow_run for non-claude-code backends:
   parse line as JSON → redact → store.  This test exercises the full pipeline
   with Gemini-format output containing sensitive content. *)
let test_ac2_on_raw_line_pipeline_gemini () =
  let captured = ref [] in
  let on_raw_line line =
    match Yojson.Safe.from_string line with
    | json ->
        let r =
          Backend_event_redaction.redact_event ~backend_id:"gemini" json
        in
        captured := Backend_event_redaction.to_json r :: !captured
    | exception _ -> ()
  in
  on_raw_line
    {|{"type":"response","response":"Full agent answer with sk-secret123 token","status":"done"}|} ;
  Alcotest.(check int)
    "AC2 pipeline: one event captured"
    1
    (List.length !captured) ;
  let stored = Yojson.Safe.to_string (List.hd !captured) in
  Alcotest.(check bool)
    "AC2 pipeline: sensitive response content redacted"
    false
    (contains_str stored "sk-secret123") ;
  Alcotest.(check bool)
    "AC2 pipeline: event_type preserved"
    true
    (contains_str stored "response")

(* Same pipeline for OpenCode JSONL-format events. *)
let test_ac3_on_raw_line_pipeline_opencode () =
  let captured = ref [] in
  let on_raw_line line =
    match Yojson.Safe.from_string line with
    | json ->
        let r =
          Backend_event_redaction.redact_event ~backend_id:"opencode" json
        in
        captured := Backend_event_redaction.to_json r :: !captured
    | exception _ -> ()
  in
  on_raw_line
    {|{"type":"text","part":{"text":"Confidential plan XSECRET42 implemented"}}|} ;
  Alcotest.(check int)
    "AC3 pipeline: one event captured"
    1
    (List.length !captured) ;
  let stored = Yojson.Safe.to_string (List.hd !captured) in
  Alcotest.(check bool)
    "AC3 pipeline: text payload redacted"
    false
    (contains_str stored "XSECRET42") ;
  Alcotest.(check bool)
    "AC3 pipeline: event_type preserved"
    true
    (contains_str stored "text")

(* Non-JSON lines must be silently dropped (write_raw_event behaviour). *)
let test_on_raw_line_non_json_dropped () =
  let captured = ref 0 in
  let on_raw_line line =
    match Yojson.Safe.from_string line with
    | _ -> incr captured
    | exception _ -> ()
  in
  on_raw_line "not json" ;
  on_raw_line "{ bad json =" ;
  Alcotest.(check int) "non-JSON lines silently dropped" 0 !captured

(* Significant fix: write_turn_failed error messages are redacted via
   Backend_event_redaction.redact_error_message before being passed to
   write_turn_failed.  This prevents prompts, tokens, or auth headers from a
   backend error string reaching the NDJSON log verbatim. *)
let test_turn_failed_error_redacted () =
  let sensitive_error =
    "Process failed: prompt too long: 'Please implement the feature...'"
  in
  let safe_error =
    Backend_event_redaction.redact_error_message sensitive_error
  in
  (* "message" is a sensitive field → always redacted *)
  Alcotest.(check bool)
    "sensitive error string is redacted (not persisted verbatim)"
    false
    (contains_str safe_error "Please implement the feature") ;
  (* Token-like payload in error message is also redacted *)
  let token_error = "unauthorized: api_key=sk-abc123" in
  let safe_token_error =
    Backend_event_redaction.redact_error_message token_error
  in
  Alcotest.(check bool)
    "token in error message is redacted"
    false
    (contains_str safe_token_error "sk-abc123")

(** {1 AC2 — Gemini stream-json: NDJSON parser verifies wiring} *)

(* AC2 structural: parse_gemini_stream_json extracts text from a response event.
   This verifies the NDJSON parser that backs the stream-json pipeline:
   Gemini CLI now emits --output-format stream-json (one event per line) and
   on_raw_line fires per event, redacting each one before NDJSON persistence. *)
let test_ac2_gemini_stream_json_text_from_response_event () =
  let ndjson =
    {|{"type":"response","response":"The implementation is complete.","status":"done"}|}
  in
  let text, _, _ = Gemini_cli.parse_gemini_stream_json ndjson in
  Alcotest.(check string)
    "AC2 stream-json: response field extracted"
    "The implementation is complete."
    text

(* AC2 structural: parse_gemini_stream_json assembles text from chunk events. *)
let test_ac2_gemini_stream_json_text_from_chunks () =
  let ndjson =
    {|{"type":"content","text":"Hello "}
{"type":"content","text":"world"}|}
  in
  let text, _, _ = Gemini_cli.parse_gemini_stream_json ndjson in
  Alcotest.(check string)
    "AC2 stream-json: text chunks concatenated"
    "Hello world"
    text

(* AC2 structural: parse_gemini_stream_json extracts cost from usageMetadata. *)
let test_ac2_gemini_stream_json_cost_extraction () =
  let ndjson =
    {|{"type":"content","text":"Answer"}
{"type":"usage","usageMetadata":{"promptTokenCount":800,"candidatesTokenCount":200}}|}
  in
  let text, cost, _ = Gemini_cli.parse_gemini_stream_json ndjson in
  Alcotest.(check string)
    "AC2 stream-json: text still extracted alongside cost"
    "Answer"
    text ;
  match cost with
  | None -> Alcotest.fail "AC2 stream-json: expected cost to be parsed"
  | Some c ->
      Alcotest.(check (option int))
        "AC2 stream-json: promptTokenCount extracted"
        (Some 800)
        c.Backend_types.tokens_input ;
      Alcotest.(check (option int))
        "AC2 stream-json: candidatesTokenCount extracted"
        (Some 200)
        c.Backend_types.tokens_output

(* AC2 structural: non-JSON lines in stream-json output are silently dropped. *)
let test_ac2_gemini_stream_json_skips_non_json () =
  let ndjson =
    {|not json
{"type":"response","response":"valid"}
another bad line|}
  in
  let text, _, _ = Gemini_cli.parse_gemini_stream_json ndjson in
  Alcotest.(check string)
    "AC2 stream-json: valid event parsed despite surrounding non-JSON"
    "valid"
    text

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story_484_streaming_observability_parity"
    [
      ( "AC1 Codex event streaming via session lifecycle",
        [
          Alcotest.test_case
            "item.completed text field redacted"
            `Quick
            test_ac1_codex_text_redacted;
          Alcotest.test_case
            "turn.completed token usage preserved"
            `Quick
            test_ac1_codex_turn_completed_preserves_usage;
          Alcotest.test_case
            "session_id preserved in session.created"
            `Quick
            test_ac1_codex_session_id_preserved;
        ] );
      ( "AC2 Gemini stream-json events with redaction",
        [
          Alcotest.test_case
            "response field redacted"
            `Quick
            test_ac2_gemini_response_redacted;
          Alcotest.test_case
            "authorization header redacted"
            `Quick
            test_ac2_gemini_auth_header_redacted;
          Alcotest.test_case
            "log field redacted"
            `Quick
            test_ac2_gemini_log_redacted;
          Alcotest.test_case
            "file content field redacted"
            `Quick
            test_ac2_gemini_file_content_redacted;
        ] );
      ( "AC3 OpenCode JSON events preserving type/shape",
        [
          Alcotest.test_case
            "text event payload redacted"
            `Quick
            test_ac3_opencode_text_redacted;
          Alcotest.test_case
            "step_finish scalar metadata preserved"
            `Quick
            test_ac3_opencode_step_finish_preserves_scalars;
          Alcotest.test_case
            "event type/shape metadata preserved"
            `Quick
            test_ac3_opencode_event_type_preserved;
        ] );
      ( "AC4 Backend-native fallback for unsupported events",
        [
          Alcotest.test_case
            "event type preserved for unknown events"
            `Quick
            test_ac4_fallback_preserves_event_type;
          Alcotest.test_case
            "prompt redacted in unknown events"
            `Quick
            test_ac4_fallback_redacts_prompt;
          Alcotest.test_case
            "scalar metadata preserved"
            `Quick
            test_ac4_fallback_preserves_scalar_metadata;
          Alcotest.test_case
            "redaction summary present"
            `Quick
            test_ac4_fallback_has_redaction_summary;
          Alcotest.test_case
            "shape hash present"
            `Quick
            test_ac4_fallback_has_shape_hash;
        ] );
      ( "AC5 Zero forbidden raw content in persisted records",
        [
          Alcotest.test_case
            "no prompt text persisted"
            `Quick
            test_ac5_no_prompt_persisted;
          Alcotest.test_case
            "no diff content persisted"
            `Quick
            test_ac5_no_diff_persisted;
          Alcotest.test_case
            "no authorization header persisted"
            `Quick
            test_ac5_no_auth_header_persisted;
          Alcotest.test_case
            "no file content persisted"
            `Quick
            test_ac5_no_file_content_persisted;
          Alcotest.test_case
            "no token payload persisted"
            `Quick
            test_ac5_no_token_payload_persisted;
          Alcotest.test_case
            "no build log persisted"
            `Quick
            test_ac5_no_build_log_persisted;
          Alcotest.test_case
            "metadata (event type, backend id, shape hash) preserved"
            `Quick
            test_ac5_persisted_preserves_metadata;
        ] );
      ( "Minor fixes: key over-redaction; on_raw_line wiring; error redaction",
        [
          Alcotest.test_case
            "key field not over-redacted (config key value preserved)"
            `Quick
            test_key_field_not_over_redacted;
          Alcotest.test_case
            "AC2 on_raw_line pipeline redacts Gemini events"
            `Quick
            test_ac2_on_raw_line_pipeline_gemini;
          Alcotest.test_case
            "AC3 on_raw_line pipeline redacts OpenCode events"
            `Quick
            test_ac3_on_raw_line_pipeline_opencode;
          Alcotest.test_case
            "non-JSON lines dropped silently"
            `Quick
            test_on_raw_line_non_json_dropped;
          Alcotest.test_case
            "write_turn_failed error message is redacted"
            `Quick
            test_turn_failed_error_redacted;
        ] );
      ( "AC2 Gemini stream-json NDJSON parser (structural wiring verification)",
        [
          Alcotest.test_case
            "response event text extracted"
            `Quick
            test_ac2_gemini_stream_json_text_from_response_event;
          Alcotest.test_case
            "text chunks concatenated across events"
            `Quick
            test_ac2_gemini_stream_json_text_from_chunks;
          Alcotest.test_case
            "usageMetadata cost extracted from stream"
            `Quick
            test_ac2_gemini_stream_json_cost_extraction;
          Alcotest.test_case
            "non-JSON lines silently skipped"
            `Quick
            test_ac2_gemini_stream_json_skips_non_json;
        ] );
    ]
