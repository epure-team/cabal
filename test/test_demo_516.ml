(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #516 — Align Gemini registry metadata with implementation.

    Covers:
    - AC1: All 8 Gemini registry fields hold exact target values
    - AC2: Specific assertions for MCP user-settings, stream-json flag, and
           config surface prevent future drift
    - AC3: Session resume via --resume is not regressed by the alignment *)

open Cabal

(** {1 Helpers} *)

let gemini_desc () =
  match Backend_registry.find "gemini-cli" with
  | Some d -> d
  | None -> Alcotest.failf "gemini-cli descriptor not found in registry"

let minimal_spec ?resume_session_id () : Backend_types.task_spec =
  {
    prompt = "test";
    instructions = "";
    mcp_servers = [];
    managed_namespace = Backend_types.default_managed_namespace;
    lsp_servers = [];
    working_dir = "/tmp";
    timeout = 60.0;
    expected_outputs = [];
    attachments = [];
    web_access = Backend_types.Web_disabled;
    model = None;
    resume_session_id;
    max_turns = None;
    read_only = false;
    json_schema = None;
  }

(** {1 AC1 — All 8 Gemini registry fields match target values exactly} *)

let test_gemini_structured_output () =
  let d = gemini_desc () in
  Alcotest.(check bool)
    "structured_output = true"
    true
    d.Backend_registry.capabilities.Backend_registry.structured_output

let test_gemini_streaming_output () =
  let d = gemini_desc () in
  Alcotest.(check bool)
    "streaming_output = true"
    true
    d.Backend_registry.capabilities.Backend_registry.streaming_output

let test_gemini_session_resume () =
  let d = gemini_desc () in
  Alcotest.(check bool)
    "session_resume = true"
    true
    d.Backend_registry.capabilities.Backend_registry.session_resume

let test_gemini_mcp_support () =
  let d = gemini_desc () in
  Alcotest.(check bool)
    "mcp_support = Mcp_user_settings"
    true
    (d.Backend_registry.capabilities.Backend_registry.mcp_support
   = Backend_registry.Mcp_user_settings)

let test_gemini_read_only_support () =
  let d = gemini_desc () in
  Alcotest.(check bool)
    "read_only_support = false"
    false
    d.Backend_registry.capabilities.Backend_registry.read_only_support

let test_gemini_project_config_surface () =
  let d = gemini_desc () in
  Alcotest.(check bool)
    "project_config_surface = Config_fixed_path"
    true
    (d.Backend_registry.capabilities.Backend_registry.project_config_surface
   = Backend_registry.Config_fixed_path)

let test_gemini_precedence_confidence () =
  let d = gemini_desc () in
  Alcotest.(check bool)
    "precedence_confidence = Low"
    true
    (d.Backend_registry.capabilities.Backend_registry.precedence_confidence
   = Backend_registry.Low)

let test_gemini_file_reading () =
  let d = gemini_desc () in
  Alcotest.(check bool)
    "file_reading = false"
    false
    d.Backend_registry.capabilities.Backend_registry.file_reading

(** {1 AC2 — Specific drift guards: stream-json flag and MCP user-settings} *)

(* Build command must include --output-format stream-json (the NDJSON pipeline
   depends on this flag; changing it silently breaks AC2 of story #484). *)
let test_build_command_uses_stream_json () =
  let cmd, _stdin =
    Gemini_cli.build_command ~mcp_config_path:None (minimal_spec ())
  in
  Alcotest.(check bool)
    "command includes --output-format"
    true
    (List.mem "--output-format" cmd) ;
  Alcotest.(check bool)
    "command includes stream-json"
    true
    (List.mem "stream-json" cmd)

(* MCP support is settings-file based: Gemini reads settings.json files, and
   workspace .gemini/settings.json can override user settings, but there is no
   per-invocation MCP injection flag.  The registry must declare
   Mcp_user_settings so routing code cannot assume per-invocation MCP config. *)
let test_gemini_mcp_is_user_settings_not_config_file () =
  let d = gemini_desc () in
  let open Backend_registry in
  Alcotest.(check bool)
    "mcp_support is NOT Mcp_config_file"
    false
    (d.capabilities.mcp_support = Mcp_config_file) ;
  Alcotest.(check bool)
    "mcp_support is NOT Mcp_none"
    false
    (d.capabilities.mcp_support = Mcp_none) ;
  Alcotest.(check bool)
    "mcp_support is Mcp_user_settings"
    true
    (d.capabilities.mcp_support = Mcp_user_settings)

(** {1 AC3 — Session resume regression: --resume wiring must not be broken} *)

(* When resume_session_id is set, build_command must include --resume <sid>. *)
let test_build_command_includes_resume_when_sid_set () =
  let sid = "gemini-session-abc123" in
  let cmd, _stdin =
    Gemini_cli.build_command
      ~mcp_config_path:None
      (minimal_spec ~resume_session_id:sid ())
  in
  Alcotest.(check bool) "--resume flag present" true (List.mem "--resume" cmd) ;
  Alcotest.(check bool) "session ID present in command" true (List.mem sid cmd)

(* When no session ID is provided, --resume must NOT appear. *)
let test_build_command_no_resume_when_no_sid () =
  let cmd, _stdin =
    Gemini_cli.build_command ~mcp_config_path:None (minimal_spec ())
  in
  Alcotest.(check bool)
    "--resume absent without session ID"
    false
    (List.mem "--resume" cmd)

(* supports_session_resume must be true; it is the adapter-level contract that
   mirrors session_resume = true in the registry. *)
let test_adapter_supports_session_resume_flag () =
  Alcotest.(check bool)
    "Gemini_cli.supports_session_resume = true"
    true
    Gemini_cli.supports_session_resume

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story_516_gemini_registry_alignment"
    [
      ( "AC1 all 8 registry fields exact",
        [
          Alcotest.test_case
            "structured_output = true"
            `Quick
            test_gemini_structured_output;
          Alcotest.test_case
            "streaming_output = true"
            `Quick
            test_gemini_streaming_output;
          Alcotest.test_case
            "session_resume = true"
            `Quick
            test_gemini_session_resume;
          Alcotest.test_case
            "mcp_support = Mcp_user_settings"
            `Quick
            test_gemini_mcp_support;
          Alcotest.test_case
            "read_only_support = false"
            `Quick
            test_gemini_read_only_support;
          Alcotest.test_case
            "project_config_surface = Config_fixed_path"
            `Quick
            test_gemini_project_config_surface;
          Alcotest.test_case
            "precedence_confidence = Low"
            `Quick
            test_gemini_precedence_confidence;
          Alcotest.test_case
            "file_reading = false"
            `Quick
            test_gemini_file_reading;
        ] );
      ( "AC2 drift guards: stream-json and MCP user-settings",
        [
          Alcotest.test_case
            "build_command uses --output-format stream-json"
            `Quick
            test_build_command_uses_stream_json;
          Alcotest.test_case
            "mcp_support is Mcp_user_settings not Mcp_config_file"
            `Quick
            test_gemini_mcp_is_user_settings_not_config_file;
        ] );
      ( "AC3 session resume regression",
        [
          Alcotest.test_case
            "--resume <sid> wired in build_command"
            `Quick
            test_build_command_includes_resume_when_sid_set;
          Alcotest.test_case
            "--resume absent without session ID"
            `Quick
            test_build_command_no_resume_when_no_sid;
          Alcotest.test_case
            "adapter supports_session_resume flag is true"
            `Quick
            test_adapter_supports_session_resume_flag;
        ] );
    ]
