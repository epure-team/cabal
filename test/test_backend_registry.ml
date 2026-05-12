(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Backend_registry — Story #476.

    Covers:
    - AC1: All 5 built-in backends have a descriptor
    - AC2: Exact baseline versions match the reference table
    - AC3: All required capability flags are present on each descriptor
    - AC4: backend_supports_file_reading is routed through the registry *)

open Cabal

(** {1 Helpers} *)

let all_ids = ["claude-code"; "codex"; "opencode"; "gemini-cli"; "copilot-cli"]

let find_desc id =
  match Backend_registry.find id with
  | Some d -> d
  | None -> Alcotest.failf "backend descriptor not found for id=%s" id

(** {1 AC1 — Registry coverage: all 5 backends have a descriptor} *)

let test_all_backends_have_descriptor () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check string) ("id matches for " ^ id) id d.Backend_registry.id)
    all_ids

let test_five_descriptors_total () =
  let all = Backend_registry.all () in
  Alcotest.(check int) "exactly 5 built-in descriptors" 5 (List.length all)

let test_display_names_non_empty () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        ("display_name non-empty for " ^ id)
        true
        (String.length d.Backend_registry.display_name > 0))
    all_ids

(** {1 AC2 — Baseline versions match the reference table} *)

let test_claude_code_baseline () =
  let d = find_desc "claude-code" in
  Alcotest.(check string)
    "claude-code baseline"
    "2.1.117"
    d.Backend_registry.baseline_version

let test_codex_baseline () =
  let d = find_desc "codex" in
  Alcotest.(check string)
    "codex baseline"
    "0.122.0"
    d.Backend_registry.baseline_version

let test_opencode_baseline () =
  let d = find_desc "opencode" in
  Alcotest.(check string)
    "opencode baseline"
    "1.14.20"
    d.Backend_registry.baseline_version

let test_gemini_cli_baseline () =
  let d = find_desc "gemini-cli" in
  Alcotest.(check string)
    "gemini-cli baseline"
    "0.38.2"
    d.Backend_registry.baseline_version

let test_copilot_cli_baseline () =
  let d = find_desc "copilot-cli" in
  Alcotest.(check string)
    "copilot-cli baseline"
    "1.0.34"
    d.Backend_registry.baseline_version

(** {1 AC3 — Capability flags are present and correct} *)

(* file_reading: claude-code and opencode have it (story #480: opencode 1.14.20
   supports repository reading) *)
let test_file_reading_capabilities () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ " has file_reading")
        true
        d.Backend_registry.capabilities.Backend_registry.file_reading)
    ["claude-code"; "opencode"] ;
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ " has no file_reading")
        false
        d.Backend_registry.capabilities.Backend_registry.file_reading)
    ["codex"; "gemini-cli"; "copilot-cli"]

(* structured_output: all except copilot-cli *)
let test_structured_output_capabilities () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ " has structured_output")
        true
        d.Backend_registry.capabilities.Backend_registry.structured_output)
    ["claude-code"; "codex"; "opencode"; "gemini-cli"] ;
  let d_cop = find_desc "copilot-cli" in
  Alcotest.(check bool)
    "copilot-cli has no structured_output"
    false
    d_cop.Backend_registry.capabilities.Backend_registry.structured_output

(* session_resume: claude-code and codex *)
let test_session_resume_capabilities () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ " has session_resume")
        true
        d.Backend_registry.capabilities.Backend_registry.session_resume)
    ["claude-code"; "codex"; "gemini-cli"] ;
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ " has no session_resume")
        false
        d.Backend_registry.capabilities.Backend_registry.session_resume)
    ["opencode"; "copilot-cli"]

(* read_only_support: claude-code and codex support it natively *)
let test_read_only_support_capabilities () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ " has read_only_support")
        true
        d.Backend_registry.capabilities.Backend_registry.read_only_support)
    ["claude-code"; "codex"]

(* streaming_output: claude-code, opencode (1.14.20 JSON event stream, story #480),
   and gemini-cli (0.38.2 stream-json, story #482);
   codex/copilot-cli do not *)
let test_streaming_output_capability () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ " has streaming_output")
        true
        d.Backend_registry.capabilities.Backend_registry.streaming_output)
    ["claude-code"; "opencode"; "gemini-cli"] ;
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ " has no streaming_output")
        false
        d.Backend_registry.capabilities.Backend_registry.streaming_output)
    ["codex"; "copilot-cli"]

(* mcp_support: not None for claude-code, opencode, copilot-cli *)
let test_mcp_support_modes () =
  let open Backend_registry in
  let d_claude = find_desc "claude-code" in
  Alcotest.(check bool)
    "claude-code mcp_support <> None"
    true
    (d_claude.capabilities.mcp_support <> Mcp_none) ;
  let d_opencode = find_desc "opencode" in
  Alcotest.(check bool)
    "opencode mcp_support <> None"
    true
    (d_opencode.capabilities.mcp_support <> Mcp_none) ;
  let d_copilot = find_desc "copilot-cli" in
  Alcotest.(check bool)
    "copilot-cli mcp_support <> None"
    true
    (d_copilot.capabilities.mcp_support <> Mcp_none)

(* project_config_surface: assert exact variant per backend *)
let test_project_config_surface_values () =
  let open Backend_registry in
  let check id expected =
    let d = find_desc id in
    Alcotest.(check bool)
      (id ^ " project_config_surface")
      true
      (d.capabilities.project_config_surface = expected)
  in
  check "claude-code" Config_explicit_flag ;
  check "codex" Config_fixed_path ;
  check "opencode" Config_fixed_path ;
  check "gemini-cli" Config_fixed_path ;
  check "copilot-cli" Config_fixed_path

(* precedence_confidence: assert exact variant per backend *)
let test_precedence_confidence_values () =
  let open Backend_registry in
  let check id expected =
    let d = find_desc id in
    Alcotest.(check bool)
      (id ^ " precedence_confidence")
      true
      (d.capabilities.precedence_confidence = expected)
  in
  check "claude-code" High ;
  check "codex" Medium ;
  check "opencode" Medium ;
  check "gemini-cli" Low ;
  check "copilot-cli" Low

(** {1 AC4 — backend_supports_file_reading routed through registry} *)

let test_registry_based_file_reading () =
  List.iter
    (fun id ->
      Alcotest.(check bool)
        (id ^ " supports file reading")
        true
        (Backend_registry.supports_file_reading id))
    ["claude-code"; "opencode"] ;
  List.iter
    (fun id ->
      Alcotest.(check bool)
        (id ^ " does not support file reading")
        false
        (Backend_registry.supports_file_reading id))
    ["codex"; "gemini-cli"; "copilot-cli"]

let test_unknown_backend_no_file_reading () =
  Alcotest.(check bool)
    "unknown backend has no file reading"
    false
    (Backend_registry.supports_file_reading "unknown-backend")

(** {1 Suite} *)

let () =
  Alcotest.run
    "Backend_registry"
    [
      ( "AC1 registry coverage",
        [
          Alcotest.test_case
            "all 5 backends have a descriptor"
            `Quick
            test_all_backends_have_descriptor;
          Alcotest.test_case
            "exactly 5 descriptors total"
            `Quick
            test_five_descriptors_total;
          Alcotest.test_case
            "display names non-empty"
            `Quick
            test_display_names_non_empty;
        ] );
      ( "AC2 baseline versions",
        [
          Alcotest.test_case
            "claude-code baseline 2.1.117"
            `Quick
            test_claude_code_baseline;
          Alcotest.test_case "codex baseline 0.122.0" `Quick test_codex_baseline;
          Alcotest.test_case
            "opencode baseline 1.14.20"
            `Quick
            test_opencode_baseline;
          Alcotest.test_case
            "gemini-cli baseline 0.38.2"
            `Quick
            test_gemini_cli_baseline;
          Alcotest.test_case
            "copilot-cli baseline 1.0.34"
            `Quick
            test_copilot_cli_baseline;
        ] );
      ( "AC3 capability flags",
        [
          Alcotest.test_case
            "file_reading: only claude-code"
            `Quick
            test_file_reading_capabilities;
          Alcotest.test_case
            "structured_output: all except copilot-cli"
            `Quick
            test_structured_output_capabilities;
          Alcotest.test_case
            "session_resume: claude-code and codex"
            `Quick
            test_session_resume_capabilities;
          Alcotest.test_case
            "read_only_support: claude-code and codex"
            `Quick
            test_read_only_support_capabilities;
          Alcotest.test_case
            "streaming_output: claude-code"
            `Quick
            test_streaming_output_capability;
          Alcotest.test_case
            "mcp_support modes non-none for key backends"
            `Quick
            test_mcp_support_modes;
          Alcotest.test_case
            "project_config_surface: exact values per backend"
            `Quick
            test_project_config_surface_values;
          Alcotest.test_case
            "precedence_confidence: exact values per backend"
            `Quick
            test_precedence_confidence_values;
        ] );
      ( "AC4 registry routing",
        [
          Alcotest.test_case
            "supports_file_reading via registry"
            `Quick
            test_registry_based_file_reading;
          Alcotest.test_case
            "unknown backend: no file reading"
            `Quick
            test_unknown_backend_no_file_reading;
        ] );
    ]
