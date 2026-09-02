(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Demo tests for Story #477 — Stable-version backend gate with force override.

    Covers:
    - AC #650: Version parsing exists for Claude Code, Codex, OpenCode, Gemini CLI, Copilot CLI
    - AC #651: Baselines are Claude 2.1.117, Codex 0.131.0, OpenCode 1.14.20, Gemini 0.38.2, Copilot 1.0.34
    - AC #652: Gate blocks below-baseline with actionable message including --force-backend
    - AC #653: Each backend's version format and comparison behavior are covered *)

open Cabal

let semver_testable =
  Alcotest.testable
    (fun fmt v ->
      let pre =
        match v.Backend_version.prerelease with None -> "" | Some p -> "-" ^ p
      in
      Format.fprintf
        fmt
        "%d.%d.%d%s"
        v.Backend_version.major
        v.Backend_version.minor
        v.Backend_version.patch
        pre)
    (fun a b ->
      a.Backend_version.major = b.Backend_version.major
      && a.Backend_version.minor = b.Backend_version.minor
      && a.Backend_version.patch = b.Backend_version.patch
      && a.Backend_version.prerelease = b.Backend_version.prerelease)

let result_semver = Alcotest.(result semver_testable string)

let ok major minor patch =
  Ok {Backend_version.major; minor; patch; prerelease = None}

let find_desc id =
  match Backend_registry.find id with
  | Some d -> d
  | None -> Alcotest.failf "descriptor not found: %s" id

let contains_string haystack needle =
  let len = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub haystack i nlen = needle || loop (i + 1))
  in
  loop 0

(* AC #650 *)
let test_version_parsing_claude_code () =
  Alcotest.check
    result_semver
    "parse claude-code"
    (ok 2 1 117)
    (Backend_version.parse_from_output "claude 2.1.117")

let test_version_parsing_codex () =
  Alcotest.check
    result_semver
    "parse codex"
    (ok 0 122 0)
    (Backend_version.parse_from_output "codex 0.122.0")

let test_version_parsing_opencode () =
  Alcotest.check
    result_semver
    "parse opencode"
    (ok 1 14 20)
    (Backend_version.parse_from_output "opencode 1.14.20")

let test_version_parsing_gemini_cli () =
  Alcotest.check
    result_semver
    "parse gemini-cli"
    (ok 0 38 2)
    (Backend_version.parse_from_output "gemini-cli/0.38.2 linux/amd64")

let test_version_parsing_copilot_cli () =
  Alcotest.check
    result_semver
    "parse copilot-cli"
    (ok 1 0 34)
    (Backend_version.parse_from_output "gh copilot version 1.0.34")

(* AC #651 *)
let test_baseline_claude_code () =
  let d = find_desc "claude-code" in
  Alcotest.check
    result_semver
    "claude-code baseline is 2.1.117"
    (ok 2 1 117)
    (Backend_version.of_string d.Backend_registry.baseline_version)

let test_baseline_codex () =
  let d = find_desc "codex" in
  Alcotest.check
    result_semver
    "codex baseline is 0.131.0"
    (ok 0 131 0)
    (Backend_version.of_string d.Backend_registry.baseline_version)

let test_baseline_opencode () =
  let d = find_desc "opencode" in
  Alcotest.check
    result_semver
    "opencode baseline is 1.14.20"
    (ok 1 14 20)
    (Backend_version.of_string d.Backend_registry.baseline_version)

let test_baseline_gemini_cli () =
  let d = find_desc "gemini-cli" in
  Alcotest.check
    result_semver
    "gemini-cli baseline is 0.38.2"
    (ok 0 38 2)
    (Backend_version.of_string d.Backend_registry.baseline_version)

let test_baseline_copilot_cli () =
  let d = find_desc "copilot-cli" in
  Alcotest.check
    result_semver
    "copilot-cli baseline is 1.0.34"
    (ok 1 0 34)
    (Backend_version.of_string d.Backend_registry.baseline_version)

(* AC #652 *)
let test_gate_blocks_claude_code_below_baseline () =
  let d = find_desc "claude-code" in
  let below =
    {Backend_version.major = 2; minor = 1; patch = 116; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:below with
  | Error msg ->
      Alcotest.(check bool)
        "mentions --force-backend"
        true
        (contains_string msg "--force-backend")
  | Ok () -> Alcotest.fail "expected block"

let test_gate_blocks_codex_below_baseline () =
  let d = find_desc "codex" in
  let below =
    {Backend_version.major = 0; minor = 130; patch = 9; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:below with
  | Error msg ->
      Alcotest.(check bool)
        "mentions 0.131.0"
        true
        (contains_string msg "0.131.0")
  | Ok () -> Alcotest.fail "expected block"

let test_gate_passes_at_baseline_gemini_cli () =
  let d = find_desc "gemini-cli" in
  let baseline =
    {Backend_version.major = 0; minor = 38; patch = 2; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:baseline with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "must pass at baseline: %s" msg

let test_gate_passes_above_baseline_copilot_cli () =
  let d = find_desc "copilot-cli" in
  let above =
    {Backend_version.major = 1; minor = 1; patch = 0; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:above with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "must pass above baseline: %s" msg

let test_gate_wired_blocks_old_version () =
  match
    Backend_completer.run_gate_for_output
      ~backend_name:"claude-code"
      ~version_output:"claude 2.0.0"
  with
  | Error msg ->
      Alcotest.(check bool)
        "wired: --force-backend"
        true
        (contains_string msg "--force-backend")
  | Ok () -> Alcotest.fail "expected block"

let test_gate_wired_skips_unparseable_output () =
  match
    Backend_completer.run_gate_for_output
      ~backend_name:"claude-code"
      ~version_output:"command not found"
  with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "must skip unparseable: %s" msg

let test_gate_wired_skips_unknown_backend () =
  match
    Backend_completer.run_gate_for_output
      ~backend_name:"unknown-backend"
      ~version_output:"x 1.0.0"
  with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "must skip unknown: %s" msg

(* AC #653 *)
let test_compare_claude_code_patch_boundary () =
  let at =
    {Backend_version.major = 2; minor = 1; patch = 117; prerelease = None}
  in
  let below =
    {Backend_version.major = 2; minor = 1; patch = 116; prerelease = None}
  in
  Alcotest.(check bool)
    "patch 117 > 116"
    true
    (Backend_version.compare at below > 0)

let test_compare_codex_minor_beats_patch () =
  let a =
    {Backend_version.major = 0; minor = 122; patch = 0; prerelease = None}
  in
  let b =
    {Backend_version.major = 0; minor = 121; patch = 99; prerelease = None}
  in
  Alcotest.(check bool)
    "minor 122.0 > 121.99"
    true
    (Backend_version.compare a b > 0)

let test_parse_opencode_v_prefix () =
  Alcotest.check
    result_semver
    "v-prefix handled"
    (ok 1 14 20)
    (Backend_version.parse_from_output "v1.14.20")

let test_parse_gemini_cli_with_arch_suffix () =
  Alcotest.check
    result_semver
    "arch suffix ignored"
    (ok 0 38 2)
    (Backend_version.parse_from_output "gemini 0.38.2 darwin/arm64")

let test_compare_copilot_cli_major_beats_all () =
  let a =
    {Backend_version.major = 2; minor = 0; patch = 0; prerelease = None}
  in
  let b =
    {Backend_version.major = 1; minor = 99; patch = 99; prerelease = None}
  in
  Alcotest.(check bool)
    "major 2.0.0 > 1.99.99"
    true
    (Backend_version.compare a b > 0)

let test_all_baselines_parseable () =
  List.iter
    (fun id ->
      let d = find_desc id in
      match Backend_version.of_string d.Backend_registry.baseline_version with
      | Ok _ -> ()
      | Error e -> Alcotest.failf "baseline for %s unparseable: %s" id e)
    ["claude-code"; "codex"; "opencode"; "gemini-cli"; "copilot-cli"]

let () =
  Alcotest.run
    "Backend_version_demo_477"
    [
      ( "AC #650 version parsing per backend",
        [
          Alcotest.test_case
            "claude-code"
            `Quick
            test_version_parsing_claude_code;
          Alcotest.test_case "codex" `Quick test_version_parsing_codex;
          Alcotest.test_case "opencode" `Quick test_version_parsing_opencode;
          Alcotest.test_case "gemini-cli" `Quick test_version_parsing_gemini_cli;
          Alcotest.test_case
            "copilot-cli"
            `Quick
            test_version_parsing_copilot_cli;
        ] );
      ( "AC #651 stable baselines configured",
        [
          Alcotest.test_case
            "claude-code 2.1.117"
            `Quick
            test_baseline_claude_code;
          Alcotest.test_case "codex 0.131.0" `Quick test_baseline_codex;
          Alcotest.test_case "opencode 1.14.20" `Quick test_baseline_opencode;
          Alcotest.test_case "gemini-cli 0.38.2" `Quick test_baseline_gemini_cli;
          Alcotest.test_case
            "copilot-cli 1.0.34"
            `Quick
            test_baseline_copilot_cli;
        ] );
      ( "AC #652 gate blocks below-baseline with actionable message",
        [
          Alcotest.test_case
            "blocks claude-code 2.1.116"
            `Quick
            test_gate_blocks_claude_code_below_baseline;
          Alcotest.test_case
            "blocks codex 0.130.9"
            `Quick
            test_gate_blocks_codex_below_baseline;
          Alcotest.test_case
            "passes at baseline gemini-cli"
            `Quick
            test_gate_passes_at_baseline_gemini_cli;
          Alcotest.test_case
            "passes above baseline copilot-cli"
            `Quick
            test_gate_passes_above_baseline_copilot_cli;
          Alcotest.test_case
            "wired: blocks old claude-code"
            `Quick
            test_gate_wired_blocks_old_version;
          Alcotest.test_case
            "wired: skip unparseable output"
            `Quick
            test_gate_wired_skips_unparseable_output;
          Alcotest.test_case
            "wired: skip unknown backend"
            `Quick
            test_gate_wired_skips_unknown_backend;
        ] );
      ( "AC #653 version format and comparison per backend",
        [
          Alcotest.test_case
            "claude-code patch boundary"
            `Quick
            test_compare_claude_code_patch_boundary;
          Alcotest.test_case
            "codex minor beats patch"
            `Quick
            test_compare_codex_minor_beats_patch;
          Alcotest.test_case
            "opencode v-prefix"
            `Quick
            test_parse_opencode_v_prefix;
          Alcotest.test_case
            "gemini-cli arch suffix"
            `Quick
            test_parse_gemini_cli_with_arch_suffix;
          Alcotest.test_case
            "copilot-cli major beats all"
            `Quick
            test_compare_copilot_cli_major_beats_all;
          Alcotest.test_case
            "all 5 baselines parseable"
            `Quick
            test_all_baselines_parseable;
        ] );
    ]
