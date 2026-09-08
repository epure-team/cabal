(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Backend_version — Story #477.

    Covers:
    - AC1: Version parsing exists for all 5 built-in backends
    - AC2: Baseline versions in the registry match the reference table
    - AC3: Gate blocks below-baseline installs; error message includes --force-backend
    - AC4: Version format and comparison behavior per backend *)

open Cabal

(** {1 Helpers} *)

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

(** {1 AC1 — Version parsing exists for each backend} *)

(* Claude Code: `claude 2.1.117` or `Claude Code 2.1.117` *)
let test_parse_claude_code () =
  Alcotest.check
    result_semver
    "claude version from 'claude 2.1.117'"
    (ok 2 1 117)
    (Backend_version.parse_from_output "claude 2.1.117")

let test_parse_claude_code_long () =
  Alcotest.check
    result_semver
    "claude version from multi-word prefix"
    (ok 2 1 0)
    (Backend_version.parse_from_output
       "Claude Code version 2.1.0 (build abc123)")

(* Codex: `codex 0.122.0` or just `0.122.0` *)
let test_parse_codex () =
  Alcotest.check
    result_semver
    "codex version from 'codex 0.122.0'"
    (ok 0 122 0)
    (Backend_version.parse_from_output "codex 0.122.0")

let test_parse_codex_bare () =
  Alcotest.check
    result_semver
    "codex version from bare '0.122.0'"
    (ok 0 122 0)
    (Backend_version.parse_from_output "0.122.0")

(* OpenCode: `opencode 1.14.20` *)
let test_parse_opencode () =
  Alcotest.check
    result_semver
    "opencode version from 'opencode 1.14.20'"
    (ok 1 14 20)
    (Backend_version.parse_from_output "opencode 1.14.20")

(* Gemini CLI: `gemini-cli/0.38.2` or `0.38.2` *)
let test_parse_gemini_cli () =
  Alcotest.check
    result_semver
    "gemini version from 'gemini-cli/0.38.2'"
    (ok 0 38 2)
    (Backend_version.parse_from_output "gemini-cli/0.38.2 linux/amd64")

let test_parse_gemini_bare () =
  Alcotest.check
    result_semver
    "gemini version from bare '0.38.2'"
    (ok 0 38 2)
    (Backend_version.parse_from_output "0.38.2")

(* Copilot CLI: `1.0.34` or `gh copilot version 1.0.34` *)
let test_parse_copilot_cli () =
  Alcotest.check
    result_semver
    "copilot version from 'gh copilot version 1.0.34'"
    (ok 1 0 34)
    (Backend_version.parse_from_output "gh copilot version 1.0.34")

let test_parse_copilot_bare () =
  Alcotest.check
    result_semver
    "copilot version from bare '1.0.34'"
    (ok 1 0 34)
    (Backend_version.parse_from_output "1.0.34")

(* Error: no version in output *)
let test_parse_no_version () =
  match Backend_version.parse_from_output "command not found" with
  | Error _ -> ()
  | Ok v ->
      Alcotest.failf
        "expected error but got %d.%d.%d"
        v.Backend_version.major
        v.Backend_version.minor
        v.Backend_version.patch

(* Error: incomplete version (only X.Y, no patch) *)
let test_parse_incomplete () =
  match Backend_version.parse_from_output "v1.2" with
  | Error _ -> ()
  | Ok v ->
      Alcotest.failf
        "expected error for 'v1.2' but got %d.%d.%d"
        v.Backend_version.major
        v.Backend_version.minor
        v.Backend_version.patch

(** {1 AC2 — Baseline versions match the reference table (via of_string)} *)

let test_baseline_claude_code_parseable () =
  let d = find_desc "claude-code" in
  Alcotest.check
    result_semver
    "claude-code baseline parses"
    (ok 2 1 117)
    (Backend_version.of_string d.Backend_registry.baseline_version)

let test_baseline_codex_parseable () =
  let d = find_desc "codex" in
  Alcotest.check
    result_semver
    "codex baseline parses"
    (ok 0 131 0)
    (Backend_version.of_string d.Backend_registry.baseline_version)

let test_baseline_opencode_parseable () =
  let d = find_desc "opencode" in
  Alcotest.check
    result_semver
    "opencode baseline parses"
    (ok 1 14 20)
    (Backend_version.of_string d.Backend_registry.baseline_version)

let test_baseline_gemini_cli_parseable () =
  let d = find_desc "gemini-cli" in
  Alcotest.check
    result_semver
    "gemini-cli baseline parses"
    (ok 0 38 2)
    (Backend_version.of_string d.Backend_registry.baseline_version)

let test_baseline_copilot_cli_parseable () =
  let d = find_desc "copilot-cli" in
  Alcotest.check
    result_semver
    "copilot-cli baseline parses"
    (ok 1 0 54)
    (Backend_version.of_string d.Backend_registry.baseline_version)

(** {1 AC3 — Gate blocks below-baseline; error message includes --force-backend} *)

let test_gate_blocks_below_baseline () =
  let d = find_desc "claude-code" in
  let below =
    {Backend_version.major = 2; minor = 1; patch = 116; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:below with
  | Error msg ->
      Alcotest.(check bool)
        "message mentions --force-backend"
        true
        (let lower = String.lowercase_ascii msg in
         let len = String.length lower in
         let needle = "--force-backend" in
         let nlen = String.length needle in
         let rec loop i =
           i + nlen <= len && (String.sub lower i nlen = needle || loop (i + 1))
         in
         loop 0)
  | Ok () -> Alcotest.fail "expected gate to block below-baseline version"

let test_gate_blocks_old_major () =
  let d = find_desc "codex" in
  let old =
    {Backend_version.major = 0; minor = 121; patch = 9; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:old with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected block for old codex version"

let test_gate_passes_at_baseline () =
  let d = find_desc "opencode" in
  let baseline =
    {Backend_version.major = 1; minor = 14; patch = 20; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:baseline with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "expected gate to pass at baseline: %s" msg

let test_gate_passes_above_baseline () =
  let d = find_desc "gemini-cli" in
  let newer =
    {Backend_version.major = 0; minor = 39; patch = 0; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:newer with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "expected gate to pass above baseline: %s" msg

let test_gate_message_contains_baseline () =
  let d = find_desc "copilot-cli" in
  let below =
    {Backend_version.major = 1; minor = 0; patch = 53; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:below with
  | Error msg ->
      Alcotest.(check bool)
        "message contains baseline version"
        true
        (let len = String.length msg in
         let needle = "1.0.54" in
         let nlen = String.length needle in
         let rec loop i =
           i + nlen <= len && (String.sub msg i nlen = needle || loop (i + 1))
         in
         loop 0)
  | Ok () -> Alcotest.fail "expected error for below-baseline copilot"

let test_gate_message_contains_display_name () =
  let d = find_desc "claude-code" in
  let below =
    {Backend_version.major = 2; minor = 0; patch = 0; prerelease = None}
  in
  match Backend_version.check_gate ~descriptor:d ~installed:below with
  | Error msg ->
      Alcotest.(check bool)
        "message contains display name"
        true
        (let len = String.length msg in
         let needle = "Claude Code" in
         let nlen = String.length needle in
         let rec loop i =
           i + nlen <= len && (String.sub msg i nlen = needle || loop (i + 1))
         in
         loop 0)
  | Ok () -> Alcotest.fail "expected error"

(** {1 AC4 — Version format and comparison per backend} *)

let test_compare_equal () =
  let v =
    {Backend_version.major = 1; minor = 2; patch = 3; prerelease = None}
  in
  Alcotest.(check int)
    "equal versions compare 0"
    0
    (Backend_version.compare v v)

let test_compare_patch_less () =
  let a =
    {Backend_version.major = 2; minor = 1; patch = 116; prerelease = None}
  in
  let b =
    {Backend_version.major = 2; minor = 1; patch = 117; prerelease = None}
  in
  Alcotest.(check bool) "a < b" true (Backend_version.compare a b < 0)

let test_compare_patch_greater () =
  let a =
    {Backend_version.major = 2; minor = 1; patch = 118; prerelease = None}
  in
  let b =
    {Backend_version.major = 2; minor = 1; patch = 117; prerelease = None}
  in
  Alcotest.(check bool) "a > b" true (Backend_version.compare a b > 0)

let test_compare_minor_beats_patch () =
  let a =
    {Backend_version.major = 0; minor = 122; patch = 0; prerelease = None}
  in
  let b =
    {Backend_version.major = 0; minor = 121; patch = 99; prerelease = None}
  in
  Alcotest.(check bool)
    "higher minor beats higher patch"
    true
    (Backend_version.compare a b > 0)

let test_compare_major_beats_all () =
  let a =
    {Backend_version.major = 1; minor = 0; patch = 0; prerelease = None}
  in
  let b =
    {Backend_version.major = 0; minor = 999; patch = 999; prerelease = None}
  in
  Alcotest.(check bool)
    "higher major beats all"
    true
    (Backend_version.compare a b > 0)

(* Claude Code: newline in version output *)
let test_parse_multiline_claude () =
  Alcotest.check
    result_semver
    "parse version from multiline output"
    (ok 2 1 117)
    (Backend_version.parse_from_output
       "Claude Code\nVersion: 2.1.117\nCopyright...")

(* Codex: prerelease suffix is captured — 0.122.0-beta has prerelease = Some "beta" *)
let test_parse_codex_extra_suffix () =
  Alcotest.check
    result_semver
    "parse codex version with prerelease suffix"
    (Ok
       {
         Backend_version.major = 0;
         minor = 122;
         patch = 0;
         prerelease = Some "beta";
       })
    (Backend_version.parse_from_output "codex 0.122.0-beta")

(* OpenCode: v-prefix *)
let test_parse_opencode_v_prefix () =
  Alcotest.check
    result_semver
    "parse opencode with v-prefix"
    (ok 1 14 20)
    (Backend_version.parse_from_output "v1.14.20")

(** {1 AC3 integration — Backend_completer gate wiring} *)

(* Helper: check whether [needle] appears in [haystack]. *)
let contains_string haystack needle =
  let len = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub haystack i nlen = needle || loop (i + 1))
  in
  loop 0

let test_gate_wired_blocks_old_claude () =
  match
    Cabal.Backend_completer.run_gate_for_output
      ~backend_name:"claude-code"
      ~version_output:"claude 2.0.0"
  with
  | Error msg ->
      Alcotest.(check bool)
        "error mentions --force-backend"
        true
        (contains_string msg "--force-backend")
  | Ok () -> Alcotest.fail "expected gate to block old claude version"

let test_gate_wired_passes_at_baseline () =
  match
    Cabal.Backend_completer.run_gate_for_output
      ~backend_name:"claude-code"
      ~version_output:"claude 2.1.117"
  with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "expected gate to pass at baseline: %s" msg

let test_gate_wired_passes_above_baseline () =
  match
    Cabal.Backend_completer.run_gate_for_output
      ~backend_name:"codex"
      ~version_output:"codex 0.132.0"
  with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "expected gate to pass above baseline: %s" msg

let test_gate_wired_skips_unknown_backend () =
  (* Unknown backend has no registry descriptor → gate skipped *)
  match
    Cabal.Backend_completer.run_gate_for_output
      ~backend_name:"my-custom-backend"
      ~version_output:"my-custom-backend 1.0.0"
  with
  | Ok () -> ()
  | Error msg ->
      Alcotest.failf "expected gate to skip for unknown backend: %s" msg

let test_gate_wired_skips_unparseable_version () =
  (* REL-1: unparseable version output → fail-safe skip *)
  match
    Cabal.Backend_completer.run_gate_for_output
      ~backend_name:"claude-code"
      ~version_output:"no version here"
  with
  | Ok () -> ()
  | Error msg ->
      Alcotest.failf "expected gate to skip unparseable version: %s" msg

let write_executable path content =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content) ;
  Unix.chmod path 0o755

let cleanup_temp_bin dir names =
  List.iter
    (fun name -> try Sys.remove (Filename.concat dir name) with _ -> ())
    names ;
  try Unix.rmdir dir with _ -> ()

let restore_path = function
  | Some path -> Unix.putenv "PATH" path
  | None -> Unix.putenv "PATH" ""

let restore_env name = function
  | Some value -> Unix.putenv name value
  | None -> Unix.putenv name ""

let process_group_launcher_path () =
  let test_dir = Filename.dirname (Unix.realpath Sys.executable_name) in
  let build_dir = Filename.dirname test_dir in
  Filename.concat build_dir "bin/process_group_launcher.exe"

let test_gate_wired_uses_copilot_binary_directly () =
  let dir = Filename.temp_dir "epure-copilot-version-" "" in
  write_executable
    (Filename.concat dir "copilot")
    "#!/bin/sh\nprintf '%s\n' '1.0.33'\n" ;
  write_executable
    (Filename.concat dir "gh")
    "#!/bin/sh\nprintf '%s\n' 'gh copilot version 1.0.34'\n" ;
  let launcher = process_group_launcher_path () in
  Alcotest.(check bool)
    "process-group launcher dependency exists"
    true
    (Sys.file_exists launcher) ;
  let old_path = Sys.getenv_opt "PATH" in
  let old_launcher = Sys.getenv_opt "CABAL_PROCESS_GROUP_LAUNCHER" in
  Fun.protect
    ~finally:(fun () ->
      restore_path old_path ;
      restore_env "CABAL_PROCESS_GROUP_LAUNCHER" old_launcher ;
      cleanup_temp_bin dir ["copilot"; "gh"])
    (fun () ->
      Unix.putenv "PATH" dir ;
      Unix.putenv "CABAL_PROCESS_GROUP_LAUNCHER" launcher ;
      Eio_posix.run @@ fun env ->
      match
        Cabal.Backend_completer.run_version_gate
          ~env
          ~backend_name:"copilot-cli"
       with
       | Ok () ->
           Alcotest.fail
             "expected direct copilot 1.0.33 to be rejected; the probe was \
              skipped or gh was used"
       | Error msg ->
           Alcotest.(check bool)
             "error carries the direct copilot version"
             true
             (contains_string msg "1.0.33"))

(** {1 Story #519 — AC1: Prerelease detection} *)

let test_is_prerelease_alpha () =
  match Backend_version.parse_from_output "1.2.3-alpha" with
  | Error _ -> Alcotest.fail "expected parse to succeed for 1.2.3-alpha"
  | Ok v ->
      Alcotest.(check bool)
        "alpha is prerelease"
        true
        (Backend_version.is_prerelease v)

let test_is_prerelease_beta () =
  match Backend_version.parse_from_output "2.0.0-beta" with
  | Error _ -> Alcotest.fail "expected parse to succeed for 2.0.0-beta"
  | Ok v ->
      Alcotest.(check bool)
        "beta is prerelease"
        true
        (Backend_version.is_prerelease v)

let test_is_prerelease_rc () =
  match Backend_version.parse_from_output "1.0.0-rc.1" with
  | Error _ -> Alcotest.fail "expected parse to succeed for 1.0.0-rc.1"
  | Ok v ->
      Alcotest.(check bool)
        "rc.1 is prerelease"
        true
        (Backend_version.is_prerelease v)

let test_is_prerelease_dev () =
  match Backend_version.parse_from_output "0.1.0-dev" with
  | Error _ -> Alcotest.fail "expected parse to succeed for 0.1.0-dev"
  | Ok v ->
      Alcotest.(check bool)
        "dev is prerelease"
        true
        (Backend_version.is_prerelease v)

let test_is_prerelease_any_suffix () =
  match Backend_version.parse_from_output "1.2.3-anything.42" with
  | Error _ -> Alcotest.fail "expected parse to succeed for 1.2.3-anything.42"
  | Ok v ->
      Alcotest.(check bool)
        "any-id is prerelease"
        true
        (Backend_version.is_prerelease v)

let test_not_prerelease_build_metadata_only () =
  match Backend_version.parse_from_output "1.2.3+build.42" with
  | Error _ -> Alcotest.fail "expected parse to succeed for 1.2.3+build.42"
  | Ok v ->
      Alcotest.(check bool)
        "build-metadata-only not prerelease"
        false
        (Backend_version.is_prerelease v)

let test_not_prerelease_stable () =
  match Backend_version.parse_from_output "1.2.3" with
  | Error _ -> Alcotest.fail "expected parse to succeed for 1.2.3"
  | Ok v ->
      Alcotest.(check bool)
        "stable not prerelease"
        false
        (Backend_version.is_prerelease v)

(** {1 Story #519 — AC2: Gate blocks prerelease versions} *)

let test_gate_blocks_prerelease_above_baseline () =
  let d = find_desc "claude-code" in
  (* 2.1.118-rc.1 is numerically above baseline 2.1.117 but is prerelease *)
  match Backend_version.parse_from_output "2.1.118-rc.1" with
  | Error _ -> Alcotest.fail "expected parse to succeed"
  | Ok installed -> (
      match Backend_version.check_gate ~descriptor:d ~installed with
      | Error _ -> ()
      | Ok () ->
          Alcotest.fail
            "expected gate to block prerelease version above baseline")

let test_gate_prerelease_error_contains_backend_name () =
  let d = find_desc "claude-code" in
  match Backend_version.parse_from_output "2.1.118-rc.1" with
  | Error _ -> Alcotest.fail "expected parse to succeed"
  | Ok installed -> (
      match Backend_version.check_gate ~descriptor:d ~installed with
      | Error msg ->
          Alcotest.(check bool)
            "error contains backend name"
            true
            (contains_string msg "Claude Code")
      | Ok () -> Alcotest.fail "expected error for prerelease")

let test_gate_prerelease_error_contains_version () =
  let d = find_desc "claude-code" in
  match Backend_version.parse_from_output "2.1.118-rc.1" with
  | Error _ -> Alcotest.fail "expected parse to succeed"
  | Ok installed -> (
      match Backend_version.check_gate ~descriptor:d ~installed with
      | Error msg ->
          Alcotest.(check bool)
            "error contains prerelease version string"
            true
            (contains_string msg "2.1.118-rc.1")
      | Ok () -> Alcotest.fail "expected error for prerelease")

let test_gate_prerelease_error_contains_baseline () =
  let d = find_desc "claude-code" in
  match Backend_version.parse_from_output "2.1.118-rc.1" with
  | Error _ -> Alcotest.fail "expected parse to succeed"
  | Ok installed -> (
      match Backend_version.check_gate ~descriptor:d ~installed with
      | Error msg ->
          Alcotest.(check bool)
            "error contains stable baseline version"
            true
            (contains_string msg "2.1.117")
      | Ok () -> Alcotest.fail "expected error for prerelease")

let test_gate_prerelease_error_contains_force_backend () =
  let d = find_desc "claude-code" in
  match Backend_version.parse_from_output "2.1.118-rc.1" with
  | Error _ -> Alcotest.fail "expected parse to succeed"
  | Ok installed -> (
      match Backend_version.check_gate ~descriptor:d ~installed with
      | Error msg ->
          Alcotest.(check bool)
            "error mentions --force-backend"
            true
            (contains_string msg "--force-backend")
      | Ok () -> Alcotest.fail "expected error for prerelease")

(** {1 Suite} *)

let () =
  Alcotest.run
    "Backend_version"
    [
      ( "AC1 version parsing per backend",
        [
          Alcotest.test_case
            "claude-code: 'claude 2.1.117'"
            `Quick
            test_parse_claude_code;
          Alcotest.test_case
            "claude-code: long prefix"
            `Quick
            test_parse_claude_code_long;
          Alcotest.test_case "codex: 'codex 0.122.0'" `Quick test_parse_codex;
          Alcotest.test_case
            "codex: bare '0.122.0'"
            `Quick
            test_parse_codex_bare;
          Alcotest.test_case
            "opencode: 'opencode 1.14.20'"
            `Quick
            test_parse_opencode;
          Alcotest.test_case
            "gemini-cli: 'gemini-cli/0.38.2'"
            `Quick
            test_parse_gemini_cli;
          Alcotest.test_case
            "gemini-cli: bare '0.38.2'"
            `Quick
            test_parse_gemini_bare;
          Alcotest.test_case
            "copilot-cli: 'gh copilot version 1.0.34'"
            `Quick
            test_parse_copilot_cli;
          Alcotest.test_case
            "copilot-cli: bare '1.0.34'"
            `Quick
            test_parse_copilot_bare;
          Alcotest.test_case "no version: error" `Quick test_parse_no_version;
          Alcotest.test_case
            "incomplete 'v1.2': error"
            `Quick
            test_parse_incomplete;
        ] );
      ( "AC2 baseline versions parseable",
        [
          Alcotest.test_case
            "claude-code 2.1.117"
            `Quick
            test_baseline_claude_code_parseable;
          Alcotest.test_case
            "codex 0.131.0"
            `Quick
            test_baseline_codex_parseable;
          Alcotest.test_case
            "opencode 1.14.20"
            `Quick
            test_baseline_opencode_parseable;
          Alcotest.test_case
            "gemini-cli 0.38.2"
            `Quick
            test_baseline_gemini_cli_parseable;
          Alcotest.test_case
            "copilot-cli 1.0.54"
            `Quick
            test_baseline_copilot_cli_parseable;
        ] );
      ( "AC3 gate blocks below-baseline",
        [
          Alcotest.test_case
            "blocks below baseline (claude-code patch)"
            `Quick
            test_gate_blocks_below_baseline;
          Alcotest.test_case
            "blocks below baseline (codex minor)"
            `Quick
            test_gate_blocks_old_major;
          Alcotest.test_case
            "passes at baseline (opencode)"
            `Quick
            test_gate_passes_at_baseline;
          Alcotest.test_case
            "passes above baseline (gemini-cli)"
            `Quick
            test_gate_passes_above_baseline;
          Alcotest.test_case
            "error contains baseline version (copilot-cli)"
            `Quick
            test_gate_message_contains_baseline;
          Alcotest.test_case
            "error contains display name (claude-code)"
            `Quick
            test_gate_message_contains_display_name;
        ] );
      ( "AC4 version format and comparison",
        [
          Alcotest.test_case "compare equal versions" `Quick test_compare_equal;
          Alcotest.test_case
            "compare: patch less"
            `Quick
            test_compare_patch_less;
          Alcotest.test_case
            "compare: patch greater"
            `Quick
            test_compare_patch_greater;
          Alcotest.test_case
            "compare: minor beats patch"
            `Quick
            test_compare_minor_beats_patch;
          Alcotest.test_case
            "compare: major beats all"
            `Quick
            test_compare_major_beats_all;
          Alcotest.test_case
            "parse: multiline (claude-code)"
            `Quick
            test_parse_multiline_claude;
          Alcotest.test_case
            "parse: extra suffix (codex)"
            `Quick
            test_parse_codex_extra_suffix;
          Alcotest.test_case
            "parse: v-prefix (opencode)"
            `Quick
            test_parse_opencode_v_prefix;
        ] );
      ( "AC3 integration: Backend_completer gate wiring",
        [
          Alcotest.test_case
            "blocks old claude-code version"
            `Quick
            test_gate_wired_blocks_old_claude;
          Alcotest.test_case
            "passes at baseline (claude-code)"
            `Quick
            test_gate_wired_passes_at_baseline;
          Alcotest.test_case
            "passes above baseline (codex)"
            `Quick
            test_gate_wired_passes_above_baseline;
          Alcotest.test_case
            "skips unknown backend"
            `Quick
            test_gate_wired_skips_unknown_backend;
          Alcotest.test_case
            "skips unparseable version (fail-safe)"
            `Quick
            test_gate_wired_skips_unparseable_version;
          Alcotest.test_case
            "uses copilot --version directly"
            `Quick
            test_gate_wired_uses_copilot_binary_directly;
        ] );
      ( "Story #519 AC1: prerelease detection",
        [
          Alcotest.test_case
            "is_prerelease: -alpha"
            `Quick
            test_is_prerelease_alpha;
          Alcotest.test_case
            "is_prerelease: -beta"
            `Quick
            test_is_prerelease_beta;
          Alcotest.test_case "is_prerelease: -rc.1" `Quick test_is_prerelease_rc;
          Alcotest.test_case "is_prerelease: -dev" `Quick test_is_prerelease_dev;
          Alcotest.test_case
            "is_prerelease: -anything.42"
            `Quick
            test_is_prerelease_any_suffix;
          Alcotest.test_case
            "not prerelease: +build only"
            `Quick
            test_not_prerelease_build_metadata_only;
          Alcotest.test_case
            "not prerelease: stable 1.2.3"
            `Quick
            test_not_prerelease_stable;
        ] );
      ( "Story #519 AC2: gate blocks prerelease",
        [
          Alcotest.test_case
            "blocks prerelease above baseline"
            `Quick
            test_gate_blocks_prerelease_above_baseline;
          Alcotest.test_case
            "prerelease error: backend name"
            `Quick
            test_gate_prerelease_error_contains_backend_name;
          Alcotest.test_case
            "prerelease error: version string"
            `Quick
            test_gate_prerelease_error_contains_version;
          Alcotest.test_case
            "prerelease error: baseline version"
            `Quick
            test_gate_prerelease_error_contains_baseline;
          Alcotest.test_case
            "prerelease error: --force-backend"
            `Quick
            test_gate_prerelease_error_contains_force_backend;
        ] );
    ]
