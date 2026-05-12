(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #517 — Enforce read-only-safe backend routing for validators.

    Covers:
    - AC1: Validator construction fails fast when backend has read_only_support=false;
           error names the backend and lists read-only-safe alternatives.
    - AC2: task_spec.read_only=true maps to each supported backend's non-mutating
           invocation mode; tests verify adapter command/behavior per backend.
    - AC3: Enforcement at the invocation boundary helper (make_validator_by_name);
           direct calls also fail for backends without read_only_support.
    - AC4: No silent degradation; error is immediate and descriptive. *)

open Cabal

let contains s needle =
  let len = String.length s and nlen = String.length needle in
  if nlen = 0 then true
  else
    let rec loop i =
      i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
    in
    loop 0

(** {1 AC1 + AC3 — Boundary helper rejects non-read-only backends} *)

(* AC3: make_validator_by_name fails hard for opencode (read_only_support=false) *)
let test_ac3_boundary_opencode () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result =
    Backend_completer.make_validator_by_name
      ~sw
      ~env
      ~backend_name:"opencode"
      ~working_dir:"."
      ()
  in
  Alcotest.(check bool)
    "AC3: make_validator_by_name fails for opencode"
    true
    (Result.is_error result)

(* AC3: make_validator_by_name fails hard for gemini-cli (read_only_support=false) *)
let test_ac3_boundary_gemini () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result =
    Backend_completer.make_validator_by_name
      ~sw
      ~env
      ~backend_name:"gemini-cli"
      ~working_dir:"."
      ()
  in
  Alcotest.(check bool)
    "AC3: make_validator_by_name fails for gemini-cli"
    true
    (Result.is_error result)

(* AC3: make_validator_by_name fails hard for copilot-cli (read_only_support=false) *)
let test_ac3_boundary_copilot () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result =
    Backend_completer.make_validator_by_name
      ~sw
      ~env
      ~backend_name:"copilot-cli"
      ~working_dir:"."
      ()
  in
  Alcotest.(check bool)
    "AC3: make_validator_by_name fails for copilot-cli"
    true
    (Result.is_error result)

(* AC1: Error message names the requested backend *)
let test_ac1_error_names_backend () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result =
    Backend_completer.make_validator_by_name
      ~sw
      ~env
      ~backend_name:"opencode"
      ~working_dir:"."
      ()
  in
  match result with
  | Ok _ -> Alcotest.fail "Expected error but got Ok"
  | Error msg ->
      Alcotest.(check bool)
        "AC1: error message names 'opencode'"
        true
        (contains msg "opencode")

(* AC1 + AC4: Error message lists available read-only-safe alternatives *)
let test_ac1_error_lists_alternatives () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result =
    Backend_completer.make_validator_by_name
      ~sw
      ~env
      ~backend_name:"gemini-cli"
      ~working_dir:"."
      ()
  in
  match result with
  | Ok _ -> Alcotest.fail "Expected error but got Ok"
  | Error msg ->
      (* Must list at least one alternative *)
      let has_claude = contains msg "claude-code" in
      let has_codex = contains msg "codex" in
      Alcotest.(check bool)
        "AC1: error lists claude-code or codex as alternative"
        true
        (has_claude || has_codex)

(* supports_read_only must be fail-closed: unknown backends return false so they
   are rejected at the read-only gate, not silently passed through. *)
let test_unknown_backend_supports_read_only_is_false () =
  Alcotest.(check bool)
    "supports_read_only for unknown backend returns false (fail-closed)"
    false
    (Backend_registry.supports_read_only "completely-unknown-backend")

(* With fail-closed semantics, make_validator_by_name hits the read-only gate
   for unknown backends (not the lookup stage). Error names the backend. *)
let test_unknown_backend_fails_at_read_only_gate () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result =
    Backend_completer.make_validator_by_name
      ~sw
      ~env
      ~backend_name:"completely-unknown-backend"
      ~working_dir:"."
      ()
  in
  match result with
  | Ok _ -> Alcotest.fail "Expected error but got Ok"
  | Error msg ->
      Alcotest.(check bool)
        "unknown backend: fails at read-only gate (fail-closed)"
        true
        (contains msg "completely-unknown-backend")

(** {2 AC1 — Registry: read_only_support values match expectations} *)

(* AC1: Registry correctly declares read_only_support for each backend *)
let test_ac1_registry_claude_code_supports_read_only () =
  let d =
    match Backend_registry.find "claude-code" with
    | Some d -> d
    | None -> Alcotest.fail "claude-code not found"
  in
  Alcotest.(check bool)
    "AC1: claude-code has read_only_support=true"
    true
    d.Backend_registry.capabilities.Backend_registry.read_only_support

let test_ac1_registry_codex_supports_read_only () =
  let d =
    match Backend_registry.find "codex" with
    | Some d -> d
    | None -> Alcotest.fail "codex not found"
  in
  Alcotest.(check bool)
    "AC1: codex has read_only_support=true"
    true
    d.Backend_registry.capabilities.Backend_registry.read_only_support

let test_ac1_registry_opencode_no_read_only () =
  Alcotest.(check bool)
    "AC1: opencode read_only_support=false"
    false
    (Backend_registry.supports_read_only "opencode")

let test_ac1_registry_gemini_no_read_only () =
  Alcotest.(check bool)
    "AC1: gemini-cli read_only_support=false"
    false
    (Backend_registry.supports_read_only "gemini-cli")

let test_ac1_registry_copilot_no_read_only () =
  Alcotest.(check bool)
    "AC1: copilot-cli read_only_support=false"
    false
    (Backend_registry.supports_read_only "copilot-cli")

(* AC1: read_only_safe_backend_ids returns exactly claude-code and codex *)
let test_ac1_read_only_safe_backend_ids () =
  let ids =
    List.sort String.compare (Backend_registry.read_only_safe_backend_ids ())
  in
  Alcotest.(check (list string))
    "AC1: read-only-safe backends are claude-code and codex"
    ["claude-code"; "codex"]
    ids

(** {3 AC2 — Adapter commands for supported backends} *)

let spec_ro () =
  Backend_types.make_task_spec
    ~prompt:"review the diff"
    ~working_dir:"."
    ~read_only:true
    ()

(* AC2: claude-code maps read_only=true to --disallowedTools (non-mutating mode) *)
let test_ac2_claude_code_read_only_mapping () =
  let args, _ = Claude_code.build_command ~mcp_config_path:None (spec_ro ()) in
  Alcotest.(check bool)
    "AC2: claude-code read_only=true uses --disallowedTools"
    true
    (List.mem "--disallowedTools" args)

(* AC2: codex maps read_only=true to -s read-only (OS-level sandbox), adjacent
   and in order so the flag pair is interpreted correctly by the CLI. *)
let test_ac2_codex_read_only_mapping () =
  let args, _ = Codex_cli.build_command ~mcp_config_path:None (spec_ro ()) in
  let rec check_adjacent = function
    | "-s" :: "read-only" :: _ -> true
    | _ :: rest -> check_adjacent rest
    | [] -> false
  in
  Alcotest.(check bool)
    "AC2: codex read_only=true uses -s read-only sandbox (adjacent, in order)"
    true
    (check_adjacent args)

(** {4 Suite} *)

let () =
  Alcotest.run
    "Story_517_read_only_safe_routing"
    [
      ( "AC1+AC3 Boundary helper rejects non-read-only backends",
        [
          Alcotest.test_case
            "make_validator_by_name fails for opencode"
            `Quick
            test_ac3_boundary_opencode;
          Alcotest.test_case
            "make_validator_by_name fails for gemini-cli"
            `Quick
            test_ac3_boundary_gemini;
          Alcotest.test_case
            "make_validator_by_name fails for copilot-cli"
            `Quick
            test_ac3_boundary_copilot;
          Alcotest.test_case
            "error names the requested backend"
            `Quick
            test_ac1_error_names_backend;
          Alcotest.test_case
            "error lists read-only-safe alternatives"
            `Quick
            test_ac1_error_lists_alternatives;
          Alcotest.test_case
            "unknown backend: supports_read_only=false (fail-closed)"
            `Quick
            test_unknown_backend_supports_read_only_is_false;
          Alcotest.test_case
            "unknown backend: fails at read-only gate"
            `Quick
            test_unknown_backend_fails_at_read_only_gate;
        ] );
      ( "AC1 Registry read_only_support values",
        [
          Alcotest.test_case
            "claude-code: read_only_support=true"
            `Quick
            test_ac1_registry_claude_code_supports_read_only;
          Alcotest.test_case
            "codex: read_only_support=true"
            `Quick
            test_ac1_registry_codex_supports_read_only;
          Alcotest.test_case
            "opencode: read_only_support=false"
            `Quick
            test_ac1_registry_opencode_no_read_only;
          Alcotest.test_case
            "gemini-cli: read_only_support=false"
            `Quick
            test_ac1_registry_gemini_no_read_only;
          Alcotest.test_case
            "copilot-cli: read_only_support=false"
            `Quick
            test_ac1_registry_copilot_no_read_only;
          Alcotest.test_case
            "read_only_safe_backend_ids returns claude-code and codex"
            `Quick
            test_ac1_read_only_safe_backend_ids;
        ] );
      ( "AC2 Adapter commands for read-only-safe backends",
        [
          Alcotest.test_case
            "claude-code: read_only=true maps to --disallowedTools"
            `Quick
            test_ac2_claude_code_read_only_mapping;
          Alcotest.test_case
            "codex: read_only=true maps to -s read-only sandbox"
            `Quick
            test_ac2_codex_read_only_mapping;
        ] );
    ]
