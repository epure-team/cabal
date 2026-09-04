(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #485 — Correct read-only semantics.

    Covers:
    - AC1: Claude wrapper builds commands enforcing read-only (bug fixed — was
           [ignore spec.read_only])
    - AC2: Codex wrapper read-only sandbox behavior remains correct
    - AC3: OpenCode, Gemini, Copilot use best stable upstream restriction model
           (documented limitation: no native sandbox; baseline preserved without
           write-power escalation)
    - AC4: Deterministic command-construction tests verify validators do not
           gain mutable powers accidentally *)

open Cabal

let contains_str s needle =
  let len = String.length s and nlen = String.length needle in
  if nlen = 0 then true
  else
    let rec loop i =
      i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
    in
    loop 0

let spec_ro () =
  Backend_types.make_task_spec
    ~prompt:"review the diff"
    ~working_dir:"."
    ~read_only:true
    ()

let spec_rw () =
  Backend_types.make_task_spec
    ~prompt:"implement the story"
    ~working_dir:"."
    ~read_only:false
    ()

let find_desc id =
  match Backend_registry.find id with
  | Some d -> d
  | None -> Alcotest.failf "backend %s not found in registry" id

(** Extract the value immediately following [flag] in an arg list. *)
let arg_value_after flag args =
  let rec find = function
    | f :: v :: _ when f = flag -> v
    | _ :: rest -> find rest
    | [] -> ""
  in
  find args

(** {1 AC1 — Claude Code read-only bug is fixed} *)

(* AC1: build_command with read_only=true must include --disallowedTools *)
let test_ac1_claude_read_only_uses_disallowedtools () =
  let args, _ = Claude_code.build_command ~mcp_config_path:None (spec_ro ()) in
  Alcotest.(check bool)
    "AC1: --disallowedTools present in read-only command"
    true
    (List.mem "--disallowedTools" args)

(* AC1: --disallowedTools must include Bash *)
let test_ac1_claude_read_only_disallows_bash () =
  let args, _ = Claude_code.build_command ~mcp_config_path:None (spec_ro ()) in
  let disallowed = arg_value_after "--disallowedTools" args in
  Alcotest.(check bool)
    "AC1: Bash in disallowedTools"
    true
    (contains_str disallowed "Bash")

(* AC1: --disallowedTools must include Edit *)
let test_ac1_claude_read_only_disallows_edit () =
  let args, _ = Claude_code.build_command ~mcp_config_path:None (spec_ro ()) in
  let disallowed = arg_value_after "--disallowedTools" args in
  Alcotest.(check bool)
    "AC1: Edit in disallowedTools"
    true
    (contains_str disallowed "Edit")

(* AC1: --disallowedTools must include Write *)
let test_ac1_claude_read_only_disallows_write () =
  let args, _ = Claude_code.build_command ~mcp_config_path:None (spec_ro ()) in
  let disallowed = arg_value_after "--disallowedTools" args in
  Alcotest.(check bool)
    "AC1: Write in disallowedTools"
    true
    (contains_str disallowed "Write")

(* AC1: --allowedTools must NOT be present in read-only mode.
   The --disallowedTools + --allowedTools combination causes silent empty output
   in claude 2.1.70+, so read-only mode must not include --allowedTools. *)
let test_ac1_claude_read_only_no_allowedtools () =
  let args, _ = Claude_code.build_command ~mcp_config_path:None (spec_ro ()) in
  Alcotest.(check bool)
    "AC1: --allowedTools absent in read-only (avoids silent-output bug)"
    false
    (List.mem "--allowedTools" args)

(* AC1: Builder mode (read_only=false) still uses --allowedTools. *)
let test_ac1_claude_read_write_uses_allowedtools () =
  let args, _ = Claude_code.build_command ~mcp_config_path:None (spec_rw ()) in
  Alcotest.(check bool)
    "AC1: --allowedTools present in read-write mode"
    true
    (List.mem "--allowedTools" args)

(** {2 AC2 — Codex read-only sandbox behavior remains correct} *)

(* AC2: Codex with read_only=true uses -s read-only (OS-level sandbox). *)
let test_ac2_codex_read_only_sandbox_flag () =
  let args, _ = Codex_cli.build_command ~mcp_config_path:None (spec_ro ()) in
  Alcotest.(check bool)
    "AC2: -s flag present for codex read-only"
    true
    (List.mem "-s" args) ;
  Alcotest.(check bool)
    "AC2: read-only sandbox value present"
    true
    (List.mem "read-only" args)

(* AC2: Codex with read_only=false uses --full-auto (builder/mutable mode). *)
let test_ac2_codex_read_write_full_auto () =
  let args, _ = Codex_cli.build_command ~mcp_config_path:None (spec_rw ()) in
  Alcotest.(check bool)
    "AC2: --full-auto present for codex read-write"
    true
    (List.mem "--full-auto" args)

(* AC2: Codex read-only does not include --full-auto. *)
let test_ac2_codex_read_only_no_full_auto () =
  let args, _ = Codex_cli.build_command ~mcp_config_path:None (spec_ro ()) in
  Alcotest.(check bool)
    "AC2: --full-auto absent in codex read-only"
    false
    (List.mem "--full-auto" args)

(** {3 AC3 — OpenCode, Gemini, Copilot: best stable upstream restriction model} *)

(* AC3: Registry documents no native read-only for opencode. *)
let test_ac3_opencode_registry_no_native_read_only () =
  let d = find_desc "opencode" in
  Alcotest.(check bool)
    "AC3: opencode read_only_support = false"
    false
    d.Backend_registry.capabilities.Backend_registry.read_only_support

(* AC3: Registry documents no native read-only for gemini-cli. *)
let test_ac3_gemini_registry_no_native_read_only () =
  let d = find_desc "gemini-cli" in
  Alcotest.(check bool)
    "AC3: gemini-cli read_only_support = false"
    false
    d.Backend_registry.capabilities.Backend_registry.read_only_support

(* AC3: Registry documents no native read-only for copilot-cli. *)
let test_ac3_copilot_registry_no_native_read_only () =
  let d = find_desc "copilot-cli" in
  Alcotest.(check bool)
    "AC3: copilot-cli read_only_support = false"
    false
    d.Backend_registry.capabilities.Backend_registry.read_only_support

(* AC3: OpenCode read_only=true does not add write-amplifying flags vs baseline. *)
let test_ac3_opencode_read_only_no_write_amplification () =
  let args_ro, _ =
    Opencode_cli.build_command ~mcp_config_path:None (spec_ro ())
  in
  let args_rw, _ =
    Opencode_cli.build_command ~mcp_config_path:None (spec_rw ())
  in
  (* OpenCode has no native read-only sandbox (read_only_support = false).
     Best stable upstream restriction model: preserve baseline without adding
     mutable-power escalation.  The command must be identical for both modes. *)
  Alcotest.(check (list string))
    "AC3: opencode read-only = baseline (no escalation, limitation documented)"
    args_rw
    args_ro

(* AC3: Gemini read_only=true does not add write-amplifying flags vs baseline. *)
let test_ac3_gemini_read_only_no_write_amplification () =
  let args_ro, _ =
    Gemini_cli.build_command ~mcp_config_path:None (spec_ro ())
  in
  let args_rw, _ =
    Gemini_cli.build_command ~mcp_config_path:None (spec_rw ())
  in
  Alcotest.(check (list string))
    "AC3: gemini read-only = baseline (no escalation, limitation documented)"
    args_rw
    args_ro

(* AC3: Copilot cannot enforce the public read-only contract and fails closed. *)
let test_ac3_copilot_read_only_no_write_amplification () =
  Alcotest.(check bool)
    "AC3: copilot rejects unsupported read-only execution"
    true
    (Result.is_error
       (Copilot_cli.Private.build_invocation ~config_home:"/isolated"
          ~mcp_config_path:None (spec_ro ()))) ;
  Alcotest.(check bool)
    "AC3: copilot still accepts its narrowed default transport"
    true
    (Result.is_ok
       (Copilot_cli.Private.build_invocation ~config_home:"/isolated"
          ~mcp_config_path:None (spec_rw ())))

(** {4 AC4 — Validators do not gain mutable powers accidentally} *)

(* AC4: Claude validator has Bash, Edit, Write all blocked. *)
let test_ac4_claude_validator_no_mutable_tools () =
  let args, _ = Claude_code.build_command ~mcp_config_path:None (spec_ro ()) in
  let disallowed = arg_value_after "--disallowedTools" args in
  Alcotest.(check bool)
    "AC4: --disallowedTools present for claude validator"
    true
    (List.mem "--disallowedTools" args) ;
  Alcotest.(check bool)
    "AC4: Bash blocked for claude validator"
    true
    (contains_str disallowed "Bash") ;
  Alcotest.(check bool)
    "AC4: Edit blocked for claude validator"
    true
    (contains_str disallowed "Edit") ;
  Alcotest.(check bool)
    "AC4: Write blocked for claude validator"
    true
    (contains_str disallowed "Write")

(* AC4: Codex validator uses OS-level read-only sandbox. *)
let test_ac4_codex_validator_os_sandbox () =
  let args, _ = Codex_cli.build_command ~mcp_config_path:None (spec_ro ()) in
  Alcotest.(check bool)
    "AC4: codex validator uses -s read-only OS sandbox"
    true
    (List.mem "-s" args && List.mem "read-only" args)

(* AC4: Claude read-only does not have an --allowedTools list that includes
   mutable tools — no accidental grant through the allowlist path. *)
let test_ac4_claude_read_only_no_mutable_allowlist () =
  let args, _ = Claude_code.build_command ~mcp_config_path:None (spec_ro ()) in
  Alcotest.(check bool)
    "AC4: no --allowedTools in claude read-only (no accidental mutable grant)"
    false
    (List.mem "--allowedTools" args)

(* AC4: Registry is the ground truth — only backends with native support claim it. *)
let test_ac4_registry_read_only_support_coverage () =
  let ids_with_support =
    List.filter_map
      (fun d ->
        if d.Backend_registry.capabilities.Backend_registry.read_only_support
        then Some d.Backend_registry.id
        else None)
      (Backend_registry.all ())
  in
  Alcotest.(check (list string))
    "AC4: only claude-code and codex have native read_only_support"
    ["claude-code"; "codex"]
    (List.sort String.compare ids_with_support)

(** {5 Suite} *)

let () =
  Alcotest.run
    "Story_485_correct_read_only_semantics"
    [
      ( "AC1 Claude Code read-only bug fixed",
        [
          Alcotest.test_case
            "--disallowedTools present in read-only command"
            `Quick
            test_ac1_claude_read_only_uses_disallowedtools;
          Alcotest.test_case
            "Bash in disallowedTools"
            `Quick
            test_ac1_claude_read_only_disallows_bash;
          Alcotest.test_case
            "Edit in disallowedTools"
            `Quick
            test_ac1_claude_read_only_disallows_edit;
          Alcotest.test_case
            "Write in disallowedTools"
            `Quick
            test_ac1_claude_read_only_disallows_write;
          Alcotest.test_case
            "--allowedTools absent in read-only (avoids silent-output bug)"
            `Quick
            test_ac1_claude_read_only_no_allowedtools;
          Alcotest.test_case
            "--allowedTools present in read-write mode"
            `Quick
            test_ac1_claude_read_write_uses_allowedtools;
        ] );
      ( "AC2 Codex read-only sandbox behavior",
        [
          Alcotest.test_case
            "-s read-only present for validator"
            `Quick
            test_ac2_codex_read_only_sandbox_flag;
          Alcotest.test_case
            "--full-auto present for builder"
            `Quick
            test_ac2_codex_read_write_full_auto;
          Alcotest.test_case
            "--full-auto absent in read-only mode"
            `Quick
            test_ac2_codex_read_only_no_full_auto;
        ] );
      ( "AC3 OpenCode/Gemini/Copilot best restriction model",
        [
          Alcotest.test_case
            "opencode: read_only_support = false in registry"
            `Quick
            test_ac3_opencode_registry_no_native_read_only;
          Alcotest.test_case
            "gemini-cli: read_only_support = false in registry"
            `Quick
            test_ac3_gemini_registry_no_native_read_only;
          Alcotest.test_case
            "copilot-cli: read_only_support = false in registry"
            `Quick
            test_ac3_copilot_registry_no_native_read_only;
          Alcotest.test_case
            "opencode: no write-amplification in read-only mode"
            `Quick
            test_ac3_opencode_read_only_no_write_amplification;
          Alcotest.test_case
            "gemini: no write-amplification in read-only mode"
            `Quick
            test_ac3_gemini_read_only_no_write_amplification;
          Alcotest.test_case
            "copilot: no write-amplification in read-only mode"
            `Quick
            test_ac3_copilot_read_only_no_write_amplification;
        ] );
      ( "AC4 Validators do not gain mutable powers accidentally",
        [
          Alcotest.test_case
            "claude validator: Bash/Edit/Write blocked via --disallowedTools"
            `Quick
            test_ac4_claude_validator_no_mutable_tools;
          Alcotest.test_case
            "codex validator: OS read-only sandbox enforced"
            `Quick
            test_ac4_codex_validator_os_sandbox;
          Alcotest.test_case
            "claude read-only: no accidental --allowedTools mutable grant"
            `Quick
            test_ac4_claude_read_only_no_mutable_allowlist;
          Alcotest.test_case
            "registry: only claude-code and codex have native read_only_support"
            `Quick
            test_ac4_registry_read_only_support_coverage;
        ] );
    ]
