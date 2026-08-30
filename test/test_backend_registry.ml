(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Backend_registry — Story #476.

    Covers:
    - AC1: All 6 built-in backends have a descriptor
    - AC2: Exact baseline versions match the reference table
    - AC3: All required capability flags are present on each descriptor
    - AC4: backend_supports_file_reading is routed through the registry *)

open Cabal

(** {1 Helpers} *)

let all_ids =
  ["claude-code"; "codex"; "opencode"; "gemini-cli"; "copilot-cli"; "pi"]

let find_desc id =
  match Backend_registry.find id with
  | Some d -> d
  | None -> Alcotest.failf "backend descriptor not found for id=%s" id

let register_host_runtime_backends () =
  Registry.clear () ;
  Adapter_loader.register_all () ;
  Registry.register (module Claude_code) ;
  Registry.register (module Gemini_cli) ;
  Registry.register (module Codex_cli) ;
  Registry.register (module Opencode_cli) ;
  Registry.register (module Copilot_cli)

let with_host_runtime_backends f =
  Registry.clear () ;
  Fun.protect
    ~finally:(fun () -> Registry.clear ())
    (fun () ->
      register_host_runtime_backends () ;
      f ())

(** {1 AC1 — Registry coverage: all 5 backends have a descriptor} *)

let test_all_backends_have_descriptor () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check string) ("id matches for " ^ id) id d.Backend_registry.id)
    all_ids

let test_five_descriptors_total () =
  let all = Backend_registry.all () in
  Alcotest.(check int) "exactly 6 built-in descriptors" 6 (List.length all)

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

(* session_resume: claude-code, codex, and gemini-cli *)
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
    ["opencode"; "copilot-cli"; "pi"]

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

let test_media_and_web_capabilities_disabled_by_default () =
  List.iter
    (fun (d : Backend_registry.descriptor) ->
      Alcotest.(check int)
        (d.id ^ " has no supported media types")
        0
        (List.length d.capabilities.media_support.media_types) ;
      Alcotest.(check bool)
        (d.id ^ " has no media evidence")
        true
        (d.capabilities.media_support.evidence = None) ;
      Alcotest.(check bool)
        (d.id ^ " has web access disabled")
        true
        (d.capabilities.web_support.maximum = Backend_types.Web_disabled) ;
      Alcotest.(check bool)
        (d.id ^ " has no web evidence")
        true
        (d.capabilities.web_support.evidence = None))
    (Backend_registry.all ())

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

(** {1 AC5 — Static-vs-runtime consistency property} *)

(** For every static descriptor in [Backend_registry], the runtime registry
    populated using host order ([Adapter_loader.register_all] followed by the
    handwritten built-ins) must hold a backend whose [Agentic_backend.id]
    matches and whose [name] is non-empty. This catches drift between the
    static facts and the actually-registered modules (e.g., a backend deleted
    in code but left in the registry table). *)
let test_runtime_modules_match_descriptors () =
  with_host_runtime_backends (fun () ->
      let descriptors = Backend_registry.all () in
      Alcotest.(check bool)
        "at least one descriptor exists"
        true
        (descriptors <> []) ;
      List.iter
        (fun (d : Backend_registry.descriptor) ->
          match Registry.get d.id with
          | None ->
              Alcotest.failf
                "registered backend module missing for descriptor id=%s"
                d.id
          | Some backend ->
              let actual_id = Agentic_backend.id backend in
              let actual_name = Agentic_backend.name backend in
              Alcotest.(check string)
                (Printf.sprintf "%s: runtime id matches descriptor" d.id)
                d.id
                actual_id ;
              Alcotest.(check bool)
                (Printf.sprintf "%s: runtime name is non-empty" d.id)
                true
                (String.length (String.trim actual_name) > 0))
        descriptors)

(** Symmetric check: every backend module the runtime registry knows about
    must correspond to a static descriptor. Catches a backend registered in
    code but missing from the static table. *)
let test_runtime_ids_have_descriptors () =
  with_host_runtime_backends (fun () ->
      let runtime_ids = Registry.list_ids () in
      List.iter
        (fun id ->
          match Backend_registry.find id with
          | None ->
              Alcotest.failf "runtime backend id=%s has no static descriptor" id
          | Some _ -> ())
        runtime_ids)

(** Native-schema capability must agree between static descriptors and the
    runtime modules hosts actually execute.  In particular, claude-code must be
    the handwritten native backend after host-order registration; a generic
    YAML replacement with [native_json_schema_output = false] is not enough to
    prove Story #628's native path. *)
let test_runtime_native_schema_capabilities_match_descriptors () =
  with_host_runtime_backends (fun () ->
      List.iter
        (fun (d : Backend_registry.descriptor) ->
          match Registry.get d.id with
          | None ->
              Alcotest.failf
                "registered backend module missing for descriptor id=%s"
                d.id
          | Some backend ->
              Alcotest.(check bool)
                (Printf.sprintf
                   "%s: runtime native_json_schema_output matches descriptor"
                   d.id)
                d.capabilities.native_json_schema_output
                (Agentic_backend.native_json_schema_output backend))
        (Backend_registry.all ()))

(** Session-resume capability must agree between static descriptors and the
    runtime modules hosts actually execute. In particular, the YAML-backed Pi
    runtime must not silently accept a resume request that it cannot transport. *)
let test_runtime_session_resume_capabilities_match_descriptors () =
  with_host_runtime_backends (fun () ->
      List.iter
        (fun (d : Backend_registry.descriptor) ->
          match Registry.get d.id with
          | None ->
              Alcotest.failf
                "registered backend module missing for descriptor id=%s"
                d.id
          | Some backend ->
              Alcotest.(check bool)
                (Printf.sprintf
                   "%s: runtime session_resume matches descriptor"
                   d.id)
                d.capabilities.session_resume
                (Agentic_backend.supports_session_resume backend))
        (Backend_registry.all ()))

(** {1 Helpers for investigation note tests (Epic #95)} *)

(** Walk up from CWD until a directory containing [dune-project] is found.
    Falls back to CWD if not found (e.g. in a nested _build sandbox). *)
let project_root () =
  match Sys.getenv_opt "PROJECT_ROOT" with
  | Some r -> r
  | None ->
      let rec walk dir =
        if Sys.file_exists (Filename.concat dir "dune-project") then dir
        else
          let parent = Filename.dirname dir in
          if parent = dir then Sys.getcwd () else walk parent
      in
      walk (Sys.getcwd ())

let investigation_note_path backend_id =
  let root = project_root () in
  let standalone =
    Filename.concat
      root
      (Printf.sprintf "docs/native-json-schema-investigation/%s.md" backend_id)
  in
  if Sys.file_exists standalone then standalone
  else
    Filename.concat
      root
      (Printf.sprintf
         "libs/cabal/docs/native-json-schema-investigation/%s.md"
         backend_id)

let read_note backend_id =
  let path = investigation_note_path backend_id in
  if not (Sys.file_exists path) then None
  else begin
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n ;
    close_in ic ;
    Some (Bytes.to_string s)
  end

let note_contains backend_id needle =
  match read_note backend_id with
  | None -> false
  | Some content ->
      let hlen = String.length content in
      let nlen = String.length needle in
      if nlen = 0 then true
      else
        let rec loop i =
          if i + nlen > hlen then false
          else if String.sub content i nlen = needle then true
          else loop (i + 1)
        in
        loop 0

(** {1 AC6 — Epic #95 native_json_schema_output} *)

(** Story #629 — claude-code: AC2(a) retrofit capability_evidence *)

let test_629_claude_code_native_json_schema_output_true () =
  let d = find_desc "claude-code" in
  Alcotest.(check bool)
    "claude-code native_json_schema_output = true (AC2(a))"
    true
    d.Backend_registry.capabilities.Backend_registry.native_json_schema_output

let test_629_claude_code_capability_evidence_present () =
  match Backend_registry.get_capability_evidence "claude-code" with
  | None ->
      Alcotest.fail "claude-code capability_evidence = None (NFR-S1 violation)"
  | Some ev ->
      Alcotest.(check string)
        "capability_evidence.tested_at_version pinned to baseline 2.1.117"
        "2.1.117"
        ev.Backend_types.tested_at_version

let test_629_investigation_note_exists () =
  let path = investigation_note_path "claude-code" in
  Alcotest.(check bool)
    "claude-code investigation note exists at canonical path"
    true
    (Sys.file_exists path)

(** Story #630 — codex: AC2(a) confirmed support *)

let test_630_codex_native_json_schema_output_true () =
  let d = find_desc "codex" in
  Alcotest.(check bool)
    "codex native_json_schema_output = true (AC2(a) pinning)"
    true
    d.Backend_registry.capabilities.Backend_registry.native_json_schema_output

let test_630_codex_capability_evidence_some () =
  match Backend_registry.get_capability_evidence "codex" with
  | Some _ -> ()
  | None ->
      Alcotest.fail
        "codex capability_evidence = None (expected Some _ when flag is true)"

let test_630_investigation_note_exists () =
  let path = investigation_note_path "codex" in
  Alcotest.(check bool)
    "codex investigation note exists at canonical path"
    true
    (Sys.file_exists path)

let test_630_investigation_note_cites_baseline () =
  Alcotest.(check bool)
    "codex investigation note cites baseline_version 0.122.0"
    true
    (note_contains "codex" "0.122.0")

(** Story #631 — opencode: AC2(b) documented non-support.

    NFR-R1 pinning test: fails if a contributor flips
    [native_json_schema_output] without updating the investigation note. *)

let test_631_opencode_native_json_schema_output_false () =
  let d = find_desc "opencode" in
  Alcotest.(check bool)
    "opencode native_json_schema_output = false (AC2(b) NFR-R1 pinning)"
    false
    d.Backend_registry.capabilities.Backend_registry.native_json_schema_output

let test_631_opencode_capability_evidence_none () =
  match Backend_registry.get_capability_evidence "opencode" with
  | None -> ()
  | Some _ ->
      Alcotest.fail
        "opencode capability_evidence = Some _ (unexpected on AC2(b) false \
         flag)"

(** AC1: investigation note at canonical path. *)
let test_631_investigation_note_exists () =
  let path = investigation_note_path "opencode" in
  Alcotest.(check bool)
    "opencode investigation note exists at canonical path (AC1)"
    true
    (Sys.file_exists path)

(** AC1: investigation note cites the pinned baseline_version. *)
let test_631_investigation_note_cites_baseline () =
  Alcotest.(check bool)
    "opencode investigation note cites baseline_version 1.14.20 (AC1 / NFR-U3)"
    true
    (note_contains "opencode" "1.14.20")

(** AC1: investigation note cites at least one authoritative source URL. *)
let test_631_investigation_note_cites_source_url () =
  Alcotest.(check bool)
    "opencode investigation note cites at least one https:// source URL (AC1)"
    true
    (note_contains "opencode" "https://")

(** AC1: investigation note records an accessed-on date. *)
let test_631_investigation_note_cites_accessed_on_date () =
  Alcotest.(check bool)
    "opencode investigation note records an accessed-on date (AC1 / NFR-U3)"
    true
    (note_contains "opencode" "accessed")

(** QG-1 (all stories): structural integrity — every descriptor with
    [native_json_schema_output = true] carries a non-[None]
    [capability_evidence] record (NFR-S1). *)
let test_qg1_all_native_true_have_evidence () =
  List.iter
    (fun (d : Backend_registry.descriptor) ->
      if d.capabilities.Backend_registry.native_json_schema_output then
        match
          d.capabilities.Backend_registry.native_json_schema_output_evidence
        with
        | None ->
            Alcotest.failf
              "backend %s: native_json_schema_output=true but \
               capability_evidence=None (NFR-S1 / QG-1 violation)"
              d.id
        | Some _ -> ())
    (Backend_registry.all ())

(** NFR-U2 — Story #631 scope isolation: only opencode is touched; all other
    backends retain their existing state. *)
let test_631_scope_isolation () =
  let d_claude = find_desc "claude-code" in
  Alcotest.(check bool)
    "claude-code retains native_json_schema_output = true (not touched by #631)"
    true
    d_claude.Backend_registry.capabilities
      .Backend_registry.native_json_schema_output ;
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (Printf.sprintf "%s retains native_json_schema_output = false" id)
        false
        d.Backend_registry.capabilities
          .Backend_registry.native_json_schema_output)
    ["gemini-cli"; "copilot-cli"]

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
            "file_reading: claude-code and opencode"
            `Quick
            test_file_reading_capabilities;
          Alcotest.test_case
            "structured_output: all except copilot-cli"
            `Quick
            test_structured_output_capabilities;
          Alcotest.test_case
            "session_resume: claude-code, codex, and gemini-cli"
            `Quick
            test_session_resume_capabilities;
          Alcotest.test_case
            "read_only_support: claude-code and codex"
            `Quick
            test_read_only_support_capabilities;
          Alcotest.test_case
            "streaming_output: claude-code, opencode, and gemini-cli"
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
          Alcotest.test_case
            "media and web support disabled for all built-ins"
            `Quick
            test_media_and_web_capabilities_disabled_by_default;
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
      ( "AC5 static/runtime consistency",
        [
          Alcotest.test_case
            "every descriptor has a runtime backend module"
            `Quick
            test_runtime_modules_match_descriptors;
          Alcotest.test_case
            "every runtime backend has a descriptor"
            `Quick
            test_runtime_ids_have_descriptors;
          Alcotest.test_case
            "runtime native schema capability matches descriptor"
            `Quick
            test_runtime_native_schema_capabilities_match_descriptors;
          Alcotest.test_case
            "runtime session resume capability matches descriptor"
            `Quick
            test_runtime_session_resume_capabilities_match_descriptors;
        ] );
      ( "AC6 Epic #95 native_json_schema_output",
        [
          Alcotest.test_case
            "#629 claude-code native_json_schema_output = true"
            `Quick
            test_629_claude_code_native_json_schema_output_true;
          Alcotest.test_case
            "#629 claude-code capability_evidence present and pinned"
            `Quick
            test_629_claude_code_capability_evidence_present;
          Alcotest.test_case
            "#629 claude-code investigation note exists"
            `Quick
            test_629_investigation_note_exists;
          Alcotest.test_case
            "#630 codex native_json_schema_output = true (pinning)"
            `Quick
            test_630_codex_native_json_schema_output_true;
          Alcotest.test_case
            "#630 codex capability_evidence = Some _"
            `Quick
            test_630_codex_capability_evidence_some;
          Alcotest.test_case
            "#630 codex investigation note exists"
            `Quick
            test_630_investigation_note_exists;
          Alcotest.test_case
            "#630 codex investigation note cites baseline 0.122.0"
            `Quick
            test_630_investigation_note_cites_baseline;
          Alcotest.test_case
            "#631 opencode native_json_schema_output = false (NFR-R1 pinning)"
            `Quick
            test_631_opencode_native_json_schema_output_false;
          Alcotest.test_case
            "#631 opencode capability_evidence = None"
            `Quick
            test_631_opencode_capability_evidence_none;
          Alcotest.test_case
            "#631 opencode investigation note exists (AC1)"
            `Quick
            test_631_investigation_note_exists;
          Alcotest.test_case
            "#631 opencode investigation note cites baseline 1.14.20 (AC1)"
            `Quick
            test_631_investigation_note_cites_baseline;
          Alcotest.test_case
            "#631 opencode investigation note cites source URL (AC1)"
            `Quick
            test_631_investigation_note_cites_source_url;
          Alcotest.test_case
            "#631 opencode investigation note cites accessed-on date (AC1)"
            `Quick
            test_631_investigation_note_cites_accessed_on_date;
          Alcotest.test_case
            "QG-1 all native_true backends have capability_evidence"
            `Quick
            test_qg1_all_native_true_have_evidence;
          Alcotest.test_case
            "#631 scope isolation (NFR-U2)"
            `Quick
            test_631_scope_isolation;
        ] );
    ]
