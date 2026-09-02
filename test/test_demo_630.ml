(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #630 — Native JSON schema wiring for codex (AC2(a)).

    Outcome: confirmed support.  The codex CLI exposes [--output-schema <FILE>]
    (shipped in codex @openai/codex@0.41.0 / rust-v0.41.0, PR #4079) which
    forwards a caller-supplied JSON Schema to the underlying OpenAI Responses
    API as [response_format: json_schema].  The three-artifact contract
    (NFR-R1) is satisfied:
      (1) investigation note at the canonical path;
      (2) this pinning test asserting [native_json_schema_output = true];
      (3) story completion notes referencing AC1 sources.

    Covers:
    - AC1: Investigation note exists at the canonical path, cites
           baseline_version 0.131.0, and records at least one source URL.
    - AC2(a): codex descriptor has native_json_schema_output = true
              (NFR-R1 pinning test).
    - AC2(a): codex has capability_evidence = Some _.
    - QG-1: Structural integrity — every built-in backend whose descriptor
            declares native_json_schema_output = true must carry a non-None
            capability_evidence record (no regression).
    - NFR-U2: Scope isolation — Story #630 must not touch other backends;
              claude-code stays true, others stay false. *)

open Cabal

(** {1 Helpers} *)

let find_desc id =
  match Backend_registry.find id with
  | Some d -> d
  | None -> Alcotest.failf "backend %s not found in registry" id

(** Resolve the project root by walking up from the dune-test CWD until a
    directory containing [dune-project] is found.  Falls back to CWD. *)
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

let investigation_note_path () =
  let root = project_root () in
  let standalone =
    Filename.concat root "docs/native-json-schema-investigation/codex.md"
  in
  if Sys.file_exists standalone then standalone
  else
    Filename.concat
      root
      "libs/cabal/docs/native-json-schema-investigation/codex.md"

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ;
  close_in ic ;
  Bytes.to_string s

let contains_substring haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  if nlen = 0 then true
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

(** {1 AC1 — Investigation note} *)

(** AC1: The investigation note must exist at the canonical path for codex. *)
let test_investigation_note_exists () =
  let path = investigation_note_path () in
  Alcotest.(check bool)
    "investigation note exists at expected path"
    true
    (Sys.file_exists path)

(** AC1: The investigation note must cite the baseline_version (0.131.0) so
    that the version range consulted is pinned to the descriptor. *)
let test_investigation_note_cites_baseline_version () =
  let path = investigation_note_path () in
  if not (Sys.file_exists path) then
    Alcotest.fail (Printf.sprintf "investigation note not found at %s" path)
  else begin
    let content = read_file path in
    Alcotest.(check bool)
      "investigation note cites baseline_version 0.131.0"
      true
      (contains_substring content "0.131.0")
  end

(** AC1: The investigation note must cite at least one authoritative source URL
    (provider API docs, release notes, or changelog). *)
let test_investigation_note_cites_source_url () =
  let path = investigation_note_path () in
  if not (Sys.file_exists path) then
    Alcotest.fail (Printf.sprintf "investigation note not found at %s" path)
  else begin
    let content = read_file path in
    Alcotest.(check bool)
      "investigation note cites at least one https:// source URL"
      true
      (contains_substring content "https://")
  end

(** AC1: The investigation note must record an accessed-on date so that the
    sources are time-stamped (NFR-U3 requirement). *)
let test_investigation_note_has_accessed_date () =
  let path = investigation_note_path () in
  if not (Sys.file_exists path) then
    Alcotest.fail (Printf.sprintf "investigation note not found at %s" path)
  else begin
    let content = read_file path in
    (* "accessed" appears in "accessed YYYY-MM-DD" or "accessed on YYYY-MM-DD" *)
    Alcotest.(check bool)
      "investigation note records accessed-on date"
      true
      (contains_substring content "accessed")
  end

(** {1 AC2(a) — codex descriptor} *)

(** AC2(a) / NFR-R1: codex must have native_json_schema_output = true.
    This is the pinning test required by the three-artifact AC2(a) contract. *)
let test_codex_native_json_schema_output_true () =
  let d = find_desc "codex" in
  Alcotest.(check bool)
    "native_json_schema_output = true for codex (pinning test)"
    true
    d.Backend_registry.capabilities.Backend_registry.native_json_schema_output

(** AC2(a): codex must have capability_evidence = Some _.
    Evidence is required whenever the flag is true (NFR-S1). *)
let test_codex_capability_evidence_some () =
  match Backend_registry.get_capability_evidence "codex" with
  | Some _ -> ()
  | None ->
      Alcotest.fail
        "capability_evidence is None for codex — expected Some _ when flag is \
         true"

(** {1 QG-1 — Structural integrity (no regression)} *)

(** QG-1: For every built-in backend with native_json_schema_output = true,
    the descriptor must carry a non-None capability_evidence record (NFR-S1).
    Ensures Story #630 does not introduce a regression. *)
let test_structural_all_native_true_have_evidence () =
  List.iter
    (fun (d : Backend_registry.descriptor) ->
      if d.capabilities.Backend_registry.native_json_schema_output then
        match
          d.capabilities.Backend_registry.native_json_schema_output_evidence
        with
        | None ->
            Alcotest.failf
              "backend %s: native_json_schema_output=true but \
               capability_evidence=None (NFR-S1 violation)"
              d.id
        | Some _ -> ())
    (Backend_registry.all ())

(** {1 NFR-U2 — Scope isolation} *)

(** NFR-U2: Story #630 flipped codex to true.  claude-code was already true
    from Story #629 and must stay true. *)
let test_claude_code_still_native_true () =
  let d = find_desc "claude-code" in
  Alcotest.(check bool)
    "claude-code retains native_json_schema_output = true (not touched by #630)"
    true
    d.Backend_registry.capabilities.Backend_registry.native_json_schema_output

(** NFR-U2: Other pending backends must retain native_json_schema_output = false. *)
let test_other_backends_still_false () =
  let others = ["opencode"; "gemini-cli"; "copilot-cli"] in
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ " retains native_json_schema_output = false (not touched by #630)")
        false
        d.Backend_registry.capabilities
          .Backend_registry.native_json_schema_output)
    others

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story #630 — Native JSON schema for codex (AC2(a) confirmed support)"
    [
      ( "AC1 investigation note",
        [
          Alcotest.test_case
            "investigation note exists for codex"
            `Quick
            test_investigation_note_exists;
          Alcotest.test_case
            "investigation note cites baseline_version 0.131.0"
            `Quick
            test_investigation_note_cites_baseline_version;
          Alcotest.test_case
            "investigation note cites source URL"
            `Quick
            test_investigation_note_cites_source_url;
          Alcotest.test_case
            "investigation note records accessed-on date"
            `Quick
            test_investigation_note_has_accessed_date;
        ] );
      ( "AC2(a) codex descriptor",
        [
          Alcotest.test_case
            "native_json_schema_output = true for codex"
            `Quick
            test_codex_native_json_schema_output_true;
          Alcotest.test_case
            "capability_evidence = Some _ for codex"
            `Quick
            test_codex_capability_evidence_some;
        ] );
      ( "QG-1 structural integrity",
        [
          Alcotest.test_case
            "all native=true backends have capability_evidence"
            `Quick
            test_structural_all_native_true_have_evidence;
        ] );
      ( "NFR-U2 scope isolation",
        [
          Alcotest.test_case
            "claude-code retains native_json_schema_output = true"
            `Quick
            test_claude_code_still_native_true;
          Alcotest.test_case
            "other backends retain native_json_schema_output = false"
            `Quick
            test_other_backends_still_false;
        ] );
    ]
