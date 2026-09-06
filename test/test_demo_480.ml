(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #480 — OpenCode parity uplift.

    AC traceability:
    - AC1: OpenCode is not treated as non-file-capable
    - AC2: MCP is configured through project-owned config; generated entries
           are disabled/template-only until explicitly approved
    - AC3: A project OpenCode setup generation path exists
    - AC4: OpenCode capability flags match stable upstream docs (1.14.20) *)

open Cabal

(** {1 Helpers} *)

let find_desc id =
  match Backend_registry.find id with
  | Some d -> d
  | None -> Alcotest.failf "descriptor not found for backend_id=%s" id

let contains_str s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let with_tmpdir f =
  let dir = Filename.temp_dir "epure_test_480_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

(** {1 AC1 — OpenCode is not treated as non-file-capable} *)

(* AC1: supports_file_reading returns true for opencode *)
let test_opencode_supports_file_reading () =
  Alcotest.(check bool)
    "opencode supports file reading"
    true
    (Backend_registry.supports_file_reading "opencode")

(* AC1: opencode descriptor has file_reading = true *)
let test_opencode_descriptor_file_reading () =
  let d = find_desc "opencode" in
  Alcotest.(check bool)
    "opencode descriptor file_reading = true"
    true
    d.Backend_registry.capabilities.Backend_registry.file_reading

(** {1 AC2 — MCP template is disabled/template-only by default} *)

(* AC2: the static generated opencode.json template does not enable any MCP
   entry.  "enabled: true" must never appear in the template so that no MCP
   server is activated before explicit approval. *)
let test_opencode_generated_config_no_enabled_mcp () =
  match Backend_config_gen.generate ~backend_id:"opencode" with
  | None -> Alcotest.fail "AC2: expected artifact for opencode"
  | Some artifact ->
      Alcotest.(check bool)
        "generated opencode.json has no \"enabled\": true entry"
        false
        (contains_str artifact.Backend_config_gen.content "\"enabled\": true")

(* AC2: template contains the Épure managed marker (project-owned config) *)
let test_opencode_generated_config_is_managed () =
  match Backend_config_gen.generate ~backend_id:"opencode" with
  | None -> Alcotest.fail "AC2: expected artifact for opencode"
  | Some artifact ->
      Alcotest.(check bool)
        "generated opencode.json carries Épure managed marker"
        true
        (Backend_config_gen.is_managed_content
           artifact.Backend_config_gen.content)

let test_opencode_generated_config_no_invalid_epure_keys () =
  match Backend_config_gen.generate ~backend_id:"opencode" with
  | None -> Alcotest.fail "AC2: expected artifact for opencode"
  | Some artifact ->
      List.iter
        (fun key ->
          Alcotest.(check bool)
            ("generated opencode.json has no invalid key " ^ key)
            false
            (contains_str artifact.Backend_config_gen.content key))
        ["_epure_attribution"; "_epure-managed"; "_epure-hash"]

(** {1 AC3 — A project OpenCode setup generation path exists} *)

(* AC3: generate returns Some for opencode *)
let test_opencode_generate_returns_some () =
  match Backend_config_gen.generate ~backend_id:"opencode" with
  | None -> Alcotest.fail "AC3: generate returned None for opencode"
  | Some _ -> ()

(* AC3: setup_project_config writes opencode.json and returns a successful
   write_outcome — must be Written or Already_current, not a skip or refusal *)
let test_opencode_setup_project_config_has_outcome () =
  with_tmpdir (fun dir ->
      let result =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      match result.Backend_config_gen.write_outcome with
      | None ->
          Alcotest.fail
            "AC3: setup_project_config returned no write_outcome for opencode"
      | Some (Backend_config_gen.Written _) -> ()
      | Some Backend_config_gen.Already_current ->
          Alcotest.fail
            "AC3: Already_current on fresh dir — file was not written"
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "AC3: unexpected hash mismatch on fresh dir: %s" msg
      | Some (Backend_config_gen.Backed_up_and_written _) ->
          Alcotest.fail "AC3: unexpected backup+write on fresh dir"
      | Some (Backend_config_gen.Skipped_user_content path) ->
          Alcotest.failf "AC3: unexpected skip of user content at %s" path
      | Some (Backend_config_gen.Invalid_managed_namespace msg) ->
          Alcotest.failf "AC3: unexpected invalid namespace: %s" msg
      | Some (Backend_config_gen.Unsafe_project_path msg) ->
          Alcotest.failf "AC3: unexpected unsafe project path: %s" msg)

(* AC3: generated artifact has the expected fixed path opencode.json *)
let test_opencode_artifact_fixed_path () =
  match Backend_config_gen.generate ~backend_id:"opencode" with
  | None -> Alcotest.fail "AC3: expected artifact for opencode"
  | Some artifact ->
      Alcotest.(check string)
        "opencode artifact project_relative_path"
        "opencode.json"
        artifact.Backend_config_gen.project_relative_path

(* AC3: setup_project_config creates the file on disk *)
let test_opencode_setup_writes_file () =
  with_tmpdir (fun dir ->
      let _ =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      let path = Filename.concat dir "opencode.json" in
      Alcotest.(check bool)
        "opencode.json exists after setup_project_config"
        true
        (Sys.file_exists path))

(** {1 AC4 — Capability flags match stable upstream docs (OpenCode 1.14.20)} *)

(* AC4: baseline version *)
let test_opencode_baseline_version () =
  let d = find_desc "opencode" in
  Alcotest.(check string)
    "opencode baseline version"
    "1.14.20"
    d.Backend_registry.baseline_version

(* AC4: MCP support mode is Mcp_config_file (reads opencode.json) *)
let test_opencode_mcp_support_mode () =
  let d = find_desc "opencode" in
  let open Backend_registry in
  Alcotest.(check bool)
    "opencode mcp_support = Mcp_config_file"
    true
    (d.capabilities.mcp_support = Mcp_config_file)

(* AC4: project config surface is Config_fixed_path (opencode.json) *)
let test_opencode_config_surface () =
  let d = find_desc "opencode" in
  let open Backend_registry in
  Alcotest.(check bool)
    "opencode project_config_surface = Config_fixed_path"
    true
    (d.capabilities.project_config_surface = Config_fixed_path)

(* AC4: structured_output = true (--format json produces JSONL) *)
let test_opencode_structured_output () =
  let d = find_desc "opencode" in
  Alcotest.(check bool)
    "opencode structured_output = true"
    true
    d.Backend_registry.capabilities.Backend_registry.structured_output

(* AC4: streaming_output = true (OpenCode 1.14.20 emits JSON events in stable) *)
let test_opencode_streaming_output () =
  let d = find_desc "opencode" in
  Alcotest.(check bool)
    "opencode streaming_output = true"
    true
    d.Backend_registry.capabilities.Backend_registry.streaming_output

(* AC4: precedence_confidence = Medium (fixed-path config, some user override
   risk) *)
let test_opencode_precedence_confidence () =
  let d = find_desc "opencode" in
  let open Backend_registry in
  Alcotest.(check bool)
    "opencode precedence_confidence = Medium"
    true
    (d.capabilities.precedence_confidence = Medium)

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story_480_opencode_parity"
    [
      ( "AC1 not non-file-capable",
        [
          Alcotest.test_case
            "supports_file_reading opencode = true"
            `Quick
            test_opencode_supports_file_reading;
          Alcotest.test_case
            "descriptor file_reading = true"
            `Quick
            test_opencode_descriptor_file_reading;
        ] );
      ( "AC2 MCP template disabled by default",
        [
          Alcotest.test_case
            "generated config has no enabled MCP entry"
            `Quick
            test_opencode_generated_config_no_enabled_mcp;
          Alcotest.test_case
            "generated config is Épure-managed"
            `Quick
            test_opencode_generated_config_is_managed;
          Alcotest.test_case
            "generated config avoids invalid _epure keys"
            `Quick
            test_opencode_generated_config_no_invalid_epure_keys;
        ] );
      ( "AC3 setup generation path exists",
        [
          Alcotest.test_case
            "generate returns Some for opencode"
            `Quick
            test_opencode_generate_returns_some;
          Alcotest.test_case
            "setup_project_config has write_outcome"
            `Quick
            test_opencode_setup_project_config_has_outcome;
          Alcotest.test_case
            "artifact uses fixed path opencode.json"
            `Quick
            test_opencode_artifact_fixed_path;
          Alcotest.test_case
            "setup_project_config creates file on disk"
            `Quick
            test_opencode_setup_writes_file;
        ] );
      ( "AC4 flags match upstream 1.14.20",
        [
          Alcotest.test_case
            "baseline version 1.14.20"
            `Quick
            test_opencode_baseline_version;
          Alcotest.test_case
            "mcp_support = Mcp_config_file"
            `Quick
            test_opencode_mcp_support_mode;
          Alcotest.test_case
            "project_config_surface = Config_fixed_path"
            `Quick
            test_opencode_config_surface;
          Alcotest.test_case
            "structured_output = true"
            `Quick
            test_opencode_structured_output;
          Alcotest.test_case
            "streaming_output = true"
            `Quick
            test_opencode_streaming_output;
          Alcotest.test_case
            "precedence_confidence = Medium"
            `Quick
            test_opencode_precedence_confidence;
        ] );
    ]
