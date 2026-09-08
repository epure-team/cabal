(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #483 — Copilot project configuration compatibility.

    Covers:
    - AC1: Supported Copilot project config files are created or updated
    - AC2: LSP config generation is integrated
    - AC3: MCP is fail-closed, with no project artifact or activation language
    - AC4: Stable limitations are documented *)

open Cabal

let contains_str s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let with_tmpdir f =
  let dir = Filename.temp_dir "epure_test_483_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let rec ensure_parent_dir path =
  let dir = Filename.dirname path in
  if dir = path || dir = "." then ()
  else begin
    ensure_parent_dir dir ;
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755
  end

let write_file path content =
  ensure_parent_dir path ;
  let oc = open_out path in
  output_string oc content ;
  close_out oc

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n ;
  close_in ic ;
  Bytes.to_string buf

let epure_mcp_server () =
  Backend_types.make_mcp_server_config
    ~name:"epure"
    ~command:"epure-mcp-server"
    ~args:["--stdio"]
    ~env:[("EPURE_PROJECT", "/tmp/project")]
    ()

let task_spec_with_mcp dir =
  Backend_types.make_task_spec
    ~prompt:"test"
    ~working_dir:dir
    ~mcp_servers:[epure_mcp_server ()]
    ()

let minimal_spec () : Backend_types.task_spec =
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
    resume_session_id = None;
    max_turns = None;
    read_only = false;
    json_schema = None;
  }

let check_failed_with_unsupported_mcp result =
  match result.Backend_types.status with
  | Backend_types.Failed msg ->
      Alcotest.(check bool) "failure mentions MCP" true (contains_str msg "MCP") ;
      Alcotest.(check bool) "failure is explicit" true
        (contains_str msg "unsupported")
  | _ -> Alcotest.fail "expected Copilot run_task to fail before invocation"

(** {1 AC1 — Project-scoped config files created or updated} *)

(* AC1: Copilot CLI stable supports project-scoped config via
   .github/copilot-instructions.md — the project_config_surface must
   reflect this (Config_fixed_path, not Config_none). *)
let test_ac1_copilot_has_project_config_surface () =
  let open Backend_registry in
  match find "copilot-cli" with
  | None -> Alcotest.fail "copilot-cli descriptor not found"
  | Some d ->
      Alcotest.(check bool)
        "AC1: copilot-cli project_config_surface = Config_fixed_path"
        true
        (d.capabilities.project_config_surface = Config_fixed_path)

(* AC1: generate returns Some artifact for copilot-cli. *)
let test_ac1_copilot_config_artifact_generated () =
  match Backend_config_gen.generate ~backend_id:"copilot-cli" with
  | None -> Alcotest.fail "AC1: expected Some artifact for copilot-cli"
  | Some a ->
      Alcotest.(check string)
        "AC1: artifact path is .github/copilot-instructions.md"
        ".github/copilot-instructions.md"
        a.Backend_config_gen.project_relative_path

(* AC1: setup_project_config writes the file to disk. *)
let test_ac1_setup_writes_copilot_config () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"copilot-cli"
           ~project_dir:dir
           ~force:false) ;
      let expected = Filename.concat dir ".github/copilot-instructions.md" in
      Alcotest.(check bool)
        "AC1: .github/copilot-instructions.md written to disk"
        true
        (Sys.file_exists expected))

(* AC1: setup_project_config returns Some write_outcome (not None). *)
let test_ac1_setup_returns_write_outcome () =
  with_tmpdir (fun dir ->
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"copilot-cli"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.write_outcome with
      | None ->
          Alcotest.fail
            "AC1: expected Some write_outcome for copilot-cli after setup"
      | Some _ -> ())

(* AC1: generated artifact bears the Épure managed marker. *)
let test_ac1_config_has_managed_marker () =
  match Backend_config_gen.generate ~backend_id:"copilot-cli" with
  | None -> Alcotest.fail "expected artifact for copilot-cli"
  | Some a ->
      Alcotest.(check bool)
        "AC1: copilot-instructions.md bears epure-managed marker"
        true
        (Backend_config_gen.is_managed_content a.Backend_config_gen.content)

(* AC1: generation is idempotent (same content on repeated calls). *)
let test_ac1_config_idempotent () =
  match
    ( Backend_config_gen.generate ~backend_id:"copilot-cli",
      Backend_config_gen.generate ~backend_id:"copilot-cli" )
  with
  | None, _ | _, None -> Alcotest.fail "expected artifact for copilot-cli"
  | Some a1, Some a2 ->
      Alcotest.(check string)
        "AC1: two generate calls produce identical content"
        a1.Backend_config_gen.content
        a2.Backend_config_gen.content

(* AC1: generated artifact has project instruction content (Markdown header). *)
let test_ac1_config_has_project_instructions () =
  match Backend_config_gen.generate ~backend_id:"copilot-cli" with
  | None -> Alcotest.fail "expected artifact for copilot-cli"
  | Some a ->
      let content = a.Backend_config_gen.content in
      Alcotest.(check bool)
        "AC1: config has a Markdown heading"
        true
        (contains_str content "# ") ;
      let non_header_lines =
        content |> String.split_on_char '\n'
        |> List.filter (fun l ->
            let t = String.trim l in
            t <> ""
            && (not (contains_str t "cabal-managed"))
            && (not (contains_str t "cabal-hash"))
            && (not (contains_str t "Generated by Cabal"))
            && (not (contains_str t "epure-managed"))
            && (not (contains_str t "epure-hash"))
            && not (contains_str t "Generated by Epure"))
      in
      Alcotest.(check bool)
        "AC1: config has substantive content beyond the managed header"
        true
        (List.length non_header_lines > 0)

(** {1 AC2 — LSP config generation is integrated} *)

(* AC2: generated config mentions LSP so that Copilot's language-server-based
   code analysis can be configured per project. *)
let test_ac2_config_has_lsp_section () =
  match Backend_config_gen.generate ~backend_id:"copilot-cli" with
  | None -> Alcotest.fail "expected artifact for copilot-cli"
  | Some a ->
      let lower = String.lowercase_ascii a.Backend_config_gen.content in
      Alcotest.(check bool)
        "AC2: config contains LSP reference"
        true
        (contains_str lower "lsp")

(** {1 AC3 — MCP fail-closed without project activation} *)

(* AC3: the quarantined runtime exposes no MCP transport. *)
let test_ac3_copilot_mcp_support_is_none () =
  let open Backend_registry in
  match find "copilot-cli" with
  | None -> Alcotest.fail "copilot-cli descriptor not found"
  | Some d ->
      Alcotest.(check bool)
        "AC3: copilot-cli mcp_support = Mcp_none"
        true
        (d.capabilities.mcp_support = Mcp_none)

(* AC3: generated instructions explain why MCP and runtime execution are
   unavailable without suggesting that project MCP configuration is usable. *)
let test_ac3_config_documents_mcp_quarantine () =
  match Backend_config_gen.generate ~backend_id:"copilot-cli" with
  | None -> Alcotest.fail "expected artifact for copilot-cli"
  | Some a ->
      let lower = String.lowercase_ascii a.Backend_config_gen.content in
      Alcotest.(check bool) "AC3: config documents MCP" true
        (contains_str lower "mcp") ;
      Alcotest.(check bool) "AC3: config documents runtime quarantine" true
        (contains_str lower "runtime is quarantined") ;
      Alcotest.(check bool)
        "AC3: config documents unproven complete MCP isolation"
        true
        (contains_str lower "complete mcp discovery isolation is unproven")

let test_ac3_generated_artifacts_exclude_mcp_activation () =
  let artifacts =
    Backend_config_gen.generate_all ~backend_id:"copilot-cli"
      ~mcp_servers:[epure_mcp_server ()]
  in
  Alcotest.(check bool)
    "AC3: no .github/mcp.json artifact"
    false
    (List.exists
       (fun artifact ->
         artifact.Backend_config_writer.project_relative_path
         = ".github/mcp.json")
       artifacts) ;
  let activation_markers =
    [
      ".github/mcp.json";
      "--additional-mcp-config";
      "\"mcpservers\"";
      "enable mcp";
      "mcp enabled";
      "activate mcp";
      "mcp activation";
      "configure mcp servers";
    ]
  in
  List.iter
    (fun artifact ->
      let content =
        String.lowercase_ascii artifact.Backend_config_writer.content
      in
      List.iter
        (fun marker ->
          Alcotest.(check bool)
            ("AC3: generated artifacts omit MCP activation marker " ^ marker)
            false
            (contains_str content marker))
        activation_markers)
    artifacts

let test_ac3_build_command_ignores_transient_mcp_config () =
  let cmd, _stdin =
    Copilot_cli.Private.build_command
      ~mcp_config_path:(Some "/tmp/transient-mcp.json")
      (minimal_spec ())
  in
  Alcotest.(check bool)
    "AC3: build_command does not pass transient MCP config flag"
    false
    (List.mem "--additional-mcp-config" cmd)

let test_ac3_requested_mcp_rejection_preserves_user_config () =
  with_tmpdir (fun dir ->
      let mcp_path = Filename.concat dir ".github/mcp.json" in
      let original = {|{"mcpServers":{"user":{"command":"user-mcp"}}}|} in
      write_file mcp_path original ;
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let result = Copilot_cli.run_task ~sw ~env (task_spec_with_mcp dir) in
      check_failed_with_unsupported_mcp result ;
      Alcotest.(check string)
        "user-authored MCP config unchanged"
        original
        (read_file mcp_path))

let test_ac3_requested_mcp_rejection_preserves_config_after_setup () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"copilot-cli"
           ~project_dir:dir
           ~force:false) ;
      let mcp_path = Filename.concat dir ".github/mcp.json" in
      let modified = {|{"mcpServers":{"user":{"command":"modified"}}}|} in
      write_file mcp_path modified ;
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let result = Copilot_cli.run_task ~sw ~env (task_spec_with_mcp dir) in
      check_failed_with_unsupported_mcp result ;
      Alcotest.(check string)
        "user MCP config created after setup remains unchanged"
        modified
        (read_file mcp_path))

(* AC3: explanatory MCP prose contains no active MCP server configuration. *)
let test_ac3_config_has_no_active_mcp_servers () =
  match Backend_config_gen.generate ~backend_id:"copilot-cli" with
  | None -> Alcotest.fail "expected artifact for copilot-cli"
  | Some a ->
      let content = a.Backend_config_gen.content in
      (* Any active (non-comment) mcpServers JSON key would indicate a live
         MCP config embedded in the instructions.  Prose may mention the key;
         the active key shape is a non-comment line containing [mcpServers:]. *)
      let lines = String.split_on_char '\n' content in
      let has_active_mcp_servers =
        List.exists
          (fun line ->
            let trimmed = String.trim line in
            let lower = String.lowercase_ascii trimmed in
            ((contains_str lower "\"mcpservers\""
             || String.starts_with ~prefix:"mcpservers" lower)
            && contains_str lower ":")
            && trimmed <> ""
            && trimmed.[0] <> '#'
            && trimmed.[0] <> '<'
            && trimmed.[0] <> '/')
          lines
      in
      Alcotest.(check bool)
        "AC3: no active mcpServers key — MCP disabled by default"
        false
        has_active_mcp_servers

(* AC3: no secret keywords in generated config (SEC-1). *)
let test_ac3_config_no_secrets () =
  match Backend_config_gen.generate ~backend_id:"copilot-cli" with
  | None -> Alcotest.fail "expected artifact for copilot-cli"
  | Some a ->
      let lower = String.lowercase_ascii a.Backend_config_gen.content in
      let secret_keywords =
        ["password"; "credential"; "bearer"; "private_key"]
      in
      List.iter
        (fun kw ->
          Alcotest.(check bool)
            ("AC3: no secret keyword '" ^ kw ^ "' in config")
            false
            (contains_str lower kw))
        secret_keywords

(** {1 AC4 — Stable limitations are documented} *)

(* AC4: generated project instructions continue to document limitations. *)
let test_ac4_limitations_documented () =
  match Backend_config_gen.generate ~backend_id:"copilot-cli" with
  | None -> Alcotest.fail "expected artifact for copilot-cli"
  | Some a ->
      let lower = String.lowercase_ascii a.Backend_config_gen.content in
      Alcotest.(check bool)
        "AC4: config documents stable limitations"
        true
        ((contains_str lower "stable" && contains_str lower "limitation")
        || contains_str lower "not supported"
        || contains_str lower "not available") ;
      Alcotest.(check bool)
        "AC4: config records authenticated attachment observation"
        true
        (contains_str lower
           "authenticated copilot cli 1.0.54 attachment behavior was observed") ;
      Alcotest.(check bool)
        "AC4: config records no positive media evidence"
        true
        (contains_str lower "no positive media evidence is recorded") ;
      Alcotest.(check bool)
        "AC4: config says dormant JSONL is not a structured-output capability"
        true
        (contains_str lower "structured_output: disabled while quarantined") ;
      Alcotest.(check bool)
        "AC4: config does not claim active sealed PNG/JPEG transport"
        false
        (contains_str lower "transports sealed png/jpeg")

(* AC4: baseline version is the exact investigated Copilot version. *)
let test_ac4_baseline_is_stable () =
  let open Backend_registry in
  match find "copilot-cli" with
  | None -> Alcotest.fail "copilot-cli descriptor not found"
  | Some d ->
      Alcotest.(check string)
        "AC4: baseline_version = 1.0.54 (stable)"
        "1.0.54"
        d.baseline_version

(* AC4: capability flags accurately reflect stable limitations. *)
let test_ac4_capability_flags_reflect_stable_limits () =
  let open Backend_registry in
  match find "copilot-cli" with
  | None -> Alcotest.fail "copilot-cli descriptor not found"
  | Some d ->
      let caps = d.capabilities in
      Alcotest.(check bool)
        "AC4: streaming_output = false"
        false
        caps.streaming_output ;
      Alcotest.(check bool)
        "AC4: structured_output = false while quarantined"
        false
        caps.structured_output ;
      Alcotest.(check bool)
        "AC4: session_resume = false"
        false
        caps.session_resume ;
      Alcotest.(check bool)
        "AC4: read_only_support = false"
        false
        caps.read_only_support ;
      Alcotest.(check bool)
        "AC4: file_reading = false"
        false
        caps.file_reading ;
      Alcotest.(check bool) "AC4: mcp_support = Mcp_none" true
        (caps.mcp_support = Mcp_none) ;
      Alcotest.(check (list bool))
        "AC4: media support and evidence are disabled"
        [true; true]
        [
          caps.media_support.media_types = [];
          caps.media_support.evidence = None;
        ] ;
      Alcotest.(check (list bool))
        "AC4: web support and evidence are disabled"
        [true; true]
        [
          caps.web_support.maximum = Backend_types.Web_disabled;
          caps.web_support.evidence = None;
        ] ;
      Alcotest.(check (list bool))
        "AC4: native schema and evidence are disabled"
        [true; true]
        [
          not caps.native_json_schema_output;
          caps.native_json_schema_output_evidence = None;
        ]

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story_483_copilot_parity"
    [
      ( "AC1 project config files created/updated",
        [
          Alcotest.test_case
            "project_config_surface = Config_fixed_path"
            `Quick
            test_ac1_copilot_has_project_config_surface;
          Alcotest.test_case
            "generate returns Some artifact"
            `Quick
            test_ac1_copilot_config_artifact_generated;
          Alcotest.test_case
            "setup writes .github/copilot-instructions.md"
            `Quick
            test_ac1_setup_writes_copilot_config;
          Alcotest.test_case
            "setup returns Some write_outcome"
            `Quick
            test_ac1_setup_returns_write_outcome;
          Alcotest.test_case
            "artifact bears epure-managed marker"
            `Quick
            test_ac1_config_has_managed_marker;
          Alcotest.test_case
            "generate is idempotent"
            `Quick
            test_ac1_config_idempotent;
          Alcotest.test_case
            "config has substantive project instruction content"
            `Quick
            test_ac1_config_has_project_instructions;
        ] );
      ( "AC2 LSP config generation integrated",
        [
          Alcotest.test_case
            "config contains LSP reference"
            `Quick
            test_ac2_config_has_lsp_section;
        ] );
      ( "AC3 MCP fail-closed without activation",
        [
          Alcotest.test_case
            "hardened runtime mcp_support = Mcp_none"
            `Quick
            test_ac3_copilot_mcp_support_is_none;
          Alcotest.test_case
            "config documents MCP quarantine"
            `Quick
            test_ac3_config_documents_mcp_quarantine;
          Alcotest.test_case
            "generated artifacts exclude MCP activation"
            `Quick
            test_ac3_generated_artifacts_exclude_mcp_activation;
          Alcotest.test_case
            "build_command ignores transient MCP config"
            `Quick
            test_ac3_build_command_ignores_transient_mcp_config;
          Alcotest.test_case
            "requested MCP rejection preserves user project config"
            `Quick
            test_ac3_requested_mcp_rejection_preserves_user_config;
          Alcotest.test_case
            "requested MCP rejection preserves config created after setup"
            `Quick
            test_ac3_requested_mcp_rejection_preserves_config_after_setup;
          Alcotest.test_case
            "generated instructions contain no active MCP servers"
            `Quick
            test_ac3_config_has_no_active_mcp_servers;
          Alcotest.test_case
            "no secret keywords in generated config"
            `Quick
            test_ac3_config_no_secrets;
        ] );
      ( "AC4 stable limitations documented",
        [
          Alcotest.test_case
            "config documents stable limitations"
            `Quick
            test_ac4_limitations_documented;
          Alcotest.test_case
            "baseline = 1.0.54 (investigated version)"
            `Quick
            test_ac4_baseline_is_stable;
          Alcotest.test_case
            "capability flags reflect stable limits"
            `Quick
            test_ac4_capability_flags_reflect_stable_limits;
        ] );
    ]
