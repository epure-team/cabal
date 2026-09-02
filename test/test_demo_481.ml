(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #481 — Codex parity uplift (Codex 0.131.0).

    Covers:
    - AC1: MCP support wired for approved entries; unapproved entries
           left disabled/template-only in generated config
    - AC2: Meaningful .codex/config.toml project config exists
    - AC3: Structured output, session ID, and resume behaviors preserved
    - AC4: Capability flags match stable upstream docs for Codex 0.131.0 *)

open Cabal

let () = Process_test_helper.install_launcher ()

let contains_str s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let with_tmpdir f =
  let dir = Filename.temp_dir "epure_test_481_" "" in
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
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let with_path_prefix dir f =
  let old_path = Sys.getenv_opt "PATH" in
  let new_path =
    match old_path with None -> dir | Some path -> dir ^ ":" ^ path
  in
  Unix.putenv "PATH" new_path ;
  Fun.protect
    ~finally:(fun () ->
      match old_path with
      | None -> Unix.putenv "PATH" ""
      | Some path -> Unix.putenv "PATH" path)
    f

let write_fake_codex bin_dir ~marker =
  let script = Filename.concat bin_dir "codex" in
  write_file
    script
    (Printf.sprintf
       "#!/bin/sh\n\
        : > %s\n\
        for f in .epure-mcp-config-*; do\n\
       \  if [ -e \"$f\" ]; then\n\
       \    echo \"transient MCP file present: $f\" >&2\n\
       \    exit 23\n\
       \  fi\n\
        done\n\
        printf '%%s\\n' \
        '{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"ok\"}}'\n\
        printf '%%s\\n' \
        '{\"type\":\"turn.completed\",\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}'\n"
       (Filename.quote marker)) ;
  Unix.chmod script 0o755

let epure_mcp_server () =
  Backend_types.make_mcp_server_config
    ~name:"epure"
    ~command:"/bin/echo"
    ~args:["hello"; "two words"]
    ~env:[("EPURE_DB", "/tmp/db")]
    ()

let epure_mcp_server_with_existing_env_ref () =
  Backend_types.make_mcp_server_config
    ~name:"epure"
    ~command:"/bin/echo"
    ~args:["hello"]
    ~env:[("EPURE_DB", "$ALREADY_SET")]
    ()

let epure_mcp_server_with_empty_env_name () =
  Backend_types.make_mcp_server_config
    ~name:"epure-empty-key"
    ~command:"/bin/echo"
    ~args:["hello"]
    ~env:[("", "/tmp/db"); ("EPURE_DB", "/tmp/db")]
    ()

let quoted_name_mcp_server () =
  Backend_types.make_mcp_server_config
    ~name:"epure tools"
    ~command:"/bin/echo"
    ~args:["quote \"value"; "back\\slash"]
    ~env:[("EPURE_VALUE", "value with \"quote\" and \\slash")]
    ()

(** {1 AC1 — MCP support wired as Mcp_config_file in the registry} *)

let test_codex_mcp_support_is_config_file () =
  let open Backend_registry in
  match find "codex" with
  | None -> Alcotest.fail "codex descriptor not found"
  | Some d ->
      Alcotest.(check bool)
        "codex mcp_support = Mcp_config_file"
        true
        (d.capabilities.mcp_support = Mcp_config_file)

(** {1 AC1 — Generated .codex/config.toml includes a disabled MCP template} *)

let test_codex_config_includes_mcp_template () =
  match Backend_config_gen.generate ~backend_id:"codex" with
  | None -> Alcotest.fail "expected artifact for codex"
  | Some a ->
      let lower = String.lowercase_ascii a.Backend_config_gen.content in
      Alcotest.(check bool)
        "config.toml contains 'mcp' reference"
        true
        (contains_str lower "mcp")

(** {1 AC1 — MCP section is disabled/template-only (not an active config key)} *)

let test_codex_config_mcp_is_commented_out () =
  match Backend_config_gen.generate ~backend_id:"codex" with
  | None -> Alcotest.fail "expected artifact for codex"
  | Some a ->
      let content = a.Backend_config_gen.content in
      (* The active (uncommented) MCP section must NOT be present;
         the template uses comment lines only. *)
      let lines = String.split_on_char '\n' content in
      let has_active_mcp =
        List.exists
          (fun line ->
            let trimmed = String.trim line in
            (* Any active MCP section header must not be present; the template
               uses commented [mcp_servers.example] lines only. *)
            (trimmed = "[mcp]" || contains_str trimmed "[mcp")
            && trimmed <> ""
            && trimmed.[0] <> '#')
          lines
      in
      Alcotest.(check bool)
        "no active [mcp] section — MCP template is comment-only"
        false
        has_active_mcp

let test_codex_config_serializes_approved_mcp_server () =
  match
    Backend_config_gen.generate_all
      ~backend_id:"codex"
      ~mcp_servers:[epure_mcp_server ()]
  with
  | [] -> Alcotest.fail "expected artifact for codex"
  | a :: _ ->
      let content = a.Backend_config_gen.content in
      Alcotest.(check bool)
        "active Codex MCP table is present"
        true
        (contains_str content "[mcp_servers.epure]") ;
      Alcotest.(check bool)
        "Codex MCP command is serialized"
        true
        (contains_str content {|command = "/bin/echo"|}) ;
      Alcotest.(check bool)
        "Codex MCP args are serialized as TOML array"
        true
        (contains_str content {|args = ["hello", "two words"]|}) ;
      Alcotest.(check bool)
        "Codex MCP env table is serialized"
        true
        (contains_str content "[mcp_servers.epure.env]") ;
      Alcotest.(check bool)
        "Codex MCP env value is persisted as env reference"
        true
        (contains_str content {|EPURE_DB = "$EPURE_DB"|}) ;
      Alcotest.(check bool)
        "Codex MCP raw env value is not persisted"
        false
        (contains_str content {|EPURE_DB = "/tmp/db"|})

let test_codex_config_preserves_existing_env_reference () =
  match
    Backend_config_gen.generate_all
      ~backend_id:"codex"
      ~mcp_servers:[epure_mcp_server_with_existing_env_ref ()]
  with
  | [] -> Alcotest.fail "expected artifact for codex"
  | a :: _ ->
      Alcotest.(check bool)
        "existing env reference preserved"
        true
        (contains_str
           a.Backend_config_gen.content
           {|EPURE_DB = "$ALREADY_SET"|})

let test_codex_config_drops_empty_env_key () =
  match
    Backend_config_gen.generate_all
      ~backend_id:"codex"
      ~mcp_servers:[epure_mcp_server_with_empty_env_name ()]
  with
  | [] -> Alcotest.fail "expected artifact for codex"
  | a :: _ ->
      let content = a.Backend_config_gen.content in
      Alcotest.(check bool)
        "empty env key is not serialized"
        false
        (contains_str content {|"" = "/tmp/db"|}) ;
      Alcotest.(check bool)
        "empty env key entry is absent"
        false
        (List.exists
           (fun line ->
             let trimmed = String.trim line in
             String.starts_with ~prefix:"\"\" =" trimmed)
           (String.split_on_char '\n' content)) ;
      Alcotest.(check bool)
        "valid env ref is preserved"
        true
        (contains_str content {|EPURE_DB = "$EPURE_DB"|})

let test_codex_config_toml_quotes_non_bare_mcp_names_and_values () =
  match
    Backend_config_gen.generate_all
      ~backend_id:"codex"
      ~mcp_servers:[quoted_name_mcp_server ()]
  with
  | [] -> Alcotest.fail "expected artifact for codex"
  | a :: _ ->
      let content = a.Backend_config_gen.content in
      Alcotest.(check bool)
        "non-bare MCP server name is quoted in table"
        true
        (contains_str content {|[mcp_servers."epure tools"]|}) ;
      Alcotest.(check bool)
        "quoted args are escaped"
        true
        (contains_str content {|args = ["quote \"value", "back\\slash"]|}) ;
      Alcotest.(check bool)
        "non-bare MCP env table uses quoted server name"
        true
        (contains_str content {|[mcp_servers."epure tools".env]|}) ;
      Alcotest.(check bool)
        "env string value is persisted as env reference"
        true
        (contains_str content {|EPURE_VALUE = "$EPURE_VALUE"|}) ;
      Alcotest.(check bool)
        "raw quoted env value is not persisted"
        false
        (contains_str content {|value with \"quote\" and \\slash|})

(** {1 AC2 — Generated artifact path is .codex/config.toml} *)

let test_codex_config_toml_path () =
  match Backend_config_gen.generate ~backend_id:"codex" with
  | None -> Alcotest.fail "expected artifact for codex"
  | Some a ->
      Alcotest.(check string)
        "codex config path"
        ".codex/config.toml"
        a.Backend_config_gen.project_relative_path

(** {1 AC2 — Config avoids provider pinning and keeps MCP template} *)

let test_codex_config_does_not_force_provider () =
  match Backend_config_gen.generate ~backend_id:"codex" with
  | None -> Alcotest.fail "expected artifact for codex"
  | Some a ->
      let content = a.Backend_config_gen.content in
      Alcotest.(check bool)
        "config does not contain stale [model] table"
        false
        (contains_str content "[model]") ;
      Alcotest.(check bool)
        "config does not force model_provider"
        false
        (contains_str content "model_provider") ;
      Alcotest.(check bool)
        "config does not force provider assignment"
        false
        (contains_str content "provider =") ;
      Alcotest.(check bool)
        "config contains commented mcp_servers example"
        true
        (contains_str content "# [mcp_servers.example]")

(** {1 AC3 — Structured output capability preserved} *)

let test_codex_structured_output_preserved () =
  match Backend_registry.find "codex" with
  | None -> Alcotest.fail "codex descriptor not found"
  | Some d ->
      Alcotest.(check bool)
        "structured_output = true (preserved)"
        true
        d.Backend_registry.capabilities.Backend_registry.structured_output

(** {1 AC3 — Session resume capability preserved} *)

let test_codex_session_resume_preserved () =
  match Backend_registry.find "codex" with
  | None -> Alcotest.fail "codex descriptor not found"
  | Some d ->
      Alcotest.(check bool)
        "session_resume = true (preserved)"
        true
        d.Backend_registry.capabilities.Backend_registry.session_resume

(** {1 AC3 — JSONL structured output parsing still works} *)

let test_codex_jsonl_parsing_preserved () =
  let input =
    {|{"type":"item.completed","item":{"type":"agent_message","text":"hello"}}
{"type":"turn.completed","usage":{"input_tokens":10,"output_tokens":5}}|}
  in
  let text, cost = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string) "last message text" "hello" text ;
  Alcotest.(check bool) "cost is Some" true (Option.is_some cost)

(** {1 AC3 — Session ID parsing still functions} *)

let test_codex_session_id_field_in_jsonl () =
  (* parse_jsonl_output scans for usage/message; session_id is a separate field.
     Verify that the presence of a session_id field in JSONL does not break
     the parser and that text extraction still works. *)
  let input =
    {|{"type":"turn.started","session_id":"sess-abc","thread_id":"thr-xyz"}
{"type":"item.completed","item":{"type":"agent_message","text":"result"}}|}
  in
  let text, _ = Codex_cli.parse_jsonl_output input in
  Alcotest.(check string)
    "text extraction unaffected by session_id"
    "result"
    text

let task_spec_with_mcp dir =
  Backend_types.make_task_spec
    ~prompt:"test"
    ~working_dir:dir
    ~mcp_servers:[epure_mcp_server ()]
    ()

let check_failed_before_codex_invocation result marker =
  match result.Backend_types.status with
  | Backend_types.Failed msg ->
      Alcotest.(check bool) "failure mentions MCP" true (contains_str msg "MCP") ;
      Alcotest.(check bool)
        "fake codex was not invoked"
        false
        (Sys.file_exists marker)
  | _ -> Alcotest.fail "expected Codex run_task to fail before invocation"

let test_codex_run_fails_when_user_config_blocks_requested_mcp () =
  with_tmpdir (fun dir ->
      let bin_dir = Filename.concat dir "bin" in
      Unix.mkdir bin_dir 0o755 ;
      let marker = Filename.concat dir "codex-invoked" in
      write_fake_codex bin_dir ~marker ;
      let config_path = Filename.concat dir ".codex/config.toml" in
      let original =
        "# user-authored config\n[mcp_servers.user]\ncommand = \"user\"\n"
      in
      write_file config_path original ;
      with_path_prefix bin_dir (fun () ->
          Eio_posix.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let result = Codex_cli.run_task ~sw ~env (task_spec_with_mcp dir) in
          check_failed_before_codex_invocation result marker ;
          Alcotest.(check string)
            "user-authored Codex config unchanged"
            original
            (read_file config_path)))

let test_codex_run_fails_when_hash_mismatch_blocks_requested_mcp () =
  with_tmpdir (fun dir ->
      let bin_dir = Filename.concat dir "bin" in
      Unix.mkdir bin_dir 0o755 ;
      let marker = Filename.concat dir "codex-invoked" in
      write_fake_codex bin_dir ~marker ;
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"codex"
           ~project_dir:dir
           ~force:false) ;
      let config_path = Filename.concat dir ".codex/config.toml" in
      let modified = read_file config_path ^ "# user modification\n" in
      write_file config_path modified ;
      with_path_prefix bin_dir (fun () ->
          Eio_posix.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let result = Codex_cli.run_task ~sw ~env (task_spec_with_mcp dir) in
          check_failed_before_codex_invocation result marker ;
          Alcotest.(check string)
            "hash-mismatched Codex config unchanged"
            modified
            (read_file config_path)))

let test_codex_run_uses_project_mcp_without_transient_file () =
  with_tmpdir (fun dir ->
      let bin_dir = Filename.concat dir "bin" in
      Unix.mkdir bin_dir 0o755 ;
      let marker = Filename.concat dir "codex-invoked" in
      write_fake_codex bin_dir ~marker ;
      with_path_prefix bin_dir (fun () ->
          Eio_posix.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let result = Codex_cli.run_task ~sw ~env (task_spec_with_mcp dir) in
          (match result.Backend_types.status with
          | Backend_types.Success -> ()
          | Backend_types.Failed msg -> Alcotest.failf "codex failed: %s" msg
          | Backend_types.Timeout -> Alcotest.fail "codex timed out"
          | Backend_types.Cancelled -> Alcotest.fail "codex cancelled") ;
          Alcotest.(check bool)
            "fake codex was invoked"
            true
            (Sys.file_exists marker)))

(** {1 AC4 — All capability flags match stable upstream docs for Codex 0.131.0} *)

let test_codex_capability_flags_match_baseline () =
  let open Backend_registry in
  match find "codex" with
  | None -> Alcotest.fail "codex descriptor not found"
  | Some d ->
      let caps = d.capabilities in
      Alcotest.(check bool)
        "AC4: baseline_version = 0.131.0"
        true
        (d.baseline_version = "0.131.0") ;
      Alcotest.(check bool)
        "AC4: structured_output = true"
        true
        caps.structured_output ;
      Alcotest.(check bool)
        "AC4: session_resume = true"
        true
        caps.session_resume ;
      Alcotest.(check bool)
        "AC4: read_only_support = true"
        true
        caps.read_only_support ;
      Alcotest.(check bool)
        "AC4: mcp_support = Mcp_config_file (parity with 0.131.0)"
        true
        (caps.mcp_support = Mcp_config_file) ;
      Alcotest.(check bool)
        "AC4: project_config_surface = Config_fixed_path"
        true
        (caps.project_config_surface = Config_fixed_path) ;
      Alcotest.(check bool)
        "AC4: precedence_confidence = Medium"
        true
        (caps.precedence_confidence = Medium)

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story_481_codex_parity"
    [
      ( "AC1 MCP registry flag",
        [
          Alcotest.test_case
            "codex mcp_support = Mcp_config_file"
            `Quick
            test_codex_mcp_support_is_config_file;
        ] );
      ( "AC1 MCP template in config",
        [
          Alcotest.test_case
            "config.toml includes mcp reference"
            `Quick
            test_codex_config_includes_mcp_template;
          Alcotest.test_case
            "mcp section is comment-only (disabled by default)"
            `Quick
            test_codex_config_mcp_is_commented_out;
          Alcotest.test_case
            "approved MCP server serialized into config.toml"
            `Quick
            test_codex_config_serializes_approved_mcp_server;
          Alcotest.test_case
            "TOML quoting handles non-bare names and values"
            `Quick
            test_codex_config_toml_quotes_non_bare_mcp_names_and_values;
          Alcotest.test_case
            "existing env references are preserved"
            `Quick
            test_codex_config_preserves_existing_env_reference;
          Alcotest.test_case
            "empty env name entries are dropped"
            `Quick
            test_codex_config_drops_empty_env_key;
        ] );
      ( "AC2 project config",
        [
          Alcotest.test_case
            "config path is .codex/config.toml"
            `Quick
            test_codex_config_toml_path;
          Alcotest.test_case
            "config does not force provider selection"
            `Quick
            test_codex_config_does_not_force_provider;
        ] );
      ( "AC3 preserved behaviors",
        [
          Alcotest.test_case
            "structured_output preserved"
            `Quick
            test_codex_structured_output_preserved;
          Alcotest.test_case
            "session_resume preserved"
            `Quick
            test_codex_session_resume_preserved;
          Alcotest.test_case
            "JSONL parsing preserved"
            `Quick
            test_codex_jsonl_parsing_preserved;
          Alcotest.test_case
            "session_id field does not break parsing"
            `Quick
            test_codex_session_id_field_in_jsonl;
          Alcotest.test_case
            "user-authored config blocks requested MCP before invocation"
            `Quick
            test_codex_run_fails_when_user_config_blocks_requested_mcp;
          Alcotest.test_case
            "hash-mismatched config blocks requested MCP before invocation"
            `Quick
            test_codex_run_fails_when_hash_mismatch_blocks_requested_mcp;
          Alcotest.test_case
            "project MCP config avoids transient MCP file"
            `Quick
            test_codex_run_uses_project_mcp_without_transient_file;
        ] );
      ( "AC4 capability flags at baseline",
        [
          Alcotest.test_case
            "all flags match Codex 0.131.0 upstream docs"
            `Quick
            test_codex_capability_flags_match_baseline;
        ] );
    ]
