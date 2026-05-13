(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Backend_config_gen — Story #478.

    Covers:
    - AC1: generate returns an artifact for each backend with a config surface
    - AC2: generation is idempotent (same content on repeated calls)
    - AC3: generated content is clearly attributable to Épure
    - AC4: unmarked user content at fixed paths is preserved
    - AC5: Config_explicit_flag backends receive an Épure-owned path
    - AC6: Config_fixed_path backends include managed marker + hash;
           hash mismatch refuses by default; --force performs backup *)

open Cabal

(** {1 Helpers} *)

let contains_str s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let get_artifact backend_id =
  match Backend_config_gen.generate ~backend_id with
  | Some a -> a
  | None -> Alcotest.failf "expected artifact for backend_id=%s" backend_id

let get_artifacts ?(mcp_servers = []) ?lsp_servers ?managed_namespace backend_id
    =
  match
    Backend_config_gen.generate_all_with_options
      ?managed_namespace
      ?lsp_servers
      ~backend_id
      ~mcp_servers
      ()
  with
  | [] -> Alcotest.failf "expected artifacts for backend_id=%s" backend_id
  | artifacts -> artifacts

let artifact_by_path artifacts path =
  match
    List.find_opt
      (fun a -> a.Backend_config_gen.project_relative_path = path)
      artifacts
  with
  | Some artifact -> artifact
  | None -> Alcotest.failf "expected artifact at path %s" path

let with_tmpdir f =
  let dir = Filename.temp_dir "epure_test_cfg_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let write_file path content =
  let oc = open_out path in
  output_string oc content ;
  close_out oc

let rec ensure_parent_dir path =
  let dir = Filename.dirname path in
  if dir = path || dir = "." then ()
  else begin
    ensure_parent_dir dir ;
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755
  end

let write_file_creating_dirs path content =
  ensure_parent_dir path ;
  write_file path content

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n ;
  close_in ic ;
  Bytes.to_string buf

let file_perm path = (Unix.stat path).Unix.st_perm land 0o777

let with_umask mask f =
  let old = Unix.umask mask in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask old)) f

let candidate_source_paths rel =
  [
    rel;
    Filename.concat ".." rel;
    Filename.concat "../.." rel;
    Filename.concat "../../.." rel;
  ]

let read_source_file rels =
  let candidates = List.concat_map candidate_source_paths rels in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> read_file path
  | None ->
      Alcotest.failf
        "could not locate source file; tried: %s"
        (String.concat ", " candidates)

let test_backend_config_gen_is_facade_not_provider () =
  let source =
    read_source_file
      ["src/backend_config_gen.ml"; "libs/cabal/src/backend_config_gen.ml"]
  in
  List.iter
    (fun forbidden ->
      Alcotest.(check bool)
        ("Backend_config_gen does not own " ^ forbidden)
        false
        (contains_str source forbidden))
    [
      "type gemini_settings_json";
      "type copilot_mcp_json";
      "let canonical_body_for";
      "let make_gemini_settings_content";
      "let make_copilot_mcp_content";
      "let generate_all_with_claude_body";
    ]

(** {1 AC1 — generate returns Some for backends with a config surface} *)

let test_generate_claude_code () =
  match Backend_config_gen.generate ~backend_id:"claude-code" with
  | Some _ -> ()
  | None -> Alcotest.fail "expected artifact for claude-code"

let test_generate_codex () =
  match Backend_config_gen.generate ~backend_id:"codex" with
  | Some _ -> ()
  | None -> Alcotest.fail "expected artifact for codex"

let test_generate_opencode () =
  match Backend_config_gen.generate ~backend_id:"opencode" with
  | Some _ -> ()
  | None -> Alcotest.fail "expected artifact for opencode"

let test_generate_gemini_cli () =
  match Backend_config_gen.generate ~backend_id:"gemini-cli" with
  | Some _ -> ()
  | None -> Alcotest.fail "expected artifact for gemini-cli"

let test_generate_copilot_cli_some () =
  match Backend_config_gen.generate ~backend_id:"copilot-cli" with
  | Some a ->
      Alcotest.(check string)
        "copilot-cli path is .github/copilot-instructions.md"
        ".github/copilot-instructions.md"
        a.Backend_config_gen.project_relative_path
  | None ->
      Alcotest.fail "expected Some artifact for copilot-cli (Config_fixed_path)"

let test_generate_unknown_none () =
  match Backend_config_gen.generate ~backend_id:"no-such-backend" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for unknown backend"

(** {1 AC2 — generate is idempotent: same content on repeated calls} *)

let test_idempotent backend_id () =
  let a = get_artifact backend_id in
  let b = get_artifact backend_id in
  Alcotest.(check string)
    ("idempotent content for " ^ backend_id)
    a.Backend_config_gen.content
    b.Backend_config_gen.content

(** {1 AC3 — generated content is clearly attributable to Épure} *)

let test_attribution backend_id () =
  let a = get_artifact backend_id in
  Alcotest.(check bool)
    ("attribution marker present for " ^ backend_id)
    true
    (contains_str a.Backend_config_gen.content "Generated by Cabal")

(** {1 AC5 — Config_explicit_flag → Épure-owned path} *)

let test_claude_code_ownership () =
  let a = get_artifact "claude-code" in
  Alcotest.(check bool)
    "claude-code ownership is Epure_owned"
    true
    (a.Backend_config_gen.ownership = Backend_config_gen.Epure_owned)

let test_claude_code_path_under_epure () =
  let a = get_artifact "claude-code" in
  Alcotest.(check bool)
    "claude-code path starts with .cabal/"
    true
    (contains_str a.Backend_config_gen.project_relative_path ".cabal/")

(** {1 AC5/AC6 — Config_fixed_path → Backend_project ownership} *)

let test_backend_project_ownership backend_id () =
  let a = get_artifact backend_id in
  Alcotest.(check bool)
    (backend_id ^ " ownership is Backend_project")
    true
    (a.Backend_config_gen.ownership = Backend_config_gen.Backend_project)

let test_codex_path () =
  let a = get_artifact "codex" in
  Alcotest.(check string)
    "codex path"
    ".codex/config.toml"
    a.Backend_config_gen.project_relative_path

let test_opencode_path () =
  let a = get_artifact "opencode" in
  Alcotest.(check string)
    "opencode path"
    "opencode.json"
    a.Backend_config_gen.project_relative_path

let test_gemini_path () =
  let a = get_artifact "gemini-cli" in
  Alcotest.(check string)
    "gemini-cli path"
    ".gemini/settings.json"
    a.Backend_config_gen.project_relative_path

let test_gemini_generate_all_paths () =
  let artifacts = get_artifacts "gemini-cli" in
  let paths =
    List.map (fun a -> a.Backend_config_gen.project_relative_path) artifacts
  in
  Alcotest.(check (list string))
    "gemini-cli artifacts include only workspace settings"
    [".gemini/settings.json"]
    paths

let test_copilot_generate_all_paths () =
  let artifacts = get_artifacts "copilot-cli" in
  let paths =
    List.map (fun a -> a.Backend_config_gen.project_relative_path) artifacts
  in
  Alcotest.(check (list string))
    "copilot-cli artifacts include instructions, settings, LSP, and MCP"
    [
      ".github/copilot-instructions.md";
      ".github/copilot/settings.json";
      ".github/lsp.json";
      ".github/mcp.json";
    ]
    paths

(** {1 AC6 — Backend_project files have managed marker + hash} *)

let test_is_managed_true backend_id () =
  let a = get_artifact backend_id in
  Alcotest.(check bool)
    ("is_managed_content true for " ^ backend_id)
    true
    (Backend_config_gen.is_managed_content a.Backend_config_gen.content)

let test_is_managed_false_for_user_content () =
  Alcotest.(check bool)
    "is_managed_content false for plain user content"
    false
    (Backend_config_gen.is_managed_content
       "# My custom config\n[settings]\nkey = value\n")

let test_hash_present_in_managed backend_id () =
  let a = get_artifact backend_id in
  match Backend_config_gen.extract_hash a.Backend_config_gen.content with
  | None ->
      Alcotest.failf
        "no hash found in Backend_project content for %s"
        backend_id
  | Some h ->
      Alcotest.(check int)
        ("hash is 32 hex chars for " ^ backend_id)
        32
        (String.length h)

let test_opencode_no_legacy_epure_schema_keys () =
  let a = get_artifact "opencode" in
  let content = a.Backend_config_gen.content in
  List.iter
    (fun key ->
      Alcotest.(check bool)
        ("opencode config does not contain invalid key " ^ key)
        false
        (contains_str content key))
    ["_epure_attribution"; "_epure-managed"; "_epure-hash"] ;
  Alcotest.(check bool)
    "opencode config retains attribution in JSONC comments"
    true
    (contains_str content "Generated by Cabal"
    && contains_str content "cabal-managed"
    && contains_str content "cabal-hash")

let test_codex_does_not_force_model_provider () =
  let a = get_artifact "codex" in
  let content = a.Backend_config_gen.content in
  Alcotest.(check bool)
    "codex config does not use stale [model] table"
    false
    (contains_str content "[model]") ;
  Alcotest.(check bool)
    "codex config does not force model_provider"
    false
    (contains_str content "model_provider") ;
  Alcotest.(check bool)
    "codex config does not force provider assignment"
    false
    (contains_str content "provider =")

let epure_mcp_server () =
  Backend_types.make_mcp_server_config
    ~name:"epure"
    ~command:"epure-mcp-server"
    ~args:["--stdio"]
    ~env:[("EPURE_PROJECT", "/tmp/project")]
    ()

let lsp_assoc extension language_id : Backend_types.lsp_file_association =
  {extension; language_id}

let lsp_server ~name ~command ?(args = []) ?(file_associations = []) () :
    Backend_types.lsp_server_config =
  {name; command; args; file_associations}

let ocaml_lsp_server () =
  lsp_server
    ~name:"ocaml-lsp"
    ~command:"ocamllsp"
    ~file_associations:
      [lsp_assoc ".ml" "ocaml"; lsp_assoc ".mli" "ocaml-interface"]
    ()

let typescript_lsp_server () =
  lsp_server
    ~name:"typescript"
    ~command:"typescript-language-server"
    ~args:["--stdio"]
    ~file_associations:
      [
        lsp_assoc ".ts" "typescript";
        lsp_assoc ".tsx" "typescriptreact";
        lsp_assoc ".js" "javascript";
        lsp_assoc ".jsx" "javascriptreact";
        lsp_assoc ".mjs" "javascript";
        lsp_assoc ".cjs" "javascript";
        lsp_assoc ".mts" "typescript";
        lsp_assoc ".cts" "typescript";
      ]
    ()

let rust_lsp_server () =
  lsp_server
    ~name:"rust"
    ~command:"rust-analyzer"
    ~file_associations:[lsp_assoc ".rs" "rust"]
    ()

let go_lsp_server () =
  lsp_server
    ~name:"gopls"
    ~command:"gopls"
    ~file_associations:[lsp_assoc ".go" "go"]
    ()

let python_lsp_server () =
  lsp_server
    ~name:"python"
    ~command:"pylsp"
    ~file_associations:
      [
        lsp_assoc ".py" "python";
        lsp_assoc ".pyw" "python";
        lsp_assoc ".pyi" "python";
      ]
    ()

let all_lsp_servers () =
  [
    ocaml_lsp_server ();
    typescript_lsp_server ();
    rust_lsp_server ();
    go_lsp_server ();
    python_lsp_server ();
  ]

let custom_namespace : Backend_types.managed_namespace =
  {
    id = "crucible";
    display_name = "Crucible";
    config_dir = ".crucible/backend-config";
  }

let invalid_id_namespace : Backend_types.managed_namespace =
  {id = "../bad"; display_name = "Bad"; config_dir = ".epure/backend-config"}

let absolute_config_namespace path : Backend_types.managed_namespace =
  {id = "safe"; display_name = "Safe"; config_dir = path}

let parent_config_namespace : Backend_types.managed_namespace =
  {id = "safe"; display_name = "Safe"; config_dir = ".epure/../evil"}

let test_invalid_namespace_id_rejected_before_write () =
  with_tmpdir (fun dir ->
      let artifact =
        match
          Backend_config_gen.generate_all_with_options
            ~managed_namespace:invalid_id_namespace
            ~mcp_servers:[]
            ~backend_id:"claude-code"
            ()
        with
        | artifact :: _ -> artifact
        | [] -> Alcotest.fail "expected claude-code artifact"
      in
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false artifact
      with
      | Backend_config_gen.Invalid_managed_namespace msg ->
          Alcotest.(check bool)
            "no artifact written"
            false
            (Sys.file_exists
               (Filename.concat dir artifact.project_relative_path)) ;
          Alcotest.(check bool) "message is explicit" true (msg <> "")
      | _ -> Alcotest.fail "expected Invalid_managed_namespace")

let test_absolute_config_dir_rejected_before_setup_write () =
  with_tmpdir (fun dir ->
      let outside = Filename.concat dir "outside" in
      let namespace = absolute_config_namespace outside in
      let result =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:namespace
          ~mcp_servers:[]
          ~backend_id:"claude-code"
          ~project_dir:dir
          ~force:false
          ()
      in
      match result.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Invalid_managed_namespace _) ->
          Alcotest.(check bool)
            "absolute config_dir not written"
            false
            (Sys.file_exists
               (Filename.concat outside "claude-code/settings.json"))
      | _ -> Alcotest.fail "expected Invalid_managed_namespace")

let test_parent_config_dir_rejected_before_setup_write () =
  with_tmpdir (fun dir ->
      let result =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:parent_config_namespace
          ~mcp_servers:[]
          ~backend_id:"claude-code"
          ~project_dir:dir
          ~force:false
          ()
      in
      match result.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Invalid_managed_namespace _) ->
          Alcotest.(check bool)
            "parent config_dir not written"
            false
            (Sys.file_exists (Filename.concat dir "evil"))
      | _ -> Alcotest.fail "expected Invalid_managed_namespace")

let test_codex_config_includes_supplied_mcp_servers () =
  let artifacts = get_artifacts ~mcp_servers:[epure_mcp_server ()] "codex" in
  let config = artifact_by_path artifacts ".codex/config.toml" in
  let content = config.Backend_config_gen.content in
  Alcotest.(check bool)
    "Codex config has active approved MCP table"
    true
    (contains_str content "[mcp_servers.epure]") ;
  Alcotest.(check bool)
    "Codex config has command"
    true
    (contains_str content {|command = "epure-mcp-server"|}) ;
  Alcotest.(check bool)
    "Codex config has args"
    true
    (contains_str content {|args = ["--stdio"]|}) ;
  Alcotest.(check bool)
    "Codex config has env table"
    true
    (contains_str content "[mcp_servers.epure.env]") ;
  Alcotest.(check bool)
    "Codex config has env value"
    true
    (contains_str content {|EPURE_PROJECT = "$EPURE_PROJECT"|}) ;
  Alcotest.(check bool)
    "Codex config does not persist raw env value"
    false
    (contains_str content {|EPURE_PROJECT = "/tmp/project"|})

let json_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let test_gemini_settings_json_default_is_strict_and_empty_mcp () =
  let artifacts = get_artifacts "gemini-cli" in
  let settings = artifact_by_path artifacts ".gemini/settings.json" in
  Alcotest.(check bool)
    "strict JSON settings do not contain inline Épure metadata"
    false
    (contains_str settings.Backend_config_gen.content "cabal-managed"
    || contains_str settings.Backend_config_gen.content "epure-managed"
    || contains_str settings.Backend_config_gen.content "_epure") ;
  let json = Yojson.Safe.from_string settings.Backend_config_gen.content in
  match json_field "mcpServers" json with
  | Some (`Assoc []) -> ()
  | Some other ->
      Alcotest.failf
        "expected empty mcpServers object, got %s"
        (Yojson.Safe.to_string other)
  | None -> Alcotest.fail "expected mcpServers in Gemini settings.json"

let test_gemini_settings_json_includes_mcp_servers () =
  let artifacts =
    get_artifacts ~mcp_servers:[epure_mcp_server ()] "gemini-cli"
  in
  let settings = artifact_by_path artifacts ".gemini/settings.json" in
  let json = Yojson.Safe.from_string settings.Backend_config_gen.content in
  match json_field "mcpServers" json with
  | Some (`Assoc servers) -> (
      match List.assoc_opt "epure" servers with
      | Some (`Assoc fields) ->
          Alcotest.(check (option string))
            "Gemini MCP command"
            (Some "epure-mcp-server")
            (match List.assoc_opt "command" fields with
            | Some (`String s) -> Some s
            | _ -> None) ;
          Alcotest.(check bool)
            "Gemini MCP args"
            true
            (match List.assoc_opt "args" fields with
            | Some (`List [`String "--stdio"]) -> true
            | _ -> false) ;
          Alcotest.(check bool)
            "Gemini MCP env"
            true
            (match List.assoc_opt "env" fields with
            | Some (`Assoc env) ->
                List.mem ("EPURE_PROJECT", `String "$EPURE_PROJECT") env
            | _ -> false)
      | _ -> Alcotest.fail "expected epure MCP server in Gemini settings")
  | _ -> Alcotest.fail "expected Gemini mcpServers object"

let test_copilot_settings_json_is_strict_minimal () =
  let artifacts = get_artifacts "copilot-cli" in
  let settings = artifact_by_path artifacts ".github/copilot/settings.json" in
  Alcotest.(check bool)
    "Copilot settings is strict JSON without Épure metadata"
    false
    (contains_str settings.Backend_config_gen.content "cabal-managed"
    || contains_str settings.Backend_config_gen.content "epure-managed"
    || contains_str settings.Backend_config_gen.content "_epure") ;
  Alcotest.(check bool)
    "Copilot settings parses as object"
    true
    (match Yojson.Safe.from_string settings.Backend_config_gen.content with
    | `Assoc _ -> true
    | _ -> false)

let test_copilot_project_mcp_json_default_empty () =
  let artifacts = get_artifacts "copilot-cli" in
  let mcp = artifact_by_path artifacts ".github/mcp.json" in
  let json = Yojson.Safe.from_string mcp.Backend_config_gen.content in
  Alcotest.(check (option bool))
    "legacy servers key absent"
    None
    (Option.map (fun _ -> true) (json_field "servers" json)) ;
  match json_field "mcpServers" json with
  | Some (`Assoc []) -> ()
  | Some other ->
      Alcotest.failf
        "expected empty mcpServers object, got %s"
        (Yojson.Safe.to_string other)
  | None -> Alcotest.fail "expected mcpServers in .github/mcp.json"

let test_copilot_project_mcp_json_includes_servers () =
  let artifacts =
    get_artifacts ~mcp_servers:[epure_mcp_server ()] "copilot-cli"
  in
  let mcp = artifact_by_path artifacts ".github/mcp.json" in
  let json = Yojson.Safe.from_string mcp.Backend_config_gen.content in
  match json_field "mcpServers" json with
  | Some (`Assoc servers) -> (
      match List.assoc_opt "epure" servers with
      | Some (`Assoc fields) ->
          Alcotest.(check (option string))
            "Copilot MCP type"
            (Some "local")
            (match List.assoc_opt "type" fields with
            | Some (`String s) -> Some s
            | _ -> None) ;
          Alcotest.(check (option string))
            "Copilot MCP command"
            (Some "epure-mcp-server")
            (match List.assoc_opt "command" fields with
            | Some (`String s) -> Some s
            | _ -> None) ;
          Alcotest.(check bool)
            "Copilot MCP args"
            true
            (match List.assoc_opt "args" fields with
            | Some (`List [`String "--stdio"]) -> true
            | _ -> false) ;
          Alcotest.(check bool)
            "Copilot MCP env"
            true
            (match List.assoc_opt "env" fields with
            | Some (`Assoc env) ->
                List.mem ("EPURE_PROJECT", `String "$EPURE_PROJECT") env
            | _ -> false) ;
          Alcotest.(check bool)
            "Copilot MCP tools default to wildcard"
            true
            (match List.assoc_opt "tools" fields with
            | Some (`List [`String "*"]) -> true
            | _ -> false)
      | _ -> Alcotest.fail "expected epure MCP server in .github/mcp.json")
  | _ -> Alcotest.fail "expected Copilot mcpServers object"

let test_copilot_lsp_json_default_empty () =
  let artifacts = get_artifacts "copilot-cli" in
  let lsp = artifact_by_path artifacts ".github/lsp.json" in
  Alcotest.(check bool)
    "Copilot LSP config is strict JSON without inline metadata"
    false
    (contains_str lsp.Backend_config_gen.content "cabal-managed"
    || contains_str lsp.Backend_config_gen.content "epure-managed"
    || contains_str lsp.Backend_config_gen.content "_epure") ;
  let json = Yojson.Safe.from_string lsp.Backend_config_gen.content in
  match json_field "lspServers" json with
  | Some (`Assoc []) -> ()
  | Some other ->
      Alcotest.failf
        "expected empty lspServers object, got %s"
        (Yojson.Safe.to_string other)
  | None -> Alcotest.fail "expected lspServers in .github/lsp.json"

let test_copilot_lsp_json_includes_detected_servers () =
  let artifacts =
    Copilot_cli.project_config_artifacts
      ~managed_namespace:Backend_types.default_managed_namespace
      ~mcp_servers:[]
      ~lsp_servers:(all_lsp_servers ())
  in
  let lsp = artifact_by_path artifacts ".github/lsp.json" in
  let json = Yojson.Safe.from_string lsp.Backend_config_gen.content in
  match json_field "lspServers" json with
  | Some (`Assoc servers) -> (
      List.iter
        (fun name ->
          Alcotest.(check bool)
            ("Copilot LSP server present: " ^ name)
            true
            (List.mem_assoc name servers))
        ["ocaml-lsp"; "typescript"; "rust"; "gopls"; "python"] ;
      match List.assoc_opt "ocaml-lsp" servers with
      | Some (`Assoc fields) -> (
          match List.assoc_opt "fileExtensions" fields with
          | Some (`Assoc exts) ->
              Alcotest.(check bool)
                "Copilot OCaml interface language id"
                true
                (List.mem (".mli", `String "ocaml-interface") exts)
          | _ -> Alcotest.fail "expected OCaml fileExtensions")
      | _ -> Alcotest.fail "expected ocaml-lsp server")
  | _ -> Alcotest.fail "expected Copilot lspServers object"

(** {1 AC6 write behavior — filesystem tests} *)

let test_write_creates_file () =
  with_tmpdir (fun dir ->
      let a = get_artifact "codex" in
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Written path ->
          Alcotest.(check bool)
            "written file exists"
            true
            (Sys.file_exists path)
      | r ->
          Alcotest.failf
            "expected Written but got %s"
            (match r with
            | Backend_config_gen.Already_current -> "Already_current"
            | Backend_config_gen.Refused_hash_mismatch s -> "Refused:" ^ s
            | Backend_config_gen.Backed_up_and_written _ -> "Backed_up"
            | Backend_config_gen.Skipped_user_content s -> "Skipped:" ^ s
            | Backend_config_gen.Written _ -> "Written"
            | Backend_config_gen.Invalid_managed_namespace s ->
                "Invalid namespace:" ^ s))

let test_write_idempotent () =
  with_tmpdir (fun dir ->
      let a = get_artifact "codex" in
      let _ =
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      in
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Already_current -> ()
      | _ -> Alcotest.fail "expected Already_current on second write")

let test_write_epure_owned_creates_file () =
  with_tmpdir (fun dir ->
      let a = get_artifact "claude-code" in
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Written path ->
          Alcotest.(check bool)
            "epure-owned file created"
            true
            (Sys.file_exists path)
      | _ -> Alcotest.fail "expected Written for epure-owned artifact")

let test_write_epure_owned_idempotent () =
  with_tmpdir (fun dir ->
      let a = get_artifact "claude-code" in
      let _ =
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      in
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Already_current -> ()
      | _ ->
          Alcotest.fail "expected Already_current for epure-owned second write")

let test_write_epure_owned_force_same_as_no_force () =
  with_tmpdir (fun dir ->
      let a = get_artifact "claude-code" in
      let _ =
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      in
      (* force:true must be identical to force:false for Epure_owned — force
         only applies to Backend_project hash-mismatch handling. *)
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:true a
      with
      | Backend_config_gen.Already_current -> ()
      | _ ->
          Alcotest.fail
            "expected Already_current for epure-owned second write with \
             force:true")

let test_write_user_content_skipped () =
  with_tmpdir (fun dir ->
      let a = get_artifact "codex" in
      let full_path =
        Filename.concat dir a.Backend_config_gen.project_relative_path
      in
      let parent = Filename.dirname full_path in
      if not (Sys.file_exists parent) then Unix.mkdir parent 0o755 ;
      write_file full_path "# User-created config\n[settings]\nkey = value\n" ;
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Skipped_user_content _ -> ()
      | _ ->
          Alcotest.fail "expected Skipped_user_content for user-authored file")

let test_write_hash_mismatch_refused () =
  with_tmpdir (fun dir ->
      let a = get_artifact "codex" in
      let full_path =
        Filename.concat dir a.Backend_config_gen.project_relative_path
      in
      let parent = Filename.dirname full_path in
      if not (Sys.file_exists parent) then Unix.mkdir parent 0o755 ;
      (* Write managed content with appended user data → hash mismatch *)
      write_file full_path (a.Backend_config_gen.content ^ "# user-added line\n") ;
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Refused_hash_mismatch _ -> ()
      | _ ->
          Alcotest.fail
            "expected Refused_hash_mismatch for modified managed file")

let test_write_hash_mismatch_force_backup () =
  with_tmpdir (fun dir ->
      let a = get_artifact "codex" in
      let full_path =
        Filename.concat dir a.Backend_config_gen.project_relative_path
      in
      let parent = Filename.dirname full_path in
      if not (Sys.file_exists parent) then Unix.mkdir parent 0o755 ;
      write_file full_path (a.Backend_config_gen.content ^ "# user-added line\n") ;
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:true a
      with
      | Backend_config_gen.Backed_up_and_written {path; backup_path} ->
          Alcotest.(check bool)
            "overwritten path exists"
            true
            (Sys.file_exists path) ;
          Alcotest.(check bool)
            "backup file created"
            true
            (Sys.file_exists backup_path)
      | _ -> Alcotest.fail "expected Backed_up_and_written with --force")

let test_write_opencode_rewrites_legacy_metadata_keys () =
  with_tmpdir (fun dir ->
      let a = get_artifact "opencode" in
      let hash =
        match Backend_config_gen.extract_hash a.Backend_config_gen.content with
        | Some h -> h
        | None -> Alcotest.fail "opencode artifact missing hash"
      in
      let full_path =
        Filename.concat dir a.Backend_config_gen.project_relative_path
      in
      write_file
        full_path
        (Printf.sprintf
           "{\n\
           \  \"_epure_attribution\": \"Generated by Epure — do not edit \
            manually\",\n\
           \  \"_epure-managed\": true,\n\
           \  \"_epure-hash\": \"%s\"\n\
            }\n"
           hash) ;
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Written path ->
          let content = read_file path in
          List.iter
            (fun key ->
              Alcotest.(check bool)
                ("legacy metadata key removed: " ^ key)
                false
                (contains_str content key))
            ["_epure_attribution"; "_epure-managed"; "_epure-hash"] ;
          Alcotest.(check bool)
            "JSONC managed header retained on rewrite"
            true
            (contains_str content "cabal-managed"
            && contains_str content "cabal-hash")
      | other ->
          Alcotest.failf
            "expected legacy opencode metadata rewrite, got %s"
            (match other with
            | Backend_config_gen.Already_current -> "Already_current"
            | Backend_config_gen.Refused_hash_mismatch s -> "Refused:" ^ s
            | Backend_config_gen.Backed_up_and_written _ -> "Backed_up"
            | Backend_config_gen.Skipped_user_content s -> "Skipped:" ^ s
            | Backend_config_gen.Written _ -> "Written"
            | Backend_config_gen.Invalid_managed_namespace s ->
                "Invalid namespace:" ^ s))

let test_setup_opencode_migrates_legacy_metadata_preserving_mcp () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let body_without_mcp =
        "{\n\
        \  \"_epure_custom\": \"keep-me\",\n\
        \  \"model\": \"anthropic/claude-sonnet-4.5\"\n\
         }"
      in
      let hash = Digest.to_hex (Digest.string body_without_mcp) in
      write_file
        path
        (Printf.sprintf
           {|{
  "_epure_attribution": "Generated by Epure — do not edit manually",
  "_epure-managed": true,
  "_epure-hash": "%s",
  "_epure_custom": "keep-me",
  "mcp": {
    "user-server": {
      "type": "local",
      "command": ["user-mcp"],
      "enabled": true,
      "environment": {}
    }
  },
  "model": "anthropic/claude-sonnet-4.5"
}
|}
           hash) ;
      let result =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      (match result.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Written _) -> ()
      | Some other ->
          Alcotest.failf
            "expected legacy opencode migration write, got %s"
            (match other with
            | Backend_config_gen.Already_current -> "Already_current"
            | Backend_config_gen.Refused_hash_mismatch s -> "Refused:" ^ s
            | Backend_config_gen.Backed_up_and_written _ -> "Backed_up"
            | Backend_config_gen.Skipped_user_content s -> "Skipped:" ^ s
            | Backend_config_gen.Written _ -> "Written"
            | Backend_config_gen.Invalid_managed_namespace s ->
                "Invalid namespace:" ^ s)
      | None -> Alcotest.fail "expected write outcome for opencode") ;
      let content = read_file path in
      List.iter
        (fun key ->
          Alcotest.(check bool)
            ("legacy metadata key removed: " ^ key)
            false
            (contains_str content key))
        ["_epure_attribution"; "_epure-managed"; "_epure-hash"] ;
      Alcotest.(check bool)
        "non-legacy _epure-like user key preserved"
        true
        (contains_str content "_epure_custom" && contains_str content "keep-me") ;
      Alcotest.(check bool)
        "managed JSONC header added"
        true
        (contains_str content "Generated by Cabal"
        && contains_str content "cabal-managed"
        && contains_str content "cabal-hash") ;
      Alcotest.(check bool)
        "existing user MCP preserved"
        true
        (contains_str content "user-server" && contains_str content "user-mcp") ;
      Alcotest.(check bool)
        "existing user model preserved"
        true
        (contains_str content "anthropic/claude-sonnet-4.5") ;
      let result2 =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      match result2.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "second setup refused migrated file: %s" msg
      | Some _ -> Alcotest.fail "expected second setup to be idempotent"
      | None -> Alcotest.fail "expected second setup write outcome")

let test_setup_opencode_migration_keeps_0600_mode () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let body = "{\n}" in
      let hash = Digest.to_hex (Digest.string body) in
      write_file
        path
        (Printf.sprintf
           {|{
  "_epure_attribution": "Generated by Epure — do not edit manually",
  "_epure-managed": true,
  "_epure-hash": "%s"
}
|}
           hash) ;
      Unix.chmod path 0o600 ;
      with_umask 0o022 (fun () ->
          ignore
            (Backend_config_gen.setup_project_config
               ~mcp_servers:[]
               ~backend_id:"opencode"
               ~project_dir:dir
               ~force:false)) ;
      Alcotest.(check int)
        "OpenCode migration must not widen 0600 permissions"
        0o600
        (file_perm path))

(** {1 AC4 — user content at Gemini's GEMINI.md path is preserved} *)

let test_setup_gemini_user_gemini_md_preserved () =
  with_tmpdir (fun dir ->
      let full_path = Filename.concat dir "GEMINI.md" in
      write_file
        full_path
        "# My Custom Gemini Instructions\n\nDo something specific.\n" ;
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"gemini-cli"
           ~project_dir:dir
           ~force:false) ;
      Alcotest.(check string)
        "user-authored GEMINI.md unchanged"
        "# My Custom Gemini Instructions\n\nDo something specific.\n"
        (read_file full_path))

(** {1 SEC-1 — no secret keys in generated content} *)

let test_no_secret_keys () =
  let secret_patterns =
    [
      "api_key";
      "apikey";
      "password";
      "credential";
      "private_key";
      "access_token";
      "bearer_token";
    ]
  in
  List.iter
    (fun backend_id ->
      match Backend_config_gen.generate ~backend_id with
      | None -> ()
      | Some a ->
          let lower = String.lowercase_ascii a.Backend_config_gen.content in
          List.iter
            (fun pattern ->
              Alcotest.(check bool)
                (Printf.sprintf
                   "backend %s: no '%s' in content"
                   backend_id
                   pattern)
                false
                (contains_str lower pattern))
            secret_patterns)
    ["claude-code"; "codex"; "opencode"; "gemini-cli"; "copilot-cli"]

(** {1 AC2 write idempotence for opencode and gemini-cli} *)

let test_write_idempotent_opencode () =
  with_tmpdir (fun dir ->
      let a = get_artifact "opencode" in
      let _ =
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      in
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Already_current -> ()
      | _ ->
          Alcotest.fail "expected Already_current on second write for opencode")

let test_write_idempotent_gemini () =
  with_tmpdir (fun dir ->
      let a = get_artifact "gemini-cli" in
      let _ =
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      in
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Already_current -> ()
      | _ ->
          Alcotest.fail
            "expected Already_current on second write for gemini-cli")

(** {1 AC5/AC1 — setup_project_config wires generate + write_artifact} *)

let test_setup_project_config_claude_code_returns_path () =
  with_tmpdir (fun dir ->
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"claude-code"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.project_config_path with
      | None ->
          Alcotest.fail
            "expected Some path for claude-code (Config_explicit_flag)"
      | Some path ->
          Alcotest.(check bool)
            "returned path is under project dir"
            true
            (contains_str path dir))

let test_setup_project_config_claude_code_writes_file () =
  with_tmpdir (fun dir ->
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"claude-code"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.project_config_path with
      | None -> Alcotest.fail "expected Some path for claude-code"
      | Some path ->
          Alcotest.(check bool)
            "config file exists on disk"
            true
            (Sys.file_exists path))

let test_setup_project_config_codex_writes_config () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"codex"
           ~project_dir:dir
           ~force:false) ;
      let expected = Filename.concat dir ".codex/config.toml" in
      Alcotest.(check bool)
        "codex config.toml written"
        true
        (Sys.file_exists expected))

let test_setup_project_config_opencode_writes_config () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"opencode"
           ~project_dir:dir
           ~force:false) ;
      let expected = Filename.concat dir "opencode.json" in
      Alcotest.(check bool)
        "opencode.json written"
        true
        (Sys.file_exists expected))

let test_setup_project_config_gemini_does_not_write_gemini_md () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"gemini-cli"
           ~project_dir:dir
           ~force:false) ;
      let expected = Filename.concat dir "GEMINI.md" in
      Alcotest.(check bool)
        "GEMINI.md not generated by default"
        false
        (Sys.file_exists expected))

let test_setup_project_config_gemini_writes_settings_json () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"gemini-cli"
           ~project_dir:dir
           ~force:false) ;
      let expected = Filename.concat dir ".gemini/settings.json" in
      Alcotest.(check bool)
        ".gemini/settings.json written"
        true
        (Sys.file_exists expected) ;
      Alcotest.(check bool)
        ".gemini/settings.json sidecar written"
        true
        (Sys.file_exists (expected ^ ".cabal-meta.json")) ;
      let json = Yojson.Safe.from_string (read_file expected) in
      match json_field "mcpServers" json with
      | Some (`Assoc []) -> ()
      | _ -> Alcotest.fail "expected default empty mcpServers")

let test_setup_project_config_gemini_writes_mcp_servers_to_settings () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~backend_id:"gemini-cli"
           ~project_dir:dir
           ~force:false
           ~mcp_servers:[epure_mcp_server ()]) ;
      let expected = Filename.concat dir ".gemini/settings.json" in
      let json = Yojson.Safe.from_string (read_file expected) in
      match json_field "mcpServers" json with
      | Some (`Assoc servers) when List.mem_assoc "epure" servers -> ()
      | _ -> Alcotest.fail "expected epure MCP server in Gemini settings.json")

let test_setup_project_config_copilot_writes_config () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"copilot-cli"
           ~project_dir:dir
           ~force:false) ;
      let expected = Filename.concat dir ".github/copilot-instructions.md" in
      Alcotest.(check bool)
        "copilot-cli: .github/copilot-instructions.md written"
        true
        (Sys.file_exists expected))

let test_setup_project_config_copilot_writes_settings_and_mcp () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"copilot-cli"
           ~project_dir:dir
           ~force:false) ;
      List.iter
        (fun rel ->
          Alcotest.(check bool)
            (rel ^ " written")
            true
            (Sys.file_exists (Filename.concat dir rel)))
        [".github/copilot/settings.json"; ".github/mcp.json"] ;
      Alcotest.(check bool)
        ".github/lsp.json written"
        true
        (Sys.file_exists (Filename.concat dir ".github/lsp.json")) ;
      Alcotest.(check bool)
        "does not write user-global ~/.copilot/config.json"
        false
        (Sys.file_exists (Filename.concat dir ".copilot/config.json")))

let test_setup_project_config_copilot_writes_detected_lsp_servers () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config_with_options
           ~mcp_servers:[]
           ~lsp_servers:[ocaml_lsp_server (); typescript_lsp_server ()]
           ~backend_id:"copilot-cli"
           ~project_dir:dir
           ~force:false
           ()) ;
      let lsp_path = Filename.concat dir ".github/lsp.json" in
      let json = Yojson.Safe.from_string (read_file lsp_path) in
      match json_field "lspServers" json with
      | Some (`Assoc servers) ->
          Alcotest.(check bool)
            "detected OCaml LSP is written"
            true
            (List.mem_assoc "ocaml-lsp" servers) ;
          Alcotest.(check bool)
            "detected TypeScript LSP is written"
            true
            (List.mem_assoc "typescript" servers)
      | _ -> Alcotest.fail "expected lspServers object")

let test_setup_project_config_copilot_writes_project_mcp_servers () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~backend_id:"copilot-cli"
           ~project_dir:dir
           ~force:false
           ~mcp_servers:[epure_mcp_server ()]) ;
      let path = Filename.concat dir ".github/mcp.json" in
      let json = Yojson.Safe.from_string (read_file path) in
      match json_field "mcpServers" json with
      | Some (`Assoc servers) when List.mem_assoc "epure" servers -> ()
      | _ -> Alcotest.fail "expected epure MCP server in .github/mcp.json")

let test_sidecar_managed_json_preserves_user_settings () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir ".gemini/settings.json" in
      let parent = Filename.dirname path in
      if not (Sys.file_exists (Filename.dirname parent)) then
        Unix.mkdir (Filename.dirname parent) 0o755 ;
      if not (Sys.file_exists parent) then Unix.mkdir parent 0o755 ;
      let user_content = {|{"mcpServers":{"user":{"command":"user-mcp"}}}|} in
      write_file path user_content ;
      ignore
        (Backend_config_gen.setup_project_config
           ~backend_id:"gemini-cli"
           ~project_dir:dir
           ~force:false
           ~mcp_servers:[epure_mcp_server ()]) ;
      Alcotest.(check string)
        "unmanaged strict JSON settings preserved"
        user_content
        (read_file path))

let test_setup_project_config_idempotent () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"codex"
           ~project_dir:dir
           ~force:false) ;
      (* Second call: file already current; must not fail *)
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"codex"
           ~project_dir:dir
           ~force:false) ;
      let expected = Filename.concat dir ".codex/config.toml" in
      Alcotest.(check bool)
        "file still present after idempotent call"
        true
        (Sys.file_exists expected))

(** {1 AC5 — claude-code build_command includes project settings path} *)

let test_build_command_includes_config_path () =
  let spec =
    Backend_types.make_task_spec ~prompt:"test" ~working_dir:"/tmp" ()
  in
  let config_path = "/tmp/epure-test/settings.json" in
  let cmd, _ =
    Claude_code.build_command
      ~project_config_path:(Some config_path)
      ~mcp_config_path:None
      spec
  in
  Alcotest.(check bool)
    "command contains --settings flag"
    true
    (List.mem "--settings" cmd) ;
  Alcotest.(check bool)
    "command contains the config path"
    true
    (List.mem config_path cmd)

let test_build_command_no_config_when_none () =
  let spec =
    Backend_types.make_task_spec ~prompt:"test" ~working_dir:"/tmp" ()
  in
  let cmd, _ =
    Claude_code.build_command
      ~project_config_path:None
      ~mcp_config_path:None
      spec
  in
  Alcotest.(check bool)
    "command does not contain --settings when path is None"
    false
    (List.mem "--settings" cmd)

(** {1 AC2 combined — opencode: setup then mcp injection then setup again} *)

(* Simulate what ensure_mcp_in_opencode_json does: read the file, inject a
   schema-valid "mcp" key with an entry, and preserve the JSONC managed header
   comments. *)
let inject_mcp_into_opencode path =
  let content =
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n ;
    close_in ic ;
    Bytes.to_string s
  in
  let hash =
    match Backend_config_gen.extract_hash content with
    | Some h -> h
    | None -> Alcotest.fail "opencode fixture has no managed hash"
  in
  let modified =
    Printf.sprintf
      {|// Generated by Epure — do not edit manually
// epure-managed
// epure-hash: %s
{
  "mcp": {
    "epure": {"type": "local", "command": ["epure", "mcp"], "enabled": true, "environment": {}}
  }
}
|}
      hash
  in
  write_file path modified

let test_opencode_mcp_injection_no_hash_mismatch () =
  with_tmpdir (fun dir ->
      let a = get_artifact "opencode" in
      (* First write via write_artifact *)
      ignore (Backend_config_gen.write_artifact ~project_dir:dir ~force:false a) ;
      let path = Filename.concat dir "opencode.json" in
      (* Simulate ensure_mcp_in_opencode_json injecting the mcp key *)
      inject_mcp_into_opencode path ;
      (* Second write must not return Refused_hash_mismatch *)
      (match
         Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
       with
      | Backend_config_gen.Refused_hash_mismatch msg ->
          Alcotest.failf
            "hash mismatch after mcp injection (bug: opencode idempotency \
             broken): %s"
            msg
      | Backend_config_gen.Already_current | Backend_config_gen.Written _ -> ()
      | Backend_config_gen.Skipped_user_content _ ->
          Alcotest.fail "unexpected Skipped: file should still be managed"
      | Backend_config_gen.Backed_up_and_written _ ->
          Alcotest.fail "unexpected Backed_up: force was false"
      | Backend_config_gen.Invalid_managed_namespace msg ->
          Alcotest.failf "unexpected invalid namespace: %s" msg) ;
      (* Third write (idempotency after second) must also be clean *)
      inject_mcp_into_opencode path ;
      match
        Backend_config_gen.write_artifact ~project_dir:dir ~force:false a
      with
      | Backend_config_gen.Refused_hash_mismatch msg ->
          Alcotest.failf "hash mismatch on third write: %s" msg
      | _ -> ())

let test_opencode_mcp_injection_preserves_lsp_defs () =
  with_tmpdir (fun dir ->
      let artifact =
        match
          Opencode_cli.project_config_artifacts
            ~managed_namespace:Backend_types.default_managed_namespace
            ~mcp_servers:[]
            ~lsp_servers:[ocaml_lsp_server ()]
        with
        | [artifact] -> artifact
        | _ -> Alcotest.fail "expected one OpenCode artifact"
      in
      ignore
        (Backend_config_gen.write_artifact
           ~project_dir:dir
           ~force:false
           artifact) ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json
        ~env
        (Backend_types.make_task_spec
           ~prompt:"test"
           ~working_dir:dir
           ~mcp_servers:[epure_mcp_server ()]
           ()) ;
      let content = read_file (Filename.concat dir "opencode.json") in
      Alcotest.(check bool)
        "OpenCode MCP merge preserves LSP command"
        true
        (contains_str content "ocamllsp") ;
      Alcotest.(check bool)
        "OpenCode MCP merge adds MCP server"
        true
        (contains_str content {|"epure"|}))

let test_custom_namespace_claude_config_path_and_markers () =
  let artifacts =
    get_artifacts
      ~managed_namespace:custom_namespace
      ~lsp_servers:[ocaml_lsp_server ()]
      "claude-code"
  in
  let artifact =
    artifact_by_path
      artifacts
      ".crucible/backend-config/claude-code/settings.json"
  in
  Alcotest.(check bool)
    "custom namespace attribution"
    true
    (contains_str artifact.Backend_config_gen.content "Generated by Crucible") ;
  Alcotest.(check bool)
    "custom managed marker"
    true
    (contains_str artifact.Backend_config_gen.content "crucible-managed")

let test_custom_namespace_sidecar_suffix () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config_with_options
           ~mcp_servers:[]
           ~managed_namespace:custom_namespace
           ~backend_id:"gemini-cli"
           ~project_dir:dir
           ~force:false
           ()) ;
      let expected = Filename.concat dir ".gemini/settings.json" in
      Alcotest.(check bool)
        "custom namespace sidecar written"
        true
        (Sys.file_exists (expected ^ ".crucible-meta.json")))

(** {1 AC2/AC3/AC4 — precedence_warning_for} *)

(* Claude Code has High precedence confidence: no warning regardless of write_outcome. *)
let test_no_warning_claude_code () =
  Alcotest.(check (option string))
    "claude-code: no precedence warning (High)"
    None
    (Backend_config_gen.precedence_warning_for
       ~backend_id:"claude-code"
       ~write_outcome:(Some Backend_config_gen.Already_current))

(* Codex and OpenCode have Medium confidence: warning emitted when config applied. *)
let test_warning_codex () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"codex"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None -> Alcotest.fail "expected warning for codex (Medium confidence)"
  | Some msg ->
      Alcotest.(check bool)
        "codex warning mentions partial/medium precedence"
        true
        (contains_str (String.lowercase_ascii msg) "partial"
        || contains_str (String.lowercase_ascii msg) "override"
        || contains_str (String.lowercase_ascii msg) "global")

let test_warning_opencode () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"opencode"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None -> Alcotest.fail "expected warning for opencode (Medium confidence)"
  | Some msg ->
      Alcotest.(check bool)
        "opencode warning is non-empty"
        true
        (String.length msg > 0)

(* Gemini CLI has Low confidence: stronger documented limitation warning. *)
let test_warning_gemini_cli () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"gemini-cli"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None -> Alcotest.fail "expected warning for gemini-cli (Low confidence)"
  | Some msg ->
      Alcotest.(check bool)
        "gemini-cli warning mentions limited precedence or user settings"
        true
        (contains_str (String.lowercase_ascii msg) "limited"
        || contains_str (String.lowercase_ascii msg) "user"
        || contains_str (String.lowercase_ascii msg) "global")

(* Copilot CLI has Low confidence + Config_fixed_path: warning about limited
   precedence controls when write_outcome is None (write not attempted). *)
let test_warning_copilot_cli () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"copilot-cli"
      ~write_outcome:None
  with
  | None -> Alcotest.fail "expected warning for copilot-cli (Low confidence)"
  | Some msg ->
      Alcotest.(check bool)
        "copilot-cli warning mentions config or user settings"
        true
        (contains_str (String.lowercase_ascii msg) "no project"
        || contains_str (String.lowercase_ascii msg) "config"
        || contains_str (String.lowercase_ascii msg) "user")

(* All warnings must mention the backend name for actionability. *)
let test_warning_mentions_backend_name backend_id display_name write_outcome ()
    =
  match
    Backend_config_gen.precedence_warning_for ~backend_id ~write_outcome
  with
  | None ->
      Alcotest.failf
        "expected warning for %s — needed for actionability"
        backend_id
  | Some msg ->
      Alcotest.(check bool)
        (Printf.sprintf "%s: warning mentions backend name" backend_id)
        true
        (contains_str msg display_name)

(* Unknown backends return None gracefully. *)
let test_no_warning_unknown () =
  Alcotest.(check (option string))
    "unknown backend: no warning"
    None
    (Backend_config_gen.precedence_warning_for
       ~backend_id:"no-such-backend"
       ~write_outcome:None)

(** {1 NEW — setup_project_config returns write_outcome for Backend_project} *)

let test_setup_returns_write_outcome_written () =
  with_tmpdir (fun dir ->
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"codex"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Written _) -> ()
      | _ ->
          Alcotest.fail
            "expected write_outcome = Some (Written _) on first codex setup")

let test_setup_returns_write_outcome_already_current () =
  with_tmpdir (fun dir ->
      let _ =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"codex"
          ~project_dir:dir
          ~force:false
      in
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"codex"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | _ ->
          Alcotest.fail
            "expected write_outcome = Some Already_current on second codex \
             setup")

let test_setup_returns_written_outcome_for_copilot () =
  with_tmpdir (fun dir ->
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"copilot-cli"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Written _) -> ()
      | None ->
          Alcotest.fail
            "expected Some write_outcome for copilot-cli (Config_fixed_path)"
      | Some other ->
          Alcotest.failf
            "expected Written but got %s"
            (match other with
            | Backend_config_gen.Already_current -> "Already_current"
            | Backend_config_gen.Refused_hash_mismatch s -> "Refused:" ^ s
            | Backend_config_gen.Backed_up_and_written _ -> "Backed_up"
            | Backend_config_gen.Skipped_user_content s -> "Skipped:" ^ s
            | Backend_config_gen.Written _ -> "Written"
            | Backend_config_gen.Invalid_managed_namespace s ->
                "Invalid namespace:" ^ s))

let test_setup_returns_refused_outcome_on_hash_mismatch () =
  with_tmpdir (fun dir ->
      let a = get_artifact "codex" in
      let full_path =
        Filename.concat dir a.Backend_config_gen.project_relative_path
      in
      let parent = Filename.dirname full_path in
      if not (Sys.file_exists parent) then Unix.mkdir parent 0o755 ;
      write_file full_path (a.Backend_config_gen.content ^ "# user-added line\n") ;
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"codex"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Refused_hash_mismatch _) -> ()
      | _ ->
          Alcotest.fail
            "expected write_outcome = Some (Refused_hash_mismatch _) on hash \
             mismatch")

let test_setup_returns_skipped_outcome_on_user_content () =
  with_tmpdir (fun dir ->
      let full_path = Filename.concat dir ".gemini/settings.json" in
      write_file_creating_dirs
        full_path
        {|{"mcpServers":{"user":{"command":"user-mcp"}}}|} ;
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"gemini-cli"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Skipped_user_content _) -> ()
      | _ ->
          Alcotest.fail
            "expected write_outcome = Some (Skipped_user_content _) for user \
             .gemini/settings.json")

let setup_outcome_for_path r rel =
  match
    List.find_opt
      (fun outcome ->
        outcome.Backend_config_gen.artifact.project_relative_path = rel)
      r.Backend_config_gen.write_outcomes
  with
  | Some outcome -> outcome
  | None -> Alcotest.failf "expected per-artifact outcome for %s" rel

let test_setup_reports_secondary_gemini_settings_skip () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir ".gemini/settings.json" in
      let original = {|{"mcpServers":{"user":{"command":"user-mcp"}}}|} in
      write_file_creating_dirs path original ;
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:"gemini-cli"
          ~project_dir:dir
          ~force:false
      in
      Alcotest.(check int)
        "gemini setup reports settings outcome"
        1
        (List.length r.Backend_config_gen.write_outcomes) ;
      let outcome = setup_outcome_for_path r ".gemini/settings.json" in
      match outcome.Backend_config_gen.result with
      | Backend_config_gen.Skipped_user_content skipped_path ->
          Alcotest.(check string) "skipped path" path skipped_path ;
          Alcotest.(check string)
            "user settings unchanged"
            original
            (read_file path)
      | _ -> Alcotest.fail "expected .gemini/settings.json to be skipped")

let test_setup_reports_secondary_gemini_settings_refusal () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"gemini-cli"
           ~project_dir:dir
           ~force:false) ;
      let path = Filename.concat dir ".gemini/settings.json" in
      let modified = {|{"mcpServers":{"user":{"command":"modified"}}}|} in
      write_file path modified ;
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:"gemini-cli"
          ~project_dir:dir
          ~force:false
      in
      let outcome = setup_outcome_for_path r ".gemini/settings.json" in
      match outcome.Backend_config_gen.result with
      | Backend_config_gen.Refused_hash_mismatch _ ->
          Alcotest.(check string)
            "hash-mismatched settings unchanged"
            modified
            (read_file path)
      | _ -> Alcotest.fail "expected .gemini/settings.json hash refusal")

let test_setup_reports_secondary_copilot_mcp_skip () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir ".github/mcp.json" in
      let original = {|{"mcpServers":{"user":{"command":"user-mcp"}}}|} in
      write_file_creating_dirs path original ;
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:"copilot-cli"
          ~project_dir:dir
          ~force:false
      in
      Alcotest.(check int)
        "copilot setup reports instructions, settings, LSP, and MCP outcomes"
        4
        (List.length r.Backend_config_gen.write_outcomes) ;
      let outcome = setup_outcome_for_path r ".github/mcp.json" in
      match outcome.Backend_config_gen.result with
      | Backend_config_gen.Skipped_user_content skipped_path ->
          Alcotest.(check string) "skipped path" path skipped_path ;
          Alcotest.(check string)
            "user MCP config unchanged"
            original
            (read_file path)
      | _ -> Alcotest.fail "expected .github/mcp.json to be skipped")

let test_setup_reports_secondary_copilot_mcp_refusal () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"copilot-cli"
           ~project_dir:dir
           ~force:false) ;
      let path = Filename.concat dir ".github/mcp.json" in
      let modified = {|{"mcpServers":{"user":{"command":"modified"}}}|} in
      write_file path modified ;
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:"copilot-cli"
          ~project_dir:dir
          ~force:false
      in
      let outcome = setup_outcome_for_path r ".github/mcp.json" in
      match outcome.Backend_config_gen.result with
      | Backend_config_gen.Refused_hash_mismatch _ ->
          Alcotest.(check string)
            "hash-mismatched MCP config unchanged"
            modified
            (read_file path)
      | _ -> Alcotest.fail "expected .github/mcp.json hash refusal")

(** {1 NEW — precedence_warning_for adapts to write_outcome} *)

(* When config was applied (Written/Already_current), Medium warning must NOT
   claim "could not be applied". *)
let test_warning_applied_does_not_say_could_not_apply () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"codex"
      ~write_outcome:(Some (Backend_config_gen.Written "/tmp/x"))
  with
  | None -> Alcotest.fail "expected warning for codex when config applied"
  | Some msg ->
      Alcotest.(check bool)
        "applied-config warning does not say 'could not be applied'"
        false
        (contains_str (String.lowercase_ascii msg) "could not be applied")

(* When write was Refused_hash_mismatch, the warning must NOT claim the config
   was written ("has been written"). *)
let test_warning_refused_does_not_claim_written () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"codex"
      ~write_outcome:
        (Some (Backend_config_gen.Refused_hash_mismatch "hash mismatch"))
  with
  | None -> Alcotest.fail "expected warning for codex when write refused"
  | Some msg ->
      Alcotest.(check bool)
        "refused warning does not say 'has been written'"
        false
        (contains_str (String.lowercase_ascii msg) "has been written")

(* When write was Skipped_user_content, the warning must NOT claim the config
   was written. *)
let test_warning_skipped_does_not_claim_written () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"gemini-cli"
      ~write_outcome:
        (Some
           (Backend_config_gen.Skipped_user_content
              "/path/.gemini/settings.json"))
  with
  | None -> Alcotest.fail "expected warning for gemini-cli when write skipped"
  | Some msg ->
      Alcotest.(check bool)
        "skipped warning does not say 'has been written'"
        false
        (contains_str (String.lowercase_ascii msg) "has been written")

(* Copilot (Config_fixed_path, write_outcome = None) must still emit a warning. *)
let test_warning_copilot_with_none_write_outcome () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"copilot-cli"
      ~write_outcome:None
  with
  | None -> Alcotest.fail "expected warning for copilot-cli (Low confidence)"
  | Some msg ->
      Alcotest.(check bool)
        "copilot warning mentions no project config or user settings"
        true
        (contains_str (String.lowercase_ascii msg) "no project"
        || contains_str (String.lowercase_ascii msg) "config"
        || contains_str (String.lowercase_ascii msg) "user")

(** {1 Suite} *)

let () =
  Alcotest.run
    "Backend_config_gen"
    [
      ( "AC1 generate returns artifact",
        [
          Alcotest.test_case
            "Backend_config_gen is facade, not provider"
            `Quick
            test_backend_config_gen_is_facade_not_provider;
          Alcotest.test_case
            "claude-code: Some artifact"
            `Quick
            test_generate_claude_code;
          Alcotest.test_case "codex: Some artifact" `Quick test_generate_codex;
          Alcotest.test_case
            "opencode: Some artifact"
            `Quick
            test_generate_opencode;
          Alcotest.test_case
            "gemini-cli: Some artifact"
            `Quick
            test_generate_gemini_cli;
          Alcotest.test_case
            "copilot-cli: Some artifact (Config_fixed_path)"
            `Quick
            test_generate_copilot_cli_some;
          Alcotest.test_case
            "unknown backend: None"
            `Quick
            test_generate_unknown_none;
        ] );
      ( "AC2 idempotency",
        [
          Alcotest.test_case
            "claude-code content idempotent"
            `Quick
            (test_idempotent "claude-code");
          Alcotest.test_case
            "codex content idempotent"
            `Quick
            (test_idempotent "codex");
          Alcotest.test_case
            "opencode content idempotent"
            `Quick
            (test_idempotent "opencode");
          Alcotest.test_case
            "gemini-cli content idempotent"
            `Quick
            (test_idempotent "gemini-cli");
        ] );
      ( "AC3 attribution",
        [
          Alcotest.test_case
            "claude-code: attribution present"
            `Quick
            (test_attribution "claude-code");
          Alcotest.test_case
            "codex: attribution present"
            `Quick
            (test_attribution "codex");
          Alcotest.test_case
            "opencode: attribution present"
            `Quick
            (test_attribution "opencode");
        ] );
      ( "AC5 ownership and paths",
        [
          Alcotest.test_case
            "claude-code: Epure_owned"
            `Quick
            test_claude_code_ownership;
          Alcotest.test_case
            "claude-code: path under .cabal/"
            `Quick
            test_claude_code_path_under_epure;
          Alcotest.test_case
            "codex: Backend_project"
            `Quick
            (test_backend_project_ownership "codex");
          Alcotest.test_case
            "opencode: Backend_project"
            `Quick
            (test_backend_project_ownership "opencode");
          Alcotest.test_case
            "gemini-cli: Backend_project"
            `Quick
            (test_backend_project_ownership "gemini-cli");
          Alcotest.test_case
            "codex path: .codex/config.toml"
            `Quick
            test_codex_path;
          Alcotest.test_case
            "opencode path: opencode.json"
            `Quick
            test_opencode_path;
          Alcotest.test_case
            "gemini-cli path: .gemini/settings.json"
            `Quick
            test_gemini_path;
          Alcotest.test_case
            "gemini-cli plural artifacts"
            `Quick
            test_gemini_generate_all_paths;
          Alcotest.test_case
            "copilot-cli plural artifacts"
            `Quick
            test_copilot_generate_all_paths;
          Alcotest.test_case
            "custom namespace controls Claude config path and markers"
            `Quick
            test_custom_namespace_claude_config_path_and_markers;
          Alcotest.test_case
            "custom namespace controls strict JSON sidecar suffix"
            `Quick
            test_custom_namespace_sidecar_suffix;
          Alcotest.test_case
            "invalid namespace id is rejected before write"
            `Quick
            test_invalid_namespace_id_rejected_before_write;
          Alcotest.test_case
            "absolute config_dir is rejected before setup write"
            `Quick
            test_absolute_config_dir_rejected_before_setup_write;
          Alcotest.test_case
            "parent config_dir is rejected before setup write"
            `Quick
            test_parent_config_dir_rejected_before_setup_write;
        ] );
      ( "AC6 managed markers and hash",
        [
          Alcotest.test_case
            "claude-code: is_managed_content true"
            `Quick
            (test_is_managed_true "claude-code");
          Alcotest.test_case
            "codex: is_managed_content true"
            `Quick
            (test_is_managed_true "codex");
          Alcotest.test_case
            "opencode: is_managed_content true"
            `Quick
            (test_is_managed_true "opencode");
          Alcotest.test_case
            "user content: is_managed_content false"
            `Quick
            test_is_managed_false_for_user_content;
          Alcotest.test_case
            "codex: hash present in content"
            `Quick
            (test_hash_present_in_managed "codex");
          Alcotest.test_case
            "opencode: hash present in content"
            `Quick
            (test_hash_present_in_managed "opencode");
          Alcotest.test_case
            "opencode: no legacy _epure schema keys"
            `Quick
            test_opencode_no_legacy_epure_schema_keys;
          Alcotest.test_case
            "codex: no forced provider config"
            `Quick
            test_codex_does_not_force_model_provider;
          Alcotest.test_case
            "codex config includes supplied MCP servers"
            `Quick
            test_codex_config_includes_supplied_mcp_servers;
          Alcotest.test_case
            "gemini settings JSON has empty mcpServers by default"
            `Quick
            test_gemini_settings_json_default_is_strict_and_empty_mcp;
          Alcotest.test_case
            "gemini settings JSON includes supplied MCP servers"
            `Quick
            test_gemini_settings_json_includes_mcp_servers;
          Alcotest.test_case
            "copilot settings JSON is strict and minimal"
            `Quick
            test_copilot_settings_json_is_strict_minimal;
          Alcotest.test_case
            "copilot project MCP JSON is empty by default"
            `Quick
            test_copilot_project_mcp_json_default_empty;
          Alcotest.test_case
            "copilot project MCP JSON includes supplied servers"
            `Quick
            test_copilot_project_mcp_json_includes_servers;
          Alcotest.test_case
            "copilot LSP JSON is empty by default"
            `Quick
            test_copilot_lsp_json_default_empty;
          Alcotest.test_case
            "copilot LSP JSON includes detected servers"
            `Quick
            test_copilot_lsp_json_includes_detected_servers;
        ] );
      ( "AC6 write behavior",
        [
          Alcotest.test_case
            "write creates new file (codex)"
            `Quick
            test_write_creates_file;
          Alcotest.test_case
            "write idempotent → Already_current"
            `Quick
            test_write_idempotent;
          Alcotest.test_case
            "write epure-owned creates file"
            `Quick
            test_write_epure_owned_creates_file;
          Alcotest.test_case
            "write epure-owned idempotent"
            `Quick
            test_write_epure_owned_idempotent;
          Alcotest.test_case
            "epure-owned: force:true same as force:false"
            `Quick
            test_write_epure_owned_force_same_as_no_force;
          Alcotest.test_case
            "user content at fixed path → Skipped"
            `Quick
            test_write_user_content_skipped;
          Alcotest.test_case
            "hash mismatch → Refused by default"
            `Quick
            test_write_hash_mismatch_refused;
          Alcotest.test_case
            "hash mismatch + --force → backup and write"
            `Quick
            test_write_hash_mismatch_force_backup;
          Alcotest.test_case
            "opencode: legacy _epure keys rewritten"
            `Quick
            test_write_opencode_rewrites_legacy_metadata_keys;
          Alcotest.test_case
            "opencode: legacy migration preserves MCP"
            `Quick
            test_setup_opencode_migrates_legacy_metadata_preserving_mcp;
          Alcotest.test_case
            "opencode: legacy migration preserves 0600 mode"
            `Quick
            test_setup_opencode_migration_keeps_0600_mode;
        ] );
      ( "AC4 user content preservation",
        [
          Alcotest.test_case
            "user GEMINI.md preserved during setup"
            `Quick
            test_setup_gemini_user_gemini_md_preserved;
        ] );
      ( "SEC-1 no secrets in generated content",
        [
          Alcotest.test_case
            "no secret keys in any generated content"
            `Quick
            test_no_secret_keys;
        ] );
      ( "AC2 write idempotence",
        [
          Alcotest.test_case
            "opencode write idempotent → Already_current"
            `Quick
            test_write_idempotent_opencode;
          Alcotest.test_case
            "gemini-cli write idempotent → Already_current"
            `Quick
            test_write_idempotent_gemini;
          Alcotest.test_case
            "opencode: mcp injection then re-write → no hash mismatch"
            `Quick
            test_opencode_mcp_injection_no_hash_mismatch;
          Alcotest.test_case
            "opencode: mcp injection preserves LSP definitions"
            `Quick
            test_opencode_mcp_injection_preserves_lsp_defs;
        ] );
      ( "AC5/AC1 setup_project_config wiring",
        [
          Alcotest.test_case
            "claude-code: returns path (Config_explicit_flag)"
            `Quick
            test_setup_project_config_claude_code_returns_path;
          Alcotest.test_case
            "claude-code: writes file to disk"
            `Quick
            test_setup_project_config_claude_code_writes_file;
          Alcotest.test_case
            "codex: writes .codex/config.toml"
            `Quick
            test_setup_project_config_codex_writes_config;
          Alcotest.test_case
            "opencode: writes opencode.json"
            `Quick
            test_setup_project_config_opencode_writes_config;
          Alcotest.test_case
            "gemini-cli: does not write GEMINI.md"
            `Quick
            test_setup_project_config_gemini_does_not_write_gemini_md;
          Alcotest.test_case
            "gemini-cli: writes .gemini/settings.json"
            `Quick
            test_setup_project_config_gemini_writes_settings_json;
          Alcotest.test_case
            "gemini-cli: writes MCP servers to settings.json"
            `Quick
            test_setup_project_config_gemini_writes_mcp_servers_to_settings;
          Alcotest.test_case
            "copilot-cli: writes .github/copilot-instructions.md"
            `Quick
            test_setup_project_config_copilot_writes_config;
          Alcotest.test_case
            "copilot-cli: writes settings, LSP, and project MCP files"
            `Quick
            test_setup_project_config_copilot_writes_settings_and_mcp;
          Alcotest.test_case
            "copilot-cli: writes detected LSP servers"
            `Quick
            test_setup_project_config_copilot_writes_detected_lsp_servers;
          Alcotest.test_case
            "copilot-cli: writes project MCP servers"
            `Quick
            test_setup_project_config_copilot_writes_project_mcp_servers;
          Alcotest.test_case
            "strict JSON sidecar policy preserves user settings"
            `Quick
            test_sidecar_managed_json_preserves_user_settings;
          Alcotest.test_case
            "setup idempotent (second call safe)"
            `Quick
            test_setup_project_config_idempotent;
        ] );
      ( "NEW setup write_outcome",
        [
          Alcotest.test_case
            "codex: write_outcome = Written on first setup"
            `Quick
            test_setup_returns_write_outcome_written;
          Alcotest.test_case
            "codex: write_outcome = Already_current on second setup"
            `Quick
            test_setup_returns_write_outcome_already_current;
          Alcotest.test_case
            "copilot-cli: write_outcome = Written on first setup"
            `Quick
            test_setup_returns_written_outcome_for_copilot;
          Alcotest.test_case
            "codex: write_outcome = Refused on hash mismatch"
            `Quick
            test_setup_returns_refused_outcome_on_hash_mismatch;
          Alcotest.test_case
            "gemini-cli: write_outcome = Skipped on user content"
            `Quick
            test_setup_returns_skipped_outcome_on_user_content;
        ] );
      ( "NEW per-artifact write_outcomes",
        [
          Alcotest.test_case
            "gemini settings skip is reported and preserved"
            `Quick
            test_setup_reports_secondary_gemini_settings_skip;
          Alcotest.test_case
            "gemini settings hash refusal is reported and preserved"
            `Quick
            test_setup_reports_secondary_gemini_settings_refusal;
          Alcotest.test_case
            "copilot MCP skip is reported and preserved"
            `Quick
            test_setup_reports_secondary_copilot_mcp_skip;
          Alcotest.test_case
            "copilot MCP hash refusal is reported and preserved"
            `Quick
            test_setup_reports_secondary_copilot_mcp_refusal;
        ] );
      ( "NEW precedence_warning conditional on write_outcome",
        [
          Alcotest.test_case
            "codex applied: warning does not say 'could not be applied'"
            `Quick
            test_warning_applied_does_not_say_could_not_apply;
          Alcotest.test_case
            "codex refused: warning does not say 'has been written'"
            `Quick
            test_warning_refused_does_not_claim_written;
          Alcotest.test_case
            "gemini-cli skipped: warning does not say 'has been written'"
            `Quick
            test_warning_skipped_does_not_claim_written;
          Alcotest.test_case
            "copilot-cli (write_outcome=None): warning still emitted"
            `Quick
            test_warning_copilot_with_none_write_outcome;
        ] );
      ( "AC5 build_command includes config path",
        [
          Alcotest.test_case
            "claude-code: --settings flag present when path given"
            `Quick
            test_build_command_includes_config_path;
          Alcotest.test_case
            "claude-code: --settings absent when path is None"
            `Quick
            test_build_command_no_config_when_none;
        ] );
      ( "AC2/AC3/AC4 precedence_warning_for",
        [
          Alcotest.test_case
            "claude-code: no warning (High confidence)"
            `Quick
            test_no_warning_claude_code;
          Alcotest.test_case
            "codex: warning emitted (Medium confidence)"
            `Quick
            test_warning_codex;
          Alcotest.test_case
            "opencode: warning emitted (Medium confidence)"
            `Quick
            test_warning_opencode;
          Alcotest.test_case
            "gemini-cli: warning emitted (Low confidence)"
            `Quick
            test_warning_gemini_cli;
          Alcotest.test_case
            "copilot-cli: warning emitted (Low/Config_none)"
            `Quick
            test_warning_copilot_cli;
          Alcotest.test_case
            "codex: warning mentions backend name"
            `Quick
            (test_warning_mentions_backend_name
               "codex"
               "Codex CLI"
               (Some Backend_config_gen.Already_current));
          Alcotest.test_case
            "opencode: warning mentions backend name"
            `Quick
            (test_warning_mentions_backend_name
               "opencode"
               "OpenCode"
               (Some Backend_config_gen.Already_current));
          Alcotest.test_case
            "gemini-cli: warning mentions backend name"
            `Quick
            (test_warning_mentions_backend_name
               "gemini-cli"
               "Gemini CLI"
               (Some Backend_config_gen.Already_current));
          Alcotest.test_case
            "copilot-cli: warning mentions backend name"
            `Quick
            (test_warning_mentions_backend_name
               "copilot-cli"
               "Copilot CLI"
               None);
          Alcotest.test_case
            "unknown backend: no warning"
            `Quick
            test_no_warning_unknown;
        ] );
    ]
