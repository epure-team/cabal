(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the per-backend model enumeration API.

    Covers:
    - [Registry.list_models id] returns the YAML-declared model list for each
      built-in adapter once [Adapter_loader.register_all] has populated the
      runtime registry.
    - [Registry.list_models] returns [None] for unregistered backend ids and
      the empty string.
    - [Agentic_backend.models] mirrors the same data through the first-class
      module API used by hosts that already hold a backend handle.
    - List invariants per backend: non-empty, no duplicates, well-formed ids
      (no whitespace, no shell metacharacters), stable across repeated calls.
    - YAML loader edge cases: missing [models:] yields [], explicit [[]]
      yields [], scalar in place of sequence yields [] (consistent with the
      [env:] mapping handling), non-string entries are dropped with a
      diagnostics warning.
    - Hand-written adapter modules ([Claude_code.models], [Codex_cli.models],
      etc.) declare the same lists as the YAML they ship next to — guards
      against silent drift.
    - [Adapter_loader.register_all] populates models for every built-in
      backend; a project-local YAML in [.cabal/adapters/<id>.yaml] overrides
      the built-in model list.
    - [Backend_types.make_task_spec] preserves both [~model:None] and a
      concrete model id round-trip so the addition stays non-breaking.
    - CHANGELOG.md mentions the model-enumeration entry under "Unreleased".
    - The new public symbols carry ocamldoc comments. *)

open Cabal

(* --- shared expected data ------------------------------------------------- *)

let expected_models =
  [
    ( "claude-code",
      [
        "claude-opus-4-7";
        "claude-sonnet-4-6";
        "claude-haiku-4-5-20251001";
        "claude-opus-4-6";
        "claude-sonnet-4-5-20250929";
      ] );
    ( "codex",
      ["gpt-5"; "gpt-4o"; "gpt-4o-mini"; "o3"; "o3-mini"; "o1"; "o1-mini"] );
    ("gemini-cli", ["gemini-2.5-pro"; "gemini-2.5-flash"; "gemini-2.0-flash"]);
    ( "copilot-cli",
      [
        "claude-opus-4-7";
        "claude-sonnet-4-6";
        "claude-haiku-4-5-20251001";
        "gpt-4o";
        "gpt-4o-mini";
        "gpt-5";
      ] );
    ( "opencode",
      [
        "claude-opus-4-7";
        "claude-sonnet-4-6";
        "claude-haiku-4-5-20251001";
        "gpt-4o";
        "gpt-4o-mini";
        "gpt-5";
      ] );
  ]

let all_backend_ids = List.map fst expected_models

let string_list = Alcotest.(list string)

(* --- helpers -------------------------------------------------------------- *)

let setup () =
  Registry.clear () ;
  Adapter_loader.register_all ()

let teardown () = Registry.clear ()

let with_registry f =
  setup () ;
  Fun.protect ~finally:teardown f

let model_id_well_formed s =
  String.length s > 0
  && String.for_all
       (fun c ->
         match c with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '/' | '_' -> true
         | _ -> false)
       s

let has_duplicates lst =
  let tbl = Hashtbl.create (List.length lst) in
  List.exists
    (fun x ->
      if Hashtbl.mem tbl x then true
      else begin
        Hashtbl.add tbl x () ;
        false
      end)
    lst

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let n = in_channel_length ic in
      let buf = Bytes.create n in
      really_input ic buf 0 n ;
      Bytes.to_string buf)

let contains ~needle haystack =
  let n = String.length needle in
  let h = String.length haystack in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i + n > h then false
      else if String.sub haystack i n = needle then true
      else loop (i + 1)
    in
    loop 0

(* Locate a source-tree file from the test process cwd.  Walks up the
   directory tree until it finds a dune-project, then tries both the raw
   relpath (standalone repo: src/adapters/…) and the vendored path
   (monorepo: libs/cabal/src/adapters/…).  Falls back to the raw relpath
   relative to cwd if no dune-project anchor is found. *)
let locate_repo_file relpath =
  let rec find_root dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_root parent
  in
  let cwd = Sys.getcwd () in
  let candidates =
    match find_root cwd with
    | Some root ->
        [
          Filename.concat root relpath;
          Filename.concat root (Filename.concat "libs/cabal" relpath);
          relpath;
        ]
    | None -> [Filename.concat "../../.." relpath; relpath]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None -> Alcotest.failf "could not locate %s from %s" relpath cwd

let yaml_path id =
  let basename =
    match id with
    | "claude-code" -> "claude-code.yaml"
    | "codex" -> "codex.yaml"
    | "gemini-cli" -> "gemini.yaml"
    | "copilot-cli" -> "copilot.yaml"
    | "opencode" -> "opencode.yaml"
    | _ -> id ^ ".yaml"
  in
  locate_repo_file (Filename.concat "src/adapters" basename)

(* --- core per-backend coverage (original 5 cases, kept) ------------------ *)

let check_models ~id ~expected =
  with_registry (fun () ->
      match Registry.list_models id with
      | None -> Alcotest.failf "expected backend '%s' to be registered" id
      | Some models -> (
          Alcotest.(check string_list)
            (Printf.sprintf "%s models" id)
            expected
            models ;
          match Registry.get id with
          | None -> Alcotest.failf "Registry.get %s returned None" id
          | Some backend ->
              Alcotest.(check string_list)
                (Printf.sprintf "%s models via Agentic_backend.models" id)
                expected
                (Agentic_backend.models backend)))

let test_claude_code_models () =
  check_models
    ~id:"claude-code"
    ~expected:
      [
        "claude-opus-4-7";
        "claude-sonnet-4-6";
        "claude-haiku-4-5-20251001";
        "claude-opus-4-6";
        "claude-sonnet-4-5-20250929";
      ]

let test_codex_models () =
  check_models
    ~id:"codex"
    ~expected:
      ["gpt-5"; "gpt-4o"; "gpt-4o-mini"; "o3"; "o3-mini"; "o1"; "o1-mini"]

let test_gemini_cli_models () =
  check_models
    ~id:"gemini-cli"
    ~expected:["gemini-2.5-pro"; "gemini-2.5-flash"; "gemini-2.0-flash"]

let test_copilot_cli_models () =
  check_models
    ~id:"copilot-cli"
    ~expected:
      [
        "claude-opus-4-7";
        "claude-sonnet-4-6";
        "claude-haiku-4-5-20251001";
        "gpt-4o";
        "gpt-4o-mini";
        "gpt-5";
      ]

let test_opencode_models () =
  check_models
    ~id:"opencode"
    ~expected:
      [
        "claude-opus-4-7";
        "claude-sonnet-4-6";
        "claude-haiku-4-5-20251001";
        "gpt-4o";
        "gpt-4o-mini";
        "gpt-5";
      ]

(* --- Registry.list_models negative cases --------------------------------- *)

let test_unknown_backend_returns_none () =
  with_registry (fun () ->
      match Registry.list_models "no-such-backend" with
      | None -> ()
      | Some _ -> Alcotest.fail "expected None for unregistered backend id")

let test_empty_string_returns_none () =
  with_registry (fun () ->
      match Registry.list_models "" with
      | None -> ()
      | Some _ -> Alcotest.fail "expected None for the empty string backend id")

(* --- list invariants (parametrised over all built-in backends) ----------- *)

let test_no_duplicates id () =
  with_registry (fun () ->
      match Registry.list_models id with
      | None -> Alcotest.failf "%s not registered" id
      | Some models ->
          Alcotest.(check bool)
            (Printf.sprintf "%s has no duplicate model ids" id)
            false
            (has_duplicates models))

let test_well_formed id () =
  with_registry (fun () ->
      match Registry.list_models id with
      | None -> Alcotest.failf "%s not registered" id
      | Some models ->
          List.iter
            (fun m ->
              Alcotest.(check bool)
                (Printf.sprintf "%s model id %S is well-formed" id m)
                true
                (model_id_well_formed m))
            models)

let test_stable_order id () =
  with_registry (fun () ->
      match (Registry.list_models id, Registry.list_models id) with
      | Some a, Some b ->
          Alcotest.(check string_list)
            (Printf.sprintf "%s model list stable across calls" id)
            a
            b
      | _ -> Alcotest.failf "%s not registered" id)

(* --- YAML adapter coverage ----------------------------------------------- *)

let test_yaml_declares_expected_models (id, expected) () =
  let path = yaml_path id in
  let content = read_file path in
  match Adapter_loader.load_string ~source:path content with
  | Error msg -> Alcotest.failf "failed to parse %s: %s" path msg
  | Ok cfg ->
      Alcotest.(check string_list)
        (Printf.sprintf "%s YAML models match expected" id)
        expected
        cfg.models

let test_yaml_missing_models_yields_empty () =
  let yaml =
    {|
name: no-models-tool
display_name: No Models
invocation_command: "tool -p -"
template_set: default
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Error msg -> Alcotest.failf "unexpected error: %s" msg
  | Ok cfg ->
      Alcotest.(check string_list) "missing models yields []" [] cfg.models

let test_yaml_empty_models_list_yields_empty () =
  let yaml =
    {|
name: empty-models-tool
display_name: Empty Models
invocation_command: "tool -p -"
template_set: default
models: []
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Error msg -> Alcotest.failf "unexpected error: %s" msg
  | Ok cfg ->
      Alcotest.(check string_list)
        "explicit empty models yields []"
        []
        cfg.models

(* The YAML loader treats a scalar in place of a sequence as [] — this
   mirrors how [env:] handles scalar input. Locking down the contract so
   a future refactor can't quietly change it. *)
let test_yaml_scalar_models_yields_empty () =
  let yaml =
    {|
name: bad-models-tool
display_name: Bad Models
invocation_command: "tool -p -"
template_set: default
models: 42
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Error msg -> Alcotest.failf "unexpected error: %s" msg
  | Ok cfg ->
      Alcotest.(check string_list) "scalar models yields []" [] cfg.models

let test_yaml_non_string_entries_dropped () =
  let yaml =
    {|
name: mixed-models-tool
display_name: Mixed Models
invocation_command: "tool -p -"
template_set: default
models:
  - good-model
  - 42
  - another-good
  - true
|}
  in
  let events = ref [] in
  Diagnostics.set_handler (fun ev -> events := ev :: !events) ;
  let r =
    Fun.protect
      ~finally:(fun () -> Diagnostics.reset_handler ())
      (fun () -> Adapter_loader.load_string ~source:"test-mixed" yaml)
  in
  match r with
  | Error msg -> Alcotest.failf "unexpected error: %s" msg
  | Ok cfg ->
      Alcotest.(check string_list)
        "string entries kept, non-strings dropped"
        ["good-model"; "another-good"]
        cfg.models ;
      let warn_count =
        List.fold_left
          (fun acc ev ->
            match ev with Diagnostics.Log (Warn, _) -> acc + 1 | _ -> acc)
          0
          !events
      in
      Alcotest.(check bool)
        "at least one warning emitted for dropped entries"
        true
        (warn_count >= 1)

(* --- Hand-written adapter modules vs expected --------------------------- *)

let test_claude_code_module_models () =
  Alcotest.(check string_list)
    "Claude_code.models"
    [
      "claude-opus-4-7";
      "claude-sonnet-4-6";
      "claude-haiku-4-5-20251001";
      "claude-opus-4-6";
      "claude-sonnet-4-5-20250929";
    ]
    Claude_code.models

let test_codex_module_models () =
  Alcotest.(check string_list)
    "Codex_cli.models"
    ["gpt-5"; "gpt-4o"; "gpt-4o-mini"; "o3"; "o3-mini"; "o1"; "o1-mini"]
    Codex_cli.models

let test_gemini_module_models () =
  Alcotest.(check string_list)
    "Gemini_cli.models"
    ["gemini-2.5-pro"; "gemini-2.5-flash"; "gemini-2.0-flash"]
    Gemini_cli.models

let test_copilot_module_models () =
  Alcotest.(check string_list)
    "Copilot_cli.models"
    [
      "claude-opus-4-7";
      "claude-sonnet-4-6";
      "claude-haiku-4-5-20251001";
      "gpt-4o";
      "gpt-4o-mini";
      "gpt-5";
    ]
    Copilot_cli.models

let test_opencode_module_models () =
  Alcotest.(check string_list)
    "Opencode_cli.models"
    [
      "claude-opus-4-7";
      "claude-sonnet-4-6";
      "claude-haiku-4-5-20251001";
      "gpt-4o";
      "gpt-4o-mini";
      "gpt-5";
    ]
    Opencode_cli.models

let test_mock_agent_module_models () =
  Alcotest.(check string_list) "Mock_agent.models" [] Mock_agent.models

(* --- register_all integration ------------------------------------------- *)

let test_register_all_populates_all_built_in () =
  with_registry (fun () ->
      List.iter
        (fun id ->
          match Registry.list_models id with
          | None -> Alcotest.failf "register_all did not register %s" id
          | Some models ->
              Alcotest.(check bool)
                (Printf.sprintf
                   "%s has non-empty model list after register_all"
                   id)
                true
                (List.length models > 0))
        all_backend_ids)

let write_file path content =
  let dir = Filename.dirname path in
  let rec mkdir_p d =
    if Sys.file_exists d then ()
    else begin
      mkdir_p (Filename.dirname d) ;
      Unix.mkdir d 0o700
    end
  in
  mkdir_p dir ;
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let with_temp_dir f =
  let dir = Filename.temp_file "cabal_model_enum_test_" "" in
  Unix.unlink dir ;
  Unix.mkdir dir 0o700 ;
  Fun.protect
    ~finally:(fun () ->
      let rec rm_rf path =
        if Sys.is_directory path then begin
          Array.iter
            (fun name -> rm_rf (Filename.concat path name))
            (Sys.readdir path) ;
          Unix.rmdir path
        end
        else Unix.unlink path
      in
      if Sys.file_exists dir then rm_rf dir)
    (fun () -> f dir)

let test_project_local_models_override () =
  with_temp_dir (fun tmpdir ->
      let adapters_dir =
        Filename.concat tmpdir (Filename.concat ".cabal" "adapters")
      in
      write_file
        (Filename.concat adapters_dir "gemini-cli.yaml")
        {|
name: gemini-cli
display_name: Gemini Override
invocation_command: "gemini-custom --flag -p -"
template_set: custom
models:
  - foo
  - bar
|} ;
      Registry.clear () ;
      Fun.protect
        ~finally:(fun () -> Registry.clear ())
        (fun () ->
          Adapter_loader.register_all ~project_dir:tmpdir () ;
          match Registry.list_models "gemini-cli" with
          | None -> Alcotest.fail "gemini-cli not registered"
          | Some models ->
              Alcotest.(check string_list)
                "project-local override surfaces its model list"
                ["foo"; "bar"]
                models))

(* --- task_spec contract -------------------------------------------------- *)

let test_make_task_spec_no_model () =
  let spec =
    Backend_types.make_task_spec ~prompt:"hello" ~working_dir:"/tmp" ()
  in
  Alcotest.(check (option string)) "model defaults to None" None spec.model

let test_make_task_spec_with_model_round_trip () =
  let spec =
    Backend_types.make_task_spec
      ~prompt:"hello"
      ~working_dir:"/tmp"
      ~model:"claude-sonnet-4-5"
      ()
  in
  Alcotest.(check (option string))
    "model round-trips"
    (Some "claude-sonnet-4-5")
    spec.model

(* --- CHANGELOG / ocamldoc checks ----------------------------------------- *)

let test_changelog_mentions_model_enumeration () =
  let path = locate_repo_file "CHANGELOG.md" in
  let content = read_file path in
  Alcotest.(check bool)
    "CHANGELOG has Unreleased section"
    true
    (contains ~needle:"## Unreleased" content) ;
  Alcotest.(check bool)
    "CHANGELOG mentions models"
    true
    (contains ~needle:"models" content
    || contains ~needle:"model enumeration" content
    || contains ~needle:"model-enumeration" content)

let find_substring content needle =
  let n = String.length needle in
  let h = String.length content in
  let rec loop i =
    if i + n > h then -1
    else if String.sub content i n = needle then i
    else loop (i + 1)
  in
  loop 0

let test_agentic_backend_mli_has_models_doc () =
  let path = locate_repo_file "src/agentic_backend.mli" in
  let content = read_file path in
  let needle = "val models" in
  let idx = find_substring content needle in
  Alcotest.(check bool) "val models declared" true (idx >= 0) ;
  let window_start = max 0 (idx - 1000) in
  let window = String.sub content window_start (idx - window_start) in
  Alcotest.(check bool)
    "val models has preceding ocamldoc"
    true
    (contains ~needle:"(**" window)

let test_registry_mli_has_list_models_doc () =
  let path = locate_repo_file "src/registry.mli" in
  let content = read_file path in
  let needle = "val list_models" in
  let idx = find_substring content needle in
  Alcotest.(check bool) "val list_models declared" true (idx >= 0) ;
  let window_start = max 0 (idx - 1000) in
  let window = String.sub content window_start (idx - window_start) in
  Alcotest.(check bool)
    "val list_models has preceding ocamldoc"
    true
    (contains ~needle:"(**" window)

(* --- Suite --------------------------------------------------------------- *)

let per_backend f =
  List.map (fun id -> Alcotest.test_case id `Quick (f id)) all_backend_ids

let per_backend_with_expected f =
  List.map
    (fun ((id, _) as pair) -> Alcotest.test_case id `Quick (f pair))
    expected_models

let () =
  Alcotest.run
    "Model_enumeration"
    [
      ( "Registry.list_models per backend",
        [
          Alcotest.test_case "claude-code" `Quick test_claude_code_models;
          Alcotest.test_case "codex" `Quick test_codex_models;
          Alcotest.test_case "gemini-cli" `Quick test_gemini_cli_models;
          Alcotest.test_case "copilot-cli" `Quick test_copilot_cli_models;
          Alcotest.test_case "opencode" `Quick test_opencode_models;
        ] );
      ( "Registry.list_models negative",
        [
          Alcotest.test_case
            "unknown backend returns None"
            `Quick
            test_unknown_backend_returns_none;
          Alcotest.test_case
            "empty string returns None"
            `Quick
            test_empty_string_returns_none;
        ] );
      ( "List invariants",
        per_backend test_no_duplicates
        @ per_backend test_well_formed
        @ per_backend test_stable_order );
      ( "YAML adapter coverage",
        per_backend_with_expected test_yaml_declares_expected_models
        @ [
            Alcotest.test_case
              "missing models yields []"
              `Quick
              test_yaml_missing_models_yields_empty;
            Alcotest.test_case
              "explicit empty list yields []"
              `Quick
              test_yaml_empty_models_list_yields_empty;
            Alcotest.test_case
              "scalar models yields []"
              `Quick
              test_yaml_scalar_models_yields_empty;
            Alcotest.test_case
              "non-string entries dropped with warning"
              `Quick
              test_yaml_non_string_entries_dropped;
          ] );
      ( "Hand-written adapter modules",
        [
          Alcotest.test_case
            "Claude_code.models"
            `Quick
            test_claude_code_module_models;
          Alcotest.test_case "Codex_cli.models" `Quick test_codex_module_models;
          Alcotest.test_case
            "Gemini_cli.models"
            `Quick
            test_gemini_module_models;
          Alcotest.test_case
            "Copilot_cli.models"
            `Quick
            test_copilot_module_models;
          Alcotest.test_case
            "Opencode_cli.models"
            `Quick
            test_opencode_module_models;
          Alcotest.test_case
            "Mock_agent.models is []"
            `Quick
            test_mock_agent_module_models;
        ] );
      ( "register_all integration",
        [
          Alcotest.test_case
            "all built-in backends populated"
            `Quick
            test_register_all_populates_all_built_in;
          Alcotest.test_case
            "project-local YAML overrides model list"
            `Quick
            test_project_local_models_override;
        ] );
      ( "task spec contract",
        [
          Alcotest.test_case
            "make_task_spec preserves model=None"
            `Quick
            test_make_task_spec_no_model;
          Alcotest.test_case
            "make_task_spec round-trips concrete model"
            `Quick
            test_make_task_spec_with_model_round_trip;
        ] );
      ( "documentation",
        [
          Alcotest.test_case
            "CHANGELOG Unreleased mentions models"
            `Quick
            test_changelog_mentions_model_enumeration;
          Alcotest.test_case
            "agentic_backend.mli has doc on val models"
            `Quick
            test_agentic_backend_mli_has_models_doc;
          Alcotest.test_case
            "registry.mli has doc on val list_models"
            `Quick
            test_registry_mli_has_list_models_doc;
        ] );
    ]
