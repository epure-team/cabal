(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Adapter_loader and Yaml_adapter. *)

open Cabal

(* --- helpers --------------------------------------------------------------- *)

let with_temp_dir f =
  let dir = Filename.temp_file "epure_adapter_test_" "" in
  Unix.unlink dir ;
  Unix.mkdir dir 0o700 ;
  Fun.protect
    ~finally:(fun () ->
      let rm_rf d =
        let rec go path =
          if Sys.is_directory path then begin
            Array.iter
              (fun name -> go (Filename.concat path name))
              (Sys.readdir path) ;
            Unix.rmdir path
          end
          else Unix.unlink path
        in
        go d
      in
      if Sys.file_exists dir then rm_rf dir)
    (fun () -> f dir)

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

(* --- load_string tests ----------------------------------------------------- *)

let test_valid_yaml () =
  let yaml =
    {|
name: test-adapter
display_name: Test Adapter
invocation_command: "test-tool --flag -p -"
template_set: default
timeout_seconds: 120.0
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Error msg -> Alcotest.failf "unexpected error: %s" msg
  | Ok cfg ->
      Alcotest.(check string) "name" "test-adapter" cfg.name ;
      Alcotest.(check string) "display_name" "Test Adapter" cfg.display_name ;
      Alcotest.(check string)
        "invocation_command"
        "test-tool --flag -p -"
        cfg.invocation_command ;
      Alcotest.(check string) "template_set" "default" cfg.template_set ;
      Alcotest.(check string) "source" "test" cfg.source ;
      Alcotest.(check bool)
        "timeout approx"
        true
        (Float.abs (cfg.timeout_seconds -. 120.0) < 0.001)

let test_missing_name () =
  let yaml =
    {|
display_name: No Name Adapter
invocation_command: "some-tool -p -"
template_set: default
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error for missing name"
  | Error msg ->
      Alcotest.(check bool)
        "mentions name"
        true
        (let open String in
         let len = length "name" in
         let mlen = length msg in
         mlen >= len
         &&
         let rec find i =
           if i + len > mlen then false
           else if sub msg i len = "name" then true
           else find (i + 1)
         in
         find 0)

let test_missing_invocation_command () =
  let yaml =
    {|
name: no-command
display_name: No Command
template_set: default
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error for missing invocation_command"
  | Error msg ->
      Alcotest.(check bool)
        "mentions invocation_command"
        true
        (let open String in
         let needle = "invocation_command" in
         let nlen = length needle in
         let mlen = length msg in
         mlen >= nlen
         &&
         let rec find i =
           if i + nlen > mlen then false
           else if sub msg i nlen = needle then true
           else find (i + 1)
         in
         find 0)

let test_missing_template_set () =
  let yaml =
    {|
name: no-template-set
display_name: No Template Set
invocation_command: "tool -p -"
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error for missing template_set"
  | Error _ -> ()

let test_default_display_name () =
  (* When display_name is absent, name is used *)
  let yaml =
    {|
name: minimal
invocation_command: "tool -p -"
template_set: default
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Error msg -> Alcotest.failf "unexpected error: %s" msg
  | Ok cfg ->
      Alcotest.(check string)
        "display_name defaults to name"
        "minimal"
        cfg.display_name

let test_malformed_yaml () =
  let yaml = "{{ not valid yaml" in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error for malformed YAML"
  | Error _ -> ()

let test_top_level_not_a_mapping () =
  let yaml = "- just\n- a\n- list\n" in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error when top level is a sequence"
  | Error _ -> ()

let test_env_field_not_a_mapping () =
  (* env must be a mapping; supplying a scalar should produce an empty
     mapping (no env), but the loader should still succeed for other fields. *)
  let yaml =
    {|
name: bad-env
display_name: Bad Env
invocation_command: "tool -p -"
template_set: default
env: "not a mapping"
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok cfg ->
      Alcotest.(check int)
        "env scalar gives empty mapping"
        0
        (List.length cfg.env_mappings)
  | Error msg -> Alcotest.failf "loader rejected scalar env: %s" msg

(* ---- diagnostics: non-string env values must surface a warning ---------- *)

let with_captured_diagnostics f =
  let events = ref [] in
  let handler ev = events := ev :: !events in
  Diagnostics.set_handler handler ;
  Fun.protect
    ~finally:(fun () -> Diagnostics.reset_handler ())
    (fun () ->
      f () ;
      List.rev !events)

let warn_count =
  List.fold_left
    (fun acc ev ->
      match ev with Diagnostics.Log (Warn, _) -> acc + 1 | _ -> acc)
    0

let test_non_string_env_values_emit_warning () =
  let yaml =
    {|
name: tricky-env
display_name: Tricky Env
invocation_command: "tool -p -"
template_set: default
env:
  GOOD: "ok"
  BAD: 1234
  ALSO_BAD: true
|}
  in
  let events =
    with_captured_diagnostics (fun () ->
        match Adapter_loader.load_string ~source:"test" yaml with
        | Ok cfg ->
            Alcotest.(check int)
              "only string-valued env mappings survive"
              1
              (List.length cfg.env_mappings)
        | Error e -> Alcotest.failf "loader rejected mixed env: %s" e)
  in
  Alcotest.(check int)
    "two non-string env values produce two warnings"
    2
    (warn_count events)

(* --- load_dir tests -------------------------------------------------------- *)

let test_load_dir_empty () =
  with_temp_dir (fun dir ->
      let results = Adapter_loader.load_dir dir in
      Alcotest.(check int) "empty dir = no results" 0 (List.length results))

let test_load_dir_valid () =
  with_temp_dir (fun dir ->
      write_file
        (Filename.concat dir "my-tool.yaml")
        {|
name: my-tool
display_name: My Tool
invocation_command: "my-tool -p -"
template_set: default
|} ;
      let results = Adapter_loader.load_dir dir in
      Alcotest.(check int) "one result" 1 (List.length results) ;
      match results with
      | [("my-tool.yaml", Ok cfg)] ->
          Alcotest.(check string) "name" "my-tool" cfg.name
      | [("my-tool.yaml", Error msg)] ->
          Alcotest.failf "unexpected error: %s" msg
      | _ -> Alcotest.fail "unexpected result structure")

let test_load_dir_mixed () =
  with_temp_dir (fun dir ->
      write_file
        (Filename.concat dir "good.yaml")
        {|
name: good
invocation_command: "good -p -"
template_set: default
|} ;
      write_file (Filename.concat dir "bad.yaml") "{{ invalid yaml" ;
      let results = Adapter_loader.load_dir dir in
      Alcotest.(check int) "two results" 2 (List.length results) ;
      let successes =
        List.filter
          (fun (_, r) -> match r with Ok _ -> true | _ -> false)
          results
      in
      let errors =
        List.filter
          (fun (_, r) -> match r with Error _ -> true | _ -> false)
          results
      in
      Alcotest.(check int) "one success" 1 (List.length successes) ;
      Alcotest.(check int) "one error" 1 (List.length errors))

(* --- register_all and project-local override ------------------------------- *)

let test_project_local_override () =
  with_temp_dir (fun tmpdir ->
      (* Create a project-local adapter under id "gemini". The builtin
         registry exposes "gemini-cli" — this test exercises arbitrary
         user-named adapters being layered on top of the builtins. *)
      let adapters_dir =
        Filename.concat tmpdir (Filename.concat ".cabal" "adapters")
      in
      write_file
        (Filename.concat adapters_dir "gemini.yaml")
        {|
name: gemini
display_name: Gemini Override
invocation_command: "gemini-custom --flag -p -"
template_set: custom
|} ;
      Registry.clear () ;
      Adapter_loader.register_all ~project_dir:tmpdir () ;
      (* Project-local "gemini" should have overridden the builtin *)
      match Registry.get "gemini" with
      | None -> Alcotest.fail "gemini not registered"
      | Some backend ->
          Alcotest.(check string)
            "gemini name overridden"
            "Gemini Override"
            (Agentic_backend.name backend))

let test_builtin_adapters_registered () =
  Registry.clear () ;
  Adapter_loader.register_all () ;
  let expected =
    ["claude-code"; "gemini-cli"; "copilot-cli"; "codex"; "opencode"]
  in
  List.iter
    (fun name ->
      match Registry.get name with
      | None -> Alcotest.failf "builtin '%s' not registered" name
      | Some _ -> ())
    expected

(* --- validate tests -------------------------------------------------------- *)

let test_validate_ok () =
  let cfg : Yaml_adapter.config =
    {
      name = "test";
      display_name = "Test";
      invocation_command = "test -p -";
      template_set = "default";
      env_mappings = [];
      timeout_seconds = 300.0;
      source = "test";
      models = [];
    }
  in
  match Adapter_loader.validate cfg with
  | Ok () -> ()
  | Error field -> Alcotest.failf "unexpected validation error: %s" field

let test_validate_empty_name () =
  let cfg : Yaml_adapter.config =
    {
      name = "";
      display_name = "Test";
      invocation_command = "test -p -";
      template_set = "default";
      env_mappings = [];
      timeout_seconds = 300.0;
      source = "test";
      models = [];
    }
  in
  match Adapter_loader.validate cfg with
  | Error "name" -> ()
  | Error f -> Alcotest.failf "expected 'name' error, got '%s'" f
  | Ok () -> Alcotest.fail "expected Error"

(* --- test suite ------------------------------------------------------------ *)

let () =
  Alcotest.run
    "Adapter_loader"
    [
      ( "load_string",
        [
          Alcotest.test_case "valid YAML" `Quick test_valid_yaml;
          Alcotest.test_case "missing name" `Quick test_missing_name;
          Alcotest.test_case
            "missing invocation_command"
            `Quick
            test_missing_invocation_command;
          Alcotest.test_case
            "missing template_set"
            `Quick
            test_missing_template_set;
          Alcotest.test_case
            "default display_name"
            `Quick
            test_default_display_name;
          Alcotest.test_case "malformed YAML" `Quick test_malformed_yaml;
          Alcotest.test_case
            "top level not a mapping"
            `Quick
            test_top_level_not_a_mapping;
          Alcotest.test_case
            "env field not a mapping"
            `Quick
            test_env_field_not_a_mapping;
          Alcotest.test_case
            "non-string env values emit warnings"
            `Quick
            test_non_string_env_values_emit_warning;
        ] );
      ( "load_dir",
        [
          Alcotest.test_case "empty dir" `Quick test_load_dir_empty;
          Alcotest.test_case "valid file" `Quick test_load_dir_valid;
          Alcotest.test_case "mixed valid/invalid" `Quick test_load_dir_mixed;
        ] );
      ( "register_all",
        [
          Alcotest.test_case
            "builtins registered"
            `Quick
            test_builtin_adapters_registered;
          Alcotest.test_case
            "project-local override"
            `Quick
            test_project_local_override;
        ] );
      ( "validate",
        [
          Alcotest.test_case "valid config" `Quick test_validate_ok;
          Alcotest.test_case "empty name" `Quick test_validate_empty_name;
        ] );
    ]
