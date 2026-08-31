(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

let () = Process_test_helper.install_launcher ()

let contains value fragment =
  let value_length = String.length value in
  let fragment_length = String.length fragment in
  let rec loop offset =
    offset + fragment_length <= value_length
    &&
    (String.sub value offset fragment_length = fragment || loop (offset + 1))
  in
  fragment_length = 0 || loop 0

let expect_ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error)

let expect_validation_ok = function
  | Ok value -> value
  | Error error ->
      Alcotest.fail (Runtime_bootstrap.render_validation_error error)

let validated_entry id =
  match Registry.find_entry id with
  | Some (Registry.Validated entry) -> entry
  | Some (Registry.Raw _) -> Alcotest.failf "%s is only raw-registered" id
  | None -> Alcotest.failf "%s is not registered" id

let with_empty_registry f =
  Registry.clear () ;
  Fun.protect ~finally:Registry.clear f

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path) ;
      Unix.rmdir path
    end
    else Unix.unlink path

let with_temp_dir label f =
  let path = Filename.temp_dir ("cabal-cbl03-" ^ label ^ "-") "" in
  Fun.protect
    ~finally:(fun () -> remove_tree path)
    (fun () -> f path)

let mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o700

let write_file path contents =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let adapter_yaml ?(invocation_command = "false") ~id ~display_name () =
  Printf.sprintf
    "name: %s\ndisplay_name: %s\ninvocation_command: \"%s\"\ntemplate_set: \
     default\n"
    id
    display_name
    invocation_command

let write_adapter ?invocation_command ~root ~id ~display_name () =
  let cabal_dir = Filename.concat root ".cabal" in
  let adapters_dir = Filename.concat cabal_dir "adapters" in
  mkdir cabal_dir ;
  mkdir adapters_dir ;
  write_file
    (Filename.concat adapters_dir (id ^ ".yaml"))
    (adapter_yaml ?invocation_command ~id ~display_name ())

let write_executable path contents =
  write_file path contents ;
  Unix.chmod path 0o700

let with_home home f =
  let previous = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" home ;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "HOME" (Option.value ~default:"" previous))
    f

let descriptor_for ?(session_resume = false) ?(native = false)
    ?(read_only = false) id =
  let native_json_schema_output_evidence =
    if native then
      Some
        {
          Backend_types.tested_at_version = "1.0.0";
          json_schema_draft = "2020-12";
          test_method = Backend_types.E2e_test;
        }
    else None
  in
  {
    Backend_registry.id;
    display_name = "Custom backend";
    binary_name = "custom-backend";
    baseline_version = "1.0.0";
    capabilities =
      {
        structured_output = true;
        streaming_output = false;
        session_resume;
        mcp_support = Backend_registry.Mcp_none;
        read_only_support = read_only;
        project_config_surface = Backend_registry.Config_none;
        precedence_confidence = Backend_registry.Low;
        generated_lsp_config = false;
        file_reading = false;
        media_support = {media_types = []; evidence = None};
        web_support =
          {maximum = Backend_types.Web_disabled; evidence = None};
        native_json_schema_output = native;
        native_json_schema_output_evidence;
      };
  }

let make_backend ?(session_resume = false) ?(native = false) ?(name = "Custom")
    id =
  let module Backend = struct
    let id = id
    let name = name
    let models = []
    let models_probe = None
    let available ~sw:_ ~env:_ = true
    let supports_session_resume = session_resume
    let native_json_schema_output = native
    let is_resume_failure _ = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "not used"

    let run_task ~sw:_ ~env:_ ?on_raw_line:_ _ =
      Backend_types.make_task_result ~status:Backend_types.Success ()
  end in
  (module Backend : Agentic_backend.S)

let test_hardened_requires_empty_registry_and_preserves_state () =
  with_empty_registry @@ fun () ->
  let sentinel = make_backend ~name:"Sentinel" "sentinel" in
  Registry.register sentinel ;
  let preserved_descriptor_id = "bootstrap-preserved-descriptor" in
  Backend_registry.register_descriptor
    (descriptor_for preserved_descriptor_id) ;
  let before_ids = Registry.list_ids () in
  let before_descriptors = Backend_registry.all () in
  let result =
    Runtime_bootstrap.register_runtime
      ~profile:Runtime_bootstrap.Hardened_builtins
      ()
  in
  Alcotest.(check bool) "bootstrap fails" true (Result.is_error result) ;
  Alcotest.(check (list string))
    "runtime registry unchanged"
    before_ids
    (Registry.list_ids ()) ;
  Alcotest.(check int)
    "descriptor registry unchanged"
    (List.length before_descriptors)
    (List.length (Backend_registry.all ())) ;
  Alcotest.(check bool)
    "additive descriptor registry unchanged"
    true
    (Option.is_some (Backend_registry.find preserved_descriptor_id)) ;
  match Registry.get "sentinel" with
  | Some backend ->
      Alcotest.(check string)
        "sentinel remains installed"
        "Sentinel"
        (Agentic_backend.name backend)
  | None -> Alcotest.fail "sentinel was removed by failed bootstrap"

let test_hardened_registers_exact_approved_runtime () =
  with_empty_registry @@ fun () ->
  expect_ok
    (Runtime_bootstrap.register_runtime
       ~profile:Runtime_bootstrap.Hardened_builtins
       ()) ;
  let expected =
    ["claude-code"; "codex"; "copilot-cli"; "gemini-cli"; "opencode"; "pi"]
  in
  Alcotest.(check (list string))
    "exact six ids"
    expected
    (List.sort String.compare (Registry.list_ids ())) ;
  List.iter
    (fun id ->
      match Backend_registry.find id with
      | Some descriptor ->
          let entry = validated_entry id in
          expect_validation_ok
            (Runtime_bootstrap.validate_backend_capabilities
               ~runtime_capabilities:entry.runtime_capabilities
               ~descriptor:entry.effective_descriptor
               ~backend:entry.backend) ;
          Alcotest.(check bool)
            (id ^ " binds the approved static descriptor")
            true
            (entry.effective_descriptor = descriptor) ;
          Alcotest.(check bool)
            (id ^ " runtime capability snapshot exactly matches")
            true
            (entry.runtime_capabilities = descriptor.capabilities) ;
          Alcotest.(check bool)
            (id ^ " enforces its stable baseline")
            true
            (entry.version_policy = Runtime_entry.Enforce_baseline)
      | None -> Alcotest.failf "%s is missing its approved descriptor" id)
    expected ;
  let expected_origins =
    [
      ("claude-code", Runtime_entry.Handwritten);
      ("codex", Runtime_entry.Handwritten);
      ("copilot-cli", Runtime_entry.Handwritten);
      ("gemini-cli", Runtime_entry.Handwritten);
      ("opencode", Runtime_entry.Handwritten);
      ("pi", Runtime_entry.Yaml);
    ]
  in
  List.iter
    (fun (id, expected_origin) ->
      let entry = validated_entry id in
      Alcotest.(check bool)
        (id ^ " has the approved implementation origin")
        true
        (entry.origin = expected_origin))
    expected_origins ;
  let pi_entry = validated_entry "pi" in
  let pi = pi_entry.backend in
  let pi_descriptor = pi_entry.effective_descriptor in
  Alcotest.(check bool)
    "Pi descriptor truthfully disables resume"
    false
    pi_descriptor.capabilities.session_resume ;
  Alcotest.(check bool)
    "Pi runtime does not transport resume"
    false
    (Agentic_backend.supports_session_resume pi)

let test_hardened_ignores_home_and_project_adapters () =
  with_empty_registry @@ fun () ->
  with_temp_dir "home" @@ fun home ->
  with_temp_dir "project" @@ fun project ->
  write_adapter ~root:home ~id:"pi" ~display_name:"Global override" () ;
  write_adapter ~root:project ~id:"pi" ~display_name:"Project override" () ;
  with_home home @@ fun () ->
  expect_ok
    (Runtime_bootstrap.register_runtime
       ~project_dir:project
       ~profile:Runtime_bootstrap.Hardened_builtins
       ()) ;
  let pi = Registry.get_exn "pi" in
  match Yaml_adapter.config_of pi with
  | Some config ->
      Alcotest.(check string) "embedded source wins" "builtin" config.source ;
      Alcotest.(check string)
        "override display is ignored"
        "Pi Coding Agent"
        config.display_name
  | None -> Alcotest.fail "Pi was not registered from embedded YAML"

let test_extensible_preserves_global_project_precedence () =
  with_empty_registry @@ fun () ->
  with_temp_dir "extensible-home" @@ fun home ->
  with_temp_dir "extensible-project" @@ fun project ->
  let executable = Filename.concat project "custom-adapter" in
  write_executable
    executable
    {|#!/bin/sh
if [ "${1-}" = "--version" ]; then
  printf '%s\n' '1.0.0-beta'
else
  printf '%s\n' "${1-unknown}"
fi
|} ;
  write_adapter
    ~root:home
    ~id:"custom-precedence"
    ~display_name:"Global custom"
    ~invocation_command:(executable ^ " global")
    () ;
  write_adapter
    ~root:project
    ~id:"custom-precedence"
    ~display_name:"Project custom"
    ~invocation_command:(executable ^ " project")
    () ;
  with_home home @@ fun () ->
  expect_ok
    (Runtime_bootstrap.register_runtime
       ~project_dir:project
       ~profile:Runtime_bootstrap.Extensible
       ()) ;
  let backend = Registry.get_exn "custom-precedence" in
  let entry = validated_entry "custom-precedence" in
  (match Yaml_adapter.config_of backend with
  | Some config ->
      Alcotest.(check string)
        "project adapter has highest precedence"
        "Project custom"
        config.display_name ;
      Alcotest.(check bool)
        "source is the project adapter"
        true
        (contains config.source project)
  | None -> Alcotest.fail "custom adapter was not YAML-backed") ;
  Alcotest.(check bool)
    "custom adapter origin is explicit"
    true
    (entry.origin = Runtime_entry.Yaml) ;
  Alcotest.(check bool)
    "YAML adapter explicitly skips stable baseline gating"
    true
    (entry.version_policy = Runtime_entry.No_version_gate) ;
  Alcotest.(check bool)
    "YAML effective descriptor does not enter the static catalog"
    true
    (Option.is_none (Backend_registry.find "custom-precedence")) ;
  let descriptor = entry.effective_descriptor in
  Alcotest.(check string)
    "project descriptor replaces global metadata"
    "Project custom"
    descriptor.display_name ;
  Alcotest.(check string)
    "descriptor binary is derived without execution"
    executable
    descriptor.binary_name ;
  Alcotest.(check string)
    "generated baseline is structurally valid"
    "0.0.0"
    descriptor.baseline_version ;
  Alcotest.(check bool)
    "generated descriptor has no positive media claim"
    true
    (descriptor.capabilities.media_support.media_types = []) ;
  Alcotest.(check bool)
    "generated descriptor disables web"
    true
    (descriptor.capabilities.web_support.maximum = Backend_types.Web_disabled) ;
  Alcotest.(check bool)
    "runtime snapshot exactly matches conservative descriptor"
    true
    (entry.runtime_capabilities = descriptor.capabilities) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let completer =
    match
      Backend_completer.make_by_name
        ~sw
        ~env
        ~backend_name:"custom-precedence"
        ~working_dir:project
        ()
    with
    | Ok completer -> completer
    | Error error -> Alcotest.fail error
  in
  match
    completer
      ~system_prompt:"system"
      ~prompt:"prompt"
      ~json_schema:None
      ~resume_session_id:None
  with
  | Ok result ->
      Alcotest.(check string)
        "project adapter is invoked through central dispatch"
        "project"
        (String.trim result.text)
  | Error error -> Alcotest.fail error

let test_extensible_yaml_does_not_inherit_host_catalog_descriptor () =
  with_empty_registry @@ fun () ->
  with_temp_dir "descriptor-failure-home" @@ fun home ->
  let id = "yaml-descriptor-owned-by-host" in
  let host_descriptor = descriptor_for id in
  Backend_registry.register_descriptor host_descriptor ;
  write_adapter ~root:home ~id ~display_name:"Rejected YAML" () ;
  with_home home @@ fun () ->
  expect_ok
    (Runtime_bootstrap.register_runtime ~profile:Runtime_bootstrap.Extensible ()) ;
  let entry = validated_entry id in
  Alcotest.(check string)
    "YAML runtime binds its own effective descriptor"
    "Rejected YAML"
    entry.effective_descriptor.display_name ;
  Alcotest.(check bool)
    "YAML runtime cannot inherit structured output"
    false
    entry.effective_descriptor.capabilities.structured_output ;
  Alcotest.(check bool)
    "YAML runtime uses explicit no-gate policy"
    true
    (entry.version_policy = Runtime_entry.No_version_gate) ;
  match Backend_registry.find id with
  | Some descriptor ->
      Alcotest.(check string)
        "host catalog descriptor remains unchanged"
        host_descriptor.display_name
        descriptor.display_name
  | None -> Alcotest.fail "host descriptor was removed"

let test_extensible_builtin_yaml_override_is_conservative () =
  with_empty_registry @@ fun () ->
  with_temp_dir "builtin-override-home" @@ fun home ->
  with_temp_dir "builtin-override-project" @@ fun project ->
  write_adapter
    ~root:home
    ~id:"claude-code"
    ~display_name:"Global Claude YAML"
    ~invocation_command:"false global"
    () ;
  write_adapter
    ~root:project
    ~id:"claude-code"
    ~display_name:"Project Claude YAML"
    ~invocation_command:"false project"
    () ;
  with_home home @@ fun () ->
  expect_ok
    (Runtime_bootstrap.register_runtime
       ~project_dir:project
       ~profile:Runtime_bootstrap.Extensible
       ()) ;
  let backend = Registry.get_exn "claude-code" in
  (match Yaml_adapter.config_of backend with
  | Some config ->
      Alcotest.(check string)
        "project built-in override has highest precedence"
        "Project Claude YAML"
        config.display_name ;
      Alcotest.(check bool)
        "project source is retained"
        true
        (contains config.source project)
  | None -> Alcotest.fail "built-in override should remain YAML-backed") ;
  let entry = validated_entry "claude-code" in
  Alcotest.(check bool)
    "built-in YAML override origin"
    true
    (entry.origin = Runtime_entry.Yaml) ;
  Alcotest.(check bool)
    "built-in YAML override skips baseline gate"
    true
    (entry.version_policy = Runtime_entry.No_version_gate) ;
  Alcotest.(check bool)
    "effective read-only claim is conservative"
    false
    entry.effective_descriptor.capabilities.read_only_support ;
  Alcotest.(check bool)
    "effective file-reading claim is conservative"
    false
    entry.effective_descriptor.capabilities.file_reading ;
  Alcotest.(check bool)
    "effective structured-output claim is conservative"
    false
    entry.effective_descriptor.capabilities.structured_output ;
  let static_descriptor =
    match Backend_registry.find "claude-code" with
    | Some descriptor -> descriptor
    | None -> Alcotest.fail "static Claude descriptor missing"
  in
  Alcotest.(check bool)
    "approved static descriptor remains unchanged"
    true
    static_descriptor.capabilities.read_only_support

let test_hardened_probes_are_off_by_default () =
  with_empty_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_ok
    (Runtime_bootstrap.register_runtime
       ~sw
       ~env
       ~profile:Runtime_bootstrap.Hardened_builtins
       ()) ;
  List.iter
    (fun id ->
      match Registry.resolved_models id with
      | Some (_, Registry.Static) -> ()
      | Some (_, Registry.Probe) ->
          Alcotest.failf "%s unexpectedly ran its model probe" id
      | Some (_, Registry.Hybrid) ->
          Alcotest.failf "%s unexpectedly has hybrid model resolution" id
      | None -> Alcotest.failf "%s has no resolved model view" id)
    (Registry.list_ids ())

let test_probe_options_are_typed_and_fail_before_mutation () =
  with_empty_registry @@ fun () ->
  let result =
    Runtime_bootstrap.register_runtime
      ~probe_models:true
      ~profile:Runtime_bootstrap.Hardened_builtins
      ()
  in
  (match result with
  | Error (Runtime_bootstrap.Invalid_options _) -> ()
  | Error error ->
      Alcotest.failf
        "unexpected error: %s"
        (Runtime_bootstrap.render_error error)
  | Ok () -> Alcotest.fail "probe_models=true without Eio context must fail") ;
  Alcotest.(check (list string))
    "invalid options do not mutate runtime registry"
    []
    (Registry.list_ids ()) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let incomplete =
    Runtime_bootstrap.register_runtime
      ~sw
      ~profile:Runtime_bootstrap.Hardened_builtins
      ()
  in
  (match incomplete with
  | Error
      (Runtime_bootstrap.Invalid_options
        Runtime_bootstrap.Incomplete_eio_context) ->
      ()
  | Error error ->
      Alcotest.failf
        "unexpected incomplete-context error: %s"
        (Runtime_bootstrap.render_error error)
  | Ok () -> Alcotest.fail "supplying only sw must fail") ;
  let extensible_conflict =
    Runtime_bootstrap.register_runtime
      ~sw
      ~env
      ~probe_models:false
      ~profile:Runtime_bootstrap.Extensible
      ()
  in
  (match extensible_conflict with
  | Error
      (Runtime_bootstrap.Invalid_options
        Runtime_bootstrap.Extensible_probe_override_conflict) ->
      ()
  | Error error ->
      Alcotest.failf
        "unexpected extensible conflict error: %s"
        (Runtime_bootstrap.render_error error)
  | Ok () -> Alcotest.fail "extensible probe conflict must fail") ;
  Alcotest.(check (list string))
    "all invalid options preserve registry"
    []
    (Registry.list_ids ())

let test_validate_backend_rejects_id_and_capability_mismatches () =
  let descriptor = descriptor_for ~session_resume:true ~native:true "expected" in
  let wrong_id = make_backend ~session_resume:true ~native:true "actual" in
  Alcotest.(check bool)
    "exact id mismatch rejected"
    true
    (Result.is_error
       (Runtime_bootstrap.validate_backend ~descriptor ~backend:wrong_id)) ;
  let wrong_session = make_backend ~native:true "expected" in
  Alcotest.(check bool)
    "session mismatch rejected"
    true
    (Result.is_error
       (Runtime_bootstrap.validate_backend ~descriptor ~backend:wrong_session)) ;
  let wrong_native = make_backend ~session_resume:true "expected" in
  Alcotest.(check bool)
    "native schema mismatch rejected"
    true
    (Result.is_error
       (Runtime_bootstrap.validate_backend ~descriptor ~backend:wrong_native)) ;
  let positive_descriptor = descriptor_for ~read_only:true "capability-snapshot" in
  let conservative_runtime_capabilities =
    {
      positive_descriptor.capabilities with
      Backend_registry.read_only_support = false;
    }
  in
  Alcotest.(check bool)
    "full runtime capability mismatch rejected"
    true
    (match
       Runtime_bootstrap.validate_backend_capabilities
         ~runtime_capabilities:conservative_runtime_capabilities
         ~descriptor:positive_descriptor
         ~backend:(make_backend "capability-snapshot")
     with
    | Error Runtime_bootstrap.Runtime_capabilities_mismatch -> true
    | _ -> false) ;
  let malformed_baseline =
    {descriptor with Backend_registry.baseline_version = "release-latest"}
  in
  Alcotest.(check bool)
    "malformed baseline rejected"
    true
    (match
       Runtime_bootstrap.validate_backend
         ~descriptor:malformed_baseline
         ~backend:(make_backend ~session_resume:true ~native:true "expected")
     with
    | Error Runtime_bootstrap.Invalid_descriptor_baseline_version -> true
    | _ -> false)

let test_custom_registration_is_atomic () =
  with_empty_registry @@ fun () ->
  let success_id = "custom-atomic-success" in
  let success_descriptor = descriptor_for success_id in
  let success_backend = make_backend success_id in
  expect_ok
    (Runtime_bootstrap.register_custom
       ~descriptor:success_descriptor
       ~backend:success_backend) ;
  Alcotest.(check bool)
    "custom runtime registered"
    true
    (Option.is_some (Registry.get success_id)) ;
  Alcotest.(check bool)
    "custom descriptor registered"
    true
    (Option.is_some (Backend_registry.find success_id)) ;
  let success_entry = validated_entry success_id in
  Alcotest.(check bool)
    "custom registration records explicit origin"
    true
    (success_entry.origin = Runtime_entry.Custom) ;
  Alcotest.(check bool)
    "custom registration safely enforces baseline by default"
    true
    (success_entry.version_policy = Runtime_entry.Enforce_baseline) ;
  Alcotest.(check bool)
    "custom runtime snapshot matches its effective descriptor"
    true
    (success_entry.runtime_capabilities = success_descriptor.capabilities) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let limits : Task_preflight.limits =
    {max_attachments = 0; max_file_size_bytes = 0; max_total_size_bytes = 0}
  in
  let spec =
    Backend_types.make_task_spec ~prompt:"custom" ~working_dir:"." ()
  in
  (match
     Runtime_dispatch.run_task
       ~sw
       ~env
       ~limits
       ~backend_id:success_id
       spec
   with
  | Ok _ -> ()
  | Error error ->
      Alcotest.failf
        "validated custom pair did not dispatch: %s"
        (Runtime_dispatch.render_error error)) ;

  let mismatch_id = "custom-atomic-mismatch" in
  let mismatch_descriptor = descriptor_for ~session_resume:true mismatch_id in
  let mismatch_backend = make_backend mismatch_id in
  Alcotest.(check bool)
    "mismatch fails"
    true
    (Result.is_error
       (Runtime_bootstrap.register_custom
          ~descriptor:mismatch_descriptor
          ~backend:mismatch_backend)) ;
  Alcotest.(check bool)
    "mismatch leaves runtime absent"
    true
    (Option.is_none (Registry.get mismatch_id)) ;
  Alcotest.(check bool)
    "mismatch leaves descriptor absent"
    true
    (Option.is_none (Backend_registry.find mismatch_id)) ;

  let runtime_collision_id = "custom-runtime-collision" in
  Registry.register (make_backend runtime_collision_id) ;
  Alcotest.(check bool)
    "runtime collision fails"
    true
    (Result.is_error
       (Runtime_bootstrap.register_custom
          ~descriptor:(descriptor_for runtime_collision_id)
          ~backend:(make_backend runtime_collision_id))) ;
  Alcotest.(check bool)
    "runtime collision does not add descriptor"
    true
    (Option.is_none (Backend_registry.find runtime_collision_id)) ;

  let descriptor_collision_id = "custom-descriptor-collision" in
  Backend_registry.register_descriptor
    (descriptor_for descriptor_collision_id) ;
  Alcotest.(check bool)
    "descriptor collision fails"
    true
    (Result.is_error
       (Runtime_bootstrap.register_custom
          ~descriptor:(descriptor_for descriptor_collision_id)
          ~backend:(make_backend descriptor_collision_id))) ;
  Alcotest.(check bool)
    "descriptor collision does not add runtime"
    true
    (Option.is_none (Registry.get descriptor_collision_id)) ;
  let invalid_evidence_id = "custom-invalid-evidence" in
  let invalid_evidence_descriptor =
    descriptor_for ~native:true invalid_evidence_id
  in
  let invalid_evidence =
    match
      invalid_evidence_descriptor.capabilities.native_json_schema_output_evidence
    with
    | Some evidence -> {evidence with Backend_types.json_schema_draft = " "}
    | None -> Alcotest.fail "native test descriptor must carry evidence"
  in
  let invalid_evidence_descriptor =
    {
      invalid_evidence_descriptor with
      capabilities =
        {
          invalid_evidence_descriptor.capabilities with
          native_json_schema_output_evidence = Some invalid_evidence;
        };
    }
  in
  Alcotest.(check bool)
    "invalid evidence fails"
    true
    (Result.is_error
       (Runtime_bootstrap.register_custom
          ~descriptor:invalid_evidence_descriptor
          ~backend:(make_backend ~native:true invalid_evidence_id))) ;
  Alcotest.(check bool)
    "invalid evidence leaves runtime absent"
    true
    (Option.is_none (Registry.get invalid_evidence_id)) ;
  Alcotest.(check bool)
    "invalid evidence leaves descriptor absent"
    true
    (Option.is_none (Backend_registry.find invalid_evidence_id)) ;

  Alcotest.(check bool)
    "built-in descriptor collision fails"
    true
    (Result.is_error
       (Runtime_bootstrap.register_custom
          ~descriptor:
            (match Backend_registry.find "claude-code" with
            | Some descriptor -> descriptor
            | None -> Alcotest.fail "claude-code descriptor missing")
          ~backend:(module Claude_code : Agentic_backend.S))) ;
  Alcotest.(check bool)
    "built-in collision leaves runtime absent"
    true
    (Option.is_none (Registry.get "claude-code"))

let () =
  Alcotest.run
    "runtime_bootstrap"
    [
      ( "hardened",
        [
          Alcotest.test_case
            "requires empty registry and preserves state"
            `Quick
            test_hardened_requires_empty_registry_and_preserves_state;
          Alcotest.test_case
            "registers exact approved runtime"
            `Quick
            test_hardened_registers_exact_approved_runtime;
          Alcotest.test_case
            "ignores HOME and project adapters"
            `Quick
            test_hardened_ignores_home_and_project_adapters;
          Alcotest.test_case
            "model probes are off by default"
            `Quick
            test_hardened_probes_are_off_by_default;
          Alcotest.test_case
            "probe options fail typed and atomically"
            `Quick
            test_probe_options_are_typed_and_fail_before_mutation;
        ] );
      ( "extensible",
        [
          Alcotest.test_case
            "custom YAML dispatch preserves global/project precedence"
            `Quick
            test_extensible_preserves_global_project_precedence;
          Alcotest.test_case
            "YAML does not inherit host catalog descriptor"
            `Quick
            test_extensible_yaml_does_not_inherit_host_catalog_descriptor;
          Alcotest.test_case
            "built-in YAML override is conservative"
            `Quick
            test_extensible_builtin_yaml_override_is_conservative;
        ] );
      ( "validation and custom registration",
        [
          Alcotest.test_case
            "rejects id and represented capability mismatches"
            `Quick
            test_validate_backend_rejects_id_and_capability_mismatches;
          Alcotest.test_case
            "custom pair registration is atomic"
            `Quick
            test_custom_registration_is_atomic;
        ] );
    ]
