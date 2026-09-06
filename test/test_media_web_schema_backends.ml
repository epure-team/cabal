(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** CBL-08 authenticated media + optional native JSON Schema E2E proof.

    This binary is compiled and run only with [CABAL_E2E_TESTS=1]. It selects
    evidence-valid media descriptors, applies
    [CABAL_E2E_BACKEND], checks their hardened runtime binding, and performs
    exactly one central [Task_runtime] invocation per selected installed backend.
    Codex carries both fixtures under native schema; future evidence-backed
    non-native media backends should exercise the schema-less one-call path.

    Web selection is a separate test. No built-in currently has a positive web
    descriptor, so P0 performs no web invocation; the full web matrix is P1. *)

open Cabal

let () = Process_test_helper.install_launcher ()

type failure =
  | Bootstrap_failed
  | Descriptor_invalid
  | Runtime_binding_missing
  | Runtime_capability_disagreement
  | Binary_lookup_failed
  | Version_probe_failed
  | Version_below_baseline
  | Fixture_coverage_missing
  | Fixture_semantics_invalid
  | Fixture_materialization_failed
  | Preflight_failed
  | Runtime_registration_untrusted
  | Prepared_inputs_already_consumed
  | Central_dispatch_failed
  | Backend_call_failed
  | Detailed_execution_invalid
  | Response_schema_or_semantics_invalid
  | Event_trace_invalid
  | Workspace_cleanup_failed
  | Unexpected_failure

let render_failure = function
  | Bootstrap_failed -> "hardened runtime bootstrap failed"
  | Descriptor_invalid -> "selected descriptor evidence is invalid"
  | Runtime_binding_missing -> "selected descriptor has no hardened runtime"
  | Runtime_capability_disagreement ->
      "descriptor and hardened runtime capabilities disagree"
  | Binary_lookup_failed ->
      "CLI binary lookup failed; verify permissions and PATH integrity"
  | Version_probe_failed ->
      "installed CLI version probe failed; verify the CLI installation"
  | Version_below_baseline ->
      "installed CLI version is below the enforced descriptor baseline"
  | Fixture_coverage_missing ->
      "selected media capability has no deterministic CBL-08 fixture"
  | Fixture_semantics_invalid ->
      "deterministic fixture failed independent semantic inspection"
  | Fixture_materialization_failed ->
      "deterministic fixture materialization failed"
  | Preflight_failed ->
      "central media/schema preflight rejected the deterministic fixtures"
  | Runtime_registration_untrusted ->
      "central dispatch rejected the hardened runtime registration"
  | Prepared_inputs_already_consumed ->
      "central prepared inputs were unexpectedly consumed more than once"
  | Central_dispatch_failed ->
      "central dispatch failed before a successful backend result"
  | Backend_call_failed ->
      "backend call failed; verify CLI authentication and configured access"
  | Detailed_execution_invalid ->
      "central detailed execution violated the CBL-08 contract"
  | Response_schema_or_semantics_invalid ->
      "public agent output failed schema or image-derived semantic checks"
  | Event_trace_invalid ->
      "normalized event trace violated the CBL-08 lifecycle contract"
  | Workspace_cleanup_failed -> "temporary E2E workspace cleanup failed"
  | Unexpected_failure -> "unexpected sanitized E2E failure"

let failure_of_detailed_error = function
  | Runtime_dispatch.Dispatch_failure Runtime_dispatch.Backend_version_unsupported
    -> Version_below_baseline
  | Runtime_dispatch.Dispatch_failure Runtime_dispatch.Version_check_failed ->
      Version_probe_failed
  | Runtime_dispatch.Dispatch_failure Runtime_dispatch.Backend_unavailable
  | Runtime_dispatch.Dispatch_failure Runtime_dispatch.Availability_check_failed ->
      Backend_call_failed
  | Runtime_dispatch.Dispatch_failure (Runtime_dispatch.Preflight_failed _) ->
      Preflight_failed
  | Runtime_dispatch.Dispatch_failure
      Runtime_dispatch.Runtime_registration_untrusted ->
      Runtime_registration_untrusted
  | Runtime_dispatch.Dispatch_failure Runtime_dispatch.Prepared_already_consumed ->
      Prepared_inputs_already_consumed
  | Runtime_dispatch.Execution_failure
      (Backend_types.Native_backend_failure_with_schema _) ->
      Backend_call_failed
  | Runtime_dispatch.Dispatch_failure _
  | Runtime_dispatch.Dispatch_failure_with_execution _
  | Runtime_dispatch.Execution_failure _ ->
      Central_dispatch_failed

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name)) ;
      Unix.rmdir path
  | Unix.S_REG | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
  | Unix.S_SOCK ->
      Unix.unlink path

let with_private_workspace f =
  match
    try Some (Filename.temp_dir "cabal-cbl08-" "") with _ -> None
  with
  | None -> Error Fixture_materialization_failed
  | Some workspace ->
      let outcome =
        try
          Unix.chmod workspace 0o700 ;
          `Result (f workspace)
        with error -> `Exception error
      in
      let cleanup =
        try
          remove_tree workspace ;
          Ok ()
        with _ -> Error Workspace_cleanup_failed
      in
      (match (outcome, cleanup) with
      | `Exception ((Out_of_memory | Stack_overflow | Sys.Break) as fatal), _ ->
          raise fatal
      | `Exception _, Ok () -> Error Unexpected_failure
      | `Exception _, Error failure -> Error failure
      | `Result (Ok value), Ok () -> Ok value
      | `Result (Error failure), Ok () -> Error failure
      | `Result (Ok _), Error failure | `Result (Error _), Error failure ->
          Error failure)

let version_of_string value =
  match Backend_version.of_string value with Ok version -> Some version | Error _ -> None

let tested_versions (descriptor : Backend_registry.descriptor) =
  let media =
    Option.map
      (fun (evidence : Backend_types.feature_evidence) ->
        evidence.Backend_types.tested_at_version)
      descriptor.capabilities.media_support.evidence
  in
  let schema =
    Option.map
      (fun (evidence : Backend_types.capability_evidence) ->
        evidence.Backend_types.tested_at_version)
      descriptor.capabilities.native_json_schema_output_evidence
  in
  List.filter_map Fun.id [media; schema]

let check_version ~env (descriptor : Backend_registry.descriptor) =
  match
    E2e_harness_config.probe_version
      ~capture:(fun command -> Backend_process.capture_version_output ~env command)
      descriptor
  with
  | E2e_harness_config.Version_probe_failed
  | E2e_harness_config.Version_output_malformed ->
      Error Version_probe_failed
  | E2e_harness_config.Version_gate_rejected -> Error Version_below_baseline
  | E2e_harness_config.Version_supported installed ->
      let exceeds_tested =
        tested_versions descriptor
        |> List.filter_map version_of_string
        |> List.exists (fun tested ->
               Backend_version.compare installed tested > 0)
      in
      if exceeds_tested then
        Printf.eprintf
          "[e2e-cbl08] debug: %s installed version is above tested evidence\n%!"
          descriptor.id ;
      Ok ()

let register_hardened_runtime () =
  Registry.clear () ;
  match
    Runtime_bootstrap.register_runtime
      ~profile:Runtime_bootstrap.Hardened_builtins
      ()
  with
  | Ok () -> Ok ()
  | Error _ -> Error Bootstrap_failed

let validate_runtime_binding (descriptor : Backend_registry.descriptor) =
  match Registry.find_entry descriptor.id with
  | None | Some (Registry.Raw _) -> Error Runtime_binding_missing
  | Some (Registry.Validated entry) ->
      if E2e_harness_config.runtime_binding_matches_descriptor descriptor entry
      then Ok ()
      else Error Runtime_capability_disagreement

let fixture_limits fixtures : Task_preflight.limits =
  {
    max_attachments = List.length fixtures;
    max_file_size_bytes = 20_000;
    max_total_size_bytes = 40_000;
  }

let exact_fixture_coverage media_types fixtures =
  let covered =
    List.map
      (fun fixture -> fixture.Media_web_schema_fixture.attachment.media_type)
      fixtures
  in
  List.sort_uniq compare covered = List.sort_uniq compare media_types

let validate_execution ~(descriptor : Backend_registry.descriptor)
    ~schema_execution ~fixtures ~attachments execution events =
  let requirements =
    Media_web_schema_e2e_support.protocol_requirements_for_backend descriptor.id
  in
  let result = execution.Backend_types.final_result in
  if result.status <> Backend_types.Success then Error Backend_call_failed
  else if execution.cleanup_status <> Backend_types.Cleanup_succeeded then
    Error Detailed_execution_invalid
  else if
    not
       (Media_web_schema_e2e_support.valid_attempts
          ~schema_execution ~attachments execution)
  then Error Detailed_execution_invalid
  else if requirements.session && execution.final_session_id = None then
    Error Detailed_execution_invalid
  else if requirements.usage && execution.total_cost = None then
    Error Detailed_execution_invalid
  else
    match
      Media_web_schema_fixture.validate_response fixtures result.agent_text
    with
    | Error _ -> Error Response_schema_or_semantics_invalid
    | Ok () -> (
        match
          Media_web_schema_e2e_support.validate_event_trace ~requirements execution
            events
        with
        | Ok () -> Ok ()
        | Error _ -> Error Event_trace_invalid)

let invoke_media_schema ~sw ~env (descriptor : Backend_registry.descriptor) model =
  with_private_workspace @@ fun working_dir ->
  let media_types = descriptor.Backend_registry.capabilities.media_support.media_types in
  let fixtures = Media_web_schema_fixture.for_media_types media_types in
  if not (exact_fixture_coverage media_types fixtures) then
    Error Fixture_coverage_missing
  else if
    List.exists
      (fun fixture ->
        Result.is_error
          (Media_web_schema_fixture.validate_fixture_semantics fixture))
      fixtures
  then Error Fixture_semantics_invalid
  else
    let attachments =
      try Media_web_schema_fixture.materialize ~working_dir fixtures
      with _ -> []
    in
    if List.length attachments <> List.length fixtures then
      Error Fixture_materialization_failed
    else
      let plan =
        Media_web_schema_e2e_support.make_media_task_plan ~descriptor ~fixtures
          ~working_dir ~attachments ~model
      in
      let events = ref [] in
      let handle =
        Task_runtime.start_task ~sw ~env ~limits:(fixture_limits fixtures)
          ~backend_id:descriptor.id
          ~on_event:(fun event -> events := event :: !events)
          plan.spec
      in
      let outcome = Task_runtime.await_detailed handle in
      Task_runtime.await_event_delivery handle ;
      match outcome with
      | Error error -> Error (failure_of_detailed_error error)
      | Ok execution ->
          validate_execution ~descriptor ~schema_execution:plan.schema_execution
            ~fixtures ~attachments execution (List.rev !events)

let run_media_backend ~sw ~env (descriptor : Backend_registry.descriptor) =
  match Task_preflight.validate_descriptor descriptor with
  | Error _ -> Error Descriptor_invalid
  | Ok () -> (
      match validate_runtime_binding descriptor with
      | Error _ as error -> error
      | Ok () -> (
          match check_version ~env descriptor with
          | Error _ as error -> error
          | Ok () ->
              invoke_media_schema ~sw ~env descriptor
                (E2e_harness_config.model_for_backend descriptor.id)))

let test_media_schema_backends () =
  Diagnostics.set_handler (fun _ -> ()) ;
  Fun.protect
    ~finally:(fun () ->
      Registry.clear () ;
      Diagnostics.reset_handler ())
    (fun () ->
      match register_hardened_runtime () with
      | Error failure -> Alcotest.fail (render_failure failure)
      | Ok () ->
          let selected =
            E2e_harness_config.selected_media_descriptors
              ~descriptors:(Backend_registry.all ()) ()
          in
          if selected = [] then
            Printf.eprintf
              "[e2e-cbl08] SKIP media: no eligible evidence-backed media backend\n%!"
          else
            Eio_posix.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            List.iter
              (fun (descriptor : Backend_registry.descriptor) ->
                match validate_runtime_binding descriptor with
                | Error failure ->
                    Alcotest.failf
                      "[e2e-cbl08] FAIL %s: %s"
                      descriptor.id
                      (render_failure failure)
                | Ok () ->
                    match
                      E2e_harness_config.lookup_executable descriptor.binary_name
                    with
                    | E2e_harness_config.Executable_absent ->
                        Printf.eprintf
                          "[e2e-cbl08] SKIP %s: CLI binary is absent from PATH\n%!"
                          descriptor.id
                    | E2e_harness_config.Executable_lookup_failed ->
                        Alcotest.failf
                          "[e2e-cbl08] FAIL %s: %s"
                          descriptor.id
                          (render_failure Binary_lookup_failed)
                    | E2e_harness_config.Executable_present ->
                      match
                        try run_media_backend ~sw ~env descriptor
                        with
                        | (Out_of_memory | Stack_overflow | Sys.Break) as fatal ->
                            raise fatal
                        | _ -> Error Unexpected_failure
                      with
                      | Ok () ->
                          Printf.eprintf
                            "[e2e-cbl08] PASS %s image central runtime proof\n%!"
                            descriptor.id
                      | Error failure ->
                          Alcotest.failf
                            "[e2e-cbl08] FAIL %s: %s"
                            descriptor.id
                            (render_failure failure))
              selected)

let test_web_selection_is_positive_only () =
  let selected =
    E2e_harness_config.selected_web_descriptors
      ~descriptors:(Backend_registry.all ()) ()
  in
  List.iter
    (fun (descriptor : Backend_registry.descriptor) ->
      if
        descriptor.capabilities.web_support.maximum
        = Backend_types.Web_disabled
      then
        Alcotest.fail
          "[e2e-cbl08] FAIL web selection included a disabled descriptor")
    selected ;
  match selected with
  | [] ->
      Printf.eprintf
        "[e2e-cbl08] SKIP web: no positive built-in capability; full matrix is P1\n%!"
  | _ ->
      Alcotest.fail
        "[e2e-cbl08] FAIL positive web capability requires the P1 invocation matrix"

let () =
  Alcotest.run
    "CBL-08 media/web/schema E2E"
    [
      ( "P0 media/schema",
        [
          Alcotest.test_case "evidence-backed media backends" `Slow
            test_media_schema_backends;
        ] );
      ( "P1 web selection",
        [
          Alcotest.test_case "positive descriptors only" `Quick
            test_web_selection_is_positive_only;
        ] );
    ]
