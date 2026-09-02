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
    && (String.sub value offset fragment_length = fragment || loop (offset + 1))
  in
  fragment_length = 0 || loop 0

let limits =
  Task_preflight.
    { max_attachments = 0; max_file_size_bytes = 0; max_total_size_bytes = 0 }

let descriptor_for ?(session_resume = false) ?(native = false)
    ?(read_only = false) ?(binary_name = "dispatch-test")
    ?(baseline_version = "1.0.0") ?(media_types = [])
    ?(web_maximum = Backend_types.Web_disabled) id =
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
  let feature_evidence : Backend_types.feature_evidence =
    {
      tested_at_version = "1.0.0";
      test_method = Backend_types.E2e_test;
      evidence_url = None;
      notes = "Reproduced by test/test_runtime_dispatch.ml";
    }
  in
  {
    Backend_registry.id;
    display_name = "Dispatch test backend";
    binary_name;
    baseline_version;
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
        media_support =
          {
            media_types;
            evidence =
              (if media_types = [] then None else Some feature_evidence);
          };
        web_support =
          {
            maximum = web_maximum;
            evidence =
              (if web_maximum = Backend_types.Web_disabled then None
               else Some feature_evidence);
          };
        native_json_schema_output = native;
        native_json_schema_output_evidence;
      };
  }

let success ?(text = "ok") ?session_id () =
  Backend_types.make_task_result ~status:Backend_types.Success ~agent_text:text
    ?session_id ()

let make_backend ?(session_resume = false) ?(native = false) ?availability
    ?(on_context = fun _ -> ()) ~id ~calls run =
  let module Backend = struct
    let id = id
    let name = "Dispatch test backend"
    let models = []
    let models_probe = None

    let available ~sw ~env =
      match availability with None -> true | Some check -> check ~sw ~env

    let supports_session_resume = session_resume
    let native_json_schema_output = native
    let is_resume_failure _ = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "not used"

    let run_task ~sw ~env ?context ?on_raw_line spec =
      incr calls;
      Option.iter on_context context;
      run ~sw ~env ?on_raw_line spec
  end in
  (module Backend : Agentic_backend.S)

let with_registry f =
  Registry.clear ();
  Fun.protect ~finally:Registry.clear f

let register_pair ?session_resume ?native ?read_only ?binary_name
    ?baseline_version ?media_types ?web_maximum ?(origin = Runtime_entry.Custom)
    ?(version_policy = Runtime_entry.Enforce_baseline) ~id backend =
  let descriptor =
    descriptor_for ?session_resume ?native ?read_only ?binary_name
      ?baseline_version ?media_types ?web_maximum id
  in
  let entry =
    match
      Runtime_entry.create ~backend ~descriptor
        ~runtime_capabilities:descriptor.capabilities ~origin ~version_policy
    with
    | Ok entry -> entry
    | Error error -> Alcotest.fail (Runtime_entry.render_validation_error error)
  in
  Registry.register_validated entry

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
    end
    else Unix.unlink path

let with_temp_dir label f =
  let path = Filename.temp_dir ("cabal-dispatch-" ^ label ^ "-") "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> f path)

let with_path_prefix directory f =
  let previous = Sys.getenv_opt "PATH" in
  let path =
    match previous with
    | Some value when value <> "" -> directory ^ ":" ^ value
    | _ -> directory
  in
  Unix.putenv "PATH" path;
  Fun.protect
    ~finally:(fun () -> Unix.putenv "PATH" (Option.value ~default:"" previous))
    f

let write_executable path contents =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents);
  Unix.chmod path 0o700

let write_binary_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let spec ?(working_dir = ".") ?(read_only = false) ?resume_session_id
    ?json_schema ?(attachments = []) ?(web_access = Backend_types.Web_disabled)
    () =
  Backend_types.make_task_spec ~prompt:"dispatch test" ~working_dir ~read_only
    ?resume_session_id ?json_schema ~attachments ~web_access ()

let run ~env ~sw ~backend_id ?(limits = limits) spec =
  Runtime_dispatch.run_task ~sw ~env ~limits ~backend_id spec

let test_missing_and_raw_registrations_fail_before_call () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let runtime_missing_id = "dispatch-runtime-missing" in
  Backend_registry.register_descriptor (descriptor_for runtime_missing_id);
  (match run ~env ~sw ~backend_id:runtime_missing_id (spec ()) with
  | Error Runtime_dispatch.Backend_not_registered -> ()
  | Error error ->
      Alcotest.failf "unexpected missing-runtime error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "missing runtime must fail");
  let raw_id = "dispatch-raw-runtime" in
  Registry.register
    (make_backend ~id:raw_id ~calls (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
         success ()));
  (match run ~env ~sw ~backend_id:raw_id (spec ()) with
  | Error Runtime_dispatch.Runtime_registration_untrusted -> ()
  | Error error ->
      Alcotest.failf "unexpected raw-registration error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "raw registration must fail");
  Alcotest.(check int) "backend never called" 0 !calls

let test_runtime_descriptor_mismatches_cannot_be_registered () =
  with_registry @@ fun () ->
  let session_calls = ref 0 in
  let availability_calls = ref 0 in
  let session_id = "dispatch-session-mismatch" in
  let session_descriptor = descriptor_for ~session_resume:true session_id in
  let session_backend =
    make_backend ~id:session_id ~calls:session_calls
      ~availability:(fun ~sw:_ ~env:_ ->
        incr availability_calls;
        failwith "availability must not run for a mismatched runtime")
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())
  in
  Alcotest.(check bool)
    "session mismatch cannot produce a validated entry" true
    (Result.is_error
       (Runtime_entry.create ~backend:session_backend
          ~descriptor:session_descriptor
          ~runtime_capabilities:session_descriptor.capabilities
          ~origin:Runtime_entry.Custom
          ~version_policy:Runtime_entry.Enforce_baseline));
  Alcotest.(check bool)
    "session mismatch leaves registry empty" true
    (Option.is_none (Registry.find_entry session_id));
  Alcotest.(check int) "session mismatch call count" 0 !session_calls;
  Alcotest.(check int)
    "mismatch rejected before availability" 0 !availability_calls;

  let native_calls = ref 0 in
  let native_id = "dispatch-native-mismatch" in
  let native_descriptor = descriptor_for ~native:true native_id in
  let native_backend =
    make_backend ~id:native_id ~calls:native_calls
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())
  in
  Alcotest.(check bool)
    "native mismatch cannot produce a validated entry" true
    (Result.is_error
       (Runtime_entry.create ~backend:native_backend
          ~descriptor:native_descriptor
          ~runtime_capabilities:native_descriptor.capabilities
          ~origin:Runtime_entry.Custom
          ~version_policy:Runtime_entry.Enforce_baseline));
  Alcotest.(check int) "native mismatch call count" 0 !native_calls

let test_raw_claude_override_invalidates_validated_pair () =
  with_registry @@ fun () ->
  with_temp_dir "raw-claude-override" @@ fun temp_dir ->
  let version_marker = Filename.concat temp_dir "version-process-ran" in
  let fake_claude = Filename.concat temp_dir "claude" in
  write_executable fake_claude
    (Printf.sprintf
       "#!/bin/sh\nprintf 'spawned\\n' >> %s\nprintf '%%s\\n' '99.0.0'\n"
       (Filename.quote version_marker));
  with_path_prefix temp_dir @@ fun () ->
  (match
     Runtime_bootstrap.register_runtime
       ~profile:Runtime_bootstrap.Hardened_builtins ()
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error));
  let static_claude =
    match Backend_registry.find "claude-code" with
    | Some descriptor -> descriptor
    | None -> Alcotest.fail "static Claude descriptor missing"
  in
  Alcotest.(check bool)
    "attack precondition has positive read-only claim" true
    static_claude.capabilities.read_only_support;
  Alcotest.(check bool)
    "attack precondition has positive file-reading claim" true
    static_claude.capabilities.file_reading;
  let availability_calls = ref 0 in
  let backend_calls = ref 0 in
  Registry.register
    (make_backend ~session_resume:true ~native:true ~id:"claude-code"
       ~calls:backend_calls
       ~availability:(fun ~sw:_ ~env:_ ->
         incr availability_calls;
         true)
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ()));
  (match Registry.find_entry "claude-code" with
  | Some (Registry.Raw _) -> ()
  | Some (Registry.Validated _) ->
      Alcotest.fail "raw override retained the validated Claude pairing"
  | None -> Alcotest.fail "raw override was not registered");
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (match run ~env ~sw ~backend_id:"claude-code" (spec ~read_only:true ()) with
  | Error Runtime_dispatch.Runtime_registration_untrusted -> ()
  | Error error ->
      Alcotest.failf "unexpected raw-override error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "raw same-id override inherited trusted capabilities");
  Alcotest.(check bool)
    "raw override rejected before version process" false
    (Sys.file_exists version_marker);
  Alcotest.(check int) "raw override availability calls" 0 !availability_calls;
  Alcotest.(check int) "raw override backend calls" 0 !backend_calls

let test_invalid_preflight_never_spawns_or_calls_backend () =
  with_registry @@ fun () ->
  with_temp_dir "preflight-order" @@ fun temp_dir ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let availability_calls = ref 0 in
  let id = "dispatch-preflight" in
  let version_marker = Filename.concat temp_dir "version-probed" in
  let binary_name = Filename.concat temp_dir "preflight-backend" in
  write_executable binary_name
    (Printf.sprintf
       "#!/bin/sh\nprintf 'probed\\n' >> %s\nprintf '%%s\\n' '1.0.0'\n"
       (Filename.quote version_marker));
  register_pair ~binary_name ~id
    (make_backend ~id ~calls
       ~availability:(fun ~sw:_ ~env:_ ->
         incr availability_calls;
         true)
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ()));
  let check_no_side_effects label =
    Alcotest.(check bool)
      (label ^ " version process marker")
      false
      (Sys.file_exists version_marker);
    Alcotest.(check int)
      (label ^ " availability call count")
      0 !availability_calls;
    Alcotest.(check int) (label ^ " backend call count") 0 !calls
  in
  let invalid_limits = { limits with max_attachments = -1 } in
  let result =
    run ~env ~sw ~backend_id:id ~limits:invalid_limits
      (spec ~working_dir:"/private/limit-secret" ())
  in
  (match result with
  | Error
      (Runtime_dispatch.Preflight_failed
         (Task_preflight.Input
            (Task_preflight.Negative_limit Task_preflight.Max_attachments)) as
       error) ->
      let diagnostic = Runtime_dispatch.render_error error in
      Alcotest.(check bool)
        "limit diagnostic is sanitized" false
        (contains diagnostic "limit-secret")
  | Error error ->
      Alcotest.failf "unexpected invalid-limit error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "negative limits must fail");
  check_no_side_effects "invalid limits";

  let missing_workspace = Filename.concat temp_dir "missing-workspace" in
  (match
     run ~env ~sw ~backend_id:id (spec ~working_dir:missing_workspace ())
   with
  | Error
      (Runtime_dispatch.Preflight_failed
         (Task_preflight.Input Task_preflight.Workspace_unavailable)) ->
      ()
  | Error error ->
      Alcotest.failf "unexpected workspace error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "missing workspace must fail");
  check_no_side_effects "invalid workspace";

  let attachment =
    Backend_types.
      {
        id = "secret-id";
        path = "/private/raw-secret.png";
        media_type = Png;
        sha256 = String.make 64 '0';
        size_bytes = 0;
      }
  in
  let attachment_limits =
    Task_preflight.
      { max_attachments = 1; max_file_size_bytes = 1; max_total_size_bytes = 1 }
  in
  let result =
    run ~env ~sw ~backend_id:id ~limits:attachment_limits
      (spec ~attachments:[ attachment ] ())
  in
  (match result with
  | Error
      (Runtime_dispatch.Preflight_failed
         (Task_preflight.Input (Task_preflight.Absolute_attachment_path _)) as
       error) ->
      let diagnostic = Runtime_dispatch.render_error error in
      Alcotest.(check bool)
        "attachment path is absent" false
        (contains diagnostic "raw-secret");
      Alcotest.(check bool)
        "attachment id is absent" false
        (contains diagnostic "secret-id")
  | Error error ->
      Alcotest.failf "unexpected invalid-input error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "absolute attachment path must fail");
  check_no_side_effects "invalid input";

  (match run ~env ~sw ~backend_id:id (spec ~read_only:true ()) with
  | Error
      (Runtime_dispatch.Preflight_failed
         (Task_preflight.Capability Task_preflight.Read_only_unsupported)) ->
      ()
  | Error error ->
      Alcotest.failf "unexpected capability error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "unsupported capability must fail");
  check_no_side_effects "invalid capability";

  let png = "\x89PNG\r\n\x1a\n" in
  write_binary_file (Filename.concat temp_dir "unsupported.png") png;
  let unsupported_attachment =
    Backend_types.
      {
        id = "unsupported-media";
        path = "unsupported.png";
        media_type = Png;
        sha256 =
          "4c4b6a3be1314ab86138bef4314dde022e600960d8689a2c8f8631802d20dab6";
        size_bytes = String.length png;
      }
  in
  let media_limits =
    Task_preflight.
      {
        max_attachments = 1;
        max_file_size_bytes = 16;
        max_total_size_bytes = 16;
      }
  in
  (match
     run ~env ~sw ~backend_id:id ~limits:media_limits
       (spec ~working_dir:temp_dir ~attachments:[ unsupported_attachment ] ())
   with
  | Error
      (Runtime_dispatch.Preflight_failed
         (Task_preflight.Capability
            (Task_preflight.Unsupported_media_type Backend_types.Png))) ->
      ()
  | Error error ->
      Alcotest.failf "unexpected unsupported-media error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "unsupported media must fail before spawn");
  check_no_side_effects "unsupported media"

let test_staging_failure_never_checks_availability_or_calls_backend () =
  with_registry @@ fun () ->
  with_temp_dir "staging-failure" @@ fun temp_dir ->
  let png = "\x89PNG\r\n\x1a\n" in
  write_binary_file (Filename.concat temp_dir "image.png") png;
  let attachment =
    Backend_types.
      {
        id = "image";
        path = "image.png";
        media_type = Png;
        sha256 = Digestif.SHA256.(to_hex (digest_string png));
        size_bytes = String.length png;
      }
  in
  let attachment_limits =
    Task_preflight.
      {
        max_attachments = 1;
        max_file_size_bytes = 16;
        max_total_size_bytes = 16;
      }
  in
  let availability_calls = ref 0 in
  let backend_calls = ref 0 in
  let id = "dispatch-staging-failure" in
  register_pair ~id ~media_types:[ Backend_types.Png ]
    ~version_policy:Runtime_entry.No_version_gate
    (make_backend ~id ~calls:backend_calls
       ~availability:(fun ~sw:_ ~env:_ ->
         incr availability_calls;
         true)
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ()));
  let previous_temp_directory = Filename.get_temp_dir_name () in
  let missing_temp_directory = Filename.concat temp_dir "missing-temp-root" in
  Fun.protect
    ~finally:(fun () -> Filename.set_temp_dir_name previous_temp_directory)
    (fun () ->
      Filename.set_temp_dir_name missing_temp_directory;
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      match
        run ~env ~sw ~backend_id:id ~limits:attachment_limits
          (spec ~working_dir:temp_dir ~attachments:[ attachment ] ())
      with
      | Error
          (Runtime_dispatch.Preflight_failed
             (Task_preflight.Input Task_preflight.Attachment_staging_failed)) ->
          ()
      | Error error -> Alcotest.fail (Runtime_dispatch.render_error error)
      | Ok _ -> Alcotest.fail "staging failure unexpectedly reached backend");
  Alcotest.(check int) "availability not checked" 0 !availability_calls;
  Alcotest.(check int) "backend not called" 0 !backend_calls

let test_enforcer_uses_resolved_backend_snapshot_for_retry () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let id = "dispatch-snapshot" in
  let first_calls = ref 0 in
  let replacement_calls = ref 0 in
  let replacement =
    make_backend ~id ~calls:replacement_calls
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
        success ~text:{|{"value":"wrong"}|} ())
  in
  let first =
    make_backend ~id ~calls:first_calls (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
        if !first_calls = 1 then begin
          Registry.register replacement;
          success ~text:"not-json" ()
        end
        else success ~text:{|{"value":"ok"}|} ())
  in
  register_pair ~id first;
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("value", `Assoc [ ("type", `String "string") ]) ] );
        ("required", `List [ `String "value" ]);
      ]
  in
  let result = run ~env ~sw ~backend_id:id (spec ~json_schema:schema ()) in
  (match result with
  | Ok result ->
      Alcotest.(check string)
        "retry result comes from initial snapshot" {|{"value":"ok"}|}
        result.Backend_types.agent_text
  | Error error -> Alcotest.fail (Runtime_dispatch.render_error error));
  Alcotest.(check int) "initial backend handles both attempts" 2 !first_calls;
  Alcotest.(check int) "replacement is not used in flight" 0 !replacement_calls

let test_dispatch_context_exposes_requested_delivery () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("value", `Assoc [ ("type", `String "string") ]) ] );
        ("required", `List [ `String "value" ]);
      ]
  in
  List.iter
    (fun (label, session_resume, expected_second) ->
      let id = "dispatch-delivery-" ^ label in
      let calls = ref 0 in
      let observed = ref [] in
      let backend =
        make_backend ~id ~calls ~session_resume
          ~on_context:(fun context ->
            observed :=
              Task_execution_context.requested_delivery context :: !observed)
          (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
            if !calls = 1 then
              success ~text:"not-json"
                ?session_id:
                  (if session_resume then Some "delivery-session" else None)
                ()
            else success ~text:{|{"value":"ok"}|} ())
      in
      register_pair ~id ~session_resume backend;
      (match run ~env ~sw ~backend_id:id (spec ~json_schema:schema ()) with
      | Ok result ->
          Alcotest.(check string)
            (label ^ ": successful retry")
            {|{"value":"ok"}|} result.Backend_types.agent_text
      | Error error -> Alcotest.fail (Runtime_dispatch.render_error error));
      Alcotest.(check int) (label ^ ": exact calls") 2 !calls;
      let attachment_deliveries =
        List.rev !observed
        |> List.map (function
          | Some delivery -> delivery.Backend_types.attachment_delivery
          | None -> Alcotest.fail "dispatch backend observed no delivery intent")
      in
      Alcotest.(check bool)
        (label ^ ": central context exposes exact attempt intents")
        true
        (attachment_deliveries
        = [ Backend_types.Upload_attachments; expected_second ]))
    [
      ("fresh", false, Backend_types.Upload_attachments);
      ("resume", true, Backend_types.Reuse_session_attachments);
    ]

let test_dispatch_reuses_one_sealed_attachment_set_across_retry () =
  with_registry @@ fun () ->
  with_temp_dir "sealed-retry" @@ fun temp_dir ->
  let png = "\x89PNG\r\n\x1a\nsealed retry bytes" in
  let original_path = Filename.concat temp_dir "original.png" in
  write_binary_file original_path png;
  let attachment =
    Backend_types.
      {
        id = "image";
        path = "original.png";
        media_type = Png;
        sha256 = Digestif.SHA256.(to_hex (digest_string png));
        size_bytes = String.length png;
      }
  in
  let attachment_limits =
    Task_preflight.
      {
        max_attachments = 1;
        max_file_size_bytes = 128;
        max_total_size_bytes = 128;
      }
  in
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("value", `Assoc [ ("type", `String "string") ]) ] );
        ("required", `List [ `String "value" ]);
      ]
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (label, session_resume, expected_second_delivery) ->
      let id = "dispatch-sealed-retry-" ^ label in
      let calls = ref 0 in
      let observed_paths = ref [] in
      let observed_deliveries = ref [] in
      let backend =
        make_backend ~id ~calls ~session_resume
          ~on_context:(fun context ->
            let paths =
              match
                Task_execution_context.authorized_attachment_paths context
                  ~backend_id:id ~attachment_references:[ attachment ]
                  ~web_access_policy:Backend_types.Web_disabled
              with
              | Ok paths -> paths
              | Error message -> Alcotest.fail message
            in
            List.iter
              (fun path ->
                Alcotest.(check string)
                  "backend observes sealed bytes" png (read_file path))
              paths;
            observed_paths := paths :: !observed_paths;
            observed_deliveries :=
              Task_execution_context.requested_delivery context
              :: !observed_deliveries)
          (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
            if !calls = 1 then
              success ~text:"not-json"
                ?session_id:
                  (if session_resume then Some "retry-session" else None)
                ()
            else success ~text:{|{"value":"ok"}|} ())
      in
      register_pair ~id ~session_resume ~media_types:[ Backend_types.Png ]
        backend;
      (match
         run ~env ~sw ~backend_id:id ~limits:attachment_limits
           (spec ~working_dir:temp_dir ~attachments:[ attachment ]
              ~json_schema:schema ())
       with
      | Ok result ->
          Alcotest.(check string)
            (label ^ " retry succeeds")
            {|{"value":"ok"}|} result.Backend_types.agent_text
      | Error error -> Alcotest.fail (Runtime_dispatch.render_error error));
      let paths = List.rev !observed_paths in
      Alcotest.(check int) (label ^ " exact calls") 2 (List.length paths);
      Alcotest.(check bool)
        (label ^ " both attempts reuse the same staged set")
        true
        (match paths with [ first; second ] -> first = second | _ -> false);
      let deliveries =
        List.rev !observed_deliveries
        |> List.map (function
          | Some delivery -> delivery.Backend_types.attachment_delivery
          | None -> Alcotest.fail "missing retry delivery")
      in
      Alcotest.(check bool)
        (label ^ " delivery policy")
        true
        (deliveries
        = [ Backend_types.Upload_attachments; expected_second_delivery ]);
      List.concat paths
      |> List.iter (fun path ->
          Alcotest.(check bool)
            (label ^ " staged file cleaned after all attempts")
            false (Sys.file_exists path)))
    [
      ("fresh", false, Backend_types.Upload_attachments);
      ("resume", true, Backend_types.Reuse_session_attachments);
    ]

let test_dispatch_sanitizes_cleanup_failure_and_retries_on_switch_release () =
  with_registry @@ fun () ->
  with_temp_dir "cleanup-failure" @@ fun temp_dir ->
  let png = "\x89PNG\r\n\x1a\ncleanup bytes" in
  write_binary_file (Filename.concat temp_dir "image.png") png;
  let attachment =
    Backend_types.
      {
        id = "image";
        path = "image.png";
        media_type = Png;
        sha256 = Digestif.SHA256.(to_hex (digest_string png));
        size_bytes = String.length png;
      }
  in
  let attachment_limits =
    Task_preflight.
      {
        max_attachments = 1;
        max_file_size_bytes = 128;
        max_total_size_bytes = 128;
      }
  in
  let id = "dispatch-cleanup-failure" in
  let staging_directory = ref None in
  let blocker = ref None in
  register_pair ~id ~media_types:[ Backend_types.Png ]
    ~version_policy:Runtime_entry.No_version_gate
    (make_backend ~id ~calls:(ref 0)
       ~on_context:(fun context ->
         match
           Task_execution_context.authorized_attachment_paths context
             ~backend_id:id ~attachment_references:[ attachment ]
             ~web_access_policy:Backend_types.Web_disabled
         with
         | Ok [ path ] ->
             let directory = Filename.dirname path in
             let path = Filename.concat directory "cleanup-blocker" in
             Unix.mkdir path 0o700;
             staging_directory := Some directory;
             blocker := Some path
         | Ok _ -> Alcotest.fail "unexpected sealed attachment set"
         | Error message -> Alcotest.fail message)
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ()));
  Eio_posix.run (fun env ->
      Eio.Switch.run (fun sw ->
          let result =
            run ~env ~sw ~backend_id:id ~limits:attachment_limits
              (spec ~working_dir:temp_dir ~attachments:[ attachment ] ())
          in
          (match result with
          | Error
              (Runtime_dispatch.Preflight_failed
                 (Task_preflight.Input Task_preflight.Attachment_cleanup_failed)
               as error) ->
              let rendered = Runtime_dispatch.render_error error in
              List.iter
                (fun secret ->
                  Alcotest.(check bool)
                    "cleanup diagnostic is sanitized" false
                    (contains rendered secret))
                [ png; temp_dir ]
          | Error error -> Alcotest.fail (Runtime_dispatch.render_error error)
          | Ok _ -> Alcotest.fail "cleanup failure unexpectedly succeeded");
          match !blocker with
          | Some path -> Unix.rmdir path
          | None -> Alcotest.fail "cleanup blocker was not installed"));
  match !staging_directory with
  | Some path ->
      Alcotest.(check bool)
        "switch release retries and removes private directory" false
        (Sys.file_exists path)
  | None -> Alcotest.fail "staging directory was not captured"

let test_sealed_attachments_cleanup_on_cancellation_and_fatal_exception () =
  with_registry @@ fun () ->
  with_temp_dir "terminal-cleanup" @@ fun temp_dir ->
  let png = "\x89PNG\r\n\x1a\nterminal bytes" in
  write_binary_file (Filename.concat temp_dir "image.png") png;
  let attachment =
    Backend_types.
      {
        id = "image";
        path = "image.png";
        media_type = Png;
        sha256 = Digestif.SHA256.(to_hex (digest_string png));
        size_bytes = String.length png;
      }
  in
  let attachment_limits =
    Task_preflight.
      {
        max_attachments = 1;
        max_file_size_bytes = 128;
        max_total_size_bytes = 128;
      }
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let run_task = spec ~working_dir:temp_dir ~attachments:[ attachment ] () in
  let capture_paths id captured context =
    match
      Task_execution_context.authorized_attachment_paths context ~backend_id:id
        ~attachment_references:[ attachment ]
        ~web_access_policy:Backend_types.Web_disabled
    with
    | Ok paths -> captured := paths
    | Error message -> Alcotest.fail message
  in
  let cancellation_id = "dispatch-sealed-cancellation" in
  let cancellation_paths = ref [] in
  let started, resolve_started = Eio.Promise.create () in
  register_pair ~id:cancellation_id ~media_types:[ Backend_types.Png ]
    ~version_policy:Runtime_entry.No_version_gate
    (make_backend ~id:cancellation_id ~calls:(ref 0)
       ~on_context:(capture_paths cancellation_id cancellation_paths)
       (fun ~sw:_ ~env ?on_raw_line:_ _ ->
         Eio.Promise.resolve resolve_started ();
         Eio.Time.sleep (Eio.Stdenv.clock env) 30.0;
         success ()));
  let cancellation_handle =
    Task_runtime.start_task ~sw ~env ~limits:attachment_limits
      ~backend_id:cancellation_id run_task
  in
  Eio.Promise.await started;
  Task_runtime.cancel cancellation_handle;
  (match Task_runtime.await cancellation_handle with
  | Ok result ->
      Alcotest.(check bool)
        "cancellation normalized" true
        (result.Backend_types.status = Backend_types.Cancelled)
  | Error error -> Alcotest.fail (Runtime_dispatch.render_error error));
  List.iter
    (fun path ->
      Alcotest.(check bool)
        "cancelled task removes sealed file" false (Sys.file_exists path))
    !cancellation_paths;

  let fatal_id = "dispatch-sealed-fatal" in
  let fatal_paths = ref [] in
  register_pair ~id:fatal_id ~media_types:[ Backend_types.Png ]
    ~version_policy:Runtime_entry.No_version_gate
    (make_backend ~id:fatal_id ~calls:(ref 0)
       ~on_context:(capture_paths fatal_id fatal_paths)
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> raise Out_of_memory));
  let fatal_handle =
    Task_runtime.start_task ~sw ~env ~limits:attachment_limits
      ~backend_id:fatal_id run_task
  in
  (match Task_runtime.await fatal_handle with
  | exception Out_of_memory -> ()
  | exception error -> Alcotest.fail (Printexc.to_string error)
  | Ok _ -> Alcotest.fail "fatal backend unexpectedly succeeded"
  | Error error -> Alcotest.fail (Runtime_dispatch.render_error error));
  List.iter
    (fun path ->
      Alcotest.(check bool)
        "fatal task removes sealed file" false (Sys.file_exists path))
    !fatal_paths

let test_sealed_attachments_cleanup_on_backend_failure_and_timeout () =
  with_registry @@ fun () ->
  with_temp_dir "terminal-result-cleanup" @@ fun temp_dir ->
  let png = "\x89PNG\r\n\x1a\nterminal result bytes" in
  write_binary_file (Filename.concat temp_dir "image.png") png;
  let attachment =
    Backend_types.
      {
        id = "image";
        path = "image.png";
        media_type = Png;
        sha256 = Digestif.SHA256.(to_hex (digest_string png));
        size_bytes = String.length png;
      }
  in
  let attachment_limits =
    Task_preflight.
      {
        max_attachments = 1;
        max_file_size_bytes = 128;
        max_total_size_bytes = 128;
      }
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (label, status) ->
      let id = "dispatch-sealed-" ^ label in
      let captured_paths = ref [] in
      register_pair ~id ~media_types:[ Backend_types.Png ]
        ~version_policy:Runtime_entry.No_version_gate
        (make_backend ~id ~calls:(ref 0)
           ~on_context:(fun context ->
             match
               Task_execution_context.authorized_attachment_paths context
                 ~backend_id:id ~attachment_references:[ attachment ]
                 ~web_access_policy:Backend_types.Web_disabled
             with
             | Ok paths -> captured_paths := paths
             | Error message -> Alcotest.fail message)
           (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
             Backend_types.make_task_result ~status ()));
      (match
         run ~env ~sw ~backend_id:id ~limits:attachment_limits
           (spec ~working_dir:temp_dir ~attachments:[ attachment ] ())
       with
      | Ok result ->
          Alcotest.(check bool)
            (label ^ " status is preserved") true
            (result.Backend_types.status = status)
      | Error error -> Alcotest.fail (Runtime_dispatch.render_error error));
      Alcotest.(check int)
        (label ^ " captures one sealed path") 1
        (List.length !captured_paths);
      List.iter
        (fun path ->
          Alcotest.(check bool)
            (label ^ " removes sealed file") false (Sys.file_exists path))
        !captured_paths)
    [
      ("failed", Backend_types.Failed "backend failure");
      ("timeout", Backend_types.Timeout);
    ]

let test_abandoned_prepared_attachments_cleanup_on_switch_release () =
  with_registry @@ fun () ->
  with_temp_dir "abandoned-prepared" @@ fun temp_dir ->
  let png = "\x89PNG\r\n\x1a\nabandoned bytes" in
  write_binary_file (Filename.concat temp_dir "image.png") png;
  let attachment =
    Backend_types.
      {
        id = "image";
        path = "image.png";
        media_type = Png;
        sha256 = Digestif.SHA256.(to_hex (digest_string png));
        size_bytes = String.length png;
      }
  in
  let attachment_limits =
    Task_preflight.
      {
        max_attachments = 1;
        max_file_size_bytes = 128;
        max_total_size_bytes = 128;
      }
  in
  let id = "dispatch-abandoned-prepared" in
  register_pair ~id ~media_types:[ Backend_types.Png ]
    ~version_policy:Runtime_entry.No_version_gate
    (make_backend ~id ~calls:(ref 0)
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ()));
  let staged_path =
    Eio_posix.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let sink = Task_event.create_sink ~sw ~now:(fun () -> 0.0) () in
    let context =
      Task_execution_context.create ~remaining_time:(fun () -> None) sink
    in
    (match
       Runtime_dispatch.prepare ~sw ~env ~limits:attachment_limits
         ~backend_id:id ~context
         (spec ~working_dir:temp_dir ~attachments:[ attachment ] ())
     with
    | Ok _prepared -> ()
    | Error error -> Alcotest.fail (Runtime_dispatch.render_error error));
    match
      Task_execution_context.authorized_attachment_paths context ~backend_id:id
        ~attachment_references:[ attachment ]
        ~web_access_policy:Backend_types.Web_disabled
    with
    | Ok [ path ] ->
        Alcotest.(check bool)
          "sealed file exists while prepared value is live" true
          (Sys.file_exists path);
        path
    | Ok _ -> Alcotest.fail "unexpected abandoned sealed attachment set"
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check bool)
    "switch release removes abandoned sealed file" false
    (Sys.file_exists staged_path);
  Alcotest.(check bool)
    "switch release removes abandoned private directory" false
    (Sys.file_exists (Filename.dirname staged_path))

let test_by_name_wrapper_resolves_override_at_each_call () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let id = "dispatch-by-name-override" in
  let first_calls = ref 0 in
  let first_availability_calls = ref 0 in
  let second_calls = ref 0 in
  let captured = ref None in
  let first =
    make_backend ~id ~calls:first_calls
      ~availability:(fun ~sw:_ ~env:_ ->
        incr first_availability_calls;
        false)
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ~text:"first" ())
  in
  register_pair ~id first;
  let completer =
    match
      Backend_completer.make_by_name ~sw ~env ~backend_name:id ~working_dir:"."
        ()
    with
    | Ok completer -> completer
    | Error error -> Alcotest.fail error
  in
  Alcotest.(check int)
    "construction does not check availability" 0 !first_availability_calls;
  let second =
    make_backend ~id ~calls:second_calls
      (fun ~sw:_ ~env:_ ?on_raw_line:_ task ->
        captured := Some task;
        success ~text:"second" ())
  in
  register_pair ~id second;
  (match
     completer ~system_prompt:"system" ~prompt:"prompt" ~json_schema:None
       ~resume_session_id:None
   with
  | Ok result -> Alcotest.(check string) "override result" "second" result.text
  | Error error -> Alcotest.fail error);
  Alcotest.(check int) "constructed backend is not retained" 0 !first_calls;
  Alcotest.(check int)
    "unavailable construction-time backend is never probed" 0
    !first_availability_calls;
  Alcotest.(check int) "override called once" 1 !second_calls;
  match !captured with
  | None -> Alcotest.fail "override did not capture a task"
  | Some task ->
      Alcotest.(check int)
        "legacy wrapper has no attachments" 0
        (List.length task.Backend_types.attachments);
      Alcotest.(check bool)
        "legacy wrapper disables web" true
        (task.web_access = Backend_types.Web_disabled)

let test_by_name_rejects_malformed_id_without_resolution () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match
    Backend_completer.make_by_name ~sw ~env ~backend_name:"../malformed-backend"
      ~working_dir:"." ()
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "malformed static backend id must be rejected"

let test_dispatch_enforces_installed_version_policy () =
  with_registry @@ fun () ->
  with_temp_dir "versions" @@ fun temp_dir ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let run_case ~label ~version_output ~expected_ok =
    let id = "dispatch-version-" ^ label in
    let binary_name = Filename.concat temp_dir (label ^ "-backend") in
    let version_marker = Filename.concat temp_dir (label ^ "-version-probed") in
    Option.iter
      (fun output ->
        write_executable binary_name
          (Printf.sprintf
             "#!/bin/sh\nprintf 'probed\\n' >> %s\nprintf '%%s\\n' %s\n"
             (Filename.quote version_marker)
             (Filename.quote output)))
      version_output;
    let calls = ref 0 in
    let availability_calls = ref 0 in
    register_pair ~id ~binary_name ~baseline_version:"1.2.3"
      (make_backend ~id ~calls
         ~availability:(fun ~sw:_ ~env:_ ->
           Option.iter
             (fun _ ->
               Alcotest.(check bool)
                 (label ^ " version precedes availability")
                 true
                 (Sys.file_exists version_marker))
             version_output;
           incr availability_calls;
           true)
         (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
           Option.iter
             (fun _ ->
               Alcotest.(check bool)
                 (label ^ " version precedes backend")
                 true
                 (Sys.file_exists version_marker))
             version_output;
           success ()));
    let result = run ~env ~sw ~backend_id:id (spec ()) in
    Alcotest.(check bool) (label ^ " result") expected_ok (Result.is_ok result);
    Alcotest.(check bool)
      (label ^ " version process ran")
      (Option.is_some version_output)
      (Sys.file_exists version_marker);
    Alcotest.(check int)
      (label ^ " backend calls")
      (if expected_ok then 1 else 0)
      !calls;
    Alcotest.(check int)
      (label ^ " availability calls")
      (if expected_ok then 1 else 0)
      !availability_calls
  in
  run_case ~label:"below" ~version_output:(Some "1.2.2") ~expected_ok:false;
  run_case ~label:"current" ~version_output:(Some "1.2.3") ~expected_ok:true;
  run_case ~label:"above" ~version_output:(Some "2.0.0") ~expected_ok:true;
  run_case ~label:"unparseable" ~version_output:(Some "unknown version")
    ~expected_ok:true;
  run_case ~label:"missing" ~version_output:None ~expected_ok:true

let test_hardened_below_baseline_stops_before_backend () =
  with_registry @@ fun () ->
  with_temp_dir "hardened-version" @@ fun temp_dir ->
  let version_marker = Filename.concat temp_dir "version-probed" in
  let backend_marker = Filename.concat temp_dir "backend-ran" in
  let fake_claude = Filename.concat temp_dir "claude" in
  write_executable fake_claude
    (Printf.sprintf
       "#!/bin/sh\n\
        if [ \"${1-}\" = \"--version\" ]; then\n\
       \  printf 'version\\n' >> %s\n\
       \  printf '%%s\\n' '2.1.116'\n\
        else\n\
       \  printf 'backend\\n' >> %s\n\
       \  printf '%%s\\n' '{}'\n\
        fi\n"
       (Filename.quote version_marker)
       (Filename.quote backend_marker));
  with_path_prefix temp_dir @@ fun () ->
  (match
     Runtime_bootstrap.register_runtime
       ~profile:Runtime_bootstrap.Hardened_builtins ()
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error));
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (match
     run ~env ~sw ~backend_id:"claude-code" (spec ~working_dir:temp_dir ())
   with
  | Error Runtime_dispatch.Backend_version_unsupported -> ()
  | Error error ->
      Alcotest.failf "unexpected hardened version error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "below-baseline hardened backend must not run");
  Alcotest.(check bool)
    "hardened version gate ran" true
    (Sys.file_exists version_marker);
  Alcotest.(check bool)
    "hardened backend did not run" false
    (Sys.file_exists backend_marker)

let test_hardened_codex_dispatch_uses_proven_media_web_transport () =
  with_registry @@ fun () ->
  with_temp_dir "codex-media-web" @@ fun temp_dir ->
  let version_marker = Filename.concat temp_dir "codex-version-ran" in
  let backend_marker = Filename.concat temp_dir "codex-backend-ran" in
  let argv_marker = Filename.concat temp_dir "codex-argv" in
  let uploaded_marker = Filename.concat temp_dir "codex-uploaded-bytes" in
  let front_path = Filename.concat temp_dir "front image.png" in
  let moved_front_path = Filename.concat temp_dir "front-original.png" in
  let back_path = Filename.concat temp_dir "back.jpg" in
  let outside_replacement =
    Filename.concat temp_dir "outside-replacement.png"
  in
  let fake_codex = Filename.concat temp_dir "codex" in
  write_executable fake_codex
    (Printf.sprintf
       {|#!/bin/sh
if [ "${1-}" = "--version" ]; then
  printf 'version\n' >> %s
  mv -- %s %s
  ln -s -- %s %s
  rm -- %s
  printf '%%s\n' 'codex-cli 0.131.0'
  exit 0
fi
printf 'backend\n' >> %s
: > %s
: > %s
expect_image=0
for arg in "$@"; do
  printf '%%s\n' "$arg" >> %s
  if [ "$expect_image" -eq 1 ]; then
    cat -- "$arg" >> %s
    expect_image=0
  elif [ "$arg" = "-i" ]; then
    expect_image=1
  fi
done
printf '%%s\n' '{"type":"thread.started","thread_id":"thread-123"}'
printf '%%s\n' '{"type":"item.completed","item":{"type":"agent_message","text":"ok"}}'
printf '%%s\n' '{"type":"turn.completed","usage":{"input_tokens":2,"cached_input_tokens":1,"output_tokens":1}}'
|}
       (Filename.quote version_marker)
       (Filename.quote front_path)
       (Filename.quote moved_front_path)
       (Filename.quote outside_replacement)
       (Filename.quote front_path)
       (Filename.quote back_path)
       (Filename.quote backend_marker)
       (Filename.quote argv_marker)
       (Filename.quote uploaded_marker)
       (Filename.quote argv_marker)
       (Filename.quote uploaded_marker));
  with_path_prefix temp_dir @@ fun () ->
  (match
     Runtime_bootstrap.register_runtime
       ~profile:Runtime_bootstrap.Hardened_builtins ()
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error));
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let limits =
    Task_preflight.
      {
        max_attachments = 2;
        max_file_size_bytes = 16;
        max_total_size_bytes = 32;
      }
  in
  let bad_bytes = "not-a-png" in
  write_binary_file (Filename.concat temp_dir "bad.png") bad_bytes;
  let bad_attachment =
    Backend_types.
      {
        id = "bad";
        path = "bad.png";
        media_type = Png;
        sha256 =
          "f6340893e73ce5f7c019eeebfc6d38224824f8f7565b421b51d54c1c0d25d5c6";
        size_bytes = String.length bad_bytes;
      }
  in
  (match
     run ~env ~sw ~backend_id:"codex" ~limits
       (spec ~working_dir:temp_dir ~attachments:[ bad_attachment ] ())
   with
  | Error
      (Runtime_dispatch.Preflight_failed
         (Task_preflight.Input (Task_preflight.Media_type_mismatch _))) ->
      ()
  | Error error ->
      Alcotest.failf "unexpected invalid-media error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "invalid media must fail");
  Alcotest.(check bool)
    "rejected media creates no Codex project config" false
    (Sys.file_exists (Filename.concat temp_dir ".codex/config.toml"));
  Alcotest.(check bool)
    "rejected media runs no version process" false
    (Sys.file_exists version_marker);
  Alcotest.(check bool)
    "rejected media runs no backend process" false
    (Sys.file_exists backend_marker);

  let png = "\x89PNG\r\n\x1a\n" in
  let jpeg = "\xff\xd8\xff" in
  write_binary_file front_path png;
  write_binary_file back_path jpeg;
  write_binary_file outside_replacement (png ^ "outside replacement");
  let attachments =
    Backend_types.
      [
        {
          id = "front";
          path = "front image.png";
          media_type = Png;
          sha256 =
            "4c4b6a3be1314ab86138bef4314dde022e600960d8689a2c8f8631802d20dab6";
          size_bytes = String.length png;
        };
        {
          id = "back";
          path = "back.jpg";
          media_type = Jpeg;
          sha256 =
            "6e568e1f67fba258184c78181539e5e8fdee447e49bb706fc0ea34fbf12336a5";
          size_bytes = String.length jpeg;
        };
      ]
  in
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ("properties", `Assoc [ ("ok", `Assoc [ ("type", `String "boolean") ]) ]);
      ]
  in
  (match
     run ~env ~sw ~backend_id:"codex" ~limits
       (spec ~working_dir:temp_dir ~attachments
          ~web_access:Backend_types.Web_search_and_fetch ~json_schema:schema ())
   with
  | Ok result ->
      Alcotest.(check bool)
        "proven Codex transport succeeds" true
        (result.Backend_types.status = Backend_types.Success);
      Alcotest.(check string) "strict agent text" "ok" result.agent_text
  | Error error -> Alcotest.fail (Runtime_dispatch.render_error error));
  Alcotest.(check bool)
    "accepted input reaches version gate" true
    (Sys.file_exists version_marker);
  Alcotest.(check bool)
    "accepted input reaches Codex adapter process" true
    (Sys.file_exists backend_marker);
  Alcotest.(check bool)
    "accepted input creates managed Codex project config" true
    (Sys.file_exists (Filename.concat temp_dir ".codex/config.toml"));
  let argv = read_file argv_marker in
  let argv_lines = String.split_on_char '\n' argv in
  Alcotest.(check int)
    "requested upload intent emits two image flags" 2
    (List.length (List.filter (String.equal "-i") argv_lines));
  let rec image_paths = function
    | "-i" :: path :: rest -> path :: image_paths rest
    | _ :: rest -> image_paths rest
    | [] -> []
  in
  let staged_paths = image_paths argv_lines in
  Alcotest.(check int) "two sealed image paths" 2 (List.length staged_paths);
  Alcotest.(check string)
    "backend receives the bytes validated before namespace replacement"
    (png ^ jpeg)
    (read_file uploaded_marker);
  List.iter
    (fun path ->
      Alcotest.(check bool)
        "Codex receives an absolute sealed path" false
        (Filename.is_relative path);
      Alcotest.(check bool)
        "sealed path is outside the mutable workspace" false
        (String.starts_with ~prefix:(temp_dir ^ Filename.dir_sep) path);
      Alcotest.(check bool)
        "sealed file is cleaned after process completion" false
        (Sys.file_exists path))
    staged_paths;
  (match staged_paths with
  | [ png_path; jpeg_path ] ->
      Alcotest.(check bool)
        "sealed PNG extension" true
        (String.ends_with ~suffix:".png" png_path);
      Alcotest.(check bool)
        "sealed JPEG extension" true
        (String.ends_with ~suffix:".jpg" jpeg_path);
      Alcotest.(check string)
        "one private task directory"
        (Filename.dirname png_path)
        (Filename.dirname jpeg_path);
      Alcotest.(check bool)
        "private task directory cleaned" false
        (Sys.file_exists (Filename.dirname png_path))
  | _ -> Alcotest.fail "unexpected image argv");
  List.iter
    (fun unexpected ->
      Alcotest.(check bool)
        ("Codex argv omits mutable path " ^ unexpected)
        false (contains argv unexpected))
    [ "front image.png"; "back.jpg"; "outside-replacement.png" ];
  List.iter
    (fun expected ->
      Alcotest.(check bool)
        ("Codex argv contains " ^ expected)
        true (contains argv expected))
    [ {|web_search="live"|}; "--output-schema" ]

let test_hardened_codex_rejects_pre_proof_version () =
  with_registry @@ fun () ->
  with_temp_dir "codex-baseline" @@ fun temp_dir ->
  let version_marker = Filename.concat temp_dir "codex-version-ran" in
  let backend_marker = Filename.concat temp_dir "codex-backend-ran" in
  let fake_codex = Filename.concat temp_dir "codex" in
  write_executable fake_codex
    (Printf.sprintf
       {|#!/bin/sh
if [ "${1-}" = "--version" ]; then
  printf 'version\n' >> %s
  printf '%%s\n' 'codex-cli 0.130.0'
  exit 0
fi
printf 'backend\n' >> %s
printf '%%s\n' '{"type":"item.completed","item":{"type":"agent_message","text":"unexpected"}}'
|}
       (Filename.quote version_marker)
       (Filename.quote backend_marker));
  with_path_prefix temp_dir @@ fun () ->
  (match
     Runtime_bootstrap.register_runtime
       ~profile:Runtime_bootstrap.Hardened_builtins ()
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error));
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (match run ~env ~sw ~backend_id:"codex" (spec ~working_dir:temp_dir ()) with
  | Error Runtime_dispatch.Backend_version_unsupported -> ()
  | Error error ->
      Alcotest.failf "unexpected Codex version error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "Codex 0.130.0 must be below the proven baseline");
  Alcotest.(check bool)
    "pre-proof Codex version is probed" true
    (Sys.file_exists version_marker);
  Alcotest.(check bool)
    "pre-proof Codex version does not execute adapter" false
    (Sys.file_exists backend_marker);
  Alcotest.(check bool)
    "pre-proof Codex version creates no project config" false
    (Sys.file_exists (Filename.concat temp_dir ".codex/config.toml"))

let test_dispatch_sanitizes_backend_exceptions () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let assert_sanitized label result =
    match result with
    | Ok _ -> Alcotest.failf "%s exception must become a dispatch error" label
    | Error error ->
        let rendered = Runtime_dispatch.render_error error in
        Alcotest.(check bool)
          (label ^ " secret removed")
          false
          (contains rendered "dispatch-secret");
        Alcotest.(check bool)
          (label ^ " path removed") false
          (contains rendered "/private/dispatch-secret")
  in
  let availability_id = "dispatch-availability-exception" in
  let availability_calls = ref 0 in
  let availability_backend =
    make_backend ~id:availability_id ~calls:(ref 0)
      ~availability:(fun ~sw:_ ~env:_ ->
        incr availability_calls;
        failwith "dispatch-secret /private/dispatch-secret")
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())
  in
  register_pair ~id:availability_id availability_backend;
  assert_sanitized "availability"
    (run ~env ~sw ~backend_id:availability_id (spec ()));
  Alcotest.(check int) "availability attempted once" 1 !availability_calls;

  let run_id = "dispatch-run-exception" in
  let run_calls = ref 0 in
  register_pair ~id:run_id
    (make_backend ~id:run_id ~calls:run_calls
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
         failwith "dispatch-secret /private/dispatch-secret"));
  assert_sanitized "run_task" (run ~env ~sw ~backend_id:run_id (spec ()));
  Alcotest.(check int) "run attempted once" 1 !run_calls

let test_dispatch_reraises_fatal_exceptions () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fatal_cases =
    [
      ("out-of-memory", Out_of_memory);
      ("stack-overflow", Stack_overflow);
      ("sys-break", Sys.Break);
    ]
  in
  let same_fatal expected actual =
    match (expected, actual) with
    | Out_of_memory, Out_of_memory
    | Stack_overflow, Stack_overflow
    | Sys.Break, Sys.Break ->
        true
    | _ -> false
  in
  let expect_fatal label expected backend_id =
    let events = ref [] in
    let handle =
      Task_runtime.start_task ~sw ~env ~limits ~backend_id
        ~on_event:(fun event -> events := event :: !events)
        (spec ())
    in
    (match Task_runtime.await handle with
    | exception actual when same_fatal expected actual -> ()
    | exception actual ->
        Alcotest.failf "%s raised the wrong exception: %s" label
          (Printexc.to_string actual)
    | Ok _ -> Alcotest.failf "%s unexpectedly succeeded" label
    | Error error ->
        Alcotest.failf "%s was sanitized as %s" label
          (Runtime_dispatch.render_error error));
    Task_runtime.await_event_delivery handle;
    let terminals =
      List.filter_map
        (fun event ->
          match event.Task_event.payload with
          | Task_event.Terminal terminal -> Some terminal
          | _ -> None)
        !events
    in
    Alcotest.(check int) (label ^ " has one terminal") 1 (List.length terminals);
    Alcotest.(check bool)
      (label ^ " terminal is generic and redacted")
      true
      (match terminals with
      | [ Task_event.Failed reason ] ->
          reason
          = Backend_event_redaction.redact_error_message
              "backend execution failed"
      | _ -> false)
  in
  List.iter
    (fun (label, fatal) ->
      let availability_id = "dispatch-fatal-availability-" ^ label in
      register_pair ~id:availability_id
        (make_backend ~id:availability_id ~calls:(ref 0)
           ~availability:(fun ~sw:_ ~env:_ -> raise fatal)
           (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ()));
      expect_fatal (label ^ " availability") fatal availability_id;

      let execution_id = "dispatch-fatal-execution-" ^ label in
      register_pair ~id:execution_id
        (make_backend ~id:execution_id ~calls:(ref 0)
           (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> raise fatal));
      expect_fatal (label ^ " execution") fatal execution_id)
    fatal_cases

let test_dispatch_normalizes_cancellation () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let id = "dispatch-cancellation" in
  register_pair ~id
    (make_backend ~id ~calls:(ref 0)
       ~availability:(fun ~sw:_ ~env:_ ->
         raise (Eio.Cancel.Cancelled (Failure "dispatch-secret")))
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ()));
  match run ~env ~sw ~backend_id:id (spec ()) with
  | exception error ->
      Alcotest.failf "unexpected exception: %s" (Printexc.to_string error)
  | Ok result ->
      Alcotest.(check bool)
        "cancellation normalized" true
        (result.Backend_types.status = Backend_types.Cancelled)
  | Error error ->
      Alcotest.failf "cancellation was converted to %s"
        (Runtime_dispatch.render_error error)

let test_prepared_no_context_detailed_projection_matches_legacy () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let id = "dispatch-prepared-no-context" in
  let calls = ref 0 in
  let backend =
    make_backend ~id ~calls (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
        if !calls mod 2 = 1 then success ~text:"not-json" ()
        else Backend_types.make_task_result ~status:Backend_types.Timeout ())
  in
  register_pair ~id ~version_policy:Runtime_entry.No_version_gate backend;
  let task = spec ~json_schema:(`Assoc [ ("type", `String "object") ]) () in
  let prepared =
    match Runtime_dispatch.prepare ~sw ~env ~limits ~backend_id:id task with
    | Ok prepared -> prepared
    | Error error -> Alcotest.fail (Runtime_dispatch.render_error error)
  in
  let detailed = Runtime_dispatch.execute_prepared_detailed ~sw ~env prepared in
  let projected =
    Runtime_dispatch.Private.project_prepared_detailed_outcome detailed
  in
  let legacy = Runtime_dispatch.execute_prepared ~sw ~env prepared in
  Alcotest.(check int) "both executions make two calls" 4 !calls;
  Alcotest.(check bool)
    "detailed result records completed timeout retry" true
    (match detailed with
    | Error
        (Runtime_dispatch.Execution_failure
           (Backend_types.Schema_retry_failed { execution; _ })) ->
        List.length execution.attempts = 2
        && execution.final_result.status = Backend_types.Timeout
    | Error
        ( Runtime_dispatch.Dispatch_failure _
        | Runtime_dispatch.Dispatch_failure_with_execution _ )
    | Error
        (Runtime_dispatch.Execution_failure
           (Backend_types.Native_backend_failure_with_schema _))
    | Ok _ ->
        false);
  Alcotest.(check bool)
    "no-context projection remains an error" true
    (Result.is_error projected);
  Alcotest.(check bool)
    "prepared detailed projection matches legacy" true (projected = legacy)

let test_validator_wrapper_dispatches_read_only_task () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let id = "dispatch-validator-wrapper" in
  let calls = ref 0 in
  let captured = ref None in
  let backend =
    make_backend ~id ~calls (fun ~sw:_ ~env:_ ?on_raw_line:_ task ->
        captured := Some task;
        success ())
  in
  (match
     Runtime_bootstrap.register_custom
       ~descriptor:(descriptor_for ~read_only:true id)
       ~backend
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error));
  let completer =
    match
      Backend_completer.make_validator_by_name ~sw ~env ~backend_name:id
        ~working_dir:"." ()
    with
    | Ok completer -> completer
    | Error error -> Alcotest.fail error
  in
  (match
     completer ~system_prompt:"system" ~prompt:"prompt" ~json_schema:None
       ~resume_session_id:None
   with
  | Ok _ -> ()
  | Error error -> Alcotest.fail error);
  Alcotest.(check int) "validator backend called" 1 !calls;
  match !captured with
  | Some task ->
      Alcotest.(check bool) "validator task is read-only" true task.read_only
  | None -> Alcotest.fail "validator task was not captured"

let () =
  Alcotest.run "runtime_dispatch"
    [
      ( "preflight",
        [
          Alcotest.test_case "missing and raw registrations fail before call"
            `Quick test_missing_and_raw_registrations_fail_before_call;
          Alcotest.test_case "runtime/descriptor mismatches cannot register"
            `Quick test_runtime_descriptor_mismatches_cannot_be_registered;
          Alcotest.test_case "raw Claude override invalidates validated pairing"
            `Quick test_raw_claude_override_invalidates_validated_pair;
          Alcotest.test_case "invalid preflight never spawns or calls" `Quick
            test_invalid_preflight_never_spawns_or_calls_backend;
          Alcotest.test_case
            "staging failure never checks availability or calls" `Quick
            test_staging_failure_never_checks_availability_or_calls_backend;
          Alcotest.test_case "hardened Codex uses proven media/web transport"
            `Quick test_hardened_codex_dispatch_uses_proven_media_web_transport;
        ] );
      ( "resolution",
        [
          Alcotest.test_case "schema retry keeps resolved backend snapshot"
            `Quick test_enforcer_uses_resolved_backend_snapshot_for_retry;
          Alcotest.test_case "dispatch context exposes requested delivery"
            `Quick test_dispatch_context_exposes_requested_delivery;
          Alcotest.test_case
            "dispatch reuses one sealed attachment set across retry" `Quick
            test_dispatch_reuses_one_sealed_attachment_set_across_retry;
          Alcotest.test_case
            "dispatch sanitizes cleanup failure and retries on release" `Quick
            test_dispatch_sanitizes_cleanup_failure_and_retries_on_switch_release;
          Alcotest.test_case
             "sealed attachments clean up on cancellation and fatal exception"
             `Quick
             test_sealed_attachments_cleanup_on_cancellation_and_fatal_exception;
          Alcotest.test_case
            "sealed attachments clean up on backend failure and timeout" `Quick
            test_sealed_attachments_cleanup_on_backend_failure_and_timeout;
          Alcotest.test_case
            "abandoned prepared attachments clean up on switch release" `Quick
            test_abandoned_prepared_attachments_cleanup_on_switch_release;
          Alcotest.test_case "by-name wrapper resolves call-time override"
            `Quick test_by_name_wrapper_resolves_override_at_each_call;
          Alcotest.test_case "by-name wrapper rejects malformed static id"
            `Quick test_by_name_rejects_malformed_id_without_resolution;
          Alcotest.test_case "installed version policy is enforced" `Quick
            test_dispatch_enforces_installed_version_policy;
          Alcotest.test_case "hardened below-baseline backend is blocked" `Quick
            test_hardened_below_baseline_stops_before_backend;
          Alcotest.test_case
            "Codex rejects versions below authenticated baseline" `Quick
            test_hardened_codex_rejects_pre_proof_version;
          Alcotest.test_case "ordinary backend exceptions are sanitized" `Quick
            test_dispatch_sanitizes_backend_exceptions;
          Alcotest.test_case "fatal backend exceptions are re-raised" `Quick
            test_dispatch_reraises_fatal_exceptions;
          Alcotest.test_case "cancellation is normalized" `Quick
            test_dispatch_normalizes_cancellation;
          Alcotest.test_case "prepared no-context projection" `Quick
            test_prepared_no_context_detailed_projection_matches_legacy;
          Alcotest.test_case "validator wrapper dispatches read-only" `Quick
            test_validator_wrapper_dispatches_read_only_task;
        ] );
    ]
