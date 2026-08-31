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

let limits =
  Task_preflight.
    {
      max_attachments = 0;
      max_file_size_bytes = 0;
      max_total_size_bytes = 0;
    }

let descriptor_for ?(session_resume = false) ?(native = false)
    ?(read_only = false) ?(binary_name = "dispatch-test")
    ?(baseline_version = "1.0.0") id =
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
        media_support = {media_types = []; evidence = None};
        web_support =
          {maximum = Backend_types.Web_disabled; evidence = None};
        native_json_schema_output = native;
        native_json_schema_output_evidence;
      };
  }

let success ?(text = "ok") () =
  Backend_types.make_task_result
    ~status:Backend_types.Success
    ~agent_text:text
    ()

let make_backend ?(session_resume = false) ?(native = false) ?availability ~id
    ~calls run =
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

    let run_task ~sw ~env ?context:_ ?on_raw_line spec =
      incr calls ;
      run ~sw ~env ?on_raw_line spec
  end in
  (module Backend : Agentic_backend.S)

let with_registry f =
  Registry.clear () ;
  Fun.protect ~finally:Registry.clear f

let register_pair ?session_resume ?native ?read_only ?binary_name
    ?baseline_version ?(origin = Runtime_entry.Custom)
    ?(version_policy = Runtime_entry.Enforce_baseline) ~id backend =
  let descriptor =
    descriptor_for
      ?session_resume
      ?native
      ?read_only
      ?binary_name
      ?baseline_version
      id
  in
  let entry =
    match
      Runtime_entry.create
        ~backend
        ~descriptor
        ~runtime_capabilities:descriptor.capabilities
        ~origin
        ~version_policy
    with
    | Ok entry -> entry
    | Error error ->
        Alcotest.fail (Runtime_entry.render_validation_error error)
  in
  Registry.register_validated entry

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter (fun name -> remove_tree (Filename.concat path name)) (Sys.readdir path) ;
      Unix.rmdir path
    end
    else Unix.unlink path

let with_temp_dir label f =
  let path = Filename.temp_dir ("cabal-dispatch-" ^ label ^ "-") "" in
  Fun.protect
    ~finally:(fun () -> remove_tree path)
    (fun () -> f path)

let with_path_prefix directory f =
  let previous = Sys.getenv_opt "PATH" in
  let path =
    match previous with
    | Some value when value <> "" -> directory ^ ":" ^ value
    | _ -> directory
  in
  Unix.putenv "PATH" path ;
  Fun.protect
    ~finally:(fun () -> Unix.putenv "PATH" (Option.value ~default:"" previous))
    f

let write_executable path contents =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents) ;
  Unix.chmod path 0o700

let spec ?(working_dir = ".") ?(read_only = false) ?resume_session_id
    ?json_schema ?(attachments = []) ?(web_access = Backend_types.Web_disabled)
    () =
  Backend_types.make_task_spec
    ~prompt:"dispatch test"
    ~working_dir
    ~read_only
    ?resume_session_id
    ?json_schema
    ~attachments
    ~web_access
    ()

let run ~env ~sw ~backend_id ?(limits = limits) spec =
  Runtime_dispatch.run_task ~sw ~env ~limits ~backend_id spec

let test_missing_and_raw_registrations_fail_before_call () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let runtime_missing_id = "dispatch-runtime-missing" in
  Backend_registry.register_descriptor (descriptor_for runtime_missing_id) ;
  (match run ~env ~sw ~backend_id:runtime_missing_id (spec ()) with
  | Error Runtime_dispatch.Backend_not_registered -> ()
  | Error error ->
      Alcotest.failf
        "unexpected missing-runtime error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "missing runtime must fail") ;
  let raw_id = "dispatch-raw-runtime" in
  Registry.register
    (make_backend
       ~id:raw_id
       ~calls
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())) ;
  (match run ~env ~sw ~backend_id:raw_id (spec ()) with
  | Error Runtime_dispatch.Runtime_registration_untrusted -> ()
  | Error error ->
      Alcotest.failf
        "unexpected raw-registration error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "raw registration must fail") ;
  Alcotest.(check int) "backend never called" 0 !calls

let test_runtime_descriptor_mismatches_cannot_be_registered () =
  with_registry @@ fun () ->
  let session_calls = ref 0 in
  let availability_calls = ref 0 in
  let session_id = "dispatch-session-mismatch" in
  let session_descriptor = descriptor_for ~session_resume:true session_id in
  let session_backend =
    make_backend
      ~id:session_id
      ~calls:session_calls
      ~availability:(fun ~sw:_ ~env:_ ->
        incr availability_calls ;
        failwith "availability must not run for a mismatched runtime")
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())
  in
  Alcotest.(check bool)
    "session mismatch cannot produce a validated entry"
    true
    (Result.is_error
       (Runtime_entry.create
          ~backend:session_backend
          ~descriptor:session_descriptor
          ~runtime_capabilities:session_descriptor.capabilities
          ~origin:Runtime_entry.Custom
          ~version_policy:Runtime_entry.Enforce_baseline)) ;
  Alcotest.(check bool)
    "session mismatch leaves registry empty"
    true
    (Option.is_none (Registry.find_entry session_id)) ;
  Alcotest.(check int) "session mismatch call count" 0 !session_calls ;
  Alcotest.(check int)
    "mismatch rejected before availability"
    0
    !availability_calls ;

  let native_calls = ref 0 in
  let native_id = "dispatch-native-mismatch" in
  let native_descriptor = descriptor_for ~native:true native_id in
  let native_backend =
    make_backend
      ~id:native_id
      ~calls:native_calls
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())
  in
  Alcotest.(check bool)
    "native mismatch cannot produce a validated entry"
    true
    (Result.is_error
       (Runtime_entry.create
          ~backend:native_backend
          ~descriptor:native_descriptor
          ~runtime_capabilities:native_descriptor.capabilities
          ~origin:Runtime_entry.Custom
          ~version_policy:Runtime_entry.Enforce_baseline)) ;
  Alcotest.(check int) "native mismatch call count" 0 !native_calls

let test_raw_claude_override_invalidates_validated_pair () =
  with_registry @@ fun () ->
  with_temp_dir "raw-claude-override" @@ fun temp_dir ->
  let version_marker = Filename.concat temp_dir "version-process-ran" in
  let fake_claude = Filename.concat temp_dir "claude" in
  write_executable
    fake_claude
    (Printf.sprintf
       "#!/bin/sh\nprintf 'spawned\\n' >> %s\nprintf '%%s\\n' '99.0.0'\n"
       (Filename.quote version_marker)) ;
  with_path_prefix temp_dir @@ fun () ->
  (match
     Runtime_bootstrap.register_runtime
       ~profile:Runtime_bootstrap.Hardened_builtins
       ()
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error)) ;
  let static_claude =
    match Backend_registry.find "claude-code" with
    | Some descriptor -> descriptor
    | None -> Alcotest.fail "static Claude descriptor missing"
  in
  Alcotest.(check bool)
    "attack precondition has positive read-only claim"
    true
    static_claude.capabilities.read_only_support ;
  Alcotest.(check bool)
    "attack precondition has positive file-reading claim"
    true
    static_claude.capabilities.file_reading ;
  let availability_calls = ref 0 in
  let backend_calls = ref 0 in
  Registry.register
    (make_backend
       ~session_resume:true
       ~native:true
       ~id:"claude-code"
       ~calls:backend_calls
       ~availability:(fun ~sw:_ ~env:_ ->
         incr availability_calls ;
         true)
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())) ;
  (match Registry.find_entry "claude-code" with
  | Some (Registry.Raw _) -> ()
  | Some (Registry.Validated _) ->
      Alcotest.fail "raw override retained the validated Claude pairing"
  | None -> Alcotest.fail "raw override was not registered") ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (match run ~env ~sw ~backend_id:"claude-code" (spec ~read_only:true ()) with
  | Error Runtime_dispatch.Runtime_registration_untrusted -> ()
  | Error error ->
      Alcotest.failf
        "unexpected raw-override error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "raw same-id override inherited trusted capabilities") ;
  Alcotest.(check bool)
    "raw override rejected before version process"
    false
    (Sys.file_exists version_marker) ;
  Alcotest.(check int) "raw override availability calls" 0 !availability_calls ;
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
  write_executable
    binary_name
    (Printf.sprintf
       "#!/bin/sh\nprintf 'probed\\n' >> %s\nprintf '%%s\\n' '1.0.0'\n"
       (Filename.quote version_marker)) ;
  register_pair
    ~binary_name
    ~id
    (make_backend
       ~id
       ~calls
       ~availability:(fun ~sw:_ ~env:_ ->
         incr availability_calls ;
         true)
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())) ;
  let check_no_side_effects label =
    Alcotest.(check bool)
      (label ^ " version process marker")
      false
      (Sys.file_exists version_marker) ;
    Alcotest.(check int)
      (label ^ " availability call count")
      0
      !availability_calls ;
    Alcotest.(check int) (label ^ " backend call count") 0 !calls
  in
  let invalid_limits = {limits with max_attachments = -1} in
  let result =
    run
      ~env
      ~sw
      ~backend_id:id
      ~limits:invalid_limits
      (spec ~working_dir:"/private/limit-secret" ())
  in
  (match result with
  | Error
      ((Runtime_dispatch.Preflight_failed
          (Task_preflight.Input
            (Task_preflight.Negative_limit Task_preflight.Max_attachments))) as
       error) ->
      let diagnostic = Runtime_dispatch.render_error error in
      Alcotest.(check bool)
        "limit diagnostic is sanitized"
        false
        (contains diagnostic "limit-secret")
  | Error error ->
      Alcotest.failf
        "unexpected invalid-limit error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "negative limits must fail") ;
  check_no_side_effects "invalid limits" ;

  let missing_workspace = Filename.concat temp_dir "missing-workspace" in
  (match run ~env ~sw ~backend_id:id (spec ~working_dir:missing_workspace ()) with
  | Error
      (Runtime_dispatch.Preflight_failed
        (Task_preflight.Input Task_preflight.Workspace_unavailable)) ->
      ()
  | Error error ->
      Alcotest.failf
        "unexpected workspace error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "missing workspace must fail") ;
  check_no_side_effects "invalid workspace" ;

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
      {max_attachments = 1; max_file_size_bytes = 1; max_total_size_bytes = 1}
  in
  let result =
    run
      ~env
      ~sw
      ~backend_id:id
      ~limits:attachment_limits
      (spec ~attachments:[attachment] ())
  in
  (match result with
  | Error
      ((Runtime_dispatch.Preflight_failed
          (Task_preflight.Input (Task_preflight.Absolute_attachment_path _))) as
       error) ->
      let diagnostic = Runtime_dispatch.render_error error in
      Alcotest.(check bool)
        "attachment path is absent"
        false
        (contains diagnostic "raw-secret") ;
      Alcotest.(check bool)
        "attachment id is absent"
        false
        (contains diagnostic "secret-id")
  | Error error ->
      Alcotest.failf
        "unexpected invalid-input error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "absolute attachment path must fail") ;
  check_no_side_effects "invalid input" ;

  (match run ~env ~sw ~backend_id:id (spec ~read_only:true ()) with
  | Error
      (Runtime_dispatch.Preflight_failed
        (Task_preflight.Capability Task_preflight.Read_only_unsupported)) ->
      ()
  | Error error ->
      Alcotest.failf
        "unexpected capability error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "unsupported capability must fail") ;
  check_no_side_effects "invalid capability"

let test_enforcer_uses_resolved_backend_snapshot_for_retry () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let id = "dispatch-snapshot" in
  let first_calls = ref 0 in
  let replacement_calls = ref 0 in
  let replacement =
    make_backend
      ~id
      ~calls:replacement_calls
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ~text:{|{"value":"wrong"}|} ())
  in
  let first =
    make_backend ~id ~calls:first_calls (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
        if !first_calls = 1 then begin
          Registry.register replacement ;
          success ~text:"not-json" ()
        end
        else success ~text:{|{"value":"ok"}|} ())
  in
  register_pair ~id first ;
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [("value", `Assoc [("type", `String "string")])] );
        ("required", `List [`String "value"]);
      ]
  in
  let result = run ~env ~sw ~backend_id:id (spec ~json_schema:schema ()) in
  (match result with
  | Ok result ->
      Alcotest.(check string)
        "retry result comes from initial snapshot"
        {|{"value":"ok"}|}
        result.Backend_types.agent_text
  | Error error -> Alcotest.fail (Runtime_dispatch.render_error error)) ;
  Alcotest.(check int) "initial backend handles both attempts" 2 !first_calls ;
  Alcotest.(check int) "replacement is not used in flight" 0 !replacement_calls

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
    make_backend
      ~id
      ~calls:first_calls
      ~availability:(fun ~sw:_ ~env:_ ->
        incr first_availability_calls ;
        false)
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ~text:"first" ())
  in
  register_pair ~id first ;
  let completer =
    match
      Backend_completer.make_by_name
        ~sw
        ~env
        ~backend_name:id
        ~working_dir:"."
        ()
    with
    | Ok completer -> completer
    | Error error -> Alcotest.fail error
  in
  Alcotest.(check int)
    "construction does not check availability"
    0
    !first_availability_calls ;
  let second =
    make_backend ~id ~calls:second_calls (fun ~sw:_ ~env:_ ?on_raw_line:_ task ->
        captured := Some task ;
        success ~text:"second" ())
  in
  register_pair ~id second ;
  (match
     completer
       ~system_prompt:"system"
       ~prompt:"prompt"
       ~json_schema:None
       ~resume_session_id:None
   with
  | Ok result -> Alcotest.(check string) "override result" "second" result.text
  | Error error -> Alcotest.fail error) ;
  Alcotest.(check int) "constructed backend is not retained" 0 !first_calls ;
  Alcotest.(check int)
    "unavailable construction-time backend is never probed"
    0
    !first_availability_calls ;
  Alcotest.(check int) "override called once" 1 !second_calls ;
  match !captured with
  | None -> Alcotest.fail "override did not capture a task"
  | Some task ->
      Alcotest.(check int)
        "legacy wrapper has no attachments"
        0
        (List.length task.Backend_types.attachments) ;
      Alcotest.(check bool)
        "legacy wrapper disables web"
        true
        (task.web_access = Backend_types.Web_disabled)

let test_by_name_rejects_malformed_id_without_resolution () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match
    Backend_completer.make_by_name
      ~sw
      ~env
      ~backend_name:"../malformed-backend"
      ~working_dir:"."
      ()
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
        write_executable
          binary_name
          (Printf.sprintf
             "#!/bin/sh\nprintf 'probed\\n' >> %s\nprintf '%%s\\n' %s\n"
             (Filename.quote version_marker)
             (Filename.quote output)))
      version_output ;
    let calls = ref 0 in
    let availability_calls = ref 0 in
    register_pair
      ~id
      ~binary_name
      ~baseline_version:"1.2.3"
      (make_backend
         ~id
         ~calls
         ~availability:(fun ~sw:_ ~env:_ ->
           Option.iter
             (fun _ ->
               Alcotest.(check bool)
                 (label ^ " version precedes availability")
                 true
                 (Sys.file_exists version_marker))
             version_output ;
           incr availability_calls ;
           true)
         (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
           Option.iter
             (fun _ ->
               Alcotest.(check bool)
                 (label ^ " version precedes backend")
                 true
                 (Sys.file_exists version_marker))
             version_output ;
           success ())) ;
    let result = run ~env ~sw ~backend_id:id (spec ()) in
    Alcotest.(check bool) (label ^ " result") expected_ok (Result.is_ok result) ;
    Alcotest.(check bool)
      (label ^ " version process ran")
      (Option.is_some version_output)
      (Sys.file_exists version_marker) ;
    Alcotest.(check int)
      (label ^ " backend calls")
      (if expected_ok then 1 else 0)
      !calls ;
    Alcotest.(check int)
      (label ^ " availability calls")
      (if expected_ok then 1 else 0)
      !availability_calls
  in
  run_case
    ~label:"below"
    ~version_output:(Some "1.2.2")
    ~expected_ok:false ;
  run_case
    ~label:"current"
    ~version_output:(Some "1.2.3")
    ~expected_ok:true ;
  run_case
    ~label:"above"
    ~version_output:(Some "2.0.0")
    ~expected_ok:true ;
  run_case
    ~label:"unparseable"
    ~version_output:(Some "unknown version")
    ~expected_ok:true ;
  run_case ~label:"missing" ~version_output:None ~expected_ok:true

let test_hardened_below_baseline_stops_before_backend () =
  with_registry @@ fun () ->
  with_temp_dir "hardened-version" @@ fun temp_dir ->
  let version_marker = Filename.concat temp_dir "version-probed" in
  let backend_marker = Filename.concat temp_dir "backend-ran" in
  let fake_claude = Filename.concat temp_dir "claude" in
  write_executable
    fake_claude
    (Printf.sprintf
       "#!/bin/sh\nif [ \"${1-}\" = \"--version\" ]; then\n  printf 'version\\n' >> %s\n  printf '%%s\\n' '2.1.116'\nelse\n  printf 'backend\\n' >> %s\n  printf '%%s\\n' '{}'\nfi\n"
       (Filename.quote version_marker)
       (Filename.quote backend_marker)) ;
  with_path_prefix temp_dir @@ fun () ->
  (match
     Runtime_bootstrap.register_runtime
       ~profile:Runtime_bootstrap.Hardened_builtins
       ()
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error)) ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (match
     run
       ~env
       ~sw
       ~backend_id:"claude-code"
       (spec ~working_dir:temp_dir ())
   with
  | Error Runtime_dispatch.Backend_version_unsupported -> ()
  | Error error ->
      Alcotest.failf
        "unexpected hardened version error: %s"
        (Runtime_dispatch.render_error error)
  | Ok _ -> Alcotest.fail "below-baseline hardened backend must not run") ;
  Alcotest.(check bool)
    "hardened version gate ran"
    true
    (Sys.file_exists version_marker) ;
  Alcotest.(check bool)
    "hardened backend did not run"
    false
    (Sys.file_exists backend_marker)

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
          (contains rendered "dispatch-secret") ;
        Alcotest.(check bool)
          (label ^ " path removed")
          false
          (contains rendered "/private/dispatch-secret")
  in
  let availability_id = "dispatch-availability-exception" in
  let availability_calls = ref 0 in
  let availability_backend =
    make_backend
      ~id:availability_id
      ~calls:(ref 0)
      ~availability:(fun ~sw:_ ~env:_ ->
        incr availability_calls ;
        failwith "dispatch-secret /private/dispatch-secret")
      (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())
  in
  register_pair ~id:availability_id availability_backend ;
  assert_sanitized
    "availability"
    (run ~env ~sw ~backend_id:availability_id (spec ())) ;
  Alcotest.(check int) "availability attempted once" 1 !availability_calls ;

  let run_id = "dispatch-run-exception" in
  let run_calls = ref 0 in
  register_pair
    ~id:run_id
    (make_backend ~id:run_id ~calls:run_calls (fun ~sw:_ ~env:_ ?on_raw_line:_ _ ->
         failwith "dispatch-secret /private/dispatch-secret")) ;
  assert_sanitized "run_task" (run ~env ~sw ~backend_id:run_id (spec ())) ;
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
    match expected, actual with
    | Out_of_memory, Out_of_memory
    | Stack_overflow, Stack_overflow
    | Sys.Break, Sys.Break ->
        true
    | _ -> false
  in
  let expect_fatal label expected invoke =
    match invoke () with
    | exception actual when same_fatal expected actual -> ()
    | exception actual ->
        Alcotest.failf
          "%s raised the wrong exception: %s"
          label
          (Printexc.to_string actual)
    | Ok _ -> Alcotest.failf "%s unexpectedly succeeded" label
    | Error error ->
        Alcotest.failf
          "%s was sanitized as %s"
          label
          (Runtime_dispatch.render_error error)
  in
  List.iter
    (fun (label, fatal) ->
      let availability_id = "dispatch-fatal-availability-" ^ label in
      register_pair
        ~id:availability_id
        (make_backend
           ~id:availability_id
           ~calls:(ref 0)
           ~availability:(fun ~sw:_ ~env:_ -> raise fatal)
           (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())) ;
      expect_fatal
        (label ^ " availability")
        fatal
        (fun () -> run ~env ~sw ~backend_id:availability_id (spec ())) ;

      let execution_id = "dispatch-fatal-execution-" ^ label in
      register_pair
        ~id:execution_id
        (make_backend
           ~id:execution_id
           ~calls:(ref 0)
           (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> raise fatal)) ;
      expect_fatal
        (label ^ " execution")
        fatal
        (fun () -> run ~env ~sw ~backend_id:execution_id (spec ())))
    fatal_cases

let test_dispatch_normalizes_cancellation () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let id = "dispatch-cancellation" in
  register_pair
    ~id
    (make_backend
       ~id
       ~calls:(ref 0)
       ~availability:(fun ~sw:_ ~env:_ ->
         raise (Eio.Cancel.Cancelled (Failure "dispatch-secret")))
       (fun ~sw:_ ~env:_ ?on_raw_line:_ _ -> success ())) ;
  match run ~env ~sw ~backend_id:id (spec ()) with
  | exception error ->
      Alcotest.failf "unexpected exception: %s" (Printexc.to_string error)
  | Ok result ->
      Alcotest.(check bool)
        "cancellation normalized"
        true
        (result.Backend_types.status = Backend_types.Cancelled)
  | Error error ->
      Alcotest.failf
        "cancellation was converted to %s"
        (Runtime_dispatch.render_error error)

let test_validator_wrapper_dispatches_read_only_task () =
  with_registry @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let id = "dispatch-validator-wrapper" in
  let calls = ref 0 in
  let captured = ref None in
  let backend =
    make_backend ~id ~calls (fun ~sw:_ ~env:_ ?on_raw_line:_ task ->
        captured := Some task ;
        success ())
  in
  (match
     Runtime_bootstrap.register_custom
       ~descriptor:(descriptor_for ~read_only:true id)
       ~backend
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error)) ;
  let completer =
    match
      Backend_completer.make_validator_by_name
        ~sw
        ~env
        ~backend_name:id
        ~working_dir:"."
        ()
    with
    | Ok completer -> completer
    | Error error -> Alcotest.fail error
  in
  (match
     completer
       ~system_prompt:"system"
       ~prompt:"prompt"
       ~json_schema:None
       ~resume_session_id:None
   with
  | Ok _ -> ()
  | Error error -> Alcotest.fail error) ;
  Alcotest.(check int) "validator backend called" 1 !calls ;
  match !captured with
  | Some task ->
      Alcotest.(check bool) "validator task is read-only" true task.read_only
  | None -> Alcotest.fail "validator task was not captured"

let () =
  Alcotest.run
    "runtime_dispatch"
    [
      ( "preflight",
        [
          Alcotest.test_case
            "missing and raw registrations fail before call"
            `Quick
            test_missing_and_raw_registrations_fail_before_call;
          Alcotest.test_case
            "runtime/descriptor mismatches cannot register"
            `Quick
            test_runtime_descriptor_mismatches_cannot_be_registered;
          Alcotest.test_case
            "raw Claude override invalidates validated pairing"
            `Quick
            test_raw_claude_override_invalidates_validated_pair;
          Alcotest.test_case
            "invalid preflight never spawns or calls"
            `Quick
            test_invalid_preflight_never_spawns_or_calls_backend;
        ] );
      ( "resolution",
        [
          Alcotest.test_case
            "schema retry keeps resolved backend snapshot"
            `Quick
            test_enforcer_uses_resolved_backend_snapshot_for_retry;
          Alcotest.test_case
            "by-name wrapper resolves call-time override"
            `Quick
            test_by_name_wrapper_resolves_override_at_each_call;
          Alcotest.test_case
            "by-name wrapper rejects malformed static id"
            `Quick
            test_by_name_rejects_malformed_id_without_resolution;
          Alcotest.test_case
            "installed version policy is enforced"
            `Quick
            test_dispatch_enforces_installed_version_policy;
          Alcotest.test_case
            "hardened below-baseline backend is blocked"
            `Quick
            test_hardened_below_baseline_stops_before_backend;
          Alcotest.test_case
            "ordinary backend exceptions are sanitized"
            `Quick
            test_dispatch_sanitizes_backend_exceptions;
          Alcotest.test_case
            "fatal backend exceptions are re-raised"
            `Quick
            test_dispatch_reraises_fatal_exceptions;
          Alcotest.test_case
            "cancellation is normalized"
            `Quick
            test_dispatch_normalizes_cancellation;
          Alcotest.test_case
            "validator wrapper dispatches read-only"
            `Quick
            test_validator_wrapper_dispatches_read_only_task;
        ] );
    ]
