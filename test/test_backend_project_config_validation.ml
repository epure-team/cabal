(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Conformance tests for backend project-config validation hooks. *)

open Cabal

let with_tmpdir f =
  let dir = Filename.temp_dir "cabal_cfg_check_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

(* Was a hand-rolled copy of the five OCaml backends -- one more instance of
   the duplication this file's subject exists to check.  It also omitted
   Mock_agent, so the moment [Backend_registry.all ()] gained that descriptor
   the conformance walk below would have failed looking for a runtime module
   that nothing here registered.  [Bootstrap.ensure] is the production path;
   [reset] is needed because [Registry.clear] does not invalidate its memo. *)
let register_builtin_runtime_backends () =
  Registry.clear () ;
  Bootstrap.reset () ;
  Bootstrap.ensure ()

let epure_mcp_server () =
  Backend_types.make_mcp_server_config
    ~name:"epure"
    ~command:"epure-mcp-server"
    ~args:["--stdio"]
    ~env:[("EPURE_PROJECT", "/tmp/project")]
    ()

let backend_or_fail backend_id =
  match Registry.get backend_id with
  | Some backend -> backend
  | None -> Alcotest.failf "runtime backend not registered: %s" backend_id

let contains hay sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length hay && (String.sub hay i n = sub || go (i + 1))
  in
  n = 0 || go 0

let check_result backend_id = function
  | Agentic_backend.Config_valid -> ()
  | Agentic_backend.Config_check_unsupported reason ->
      Alcotest.(check bool)
        (backend_id ^ " unsupported check has documented reason")
        true
        (String.trim reason <> "")
  | Agentic_backend.Config_invalid msg ->
      Alcotest.failf "%s project config is invalid: %s" backend_id msg

let check_builtin_backend ~sw ~env (desc : Backend_registry.descriptor) =
  with_tmpdir (fun project_dir ->
      let setup_result =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:desc.id
          ~project_dir
          ~force:false
      in
      let backend = backend_or_fail desc.id in
      Agentic_backend.check_project_config
        ~sw
        ~env
        ~project_dir
        ~setup_result
        backend
      |> check_result desc.id)

let test_builtin_project_config_validation_conformance () =
  register_builtin_runtime_backends () ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Backend_registry.all () |> List.iter (check_builtin_backend ~sw ~env)

(* --- what a native CLI's exit status is allowed to mean ------------------- *)

let with_stub_cli ~binary ~exit_code ~stdout ~stderr f =
  with_tmpdir (fun bindir ->
      let script = Filename.concat bindir binary in
      let oc = open_out script in
      Printf.fprintf
        oc
        "#!/bin/sh\n\
         if [ \"$1\" = \"--version\" ]; then echo '9.9.9'; exit 0; fi\n\
         printf '%%s' %s\n\
         printf '%%s' %s >&2\n\
         exit %d\n"
        (Filename.quote stdout)
        (Filename.quote stderr)
        exit_code ;
      close_out oc ;
      Unix.chmod script 0o755 ;
      let old_path = Sys.getenv "PATH" in
      Unix.putenv "PATH" (bindir ^ ":" ^ old_path) ;
      Fun.protect
        ~finally:(fun () -> Unix.putenv "PATH" old_path)
        (fun () -> f ()))

let native_cli_verdict ~backend_id ~binary ~exit_code ~stdout ~stderr =
  register_builtin_runtime_backends () ;
  with_stub_cli ~binary ~exit_code ~stdout ~stderr @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun project_dir ->
      let setup_result =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id
          ~project_dir
          ~force:false
      in
      Agentic_backend.check_project_config
        ~sw
        ~env
        ~project_dir
        ~setup_result
        (backend_or_fail backend_id))

let cli_backends = [("claude-code", "claude"); ("opencode", "opencode")]

let expect_valid label = function
  | Agentic_backend.Config_valid -> ()
  | Agentic_backend.Config_check_unsupported reason ->
      Alcotest.failf "%s unexpectedly unchecked: %s" label reason
  | Agentic_backend.Config_invalid msg ->
      Alcotest.failf "%s unexpectedly invalid: %s" label msg

let expect_unsupported label = function
  | Agentic_backend.Config_check_unsupported reason -> reason
  | Agentic_backend.Config_valid -> Alcotest.failf "%s was accepted" label
  | Agentic_backend.Config_invalid msg ->
      Alcotest.failf "%s was rejected as invalid: %s" label msg

let test_clean_success_is_a_verdict_for_each_native_cli () =
  List.iter
    (fun (backend_id, binary) ->
      native_cli_verdict ~backend_id ~binary ~exit_code:0 ~stdout:"" ~stderr:""
      |> expect_valid backend_id)
    cli_backends

let test_non_zero_exit_is_not_a_verdict_for_each_native_cli () =
  List.iter
    (fun (backend_id, binary) ->
      native_cli_verdict
        ~backend_id
        ~binary
        ~exit_code:17
        ~stdout:""
        ~stderr:"error: unknown option or unauthenticated"
      |> expect_unsupported backend_id
      |> ignore)
    cli_backends

let test_process_diagnostics_survive_in_the_message () =
  let reason =
    native_cli_verdict
      ~backend_id:"claude-code"
      ~binary:"claude"
      ~exit_code:17
      ~stdout:"partial stdout without newline"
      ~stderr:"Invalid settings: unknown key permissions — déjà vu\n"
    |> expect_unsupported "claude-code diagnostics"
  in
  [
    "claude --settings";
    "status: Process exited with code 17";
    "exit code: 17";
    "partial stdout without newline";
    "unknown key permissions — déjà vu";
  ]
  |> List.iter (fun expected ->
      Alcotest.(check bool)
        ("diagnostic contains " ^ expected)
        true
        (contains reason expected))

let synthetic_result ~status ~exit_code ~stdout ~stderr =
  {
    Backend_process.status;
    stdout;
    stderr;
    exit_code;
    elapsed = Backend_types.duration_of_seconds 0.0;
    cost = None;
    session_id = None;
  }

let test_non_verdict_status_matrix () =
  [
    (Backend_types.Failed "Process killed by signal 15", 143, "signal", "", "");
    (Backend_types.Timeout, -1, "timed out", " \n\t", "");
    ( Backend_types.Cancelled,
      -1,
      "cancelled",
      " APPROVE \nREJECT\n{malformed",
      "\tunknown\194\160verdict\n" );
    ( Backend_types.Success,
      9,
      "success",
      "APPROVE",
      "contradictory non-zero exit" );
  ]
  |> List.iter (fun (status, exit_code, status_text, stdout, stderr) ->
      let reason =
        synthetic_result ~status ~exit_code ~stdout ~stderr
        |> Backend_process.config_check_from_process_result
             ~backend_name:"Synthetic CLI"
             ~cmd:["synthetic"; "check"]
        |> expect_unsupported status_text
      in
      Alcotest.(check bool)
        (status_text ^ " status is surfaced")
        true
        (contains reason status_text))

let test_success_status_is_the_native_verdict_not_output_text () =
  synthetic_result
    ~status:Backend_types.Success
    ~exit_code:0
    ~stdout:"APPROVE\nreject\n{malformed\n"
    ~stderr:"warning only\n"
  |> Backend_process.config_check_from_process_result
       ~backend_name:"Synthetic CLI"
       ~cmd:["synthetic"; "check"]
  |> expect_valid "successful native check"

let test_user_owned_opencode_config_is_not_clobbered_or_checked () =
  register_builtin_runtime_backends () ;
  with_tmpdir (fun project_dir ->
      let config_path = Filename.concat project_dir "opencode.json" in
      let user_content = "{\"theme\":\"user-owned\"}\n" in
      let oc = open_out config_path in
      output_string oc user_content ;
      close_out oc ;
      Unix.chmod config_path 0o444 ;
      let setup_result =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:"opencode"
          ~project_dir
          ~force:false
      in
      (match setup_result.write_outcome with
      | Some (Backend_config_writer.Skipped_user_content _) -> ()
      | _ -> Alcotest.fail "user-owned OpenCode config was not skipped") ;
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      Agentic_backend.check_project_config
        ~sw
        ~env
        ~project_dir
        ~setup_result
        (backend_or_fail "opencode")
      |> expect_unsupported "user-owned opencode config"
      |> ignore ;
      Alcotest.(check string)
        "user-owned config remains byte-for-byte unchanged"
        user_content
        (let ic = open_in config_path in
         Fun.protect
           ~finally:(fun () -> close_in ic)
           (fun () -> really_input_string ic (in_channel_length ic))) ;
      Alcotest.(check int)
        "read-only mode is preserved"
        0o444
        ((Unix.stat config_path).st_perm land 0o777))

let test_missing_generated_claude_config_is_invalid_without_cli_verdict () =
  register_builtin_runtime_backends () ;
  with_tmpdir (fun project_dir ->
      let sentinel = Filename.concat project_dir "unrelated.txt" in
      let oc = open_out sentinel in
      output_string oc "unchanged\n" ;
      close_out oc ;
      let setup_result =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:"claude-code"
          ~project_dir
          ~force:false
      in
      let config_path =
        match setup_result.project_config_path with
        | Some path -> path
        | None -> Alcotest.fail "Claude config was not generated"
      in
      Sys.remove config_path ;
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      (match
         Agentic_backend.check_project_config
           ~sw
           ~env
           ~project_dir
           ~setup_result
           (backend_or_fail "claude-code")
       with
      | Agentic_backend.Config_invalid msg ->
          Alcotest.(check bool)
            "missing generated path is named"
            true
            (contains msg config_path)
      | _ -> Alcotest.fail "missing generated config was not invalid") ;
      Alcotest.(check string)
        "unrelated file remains untouched"
        "unchanged\n"
        (let ic = open_in sentinel in
         Fun.protect
           ~finally:(fun () -> close_in ic)
           (fun () -> really_input_string ic (in_channel_length ic))))

let test_missing_cli_is_a_transport_limitation () =
  register_builtin_runtime_backends () ;
  with_tmpdir (fun empty_path ->
      let old_path = Sys.getenv "PATH" in
      Unix.putenv "PATH" empty_path ;
      Fun.protect
        ~finally:(fun () -> Unix.putenv "PATH" old_path)
        (fun () ->
          Eio_posix.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          List.iter
            (fun (backend_id, _) ->
              with_tmpdir (fun project_dir ->
                  let setup_result =
                    Backend_config_gen.setup_project_config
                      ~mcp_servers:[epure_mcp_server ()]
                      ~backend_id
                      ~project_dir
                      ~force:false
                  in
                  Agentic_backend.check_project_config
                    ~sw
                    ~env
                    ~project_dir
                    ~setup_result
                    (backend_or_fail backend_id)
                  |> expect_unsupported (backend_id ^ " missing executable")
                  |> ignore))
            cli_backends))

let test_project_path_metacharacters_are_not_executed () =
  register_builtin_runtime_backends () ;
  with_stub_cli ~binary:"claude" ~exit_code:0 ~stdout:"" ~stderr:"" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun parent ->
      let project_dir = Filename.concat parent "project; touch injected; #" in
      Unix.mkdir project_dir 0o755 ;
      let setup_result =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:"claude-code"
          ~project_dir
          ~force:false
      in
      Agentic_backend.check_project_config
        ~sw
        ~env
        ~project_dir
        ~setup_result
        (backend_or_fail "claude-code")
      |> expect_valid "metacharacter path" ;
      Alcotest.(check bool)
        "argv execution does not interpret shell metacharacters"
        false
        (Sys.file_exists (Filename.concat project_dir "injected")))

let () =
  Alcotest.run
    "Backend_project_config_validation"
    [
      ( "conformance",
        [
          Alcotest.test_case
            "built-in generated configs are valid or explicitly unsupported"
            `Quick
            test_builtin_project_config_validation_conformance;
        ] );
      ( "native CLI exit status",
        [
          Alcotest.test_case
            "clean success is a verdict for Claude and OpenCode"
            `Quick
            test_clean_success_is_a_verdict_for_each_native_cli;
          Alcotest.test_case
            "non-zero exit is unchecked for Claude and OpenCode"
            `Quick
            test_non_zero_exit_is_not_a_verdict_for_each_native_cli;
          Alcotest.test_case
            "command, status, exit code, stdout and stderr are preserved"
            `Quick
            test_process_diagnostics_survive_in_the_message;
          Alcotest.test_case
            "failure, signal, timeout and cancellation stay unchecked"
            `Quick
            test_non_verdict_status_matrix;
          Alcotest.test_case
            "native success status, not free-form output, is the verdict"
            `Quick
            test_success_status_is_the_native_verdict_not_output_text;
          Alcotest.test_case
            "user-owned OpenCode config is not clobbered or checked"
            `Quick
            test_user_owned_opencode_config_is_not_clobbered_or_checked;
          Alcotest.test_case
            "missing generated Claude config is invalid and isolated"
            `Quick
            test_missing_generated_claude_config_is_invalid_without_cli_verdict;
          Alcotest.test_case
            "missing CLI is a transport limitation"
            `Quick
            test_missing_cli_is_a_transport_limitation;
          Alcotest.test_case
            "project path metacharacters are not executed"
            `Quick
            test_project_path_metacharacters_are_not_executed;
        ] );
    ]
