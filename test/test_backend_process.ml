(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the Backend_process shared module. *)

open Cabal

let () =
  Process_test_helper.install_launcher () ;
  Process_test_helper.run_if_requested ()

(** {1 MCP Config Tests} *)

let test_write_mcp_config_single_server () =
  Eio_posix.run @@ fun env ->
  let tmp_path = Filename.temp_file "epure-test-bp-mcp-" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp_path with _ -> ())
    (fun () ->
      let config =
        Backend_types.make_mcp_server_config
          ~name:"test-server"
          ~command:"/usr/bin/test-server"
          ~args:["--port"; "8080"]
          ~env:[("API_KEY", "secret")]
          ()
      in
      Backend_process.write_mcp_config ~env ~path:tmp_path [config] ;
      let fs = Eio.Stdenv.fs env in
      let content = Eio.Path.load Eio.Path.(fs / tmp_path) in
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let servers = json |> member "mcpServers" in
      Alcotest.(check bool) "mcpServers exists" true (servers <> `Null) ;
      let server = servers |> member "test-server" in
      Alcotest.(check string)
        "command"
        "/usr/bin/test-server"
        (server |> member "command" |> to_string) ;
      let args = server |> member "args" |> to_list |> List.map to_string in
      Alcotest.(check (list string)) "args" ["--port"; "8080"] args ;
      let env_obj = server |> member "env" in
      Alcotest.(check string)
        "env API_KEY"
        "secret"
        (env_obj |> member "API_KEY" |> to_string))

let test_write_mcp_config_empty () =
  Eio_posix.run @@ fun env ->
  let tmp_path = Filename.temp_file "epure-test-bp-mcp-" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp_path with _ -> ())
    (fun () ->
      Backend_process.write_mcp_config ~env ~path:tmp_path [] ;
      let fs = Eio.Stdenv.fs env in
      let content = Eio.Path.load Eio.Path.(fs / tmp_path) in
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let servers = json |> member "mcpServers" in
      Alcotest.(check bool)
        "mcpServers is empty assoc"
        true
        (servers = `Assoc []))

let mcp_config_tests =
  [
    ("write single server config", `Quick, test_write_mcp_config_single_server);
    ("write empty config", `Quick, test_write_mcp_config_empty);
  ]

(** {1 Setup/Cleanup MCP Config Tests} *)

let test_setup_mcp_config_no_servers () =
  Eio_posix.run @@ fun env ->
  let spec =
    Backend_types.make_task_spec
      ~prompt:"test"
      ~working_dir:"/tmp"
      ~mcp_servers:[]
      ()
  in
  let result = Backend_process.setup_mcp_config ~env spec in
  Alcotest.(check bool) "no config when no servers" true (Option.is_none result)

let test_setup_mcp_config_with_servers () =
  Eio_posix.run @@ fun env ->
  let server =
    Backend_types.make_mcp_server_config ~name:"test" ~command:"test-cmd" ()
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"test"
      ~working_dir:"/tmp"
      ~mcp_servers:[server]
      ()
  in
  let result = Backend_process.setup_mcp_config ~env spec in
  Alcotest.(check bool) "config created" true (Option.is_some result) ;
  (* Clean up *)
  match result with
  | Some path -> Backend_process.cleanup_mcp_config ~env path
  | None -> ()

let test_setup_mcp_config_uses_managed_namespace_prefix () =
  Eio_posix.run @@ fun env ->
  let server =
    Backend_types.make_mcp_server_config ~name:"test" ~command:"test-cmd" ()
  in
  let managed_namespace : Backend_types.managed_namespace =
    {
      id = "crucible";
      display_name = "Crucible";
      config_dir = ".crucible/backend-config";
    }
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"test"
      ~working_dir:"/tmp"
      ~mcp_servers:[server]
      ~managed_namespace
      ()
  in
  match Backend_process.setup_mcp_config ~env spec with
  | None -> Alcotest.fail "expected MCP config path"
  | Some path ->
      Alcotest.(check bool)
        "temp file uses namespace id prefix"
        true
        (String.starts_with
           ~prefix:(Filename.concat "/tmp" ".crucible-mcp-config-")
           path) ;
      Backend_process.cleanup_mcp_config ~env path

let invalid_namespace : Backend_types.managed_namespace =
  {id = "../bad"; display_name = "Bad"; config_dir = ".epure/backend-config"}

let test_setup_mcp_config_rejects_invalid_namespace_before_temp_file () =
  Eio_posix.run @@ fun env ->
  let server =
    Backend_types.make_mcp_server_config ~name:"test" ~command:"test-cmd" ()
  in
  let spec =
    {
      (Backend_types.make_task_spec
         ~prompt:"test"
         ~working_dir:"/tmp"
         ~mcp_servers:[server]
         ())
      with
      Backend_types.managed_namespace = invalid_namespace;
    }
  in
  try
    ignore (Backend_process.setup_mcp_config ~env spec) ;
    Alcotest.fail "expected invalid namespace to be rejected before temp file"
  with Invalid_argument msg ->
    Alcotest.(check bool) "explicit namespace error" true (String.length msg > 0)

let test_run_task_with_invalid_namespace_fails_before_command () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec =
    {
      (Backend_types.make_task_spec ~prompt:"test" ~working_dir:"/tmp" ()) with
      Backend_types.managed_namespace = invalid_namespace;
    }
  in
  let result =
    Backend_process.run_task_with
      ~sw
      ~env
      ~spec
      ~build_command:(fun ~mcp_config_path:_ _ ->
        Alcotest.fail "build_command must not be called for invalid namespace")
      ()
  in
  match result.Backend_types.status with
  | Backend_types.Failed msg ->
      Alcotest.(check bool)
        "failed before backend command"
        true
        (String.length msg > 0)
  | _ -> Alcotest.fail "expected Failed status for invalid namespace"

let setup_cleanup_tests =
  [
    ( "setup returns None when no servers",
      `Quick,
      test_setup_mcp_config_no_servers );
    ( "setup creates config with servers",
      `Quick,
      test_setup_mcp_config_with_servers );
    ( "setup uses managed namespace prefix",
      `Quick,
      test_setup_mcp_config_uses_managed_namespace_prefix );
    ( "setup rejects invalid namespace before temp file",
      `Quick,
      test_setup_mcp_config_rejects_invalid_namespace_before_temp_file );
    ( "run_task_with rejects invalid namespace before command",
      `Quick,
      test_run_task_with_invalid_namespace_fails_before_command );
  ]

(** {1 Git Diff Tests} *)

let test_get_git_diff_in_git_repo () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let working_dir = Sys.getcwd () in
  let files = Backend_process.get_git_diff ~sw ~env ~working_dir in
  (* Verify all returned paths are non-empty strings.
     Also check each path doesn't contain newlines (well-formed). *)
  List.iter
    (fun f ->
      Alcotest.(check bool)
        (Printf.sprintf "non-empty path: %s" f)
        true
        (String.length f > 0) ;
      Alcotest.(check bool)
        (Printf.sprintf "no newlines in path: %s" f)
        true
        (not (String.contains f '\n')))
    files

let test_get_git_diff_not_git_repo () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let files = Backend_process.get_git_diff ~sw ~env ~working_dir:"/tmp" in
  Alcotest.(check (list string)) "empty for non-git dir" [] files

let test_get_git_diff_includes_untracked () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Create a temporary git repo with an untracked file *)
  let tmp_dir = Filename.temp_dir "epure-test-git-" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" tmp_dir)))
    (fun () ->
      ignore (Sys.command (Printf.sprintf "git init %s" tmp_dir)) ;
      (* Create initial commit so HEAD exists *)
      ignore
        (Sys.command
           (Printf.sprintf
              "cd %s && git config user.email test@test && git config \
               user.name test && touch .keep && git add .keep && git commit -m \
               init"
              tmp_dir)) ;
      (* Create an untracked file *)
      let oc = open_out (Filename.concat tmp_dir "new_file.ml") in
      output_string oc "let x = 42\n" ;
      close_out oc ;
      (* get_git_diff should include the untracked file *)
      let files = Backend_process.get_git_diff ~sw ~env ~working_dir:tmp_dir in
      Alcotest.(check bool)
        "untracked file in list"
        true
        (List.mem "new_file.ml" files) ;
      (* get_git_diff_content should include the file content *)
      let diff =
        Backend_process.get_git_diff_content ~sw ~env ~working_dir:tmp_dir
      in
      Alcotest.(check bool) "diff is non-empty" true (String.length diff > 0) ;
      let has_new_file =
        let pat = "new_file.ml" in
        let plen = String.length pat and dlen = String.length diff in
        let rec find i =
          if i + plen > dlen then false
          else if String.sub diff i plen = pat then true
          else find (i + 1)
        in
        find 0
      in
      Alcotest.(check bool) "diff mentions new_file.ml" true has_new_file)

let git_diff_tests =
  [
    ("git diff in git repo", `Quick, test_get_git_diff_in_git_repo);
    ("git diff in non-git dir", `Quick, test_get_git_diff_not_git_repo);
    ( "git diff includes untracked files",
      `Quick,
      test_get_git_diff_includes_untracked );
  ]

(** {1 Availability Check Tests} *)

let test_check_available_true () =
  Eio_posix.run @@ fun env ->
  (* 'true' is always available on Unix *)
  let result = Backend_process.check_available ~env ["true"] in
  Alcotest.(check bool) "true is available" true result

let test_check_available_false () =
  Eio_posix.run @@ fun env ->
  let result =
    Backend_process.check_available ~env ["nonexistent-command-xyz-12345"]
  in
  Alcotest.(check bool) "nonexistent not available" false result

(** A hanging binary on the PATH (or [sleep 30]) must not freeze the
    availability check: [check_available] should honour [~timeout_seconds]
    and return [false] rather than blocking the host. *)
let test_check_available_times_out_on_hang () =
  if not (Sys.file_exists "/bin/sleep" || Sys.file_exists "/usr/bin/sleep") then
    Alcotest.skip ()
  else
    Eio_posix.run @@ fun env ->
    let started = Unix.gettimeofday () in
    let result =
      Backend_process.check_available ~env ~timeout_seconds:0.5 ["sleep"; "30"]
    in
    let elapsed = Unix.gettimeofday () -. started in
    Alcotest.(check bool)
      "hanging binary reported unavailable on timeout"
      false
      result ;
    Alcotest.(check bool)
      "check returned within ~1s, not 30s"
      true
      (elapsed < 4.0)

let test_capture_version_output_times_out_on_hang () =
  if not (Sys.file_exists "/bin/sleep" || Sys.file_exists "/usr/bin/sleep") then
    Alcotest.skip ()
  else
    Eio_posix.run @@ fun env ->
    let started = Unix.gettimeofday () in
    let result =
      Backend_process.capture_version_output
        ~env
        ~timeout_seconds:0.5
        ["sleep"; "30"]
    in
    let elapsed = Unix.gettimeofday () -. started in
    Alcotest.(check bool)
      "hanging --version probe returns Error on timeout"
      true
      (Result.is_error result) ;
    Alcotest.(check bool)
      "probe returned within ~1s, not 30s"
      true
      (elapsed < 4.0)

let availability_tests =
  [
    ("check_available with true", `Quick, test_check_available_true);
    ("check_available with nonexistent", `Quick, test_check_available_false);
    ( "check_available timeout on hang",
      `Quick,
      test_check_available_times_out_on_hang );
    ( "capture_version_output timeout on hang",
      `Quick,
      test_capture_version_output_times_out_on_hang );
  ]

(** {1 Process-tree Tests} *)

let helper_path () = Unix.realpath Sys.executable_name

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value ;
  Fun.protect
    ~finally:(fun () -> Unix.putenv name (Option.value ~default:"" previous))
    f

let contains text needle =
  let text_length = String.length text
  and needle_length = String.length needle in
  let rec loop index =
    index + needle_length <= text_length
    && (String.sub text index needle_length = needle || loop (index + 1))
  in
  needle_length = 0 || loop 0

let fresh_marker () =
  let path = Filename.temp_file "cabal-process-group-" ".marker" in
  Sys.remove path ;
  path

let wait_for_file ~clock path =
  let deadline = Eio.Time.now clock +. 2.0 in
  let rec loop () =
    if Sys.file_exists path then ()
    else if Eio.Time.now clock >= deadline then
      Alcotest.failf "timed out waiting for %s" path
    else begin
      Eio.Time.sleep clock 0.02 ;
      loop ()
    end
  in
  loop ()

let arrange_gated_supervisor_death ~sw ~clock marker =
  Eio.Fiber.fork ~sw (fun () ->
      wait_for_file ~clock marker ;
      wait_for_file ~clock (marker ^ ".exec-confirmed") ;
      Process_test_helper.write_file
        (marker ^ ".allow-supervisor-death")
        "continue\n")

let child_pid marker =
  let path = marker ^ ".pid" in
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      match int_of_string_opt (String.trim (input_line channel)) with
      | Some pid when pid > 0 -> pid
      | _ -> Alcotest.failf "invalid child PID in %s" path)

let wait_for_pid_exit ~clock pid =
  let deadline = Eio.Time.now clock +. 2.0 in
  let rec loop () =
    try
      Unix.kill pid 0 ;
      if Eio.Time.now clock >= deadline then
        Alcotest.failf "child PID %d remained after group termination" pid
      else begin
        Eio.Time.sleep clock 0.02 ;
        loop ()
      end
    with
    | Unix.Unix_error (Unix.ESRCH, _, _) -> ()
    | Unix.Unix_error (error, _, _) ->
        Alcotest.failf
          "could not probe child PID %d: %s"
          pid
          (Unix.error_message error)
  in
  loop ()

let test_timeout_terminates_real_descendant_and_preserves_output () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      let result =
        Backend_process.run_process
          ~sw
          ~env
          ~cmd:
            [
              helper_path ();
              "--process-descendant-helper";
              "spawn-child";
              marker;
            ]
          ~working_dir:"/tmp"
          ~timeout_seconds:0.2
          ()
      in
      Alcotest.(check bool)
        "timeout status is preserved"
        true
        (result.Backend_process.status = Backend_types.Timeout) ;
      Alcotest.(check bool)
        "stdout captured before timeout"
        true
        (contains result.stdout "child-started") ;
      Alcotest.(check bool)
        "stderr captured before timeout"
        true
        (contains result.stderr "parent-stderr") ;
      let clock = Eio.Stdenv.clock env in
      wait_for_file ~clock marker ;
      wait_for_pid_exit ~clock (child_pid marker))

let test_cancellation_terminates_real_descendant () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      let cancelled =
        Eio.Time.with_timeout (Eio.Stdenv.clock env) 0.2 (fun () ->
            ignore
              (Backend_process.run_process
                 ~sw
                 ~env
                 ~cmd:
                   [
                     helper_path ();
                     "--process-descendant-helper";
                     "spawn-child";
                     marker;
                   ]
                 ~working_dir:"/tmp"
                 ~timeout_seconds:30.0
                 ()) ;
            Ok ())
      in
      Alcotest.(check bool)
        "outer cancellation fired"
        true
        (match cancelled with Error `Timeout -> true | Ok () -> false) ;
      let clock = Eio.Stdenv.clock env in
      wait_for_file ~clock marker ;
      wait_for_pid_exit ~clock (child_pid marker))

let test_timeout_kills_grandchild_when_backend_exits_on_term () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".parent-terminated"])
    (fun () ->
      let result =
        Backend_process.run_process
          ~sw
          ~env
          ~cmd:
            [
              helper_path ();
              "--process-descendant-helper";
              "spawn-term-ignoring-child";
              marker;
            ]
          ~working_dir:"/tmp"
          ~timeout_seconds:0.2
          ()
      in
      Alcotest.(check bool)
        "timeout status survives the anchored group cleanup"
        true
        (result.Backend_process.status = Backend_types.Timeout) ;
      let clock = Eio.Stdenv.clock env in
      wait_for_file ~clock marker ;
      wait_for_file ~clock (marker ^ ".parent-terminated") ;
      wait_for_pid_exit ~clock (child_pid marker))

let test_timeout_kills_descendant_after_backend_exits_and_retains_pipe () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".terminated"])
    (fun () ->
      let result =
        Backend_process.run_process
          ~sw
          ~env
          ~cmd:
            [
              helper_path ();
              "--process-descendant-helper";
              "spawn-exit-child";
              marker;
            ]
          ~working_dir:"/tmp"
          ~timeout_seconds:0.2
          ()
      in
      Alcotest.(check bool)
        "retained descendant pipe keeps run_process inside its timeout"
        true
        (result.Backend_process.status = Backend_types.Timeout) ;
      Alcotest.(check bool)
        "backend output before its natural exit is retained"
        true
        (contains result.stdout "child-started") ;
      let clock = Eio.Stdenv.clock env in
       wait_for_file ~clock marker ;
       wait_for_pid_exit ~clock (child_pid marker))

let test_unexpected_supervisor_death_kills_term_ignoring_backend () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ marker;
          marker ^ ".pid";
          marker ^ ".exec-confirmed";
          marker ^ ".allow-supervisor-death";
        ])
    (fun () ->
      with_env
        "CABAL_PROCESS_GROUP_TEST_EXEC_CONFIRMED_MARKER"
        (marker ^ ".exec-confirmed")
      @@ fun () ->
      let clock = Eio.Stdenv.clock env in
      arrange_gated_supervisor_death ~sw ~clock marker ;
      let started = Eio.Time.now clock in
      let result =
        Backend_process.run_process
          ~sw
          ~env
          ~cmd:
            [ helper_path ();
              "--process-descendant-helper";
              "gated-kill-supervisor-ignoring-term";
              marker;
            ]
          ~working_dir:"/tmp"
          ~timeout_seconds:5.0
          ()
      in
      Alcotest.(check bool)
        "unexpected supervisor death is not reported as backend success"
        true
        (match result.Backend_process.status with Backend_types.Failed _ -> true | _ -> false) ;
      Alcotest.(check bool)
        "TERM-ignoring unexpected supervisor cleanup forfeits grace"
        true
        (Eio.Time.now clock -. started < 1.0) ;
      wait_for_file ~clock marker ;
      wait_for_pid_exit ~clock (child_pid marker))

let test_unexpected_supervisor_death_terminates_term_responsive_backend_promptly () =
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_GROUP_KILL" "1" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ marker;
          marker ^ ".pid";
          marker ^ ".terminated";
          marker ^ ".exec-confirmed";
          marker ^ ".allow-supervisor-death";
        ])
    (fun () ->
      with_env
        "CABAL_PROCESS_GROUP_TEST_EXEC_CONFIRMED_MARKER"
        (marker ^ ".exec-confirmed")
      @@ fun () ->
      let clock = Eio.Stdenv.clock env in
      arrange_gated_supervisor_death ~sw ~clock marker ;
      let started = Eio.Time.now clock in
      let result =
        Backend_process.run_process
          ~sw
          ~env
          ~cmd:
            [ helper_path ();
              "--process-descendant-helper";
              "gated-kill-supervisor-exiting-on-term";
              marker;
            ]
          ~working_dir:"/tmp"
          ~timeout_seconds:5.0
          ()
      in
      Alcotest.(check bool)
        "unexpected supervisor death is reported as backend failure"
        true
        (match result.Backend_process.status with Backend_types.Failed _ -> true | _ -> false) ;
      Alcotest.(check bool)
        "TERM-responsive backend cleanup forfeits the default grace"
        true
        (Eio.Time.now clock -. started < 1.0) ;
      wait_for_file ~clock marker ;
      wait_for_file ~clock (marker ^ ".terminated") ;
      wait_for_pid_exit ~clock (child_pid marker))

let test_release_commit_boundary_survives_elapsed_timeout () =
  let marker = fresh_marker () in
  let release_started = marker ^ ".release-started" in
  let release_gate = marker ^ ".allow-release" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [release_started; release_gate])
    (fun () ->
      with_env
        "CABAL_PROCESS_GROUP_TEST_RELEASE_STARTED_MARKER"
        release_started
      @@ fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_RELEASE_GATE" release_gate @@ fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let timeout_seconds = 1.0 in
      let completion =
        Eio.Fiber.fork_promise ~sw (fun () ->
            Backend_process.run_process
              ~sw
              ~env
              ~cmd:[helper_path (); "--process-descendant-helper"; "success"]
              ~working_dir:"/tmp"
              ~timeout_seconds
              ())
      in
      Fun.protect
        ~finally:(fun () ->
          if not (Sys.file_exists release_gate) then
            Process_test_helper.write_file release_gate "continue\n")
        (fun () ->
          wait_for_file ~clock release_started ;
          Eio.Time.sleep clock (timeout_seconds +. 0.1) ;
          Alcotest.(check bool)
            "normal completion waits for gated RELEASE outside task timeout"
            true
            (Option.is_none (Eio.Promise.peek completion)) ;
          Process_test_helper.write_file release_gate "continue\n" ;
          let result = Eio.Promise.await_exn completion in
          Alcotest.(check bool)
            "completed backend remains successful while RELEASE is delayed"
            true
            (result.Backend_process.status = Backend_types.Success)))

let test_backend_process_preserves_child_signal_semantics () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (name, signal, action) ->
      let result =
        Backend_process.run_process
          ~sw
          ~env
          ~cmd:[helper_path (); "--process-descendant-helper"; action]
          ~working_dir:"/tmp"
          ~timeout_seconds:5.0
          ()
      in
      Alcotest.(check bool)
        (name ^ " is not reported as an ordinary exit")
        true
        (result.status
        = Backend_types.Failed
            (Printf.sprintf "Process killed by signal %d" signal)) ;
      Alcotest.(check int)
        (name ^ " exit-code convention uses the signal value")
        (128 + signal)
        result.exit_code)
    [
      ("self SIGTERM", Sys.sigterm, "self-term");
      ("self SIGKILL", Sys.sigkill, "self-kill");
    ]

let test_timeout_covers_large_stdin_for_nonreading_child () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let input = String.make (1024 * 1024) 'i' in
  let outcome =
    Eio.Time.with_timeout clock 4.0 (fun () ->
        Ok
          (Backend_process.run_process
             ~sw
             ~env
             ~cmd:[helper_path (); "--process-descendant-helper"; "nonreading"]
             ~stdin_content:(Some input)
             ~working_dir:"/tmp"
             ~timeout_seconds:0.2
             ()))
  in
  match outcome with
  | Error `Timeout ->
      Alcotest.fail "stdin delivery blocked outside process timeout"
  | Ok result ->
      Alcotest.(check bool)
        "non-reading child returns Cabal Timeout"
        true
        (result.Backend_process.status = Backend_types.Timeout)

let test_timeout_drains_term_handler_output_during_grace () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let started = Eio.Time.now clock in
  let result =
    Backend_process.run_process
      ~sw
      ~env
      ~cmd:[helper_path (); "--process-descendant-helper"; "write-after-term"]
      ~working_dir:"/tmp"
      ~timeout_seconds:0.2
      ()
  in
  let expected = String.make (1024 * 1024) 't' ^ "\nTERM-WRITE-COMPLETE\n" in
  Alcotest.(check bool)
    "timeout classification is preserved"
    true
    (result.Backend_process.status = Backend_types.Timeout) ;
  Alcotest.(check string)
    "all TERM-handler output is drained before the bounded cleanup finishes"
    expected
    result.stdout ;
  Alcotest.(check bool)
    "TERM-handler cleanup remains bounded"
    true
    (Eio.Time.now clock -. started < 4.0)

let test_probe_timeout_kills_forked_descendant () =
  Eio_posix.run @@ fun env ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      let result =
        Backend_process.capture_version_output
          ~env
          ~timeout_seconds:0.2
          [helper_path (); "--process-descendant-helper"; "spawn-child"; marker]
      in
      Alcotest.(check bool)
        "forking probe times out"
        true
        (Result.is_error result) ;
      let clock = Eio.Stdenv.clock env in
      wait_for_file ~clock marker ;
      wait_for_pid_exit ~clock (child_pid marker))

let test_streaming_large_line_without_repeated_pending_copies () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let lines = ref [] in
  let expected = String.make (1024 * 1024) 'x' in
  let result =
    Backend_process.run_process
      ~sw
      ~env
      ~cmd:[helper_path (); "--process-descendant-helper"; "large-no-newline"]
      ~working_dir:"/tmp"
      ~timeout_seconds:5.0
      ~on_stdout:(fun line -> lines := line :: !lines)
      ()
  in
  Alcotest.(check string)
    "captured no-newline output is exact"
    expected
    result.stdout ;
  Alcotest.(check (list string))
    "stream callback emits one exact final line"
    [expected]
    (List.rev !lines)

let test_missing_exec_handshake_cannot_report_backend_success () =
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ marker;
          marker ^ ".pid";
          marker ^ ".terminated";
          marker ^ ".released";
          marker ^ ".control-closed";
        ])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_LAUNCHER" (helper_path ()) @@ fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let result =
        Backend_process.run_process
          ~sw
          ~env
          ~cmd:
            [ helper_path ();
              "--process-descendant-helper";
              "fake-launcher-missing-exec-zero";
              marker;
            ]
          ~working_dir:"/tmp"
          ~timeout_seconds:5.0
          ()
      in
      let failure =
        match result.Backend_process.status with
        | Backend_types.Failed message -> message
        | Backend_types.Success ->
            Alcotest.fail "zero status without EXEC was reported as Success"
        | Backend_types.Timeout | Backend_types.Cancelled ->
            Alcotest.fail "invalid handshake did not return an explicit failure"
      in
      Alcotest.(check bool)
        "failure identifies the invalid EXEC handshake"
        true
        (contains failure "handshake" && contains failure "EXEC") ;
      let clock = Eio.Stdenv.clock env in
      wait_for_file ~clock marker ;
      wait_for_file ~clock (marker ^ ".terminated") ;
      Alcotest.(check bool)
        "invalid handshake is cleaned up rather than released"
        false
        (Sys.file_exists (marker ^ ".released")) ;
      wait_for_pid_exit ~clock (child_pid marker))

let process_tree_tests =
  [
    ( "timeout terminates descendant and preserves output",
      `Quick,
      test_timeout_terminates_real_descendant_and_preserves_output );
    ( "cancellation terminates descendant",
      `Quick,
      test_cancellation_terminates_real_descendant );
    ( "timeout kills TERM-ignoring grandchild",
      `Quick,
      test_timeout_kills_grandchild_when_backend_exits_on_term );
    ( "timeout kills descendant after backend exits with pipe retained",
      `Quick,
      test_timeout_kills_descendant_after_backend_exits_and_retains_pipe );
    ( "unexpected supervisor death kills TERM-ignoring backend",
      `Quick,
      test_unexpected_supervisor_death_kills_term_ignoring_backend );
    ( "unexpected supervisor death promptly terminates TERM-responsive backend",
      `Quick,
      test_unexpected_supervisor_death_terminates_term_responsive_backend_promptly );
    ( "release commit boundary survives elapsed timeout",
      `Quick,
      test_release_commit_boundary_survives_elapsed_timeout );
    ( "backend child signal semantics are preserved",
      `Quick,
      test_backend_process_preserves_child_signal_semantics );
    ( "timeout covers large stdin to non-reading child",
      `Quick,
      test_timeout_covers_large_stdin_for_nonreading_child );
    ( "timeout drains TERM-handler output during grace",
      `Quick,
      test_timeout_drains_term_handler_output_during_grace );
    ( "forking probe timeout kills descendant",
      `Quick,
      test_probe_timeout_kills_forked_descendant );
    ( "streaming large no-newline output is exact",
      `Quick,
      test_streaming_large_line_without_repeated_pending_copies );
    ( "missing EXEC handshake cannot report backend success",
      `Quick,
      test_missing_exec_handshake_cannot_report_backend_success );
  ]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Backend_process"
    [
      ("MCP Config", mcp_config_tests);
      ("Setup/Cleanup", setup_cleanup_tests);
      ("Git Diff", git_diff_tests);
      ("Availability", availability_tests);
      ("Process tree", process_tree_tests);
    ]
