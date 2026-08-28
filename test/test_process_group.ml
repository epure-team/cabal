(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Integration tests for the portable launcher and process-group abstraction. *)

open Cabal
module G = Process_group

let () =
  Process_test_helper.install_launcher () ;
  Process_test_helper.run_if_requested ()

let helper_path () = Unix.realpath Sys.executable_name

let with_launcher test = test ()

let spawn ~sw ~env ?cwd ?launcher ?handshake_timeout_seconds command =
  G.spawn
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~mgr:(Eio.Stdenv.process_mgr env)
    ?cwd
    ?launcher
    ?handshake_timeout_seconds
    command

let is_signaled = function `Signaled _ -> true | `Exited _ -> false

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value ;
  Fun.protect
    ~finally:(fun () -> Unix.putenv name (Option.value ~default:"" previous))
    f

let close_noerr flow = try Eio.Flow.close flow with _ -> ()

let fresh_marker () =
  let path = Filename.temp_file "cabal-process-group-" ".marker" in
  Sys.remove path ;
  path

let allow_supervisor_death marker =
  Process_test_helper.write_file
    (marker ^ ".allow-supervisor-death")
    "continue\n"

let wait_for_file ?(timeout_seconds = 2.0) ~clock path =
  let deadline = Eio.Time.now clock +. timeout_seconds in
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

let pid_is_live pid =
  try
    Unix.kill pid 0 ;
    true
  with
  | Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | Unix.Unix_error (error, _, _) ->
      Alcotest.failf
        "could not probe child PID %d: %s"
        pid
        (Unix.error_message error)

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
        Alcotest.failf "child PID %d remained after termination" pid
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

let wait_for_supervisor_reap ~clock target =
  let deadline = Eio.Time.now clock +. 2.0 in
  let rec loop () =
    match G.awaited_status target with
    | Some status -> status
    | None when Eio.Time.now clock >= deadline ->
        Alcotest.fail "timed out waiting for supervisor reap"
    | None ->
        Eio.Time.sleep clock 0.02 ;
        loop ()
  in
  loop ()

let marker_timestamp path =
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      match float_of_string_opt (String.trim (input_line channel)) with
      | Some timestamp -> timestamp
      | None -> Alcotest.failf "invalid timestamp in %s" path)

let cleanup_raw_launcher ~clock process control =
  try
    (try Eio.Flow.copy_string "TERM 0\n" control with _ -> ()) ;
    match
      Eio.Time.with_timeout clock 0.8 (fun () -> Ok (Eio.Process.await process))
    with
    | Ok _ -> ()
    | Error `Timeout ->
        Eio.Process.signal process Sys.sigkill ;
        ignore
          (Eio.Time.with_timeout clock 0.5 (fun () ->
               Ok (Eio.Process.await process)))
  with _ -> ()

let fd_counter () =
  let rec first_available = function
    | [] -> None
    | path :: rest -> (
        try
          ignore (Sys.readdir path) ;
          Some (fun () -> Array.length (Sys.readdir path))
        with Sys_error _ -> first_available rest)
  in
  first_available ["/proc/self/fd"; "/dev/fd"]

(* This harness deliberately never parses the handshake or obtains a PGID. It
   only closes the launcher's parent-side protocol descriptors. *)
let spawn_launcher_without_cabal ~sw ~env ?process_env command =
  let handshake_r, handshake_w = Eio_unix.pipe sw in
  let control_r, control_w = Eio_unix.pipe sw in
  let status_r, status_w = Eio_unix.pipe sw in
  let launcher = Process_test_helper.launcher_path () in
  let process =
    Eio_unix.Process.spawn_unix
      ~sw
      (Eio.Stdenv.process_mgr env)
      ~fds:
        [
          (0, Eio_unix.Fd.stdin, `Blocking);
          (1, Eio_unix.Fd.stdout, `Blocking);
          (2, Eio_unix.Fd.stderr, `Blocking);
          (3, Eio_unix.Resource.fd handshake_w, `Blocking);
          (4, Eio_unix.Resource.fd control_r, `Blocking);
          (5, Eio_unix.Resource.fd status_w, `Blocking);
          (6, Eio_unix.Fd.stdin, `Blocking);
        ]
      ~env:(Option.value ~default:(Unix.environment ()) process_env)
      ~executable:launcher
      (launcher :: "--" :: command)
  in
  close_noerr handshake_w ;
  close_noerr control_r ;
  close_noerr status_w ;
  (process, handshake_r, control_w, status_r)

let read_launcher_pgid ~clock process handshake =
  let buffer = Buffer.create 32 in
  let chunk = Cstruct.create 64 in
  let rec read_pgid () =
    match Eio.Flow.single_read handshake chunk with
    | count ->
        Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 count)) ;
        let text = Buffer.contents buffer in
        (match String.index_opt text '\n' with
        | Some newline -> String.sub text 0 newline
        | None -> read_pgid ())
    | exception End_of_file -> Alcotest.fail "launcher closed handshake before PGID"
  in
  let record =
    match Eio.Time.with_timeout clock 1.0 (fun () -> Ok (read_pgid ())) with
    | Ok record -> record
    | Error `Timeout -> Alcotest.fail "timed out waiting for launcher PGID"
  in
  let expected = Eio.Process.pid process in
  Alcotest.(check string)
    "raw harness validates launcher PGID before ACK"
    (Printf.sprintf "PGID %d" expected)
    record

let acknowledge_launcher ~clock process handshake control =
  read_launcher_pgid ~clock process handshake ;
  Eio.Flow.copy_string "ACK\n" control

let read_handshake_to_eof ~clock handshake =
  let buffer = Buffer.create 32 in
  let chunk = Cstruct.create 64 in
  let rec loop () =
    match Eio.Flow.single_read handshake chunk with
    | count ->
        Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 count)) ;
        loop ()
    | exception End_of_file -> Buffer.contents buffer
  in
  match Eio.Time.with_timeout clock 2.0 (fun () -> Ok (loop ())) with
  | Ok text -> text
  | Error `Timeout -> Alcotest.fail "timed out waiting for launcher handshake EOF"

let check_signaled name expected = function
  | `Signaled actual -> Alcotest.(check int) name expected actual
  | `Exited code ->
      Alcotest.failf "%s exited %d instead of receiving a signal" name code

let handshake_name = function
  | G.Established pgid -> Printf.sprintf "Established %d" pgid
  | G.Launcher_failed {pgid; message} ->
      Printf.sprintf
        "Launcher_failed (%s): %s"
        (match pgid with
        | Some value -> string_of_int value
        | None -> "no PGID")
        message
  | G.Timed_out -> "Timed_out"
  | G.Invalid message -> "Invalid: " ^ message

let test_handshake_exec_success () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let target =
    spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "sleep"]
  in
  Fun.protect
    ~finally:(fun () ->
      G.terminate ~grace_seconds:0.01 ~clock:(Eio.Stdenv.clock env) target)
    (fun () ->
      match G.handshake target with
      | G.Established pgid ->
          Alcotest.(check int) "setsid PGID is launcher PID" (G.pid target) pgid ;
          Alcotest.(check (option int))
            "confirmed group ID"
            (Some pgid)
            (G.group_id target)
      | outcome ->
           Alcotest.failf
             "expected successful handshake, got %s"
             (handshake_name outcome))

let test_raw_explicit_exec_record_confirms_success () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let process, handshake, control, status =
    spawn_launcher_without_cabal
      ~sw
      ~env
      [helper_path (); "--process-descendant-helper"; "sleep"]
  in
  let clock = Eio.Stdenv.clock env in
  Fun.protect
    ~finally:(fun () ->
      cleanup_raw_launcher ~clock process control ;
      close_noerr handshake ;
      close_noerr control ;
      close_noerr status)
    (fun () ->
      acknowledge_launcher ~clock process handshake control ;
      Alcotest.(check string)
        "only explicit EXEC confirms the raw launcher handshake"
        "EXEC\n"
        (read_handshake_to_eof ~clock handshake))

let test_raw_no_ack_timeout_reports_failure_without_backend () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let process, handshake, control, status =
    spawn_launcher_without_cabal
      ~sw
      ~env
      [helper_path (); "--process-descendant-helper"; "child"; marker]
  in
  let clock = Eio.Stdenv.clock env in
  Fun.protect
    ~finally:(fun () ->
      cleanup_raw_launcher ~clock process control ;
      close_noerr handshake ;
      close_noerr control ;
      close_noerr status ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      read_launcher_pgid ~clock process handshake ;
      let failure = read_handshake_to_eof ~clock handshake in
      Alcotest.(check bool)
        "no-ACK handshake has a controlled ERROR record"
        true
        (String.length failure >= 6 && String.sub failure 0 6 = "ERROR ") ;
      Alcotest.(check bool)
        "no-ACK launcher never forks the backend"
        false
        (Sys.file_exists marker) ;
      Alcotest.(check bool)
        "no-ACK launcher exits"
        true
        (is_signaled (Eio.Process.await process)))

let test_raw_external_death_after_pgid_has_no_exec_record () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let process, handshake, control, status =
    spawn_launcher_without_cabal
      ~sw
      ~env
      [helper_path (); "--process-descendant-helper"; "child"; marker]
  in
  let clock = Eio.Stdenv.clock env in
  Fun.protect
    ~finally:(fun () ->
      cleanup_raw_launcher ~clock process control ;
      close_noerr handshake ;
      close_noerr control ;
      close_noerr status ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      read_launcher_pgid ~clock process handshake ;
      Unix.kill (Eio.Process.pid process) Sys.sigkill ;
      Alcotest.(check string)
        "external launcher death leaves no implicit EXEC record"
        ""
        (read_handshake_to_eof ~clock handshake) ;
      Alcotest.(check bool)
        "external death after PGID never forks the backend"
        false
        (Sys.file_exists marker) ;
      Alcotest.(check bool)
        "externally killed launcher is reaped"
        true
        (is_signaled (Eio.Process.await process)))

let test_invalid_handshake_sequences_never_establish_exec () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  List.iter
    (fun mode ->
      let target =
        spawn
          ~sw
          ~env
          ~launcher:(helper_path ())
          [helper_path (); "--process-descendant-helper"; "fake-launcher-handshake"; mode]
      in
      Fun.protect
        ~finally:(fun () -> try G.terminate ~grace_seconds:0.1 ~clock target with _ -> ())
        (fun () ->
          Alcotest.(check bool)
            (mode ^ " is never an established exec handshake")
            true
            (match G.handshake target with G.Invalid _ -> true | _ -> false) ;
          Alcotest.(check bool)
            (mode ^ " retains confirmed ownership until reap")
            true
            (Option.is_some (G.group_id target)) ;
          G.terminate ~grace_seconds:0.1 ~clock target ;
          Alcotest.(check (option int))
            (mode ^ " ownership is cleared after reap")
            None
            (G.group_id target)))
    ["bare-pgid"; "duplicate-exec"]

let test_spawn_rejects_invalid_handshake_timeouts () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (name, handshake_timeout_seconds) ->
      let rejected =
        try
          ignore
            (spawn
               ~sw
               ~env
               ~handshake_timeout_seconds
               [helper_path (); "--process-descendant-helper"; "sleep"]) ;
          false
        with Invalid_argument _ -> true
      in
      Alcotest.(check bool) (name ^ " handshake timeout is rejected before spawn") true rejected)
    [ ("zero", 0.0);
      ("negative", -0.1);
      ("NaN", Float.nan);
      ("+infinity", Float.infinity);
      ("-infinity", -.Float.infinity);
    ] ;
  let target =
    spawn
      ~sw
      ~env
      ~handshake_timeout_seconds:0.5
      [helper_path (); "--process-descendant-helper"; "sleep"]
  in
  Fun.protect
    ~finally:(fun () ->
      try G.terminate ~grace_seconds:0.1 ~clock:(Eio.Stdenv.clock env) target
      with _ -> ())
    (fun () ->
      Alcotest.(check bool)
        "finite positive handshake timeout remains accepted"
        true
        (match G.handshake target with G.Established _ -> true | _ -> false))

let test_mismatched_handshake_pgid_is_not_owned () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".terminated"])
    (fun () ->
      let target =
        spawn
          ~sw
          ~env
          ~launcher:(helper_path ())
          [
            helper_path ();
            "--process-descendant-helper";
            "fake-launcher-pgid-one";
            marker;
          ]
      in
      Alcotest.(check bool)
        "mismatched PGID makes the handshake invalid"
        true
        (match G.handshake target with G.Invalid _ -> true | _ -> false) ;
      Alcotest.(check (option int))
        "mismatched PGID is never owned"
        None
        (G.group_id target) ;
      let clock = Eio.Stdenv.clock env in
      let started = Eio.Time.now clock in
      G.terminate ~grace_seconds:0.1 ~clock target ;
      Alcotest.(check bool)
        "unconfirmed private control gets a bounded direct-KILL backstop"
        true
        (Eio.Time.now clock -. started >= 0.25) ;
      Alcotest.(check (option bool))
        "direct fake launcher was reaped"
        (Some true)
        (Option.map is_signaled (G.awaited_status target)))

let test_default_launcher_is_resolved_on_path () =
  let launcher = Process_test_helper.launcher_path () in
  let directory = Filename.temp_dir "cabal-launcher-path-" "" in
  let path_launcher =
    Filename.concat directory "cabal-process-group-launcher"
  in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove path_launcher with Sys_error _ -> ()) ;
      try Unix.rmdir directory with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.symlink launcher path_launcher ;
      with_env "CABAL_PROCESS_GROUP_LAUNCHER" "" @@ fun () ->
      with_env "PATH" directory @@ fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let target =
        spawn
          ~sw
          ~env
          [helper_path (); "--process-descendant-helper"; "success"]
      in
      Alcotest.(check bool)
        "unqualified default launcher was found on PATH"
        true
        (match G.handshake target with G.Established _ -> true | _ -> false) ;
      Alcotest.(check bool)
        "PATH launcher exits normally"
        true
        (G.await target = `Exited 0))

let test_preexec_failure_is_reported () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let target = spawn ~sw ~env ["cabal-command-that-does-not-exist-98765"] in
  (match G.handshake target with
  | G.Launcher_failed {pgid = Some _; message} ->
      Alcotest.(check bool) "launcher reports exec error" true (message <> "")
  | _ -> Alcotest.fail "expected launcher pre-exec failure handshake") ;
  Alcotest.(check bool)
    "launcher exits with failure"
    true
    (G.await target = `Exited 127)

let test_natural_exit_is_reaped_once () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let target =
    spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "success"]
  in
  let first = G.await target in
  let second = G.await target in
  Alcotest.(check bool) "first await succeeds" true (first = `Exited 0) ;
  Alcotest.(check bool) "second await is cached" true (second = first) ;
  G.terminate ~grace_seconds:0.1 ~clock:(Eio.Stdenv.clock env) target ;
  Alcotest.(check bool)
    "termination tolerates natural exit"
    true
    (G.await target = first) ;
  Alcotest.(check (option bool))
    "reaped status is cached"
    (Some true)
    (Option.map (( = ) (`Exited 0)) (G.awaited_status target))

let test_backend_status_keeps_group_anchored_until_release () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let target =
    spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "success"]
  in
  Alcotest.(check bool)
    "backend completion is available before release"
    true
    (G.await_backend target = `Exited 0) ;
  Alcotest.(check bool)
    "supervisor remains unreaped before release"
    true
    (Option.is_none (G.awaited_status target)) ;
  Alcotest.(check bool)
    "confirmed group remains anchored before release"
    true
    (Option.is_some (G.group_id target)) ;
  G.release target ;
  Alcotest.(check bool)
    "release returns the backend status"
    true
    (G.await target = `Exited 0) ;
  Alcotest.(check (option int))
    "released target retires its PGID ownership"
    None
    (G.group_id target) ;
  Alcotest.(check (option bool))
    "released supervisor status is cached"
    (Some true)
    (Option.map (( = ) (`Exited 0)) (G.awaited_status target))

let test_signal_status_is_preserved () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (name, signal, action) ->
      let target =
        spawn ~sw ~env [helper_path (); "--process-descendant-helper"; action]
      in
      check_signaled name signal (G.await target))
    [
      ("self SIGTERM remains Signaled", Sys.sigterm, "self-term");
      ("self SIGKILL remains Signaled", Sys.sigkill, "self-kill");
    ]

let test_cancelled_before_pgid_never_spawns_backend () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_BEFORE_PGID_DELAY_SECONDS" "0.3"
      @@ fun () ->
      let process, handshake, control, status =
        spawn_launcher_without_cabal
          ~sw
          ~env
          [helper_path (); "--process-descendant-helper"; "child"; marker]
      in
      (* This is the parent-side effect of cancellation before it receives any
         handshake bytes. The launcher must fail its PGID write before forking
         the backend. *)
      close_noerr handshake ;
      close_noerr control ;
      close_noerr status ;
      ignore process ;
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.4 ;
      Alcotest.(check bool)
        "no backend was forked before handshake delivery failed"
        false
        (Sys.file_exists marker))

let test_control_loss_terminates_unknown_group () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".terminated"])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_TERM_GRACE_SECONDS" "0.1" @@ fun () ->
      let process, handshake, control, status =
        spawn_launcher_without_cabal
          ~sw
          ~env
          [
            helper_path ();
            "--process-descendant-helper";
            "spawn-term-observing-child";
            marker;
          ]
       in
       let clock = Eio.Stdenv.clock env in
       acknowledge_launcher ~clock process handshake control ;
       wait_for_file ~clock marker ;
      (* Do not inspect FD3: Cabal never learns the PGID and only loses its
         protocol ownership. The launcher must clean up independently. *)
      close_noerr handshake ;
      close_noerr control ;
      close_noerr status ;
      wait_for_file ~clock (marker ^ ".terminated") ;
      wait_for_pid_exit ~clock (child_pid marker) ;
      let outcome =
        Eio.Time.with_timeout clock 1.0 (fun () ->
            Ok (Eio.Process.await process))
      in
      Alcotest.(check bool)
        "self-terminating supervisor is reaped after parent loss"
        true
        (match outcome with Ok _ -> true | Error `Timeout -> false))

let test_repeated_direct_sigterm_keeps_the_original_grace () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let process = ref None in
  Fun.protect
    ~finally:(fun () ->
      Option.iter
        (fun launcher_pid ->
          try Unix.kill (-launcher_pid) Sys.sigkill
          with Unix.Unix_error _ -> ())
        !process ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".terminated"])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_TERM_GRACE_SECONDS" "0.5" @@ fun () ->
      let spawned, handshake, control, status =
        spawn_launcher_without_cabal
          ~sw
          ~env
          ~process_env:[|"CABAL_PROCESS_GROUP_TEST_TERM_GRACE_SECONDS=0.5"|]
          [
            helper_path ();
            "--process-descendant-helper";
            "spawn-term-recording-ignoring-child";
            marker;
          ]
      in
       process := Some (Eio.Process.pid spawned) ;
       let clock = Eio.Stdenv.clock env in
       acknowledge_launcher ~clock spawned handshake control ;
       Fun.protect
        ~finally:(fun () ->
          close_noerr handshake ;
          close_noerr control ;
          close_noerr status)
        (fun () ->
          wait_for_file ~clock marker ;
          let started = Eio.Time.now clock in
          Unix.kill (Eio.Process.pid spawned) Sys.sigterm ;
          Eio.Time.sleep clock 0.1 ;
          Unix.kill (Eio.Process.pid spawned) Sys.sigterm ;
          wait_for_file ~clock (marker ^ ".terminated") ;
          let outcome =
            Eio.Time.with_timeout clock 2.0 (fun () ->
                Ok (Eio.Process.await spawned))
          in
          (match outcome with Ok _ -> process := None | Error `Timeout -> ()) ;
          let elapsed = Eio.Time.now clock -. started in
          Alcotest.(check bool)
            "launcher is killed after the grace period"
            true
            (match outcome with
            | Ok status -> is_signaled status
            | Error `Timeout -> false) ;
          if elapsed < 0.4 then
            Alcotest.failf
              "repeated SIGTERM ended the launcher after %.3fs, before the \
               grace"
              elapsed ;
          Alcotest.(check bool)
            "graceful cleanup remains bounded"
            true
           (elapsed < 1.5) ;
           wait_for_pid_exit ~clock (child_pid marker)))

let test_malformed_control_records_use_fallback () =
  with_launcher @@ fun () ->
  List.iter
    (fun seconds ->
      Alcotest.(check bool)
        (Printf.sprintf "OCaml parses %s" seconds)
        true
        (Option.is_some (float_of_string_opt seconds)))
    ["nan"; "infinity"; "-infinity"] ;
  with_env "CABAL_PROCESS_GROUP_TEST_TERM_GRACE_SECONDS" "0.2" @@ fun () ->
  List.iter
    (fun record ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let marker = fresh_marker () in
      let process = ref None in
      let reaped = ref false in
      Fun.protect
        ~finally:(fun () ->
          Option.iter
            (fun (launcher, control, handshake, status) ->
              if not !reaped then cleanup_raw_launcher ~clock:(Eio.Stdenv.clock env) launcher control ;
              close_noerr handshake ;
              close_noerr control ;
              close_noerr status)
            !process ;
          List.iter
            (fun path -> try Sys.remove path with Sys_error _ -> ())
            [marker; marker ^ ".pid"; marker ^ ".terminated"])
        (fun () ->
          let launcher, handshake, control, status =
            spawn_launcher_without_cabal
              ~sw
              ~env
              [
                helper_path ();
                "--process-descendant-helper";
                "term-recording-ignoring-child";
                marker;
              ]
          in
          process := Some (launcher, control, handshake, status) ;
          let clock = Eio.Stdenv.clock env in
          acknowledge_launcher ~clock launcher handshake control ;
          wait_for_file ~clock marker ;
          let descendant = child_pid marker in
          let started = Eio.Time.now clock in
          let started_wall = Unix.gettimeofday () in
          Eio.Flow.copy_string (record ^ "\n") control ;
          wait_for_file ~clock (marker ^ ".terminated") ;
          let observed = marker_timestamp (marker ^ ".terminated") in
          let outcome =
            Eio.Time.with_timeout clock 1.0 (fun () ->
                Ok (Eio.Process.await launcher))
          in
          (match outcome with Ok _ -> reaped := true | Error `Timeout -> ()) ;
          Alcotest.(check bool)
            (record ^ " delivers TERM before fallback KILL")
            true
            (observed -. started_wall < 0.15) ;
          Alcotest.(check bool)
            (record ^ " uses the fallback deadline")
            true
            (Eio.Time.now clock -. started >= 0.15) ;
          Alcotest.(check bool)
            (record ^ " fallback remains bounded")
            true
            (match outcome with Ok status -> is_signaled status | Error `Timeout -> false) ;
          wait_for_pid_exit ~clock descendant))
    ["TERM nan"; "TERM infinity"; "TERM -infinity"; "TERM -0.1"; "TERM garbage"]

let test_first_valid_control_record_wins () =
  with_launcher @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_TERM_GRACE_SECONDS" "0.2" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let process = ref None in
  let reaped = ref false in
  Fun.protect
    ~finally:(fun () ->
      Option.iter
        (fun (launcher, control, handshake, status) ->
          if not !reaped then cleanup_raw_launcher ~clock:(Eio.Stdenv.clock env) launcher control ;
          close_noerr handshake ;
          close_noerr control ;
          close_noerr status)
        !process ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".terminated"])
    (fun () ->
      let launcher, handshake, control, status =
        spawn_launcher_without_cabal
          ~sw
          ~env
          [
            helper_path ();
            "--process-descendant-helper";
            "term-recording-ignoring-child";
            marker;
          ]
      in
      process := Some (launcher, control, handshake, status) ;
      let clock = Eio.Stdenv.clock env in
      acknowledge_launcher ~clock launcher handshake control ;
      wait_for_file ~clock marker ;
      let descendant = child_pid marker in
      let started = Eio.Time.now clock in
      let started_wall = Unix.gettimeofday () in
      Eio.Flow.copy_string "TERM 0.6\n" control ;
      wait_for_file ~clock (marker ^ ".terminated") ;
      let observed = marker_timestamp (marker ^ ".terminated") in
      Eio.Time.sleep clock 0.15 ;
      Eio.Flow.copy_string "TERM 0.05\n" control ;
      let outcome =
        Eio.Time.with_timeout clock 1.2 (fun () -> Ok (Eio.Process.await launcher))
      in
      (match outcome with Ok _ -> reaped := true | Error `Timeout -> ()) ;
      let elapsed = Eio.Time.now clock -. started in
      Alcotest.(check bool)
        "first valid TERM is observed before the later record"
        true
        (observed -. started_wall < 0.2) ;
      Alcotest.(check bool)
        "later shorter TERM does not shorten the first deadline"
        true
        (elapsed >= 0.5) ;
      Alcotest.(check bool)
        "later TERM does not reset the first deadline"
        true
        (elapsed < 0.75) ;
      Alcotest.(check bool)
        "first-valid TERM launcher is reaped"
        true
        (match outcome with Ok status -> is_signaled status | Error `Timeout -> false) ;
      wait_for_pid_exit ~clock descendant)

let test_cancelled_handshake_after_fork_reaps_child () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_HANDSHAKE_DELAY_SECONDS" "1.0"
      @@ fun () ->
      let outcome =
        Eio.Time.with_timeout (Eio.Stdenv.clock env) 0.1 (fun () ->
            ignore
              (spawn
                 ~sw
                 ~env
                 [
                   helper_path (); "--process-descendant-helper"; "child"; marker;
                 ]) ;
            Ok ())
      in
      Alcotest.(check bool)
        "cancellation interrupts the delayed post-fork handshake"
        true
        (match outcome with Error `Timeout -> true | Ok () -> false) ;
      let clock = Eio.Stdenv.clock env in
      wait_for_file ~clock marker ;
      wait_for_pid_exit ~clock (child_pid marker))

let test_child_switch_shutdown_terminates_unawaited_target () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".terminated"])
    (fun () ->
      let clock = Eio.Stdenv.clock env in
      let supervisor_pid = ref None in
      let started = Eio.Time.now clock in
      let outcome =
        Eio.Time.with_timeout clock 4.0 (fun () ->
            Eio.Switch.run @@ fun child_switch ->
            let target =
              spawn
                ~sw:child_switch
                ~env
                [
                  helper_path ();
                  "--process-descendant-helper";
                  "spawn-term-observing-child";
                  marker;
                ]
            in
            supervisor_pid := Some (G.pid target) ;
            wait_for_file ~clock marker ;
            Ok ())
      in
      Alcotest.(check bool)
        "child switch finishes without awaiting backend status"
        true
        (match outcome with Ok () -> true | Error `Timeout -> false) ;
      Alcotest.(check bool)
        "switch finalizer waited for the default TERM grace before KILL"
        true
        (Eio.Time.now clock -. started >= 1.5) ;
      let supervisor_pid =
        match !supervisor_pid with
        | Some pid -> pid
        | None -> Alcotest.fail "target was not spawned"
      in
      wait_for_file ~clock (marker ^ ".terminated") ;
      wait_for_pid_exit ~clock supervisor_pid ;
      wait_for_pid_exit ~clock (child_pid marker))

let test_explicit_relative_launcher_uses_host_cwd_and_failed_path_leaks_no_pipes
    () =
  let launcher = Process_test_helper.launcher_path () in
  let host_cwd = Sys.getcwd () in
  let link =
    Filename.temp_file ~temp_dir:host_cwd ".cabal-relative-launcher-" ""
  in
  let backend_cwd = Filename.temp_dir "cabal-backend-cwd-" "" in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove link with Sys_error _ -> ()) ;
      try Unix.rmdir backend_cwd with Unix.Unix_error _ -> ())
    (fun () ->
      Sys.remove link ;
      Unix.symlink launcher link ;
      let relative_launcher = "./" ^ Filename.basename link in
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let count_fds = fd_counter () in
      let before_failure = Option.map (fun count -> count ()) count_fds in
      let failed =
        try
          ignore
            (spawn
               ~sw
               ~env
               ~launcher:"./cabal-missing-relative-launcher-98765"
               [helper_path (); "--process-descendant-helper"; "success"]) ;
          false
        with Unix.Unix_error _ -> true
      in
      Alcotest.(check bool)
        "missing explicit launcher fails before spawn"
        true
        failed ;
      (match (before_failure, count_fds) with
      | Some before, Some count -> (
          try
            Alcotest.(check int)
              "failed launcher resolution leaks no pipe descriptors"
              before
              (count ())
          with Sys_error _ -> ())
      | None, _ | Some _, None -> ()) ;
      let target =
        spawn
          ~sw
          ~env
          ~cwd:Eio.Path.(Eio.Stdenv.fs env / backend_cwd)
          ~launcher:relative_launcher
          [helper_path (); "--process-descendant-helper"; "success"]
      in
      Alcotest.(check bool)
        "relative launcher resolved from host cwd"
        true
        (G.await target = `Exited 0))

let test_long_launcher_error_is_bounded () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let target = spawn ~sw ~env [String.make 8192 'x'] in
  Alcotest.(check bool)
    "long exec failure remains a launcher failure"
    true
    (match G.handshake target with G.Launcher_failed _ -> true | _ -> false) ;
  Alcotest.(check bool)
    "long exec failure exits through the supervisor"
    true
    (G.await target = `Exited 127)

let test_explicit_termination () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let target =
    spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "sleep"]
  in
  G.terminate ~grace_seconds:0.1 ~clock:(Eio.Stdenv.clock env) target ;
  G.terminate ~grace_seconds:0.1 ~clock:(Eio.Stdenv.clock env) target ;
  Alcotest.(check bool)
    "termination reaps the target"
    true
    (is_signaled (G.await target))

let test_explicit_termination_propagates_custom_grace () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let target =
    spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "sleep"]
  in
  let started = Eio.Time.now clock in
  G.terminate ~grace_seconds:4.0 ~clock target ;
  let elapsed = Eio.Time.now clock -. started in
  Alcotest.(check bool)
    "custom-grace termination reaps the target"
    true
    (is_signaled (G.await target)) ;
  Alcotest.(check bool)
    "launcher honors the requested grace beyond its fallback"
    true
    (elapsed >= 3.5) ;
  Alcotest.(check bool)
    "custom-grace termination remains bounded"
    true
    (elapsed < 5.5)

let test_explicit_termination_rejects_invalid_grace () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let target =
    spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "sleep"]
  in
  Fun.protect
    ~finally:(fun () ->
      try G.terminate ~grace_seconds:0.1 ~clock target with _ -> ())
    (fun () ->
      List.iter
        (fun grace_seconds ->
          try
            G.terminate ~grace_seconds ~clock target ;
            Alcotest.fail "invalid termination grace was accepted"
          with Invalid_argument _ -> ())
        [-1.0; Float.nan; Float.infinity])

let test_await_does_not_block_concurrent_termination () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let target =
    spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "sleep"]
  in
  Fun.protect
    ~finally:(fun () ->
      match G.group_id target with
      | Some pgid -> (
          try Unix.kill (-pgid) Sys.sigkill with Unix.Unix_error _ -> ())
      | None -> ())
    (fun () ->
      let awaiting = Eio.Fiber.fork_promise ~sw (fun () -> G.await target) in
      (* Let the concurrent await begin before terminating the target. *)
      Eio.Fiber.yield () ;
      let outcome =
        Eio.Time.with_timeout clock 1.0 (fun () ->
            G.terminate ~grace_seconds:0.1 ~clock target ;
            Ok (Eio.Promise.await_exn awaiting))
      in
      match outcome with
      | Error `Timeout -> Alcotest.fail "terminate blocked behind await"
      | Ok status ->
          Alcotest.(check bool)
            "awaited process was terminated"
            true
            (is_signaled status) ;
          Alcotest.(check bool)
            "cached await returns the same reap"
            true
            (G.await target = status))

let test_fallback_never_uses_an_unconfirmed_group () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* This executable deliberately implements no handshake and remains in the
     test runner's own process group. A negative-PID signal would miss it;
     successful termination therefore proves the direct-PID fallback path. *)
  let target =
    spawn
      ~sw
      ~env
      ~launcher:(helper_path ())
      ~handshake_timeout_seconds:0.1
      [helper_path (); "--process-descendant-helper"; "broken-launcher"]
  in
  Alcotest.(check bool)
    "group was not confirmed"
    true
    (Option.is_none (G.group_id target)) ;
  Alcotest.(check bool)
    "handshake timeout is explicit"
    true
    (match G.handshake target with G.Timed_out -> true | _ -> false) ;
  G.terminate ~grace_seconds:0.1 ~clock:(Eio.Stdenv.clock env) target ;
  Alcotest.(check bool)
    "direct fallback was reaped"
    true
    (is_signaled (G.await target))

let test_unconfirmed_control_delivery_waits_for_fallback () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let target =
    spawn
      ~sw
      ~env
      ~launcher:(helper_path ())
      [
        helper_path ();
        "--process-descendant-helper";
        "fake-launcher-closed-control";
        marker;
      ]
  in
  let clock = Eio.Stdenv.clock env in
  Fun.protect
    ~finally:(fun () ->
      (try G.terminate ~grace_seconds:0.1 ~clock target with _ -> ()) ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".terminated"])
    (fun () ->
      Alcotest.(check (option int))
        "closed-control launcher has no confirmed PGID"
        None
        (G.group_id target) ;
      wait_for_file ~clock marker ;
      let started = Eio.Time.now clock in
      let started_wall = Unix.gettimeofday () in
      G.terminate ~grace_seconds:0.1 ~clock target ;
      wait_for_file ~clock (marker ^ ".terminated") ;
      let observed = marker_timestamp (marker ^ ".terminated") in
      let elapsed = Eio.Time.now clock -. started in
      Alcotest.(check bool)
        "failed FD4 delivery falls back to direct TERM before the backstop"
        true
        (observed -. started_wall < 0.2) ;
      Alcotest.(check bool)
        "failed FD4 delivery waits through the launcher fallback"
        true
        (elapsed >= 2.0) ;
      Alcotest.(check bool)
        "failed FD4 delivery remains bounded"
        true
        (elapsed < 3.0) ;
      Alcotest.(check bool)
        "failed FD4 direct supervisor was reaped"
        true
        (Option.is_some (G.awaited_status target)))

let test_pre_ack_cancellation_never_forks_backend () =
  with_launcher @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_BEFORE_ACK_DELAY_SECONDS" "1.0" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let clock = Eio.Stdenv.clock env in
  let target =
    spawn
      ~sw
      ~env
      ~handshake_timeout_seconds:0.1
      [helper_path (); "--process-descendant-helper"; "child"; marker]
  in
  let stopped = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !stopped then
        (try Unix.kill (G.pid target) Sys.sigcont with Unix.Unix_error _ -> ()) ;
      (try G.terminate ~grace_seconds:0.1 ~clock target with _ -> ()) ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      Alcotest.(check bool)
        "parent records confirmed ownership before launcher consumes ACK"
        true
        (Option.is_some (G.group_id target)) ;
      Unix.kill (G.pid target) Sys.sigstop ;
      stopped := true ;
      G.terminate ~grace_seconds:0.1 ~clock target ;
      stopped := false ;
      Eio.Time.sleep clock 0.1 ;
      Alcotest.(check bool)
        "stopped pre-ACK launcher never created the backend marker"
        false
        (Sys.file_exists marker) ;
      Alcotest.(check bool)
        "stopped pre-ACK supervisor was reaped"
        true
        (Option.is_some (G.awaited_status target)))

let test_stopped_confirmed_supervisor_does_not_delay_group_term () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let clock = Eio.Stdenv.clock env in
  let target =
    spawn
      ~sw
      ~env
      [
        helper_path ();
        "--process-descendant-helper";
        "term-recording-ignoring-child";
        marker;
      ]
  in
  let stopped = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !stopped then
        (try Unix.kill (G.pid target) Sys.sigcont with Unix.Unix_error _ -> ()) ;
      (try G.terminate ~grace_seconds:0.1 ~clock target with _ -> ()) ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".terminated"])
    (fun () ->
      wait_for_file ~clock marker ;
      let descendant = child_pid marker in
      Unix.kill (G.pid target) Sys.sigstop ;
      stopped := true ;
      let started = Eio.Time.now clock in
      let started_wall = Unix.gettimeofday () in
      G.terminate ~grace_seconds:0.5 ~clock target ;
      stopped := false ;
      wait_for_file ~clock (marker ^ ".terminated") ;
      let observed = marker_timestamp (marker ^ ".terminated") in
      let elapsed = Eio.Time.now clock -. started in
      Alcotest.(check bool)
        "parent group TERM reaches the stopped supervisor's backend immediately"
        true
        (observed -. started_wall < 0.2) ;
      Alcotest.(check bool)
        "parent preserves the full requested grace before group KILL"
        true
        (elapsed >= 0.45 && elapsed < 1.5) ;
      wait_for_pid_exit ~clock descendant ;
      Alcotest.(check bool)
        "stopped confirmed supervisor was reaped"
        true
        (Option.is_some (G.awaited_status target)))

let test_unexpected_supervisor_death_cleans_confirmed_descendants () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let clock = Eio.Stdenv.clock env in
  let target =
    spawn
      ~sw
      ~env
        [ helper_path ();
        "--process-descendant-helper";
        "gated-kill-supervisor-ignoring-term";
        marker;
      ]
  in
  Fun.protect
    ~finally:(fun () ->
      (try G.terminate ~grace_seconds:0.1 ~clock target with _ -> ()) ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".allow-supervisor-death"])
    (fun () ->
      wait_for_file ~clock marker ;
      let descendant = child_pid marker in
      allow_supervisor_death marker ;
      let started = Eio.Time.now clock in
      let outcome =
        Eio.Time.with_timeout clock 4.0 (fun () -> Ok (G.await target))
      in
      Alcotest.(check bool)
        "unexpected supervisor death keeps await bounded"
        true
        (match outcome with Ok _ -> true | Error `Timeout -> false) ;
      Alcotest.(check bool)
        "unexpected supervisor cleanup forfeits grace and returns promptly"
        true
        (Eio.Time.now clock -. started < 1.0) ;
      wait_for_pid_exit ~clock descendant ;
      Alcotest.(check (option int))
        "unexpected supervisor cleanup clears group ownership only after KILL"
        None
        (G.group_id target) ;
      Alcotest.(check bool)
        "unexpected supervisor direct status is cached"
        true
        (Option.is_some (G.awaited_status target)))

let test_unexpected_supervisor_death_coordinates_with_termination () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let clock = Eio.Stdenv.clock env in
  let target =
    spawn
      ~sw
      ~env
      [ helper_path ();
        "--process-descendant-helper";
        "gated-kill-supervisor-recording-term";
        marker;
      ]
  in
  Fun.protect
    ~finally:(fun () ->
      (try G.terminate ~grace_seconds:0.1 ~clock target with _ -> ()) ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ marker;
          marker ^ ".pid";
          marker ^ ".term-observed";
          marker ^ ".allow-supervisor-death";
        ])
    (fun () ->
      wait_for_file ~clock marker ;
      let descendant = child_pid marker in
      let terminating =
        Eio.Fiber.fork_promise ~sw (fun () ->
            G.terminate ~grace_seconds:0.3 ~clock target)
      in
      wait_for_file ~clock (marker ^ ".term-observed") ;
      allow_supervisor_death marker ;
      let outcome =
        Eio.Time.with_timeout clock 2.0 (fun () ->
            Eio.Promise.await_exn terminating ;
            Ok ())
      in
      Alcotest.(check bool)
        "termination leader completes after unexpected supervisor death"
        true
        (match outcome with Ok () -> true | Error `Timeout -> false) ;
      wait_for_pid_exit ~clock descendant ;
      Alcotest.(check (option int))
        "coordinated termination retires the confirmed group"
        None
        (G.group_id target) ;
      Alcotest.(check bool)
        "coordinated termination caches direct supervisor status"
        true
        (Option.is_some (G.awaited_status target)))

let file_contents_or_empty path =
  if not (Sys.file_exists path) then ""
  else
    let channel = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let length = in_channel_length channel in
        really_input_string channel length)

let signal_log_records path =
  file_contents_or_empty path |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         if line = "" then None
         else
           try Some (Scanf.sscanf line "%d %d" (fun pgid signal -> (pgid, signal)))
           with _ -> Alcotest.failf "invalid signal-log record %S" line)

let wait_for_signal_log_count ~clock path count =
  let deadline = Eio.Time.now clock +. 2.0 in
  let rec loop () =
    let records = signal_log_records path in
    if List.length records >= count then records
    else if Eio.Time.now clock >= deadline then
      Alcotest.failf
        "timed out waiting for %d signal-log records (saw %d)"
        count
        (List.length records)
    else begin
      Eio.Time.sleep clock 0.02 ;
      loop ()
    end
  in
  loop ()

let wait_for_signal_log_suffix ~clock path suffix =
  let deadline = Eio.Time.now clock +. 2.0 in
  let rec has_suffix records suffix =
    match (records, suffix) with
    | _, [] -> true
    | [], _ :: _ -> false
    | _ :: records, _ when List.length records >= List.length suffix ->
        has_suffix records suffix
    | _ -> records = suffix
  in
  let rec loop () =
    let records = signal_log_records path in
    if has_suffix records suffix then records
    else if Eio.Time.now clock >= deadline then
      Alcotest.fail "timed out waiting for signal-log suffix"
    else begin
      Eio.Time.sleep clock 0.02 ;
      loop ()
    end
  in
  loop ()

let test_release_retires_ownership_before_later_termination () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let signal_log = fresh_marker () in
  Fun.protect
    ~finally:(fun () -> try Sys.remove signal_log with Sys_error _ -> ())
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" signal_log @@ fun () ->
      let target =
        spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "success"]
      in
      let clock = Eio.Stdenv.clock env in
      Alcotest.(check bool)
        "backend completed before ownership release"
        true
        (G.await_backend target = `Exited 0) ;
      G.release target ;
      Alcotest.(check (option int))
        "RELEASE atomically hides the PGID"
        None
        (G.group_id target) ;
      G.terminate ~grace_seconds:0.1 ~clock target ;
      Alcotest.(check string)
        "later terminate sends no stale group signal"
        ""
        (file_contents_or_empty signal_log) ;
      Alcotest.(check bool)
        "released supervisor reaps normally"
        true
        (G.await target = `Exited 0) ;
      Alcotest.(check bool)
        "released supervisor status is retained"
        true
        (Option.is_some (G.awaited_status target)))

let test_premature_release_retains_ownership_until_switch_cleanup () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  let marker = fresh_marker () in
  let target = ref None in
  let descendant = ref None in
  let retained_ownership = ref false in
  let emergency_cleanup_used = ref false in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".terminated"])
    (fun () ->
      let clock = Eio.Stdenv.clock env in
      let outcome =
        Eio.Time.with_timeout clock 4.0 (fun () ->
            Eio.Switch.run @@ fun caller_sw ->
            let owned =
              spawn
                ~sw:caller_sw
                ~env
                [ helper_path ();
                  "--process-descendant-helper";
                  "child";
                  marker;
                ]
            in
            target := Some owned ;
            wait_for_file ~clock marker ;
            descendant := Some (child_pid marker) ;
            let pgid =
              match G.group_id owned with
              | Some pgid -> pgid
              | None -> Alcotest.fail "running target has no confirmed PGID"
            in
            G.release owned ;
            retained_ownership := Option.is_some (G.group_id owned) ;
            if not !retained_ownership then begin
              (* Keep the pre-fix regression bounded. This branch is forbidden
                 on the passing path; caller-switch cleanup must own the kill. *)
              emergency_cleanup_used := true ;
              (try Unix.kill (-pgid) Sys.sigkill with Unix.Unix_error _ -> ())
            end ;
            Ok ())
      in
      Alcotest.(check bool)
        "caller-switch shutdown remains bounded after premature release"
        true
        (match outcome with Ok () -> true | Error `Timeout -> false) ;
      Alcotest.(check bool)
        "premature release retains process-group ownership"
        true
        !retained_ownership ;
      Alcotest.(check bool)
        "switch cleanup, not a test backstop, terminates the group"
        false
        !emergency_cleanup_used ;
      let owned =
        match !target with Some target -> target | None -> Alcotest.fail "no target"
      in
      wait_for_file ~clock (marker ^ ".terminated") ;
      Option.iter (wait_for_pid_exit ~clock) !descendant ;
      wait_for_pid_exit ~clock (G.pid owned) ;
      Alcotest.(check (option int))
        "switch cleanup retires ownership"
        None
        (G.group_id owned) ;
      Alcotest.(check bool)
        "switch cleanup reaps the direct supervisor"
        true
        (Option.is_some (G.awaited_status owned)))

let test_failed_release_after_external_supervisor_death_has_no_group_signal () =
  with_launcher @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_CLOSE_CONTROL_AFTER_EXEC" "1" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let signal_log = fresh_marker () in
  Fun.protect
    ~finally:(fun () -> try Sys.remove signal_log with Sys_error _ -> ())
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" signal_log @@ fun () ->
      let target =
        spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "success"]
      in
      let clock = Eio.Stdenv.clock env in
      Fun.protect
        ~finally:(fun () -> try G.terminate ~grace_seconds:0.1 ~clock target with _ -> ())
        (fun () ->
          ignore (G.await_backend target) ;
          Unix.kill (G.pid target) Sys.sigkill ;
          (* FD4 was already closed by the live launcher hook. Releasing before
             the liveness observer runs deterministically exercises its failed
             write path after external supervisor death. *)
          G.release target ;
          Alcotest.(check (option int))
            "failed RELEASE clears cached PGID after external supervisor death"
            None
            (G.group_id target) ;
          ignore (wait_for_supervisor_reap ~clock target) ;
          G.terminate ~grace_seconds:0.1 ~clock target ;
          Alcotest.(check string)
            "release after observed death emits no stale negative-PID signal"
            ""
            (file_contents_or_empty signal_log) ;
          Alcotest.(check bool)
            "observer caches externally killed supervisor status after failed RELEASE"
            true
            (Option.is_some (G.awaited_status target))))

let test_failed_release_uses_direct_cleanup_without_group_signal () =
  with_launcher @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_CLOSE_CONTROL_AFTER_EXEC" "1" @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_TERM_GRACE_SECONDS" "0.1" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let signal_log = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; signal_log])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" signal_log @@ fun () ->
      let target =
        spawn
          ~sw
          ~env
          [helper_path (); "--process-descendant-helper"; "spawn-exit-child"; marker]
      in
      let clock = Eio.Stdenv.clock env in
      Fun.protect
        ~finally:(fun () -> try G.terminate ~grace_seconds:0.1 ~clock target with _ -> ())
        (fun () ->
          Alcotest.(check bool)
            "backend completes before the live RELEASE write failure"
            true
            (G.await_backend target = `Exited 0) ;
          wait_for_file ~clock marker ;
          let descendant = child_pid marker in
          G.release target ;
          Alcotest.(check (option int))
            "failed RELEASE clears cached group ownership before direct TERM"
            None
            (G.group_id target) ;
          Alcotest.(check string)
            "failed RELEASE sends no parent negative-PID signal"
            ""
            (file_contents_or_empty signal_log) ;
          wait_for_pid_exit ~clock descendant ;
          ignore (G.await target) ;
          Alcotest.(check bool)
            "direct launcher cleanup reaps the full group"
            true
            (Option.is_some (G.awaited_status target)) ;
          Alcotest.(check string)
            "later await and terminate keep stale group signals absent"
            ""
            (file_contents_or_empty signal_log)))

let test_terminating_target_cannot_release_ownership () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".terminated"])
    (fun () ->
      let clock = Eio.Stdenv.clock env in
      let target =
        spawn
          ~sw
          ~env
          [
            helper_path ();
            "--process-descendant-helper";
            "term-recording-ignoring-child";
            marker;
          ]
      in
      wait_for_file ~clock marker ;
      let terminating =
        Eio.Fiber.fork_promise ~sw (fun () ->
            G.terminate ~grace_seconds:0.5 ~clock target)
      in
      wait_for_file ~clock (marker ^ ".terminated") ;
      G.release target ;
      Alcotest.(check bool)
        "concurrent RELEASE cannot transfer a terminating PGID"
        true
        (Option.is_some (G.group_id target)) ;
      Eio.Promise.await_exn terminating ;
      Alcotest.(check (option int))
        "reap retires terminating ownership"
        None
        (G.group_id target) ;
      Alcotest.(check bool)
        "terminating supervisor was reaped"
        true
        (Option.is_some (G.awaited_status target)))

let test_unexpected_supervisor_death_immediately_escalates_and_retires () =
  with_launcher @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_GROUP_KILL" "1" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let signal_log = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ marker;
          marker ^ ".pid";
          marker ^ ".terminated";
          marker ^ ".allow-supervisor-death";
          signal_log;
        ])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" signal_log @@ fun () ->
      let clock = Eio.Stdenv.clock env in
      let target =
        spawn
          ~sw
          ~env
          [ helper_path ();
            "--process-descendant-helper";
            "gated-kill-supervisor-exiting-on-term";
            marker;
          ]
      in
      let pgid =
        match G.group_id target with
        | Some pgid -> pgid
        | None -> Alcotest.fail "post-EXEC target has no confirmed PGID"
      in
      wait_for_file ~clock marker ;
      let descendant = child_pid marker in
      allow_supervisor_death marker ;
      let started = Eio.Time.now clock in
      let completion =
        Eio.Time.with_timeout clock 1.0 (fun () -> Ok (G.await target))
      in
      Alcotest.(check bool)
        "public completion resolves promptly after supervisor loss"
        true
        (match completion with Ok _ -> true | Error `Timeout -> false) ;
      Alcotest.(check bool)
        "catastrophic cleanup does not consume the default grace"
        true
        (Eio.Time.now clock -. started < 1.0) ;
      wait_for_file ~clock (marker ^ ".terminated") ;
      wait_for_pid_exit ~clock descendant ;
      wait_for_pid_exit ~clock (G.pid target) ;
      Alcotest.(check (option int))
        "catastrophic cleanup retires group ownership"
        None
        (G.group_id target) ;
      let expected_pair = [(pgid, Sys.sigterm); (pgid, Sys.sigkill)] in
      let records =
        wait_for_signal_log_suffix ~clock signal_log expected_pair
      in
      Alcotest.(check (list (pair int int)))
        "catastrophic TERM and KILL attempts are adjacent"
        expected_pair
        (List.rev records |> function kill :: term :: _ -> [term; kill] | _ -> []) ;
      Eio.Time.sleep clock 2.3 ;
      Alcotest.(check (list (pair int int)))
        "retirement leaves no delayed negative-PGID signal"
        records
        (signal_log_records signal_log) ;
      Alcotest.(check bool)
        "direct supervisor completion remains public"
        true
        (Option.is_some (G.awaited_status target)))

let test_observer_takes_over_sleeping_termination_leader () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let signal_log = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ marker;
          marker ^ ".pid";
          marker ^ ".term-observed";
          marker ^ ".allow-supervisor-death";
          signal_log;
        ])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" signal_log @@ fun () ->
      let clock = Eio.Stdenv.clock env in
      let target =
        spawn
          ~sw
          ~env
          [ helper_path ();
            "--process-descendant-helper";
            "gated-kill-supervisor-recording-term";
            marker;
          ]
      in
      let pgid =
        match G.group_id target with
        | Some pgid -> pgid
        | None -> Alcotest.fail "post-EXEC target has no confirmed PGID"
      in
      wait_for_file ~clock marker ;
      let descendant = child_pid marker in
      let started = Eio.Time.now clock in
      let terminating =
        Eio.Fiber.fork_promise ~sw (fun () ->
            G.terminate ~grace_seconds:0.8 ~clock target)
      in
      wait_for_file ~clock (marker ^ ".term-observed") ;
      allow_supervisor_death marker ;
      let completion =
        Eio.Time.with_timeout clock 1.0 (fun () -> Ok (G.await target))
      in
      Alcotest.(check bool)
        "observer takeover resolves public completion"
        true
        (match completion with Ok _ -> true | Error `Timeout -> false) ;
      Alcotest.(check bool)
        "observer takeover does not wait for the old leader's grace"
        true
        (Eio.Time.now clock -. started < 0.7) ;
      Eio.Promise.await_exn terminating ;
      wait_for_pid_exit ~clock descendant ;
      let expected =
        [ (pgid, Sys.sigterm); (pgid, Sys.sigterm); (pgid, Sys.sigkill) ]
      in
      Alcotest.(check (list (pair int int)))
        "takeover TERM/KILL is adjacent and old leader sends no stale KILL"
        expected
        (wait_for_signal_log_count ~clock signal_log 3) ;
      Alcotest.(check (option int))
        "observer takeover retires ownership"
        None
        (G.group_id target))

let test_invalid_signal_log_is_best_effort () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let missing_parent = fresh_marker () in
  let invalid_log = Filename.concat missing_parent "signals.log" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" invalid_log @@ fun () ->
      let clock = Eio.Stdenv.clock env in
      let target =
        spawn
          ~sw
          ~env
          [helper_path (); "--process-descendant-helper"; "term-ignoring-child"; marker]
      in
      wait_for_file ~clock marker ;
      let descendant = child_pid marker in
      let outcome =
        Eio.Time.with_timeout clock 1.0 (fun () ->
            G.terminate ~grace_seconds:0.05 ~clock target ;
            Ok ())
      in
      Alcotest.(check bool)
        "invalid instrumentation path cannot block or raise from cleanup"
        true
        (match outcome with Ok () -> true | Error `Timeout -> false) ;
      Alcotest.(check (option int))
        "invalid instrumentation path still retires ownership"
        None
        (G.group_id target) ;
      wait_for_pid_exit ~clock descendant)

let test_fifo_signal_log_is_nonblocking () =
  with_launcher @@ fun () ->
  let fifo = fresh_marker () in
  (try Unix.mkfifo fifo 0o600
   with Unix.Unix_error _ -> Alcotest.skip ()) ;
  Fun.protect
    ~finally:(fun () -> try Sys.remove fifo with Sys_error _ -> ())
    (fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" fifo @@ fun () ->
      let marker = fresh_marker () in
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (fun path -> try Sys.remove path with Sys_error _ -> ())
            [marker; marker ^ ".pid"])
        (fun () ->
          let clock = Eio.Stdenv.clock env in
          let target =
            spawn
              ~sw
              ~env
              [ helper_path ();
                "--process-descendant-helper";
                "term-ignoring-child";
                marker;
              ]
          in
          wait_for_file ~clock marker ;
          let descendant = child_pid marker in
          let outcome =
            Eio.Time.with_timeout clock 1.0 (fun () ->
                G.terminate ~grace_seconds:0.05 ~clock target ;
                Ok ())
          in
          Alcotest.(check bool)
            "FIFO instrumentation with no reader never blocks cleanup"
            true
            (match outcome with Ok () -> true | Error `Timeout -> false) ;
          Alcotest.(check (option int))
            "FIFO instrumentation still permits ownership retirement"
            None
            (G.group_id target) ;
          wait_for_pid_exit ~clock descendant))

let test_cleanup_failures_still_kill_retire_and_resolve () =
  with_launcher @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_GROUP_TERM" "1" @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_CLEANUP_SLEEP" "1" @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_DIRECT_AWAIT" "1" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let missing_parent = fresh_marker () in
  let invalid_log = Filename.concat missing_parent "signals.log" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" invalid_log @@ fun () ->
      let clock = Eio.Stdenv.clock env in
      let target =
        spawn
          ~sw
          ~env
          [helper_path (); "--process-descendant-helper"; "term-ignoring-child"; marker]
      in
      wait_for_file ~clock marker ;
      let descendant = child_pid marker in
      let outcome =
        Eio.Time.with_timeout clock 1.0 (fun () ->
            G.terminate ~grace_seconds:10.0 ~clock target ;
            Ok (G.await target))
      in
      Alcotest.(check bool)
        "TERM, logging, timer, and await failures do not strand completion"
        true
        (match outcome with Ok _ -> true | Error `Timeout -> false) ;
      Alcotest.(check (option int))
        "failure-safe cleanup retires ownership"
        None
        (G.group_id target) ;
      wait_for_pid_exit ~clock descendant)

let test_catastrophic_term_and_logging_failures_still_kill () =
  with_launcher @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_GROUP_TERM" "1" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  let missing_parent = fresh_marker () in
  let invalid_log = Filename.concat missing_parent "signals.log" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".allow-supervisor-death"])
    (fun () ->
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" invalid_log @@ fun () ->
      let clock = Eio.Stdenv.clock env in
      let target =
        spawn
          ~sw
          ~env
          [ helper_path ();
            "--process-descendant-helper";
            "gated-kill-supervisor-ignoring-term";
            marker;
          ]
      in
      wait_for_file ~clock marker ;
      let descendant = child_pid marker in
      allow_supervisor_death marker ;
      let outcome =
        Eio.Time.with_timeout clock 1.0 (fun () -> Ok (G.await target))
      in
      Alcotest.(check bool)
        "catastrophic KILL survives injected TERM and logging failures"
        true
        (match outcome with Ok _ -> true | Error `Timeout -> false) ;
      Alcotest.(check (option int))
        "catastrophic failure path retires ownership"
        None
        (G.group_id target) ;
      wait_for_pid_exit ~clock descendant)

let test_unconfirmed_timer_and_await_failures_do_not_strand_claim () =
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_CLEANUP_SLEEP" "1" @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_DIRECT_AWAIT" "1" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let marker = fresh_marker () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".terminated"])
    (fun () ->
      let clock = Eio.Stdenv.clock env in
      let target =
        spawn
          ~sw
          ~env
          ~launcher:(helper_path ())
          [ helper_path ();
            "--process-descendant-helper";
            "fake-launcher-closed-control";
            marker;
          ]
      in
      wait_for_file ~clock marker ;
      let outcome =
        Eio.Time.with_timeout clock 1.0 (fun () ->
            G.terminate ~grace_seconds:10.0 ~clock target ;
            Ok (G.await target))
      in
      Alcotest.(check bool)
        "unconfirmed timer and await failures resolve public completion"
        true
        (match outcome with Ok _ -> true | Error `Timeout -> false) ;
      Alcotest.(check (option int))
        "unconfirmed failure path remains group-unowned"
        None
        (G.group_id target) ;
      wait_for_pid_exit ~clock (G.pid target))

let test_failed_release_timer_and_await_failures_do_not_strand_claim () =
  with_launcher @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_CLOSE_CONTROL_AFTER_EXEC" "1" @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_CLEANUP_SLEEP" "1" @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_DIRECT_AWAIT" "1" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let target =
    spawn ~sw ~env [helper_path (); "--process-descendant-helper"; "success"]
  in
  Alcotest.(check bool)
    "backend completes before injected failed-RELEASE cleanup"
    true
    (G.await_backend target = `Exited 0) ;
  let outcome =
    Eio.Time.with_timeout clock 1.0 (fun () ->
        G.release target ;
        Ok (G.await target))
  in
  Alcotest.(check bool)
    "failed-RELEASE timer and await failures resolve public completion"
    true
    (match outcome with Ok _ -> true | Error `Timeout -> false) ;
  Alcotest.(check (option int))
    "failed-RELEASE failure path retires ownership"
    None
    (G.group_id target) ;
  wait_for_pid_exit ~clock (G.pid target)

exception Test_switch_shutdown

let test_follower_and_simultaneous_switch_shutdown_do_not_deadlock () =
  with_launcher @@ fun () ->
  Eio_posix.run @@ fun env ->
  let marker = fresh_marker () in
  let target = ref None in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; marker ^ ".terminated"])
    (fun () ->
      let clock = Eio.Stdenv.clock env in
      let outcome =
        Eio.Time.with_timeout clock 2.0 (fun () ->
            (try
               Eio.Switch.run @@ fun caller_sw ->
               let owned =
                 spawn
                   ~sw:caller_sw
                   ~env
                   [ helper_path ();
                     "--process-descendant-helper";
                     "term-recording-ignoring-child";
                     marker;
                   ]
               in
               target := Some owned ;
               wait_for_file ~clock marker ;
               ignore
                 (Eio.Fiber.fork_promise ~sw:caller_sw (fun () ->
                      G.terminate ~grace_seconds:0.3 ~clock owned)) ;
               wait_for_file ~clock (marker ^ ".terminated") ;
               ignore
                 (Eio.Fiber.fork_promise ~sw:caller_sw (fun () ->
                      G.terminate ~grace_seconds:0.3 ~clock owned)) ;
               Eio.Fiber.yield () ;
               raise Test_switch_shutdown
             with Test_switch_shutdown -> ()) ;
            Ok ())
      in
      Alcotest.(check bool)
        "cleanup leader, follower, and caller-switch shutdown all complete"
        true
        (match outcome with Ok () -> true | Error `Timeout -> false) ;
      let owned =
        match !target with Some target -> target | None -> Alcotest.fail "no target"
      in
      Alcotest.(check (option int))
        "simultaneous switch shutdown retires ownership"
        None
        (G.group_id owned) ;
      wait_for_pid_exit ~clock (child_pid marker) ;
      wait_for_pid_exit ~clock (G.pid owned))

let test_reentrant_diagnostics_and_group_kill_failure_do_not_deadlock () =
  with_launcher @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_GROUP_TERM" "1" @@ fun () ->
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_GROUP_KILL" "1" @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun outer_sw ->
  let marker = fresh_marker () in
  let signal_log = fresh_marker () in
  let target = ref None in
  let leader = ref None in
  let follower = ref None in
  let diagnostics = ref 0 in
  Fun.protect
    ~finally:(fun () ->
      Diagnostics.reset_handler () ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [marker; marker ^ ".pid"; signal_log])
    (fun () ->
      Diagnostics.set_handler (fun event ->
          match (event, !target) with
          | Diagnostics.Log (Diagnostics.Warn, _), Some owned ->
              incr diagnostics ;
              ignore (G.group_id owned)
          | _ -> ()) ;
      with_env "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" signal_log @@ fun () ->
      let clock = Eio.Stdenv.clock env in
      let outcome =
        Eio.Time.with_timeout clock 2.0 (fun () ->
            (try
               Eio.Switch.run @@ fun caller_sw ->
               let owned =
                 spawn
                   ~sw:caller_sw
                   ~env
                   [ helper_path ();
                     "--process-descendant-helper";
                     "term-ignoring-child";
                     marker;
                   ]
               in
               target := Some owned ;
               wait_for_file ~clock marker ;
               leader :=
                 Some
                   (Eio.Fiber.fork_promise ~sw:outer_sw (fun () ->
                        G.terminate ~grace_seconds:0.3 ~clock owned)) ;
               ignore (wait_for_signal_log_count ~clock signal_log 1) ;
               follower :=
                 Some
                   (Eio.Fiber.fork_promise ~sw:outer_sw (fun () ->
                        G.terminate ~grace_seconds:0.3 ~clock owned)) ;
               Eio.Fiber.yield () ;
               raise Test_switch_shutdown
             with Test_switch_shutdown -> ()) ;
            Option.iter Eio.Promise.await_exn !leader ;
            Option.iter Eio.Promise.await_exn !follower ;
            Ok ())
      in
      Alcotest.(check bool)
        "reentrant diagnostics, follower, and caller switch all complete"
        true
        (match outcome with Ok () -> true | Error `Timeout -> false) ;
      let owned =
        match !target with Some target -> target | None -> Alcotest.fail "no target"
      in
      Alcotest.(check bool)
        "both injected group signal failures were diagnosed after cleanup"
        true
        (!diagnostics >= 2) ;
      Alcotest.(check (option int))
        "group-KILL failure still retires the claim"
        None
        (G.group_id owned) ;
      ignore (G.await owned) ;
      wait_for_pid_exit ~clock (child_pid marker) ;
      wait_for_pid_exit ~clock (G.pid owned))

let test_direct_kill_failure_retires_followers_and_switch () =
  let marker = fresh_marker () in
  with_env "CABAL_PROCESS_GROUP_TEST_FAIL_DIRECT_KILL" "1" @@ fun () ->
  with_env
    "CABAL_PROCESS_GROUP_TEST_DIRECT_KILL_ATTEMPT_MARKER"
    (marker ^ ".direct-kill-attempted")
  @@ fun () ->
  with_env
    "CABAL_PROCESS_GROUP_TEST_OWNER_FINALIZATION_STARTED_MARKER"
    (marker ^ ".owner-finalization-started")
  @@ fun () ->
  with_env
    "CABAL_PROCESS_GROUP_TEST_OWNER_FINALIZATION_GATE"
    (marker ^ ".allow-owner-finalization")
  @@ fun () ->
  Eio_posix.run @@ fun env ->
  let target = ref None in
  let diagnostics = ref 0 in
  let verified_while_caller_switch_open = ref false in
  Fun.protect
    ~finally:(fun () ->
      Diagnostics.reset_handler () ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ marker;
          marker ^ ".terminated";
          marker ^ ".direct-kill-attempted";
          marker ^ ".owner-finalization-started";
          marker ^ ".allow-owner-finalization";
        ])
    (fun () ->
      Diagnostics.set_handler (fun event ->
          match (event, !target) with
          | Diagnostics.Log (Diagnostics.Warn, _), Some owned ->
              incr diagnostics ;
              ignore (G.group_id owned)
          | _ -> ()) ;
      let clock = Eio.Stdenv.clock env in
      let outcome =
        Eio.Time.with_timeout clock 6.0 (fun () ->
            Eio.Switch.run @@ fun caller_sw ->
            let owned =
              spawn
                ~sw:caller_sw
                ~env
                ~launcher:(helper_path ())
                [ helper_path ();
                  "--process-descendant-helper";
                  "fake-launcher-closed-control-ignoring-term";
                  marker;
                ]
            in
            target := Some owned ;
            wait_for_file ~clock marker ;
            let direct_pid = G.pid owned in
             let leader =
               Eio.Fiber.fork_promise ~sw:caller_sw (fun () ->
                   G.terminate ~grace_seconds:0.1 ~clock owned)
             in
             wait_for_file ~clock (marker ^ ".terminated") ;
             Fun.protect
              ~finally:(fun () ->
                if
                  Sys.file_exists (marker ^ ".owner-finalization-started")
                  && not (Sys.file_exists (marker ^ ".allow-owner-finalization"))
                then
                  Process_test_helper.write_file
                    (marker ^ ".allow-owner-finalization")
                    "continue\n")
              (fun () ->
                 wait_for_file
                   ~timeout_seconds:3.0
                   ~clock
                   (marker ^ ".owner-finalization-started") ;
                 let follower =
                   Eio.Fiber.fork_promise ~sw:caller_sw (fun () ->
                       G.terminate ~grace_seconds:0.1 ~clock owned)
                 in
                 Eio.Fiber.yield () ;
                 Alcotest.(check bool)
                   "leader remains pending until owner reap"
                  true
                  (Option.is_none (Eio.Promise.peek leader)) ;
                Alcotest.(check bool)
                  "follower remains pending until owner reap"
                  true
                  (Option.is_none (Eio.Promise.peek follower)) ;
                 Alcotest.(check bool)
                   "direct PID remains live while owner finalization is gated"
                   true
                   (pid_is_live direct_pid) ;
                 Alcotest.(check bool)
                   "direct status remains private until owner reap"
                   true
                   (Option.is_none (G.awaited_status owned)) ;
                 Process_test_helper.write_file
                   (marker ^ ".allow-owner-finalization")
                   "continue\n" ;
                 Eio.Promise.await_exn leader ;
                 Eio.Promise.await_exn follower ;
                 let backend_status = G.await owned in
                 Alcotest.(check bool)
                   "terminate completion implies the direct PID was reaped"
                   false
                   (pid_is_live direct_pid) ;
                 Alcotest.(check bool)
                   "backend fallback returns the actual reaped status"
                   true
                   (backend_status = `Signaled Sys.sigkill) ;
                 Alcotest.(check bool)
                   "actual direct status is public only after reap"
                   true
                   (G.awaited_status owned = Some (`Signaled Sys.sigkill)) ;
                 verified_while_caller_switch_open := true) ;
            Ok ())
      in
      Alcotest.(check bool)
        "direct-KILL failure, followers, and caller switch all complete"
        true
        (match outcome with Ok () -> true | Error `Timeout -> false) ;
      Alcotest.(check bool)
        "reap was verified before caller-switch shutdown"
        true
        !verified_while_caller_switch_open ;
      let owned =
        match !target with Some target -> target | None -> Alcotest.fail "no target"
      in
      Alcotest.(check bool)
        "injected direct-KILL failure was diagnosed after cleanup"
        true
        (!diagnostics >= 1) ;
      Alcotest.(check bool)
        "direct-KILL fault injection reached the failed signal attempt"
        true
        (Sys.file_exists (marker ^ ".direct-kill-attempted")) ;
      Alcotest.(check (option int))
        "direct-KILL failure leaves no cleanup claim"
        None
        (G.group_id owned))

let () =
  Alcotest.run
    "Process_group"
    [
      ( "launcher",
        [
          ("handshake and exec success", `Quick, test_handshake_exec_success);
          ( "raw explicit EXEC confirms success",
            `Quick,
            test_raw_explicit_exec_record_confirms_success );
          ( "raw no-ACK timeout reports no backend",
            `Quick,
            test_raw_no_ack_timeout_reports_failure_without_backend );
          ( "raw external death after PGID has no EXEC",
            `Quick,
            test_raw_external_death_after_pgid_has_no_exec_record );
          ( "invalid handshake sequences never establish exec",
            `Quick,
            test_invalid_handshake_sequences_never_establish_exec );
          ( "invalid handshake timeouts are rejected before spawn",
            `Quick,
            test_spawn_rejects_invalid_handshake_timeouts );
          ( "mismatched handshake PGID is not owned",
            `Quick,
            test_mismatched_handshake_pgid_is_not_owned );
          ( "unqualified launcher resolves on PATH",
            `Quick,
            test_default_launcher_is_resolved_on_path );
          ("pre-exec failure", `Quick, test_preexec_failure_is_reported);
          ("natural exit reaped once", `Quick, test_natural_exit_is_reaped_once);
          ( "natural exit retires automatically",
            `Quick,
            test_backend_status_keeps_group_anchored_until_release );
          ( "child signal status is preserved",
            `Quick,
            test_signal_status_is_preserved );
          ( "cancelled before PGID does not fork backend",
            `Quick,
            test_cancelled_before_pgid_never_spawns_backend );
          ( "control loss terminates an unknown group",
            `Quick,
            test_control_loss_terminates_unknown_group );
           ( "repeated direct SIGTERM preserves grace",
             `Quick,
             test_repeated_direct_sigterm_keeps_the_original_grace );
           ( "malformed FD4 TERM records use fallback",
             `Quick,
             test_malformed_control_records_use_fallback );
           ( "first valid FD4 TERM record wins",
             `Quick,
             test_first_valid_control_record_wins );
           ( "cancelled post-fork handshake reaps child",
            `Quick,
            test_cancelled_handshake_after_fork_reaps_child );
          ( "child switch shutdown terminates unawaited target",
            `Quick,
            test_child_switch_shutdown_terminates_unawaited_target );
          ( "explicit relative launcher uses host cwd",
            `Quick,
            test_explicit_relative_launcher_uses_host_cwd_and_failed_path_leaks_no_pipes
          );
          ( "long launcher error is bounded",
            `Quick,
            test_long_launcher_error_is_bounded );
          ("explicit termination", `Quick, test_explicit_termination);
           ( "await and termination do not deadlock",
             `Quick,
             test_await_does_not_block_concurrent_termination );
           ( "explicit termination propagates custom grace",
             `Quick,
             test_explicit_termination_propagates_custom_grace );
           ( "explicit termination rejects invalid grace",
             `Quick,
             test_explicit_termination_rejects_invalid_grace );
           ( "fallback avoids unconfirmed negative PGID",
             `Quick,
             test_fallback_never_uses_an_unconfirmed_group );
           ( "unconfirmed failed FD4 delivery waits for fallback",
             `Quick,
             test_unconfirmed_control_delivery_waits_for_fallback );
           ( "pre-ACK cancellation never forks backend",
             `Quick,
             test_pre_ack_cancellation_never_forks_backend );
            ( "stopped confirmed supervisor does not delay group TERM",
              `Quick,
              test_stopped_confirmed_supervisor_does_not_delay_group_term );
           ( "unexpected supervisor death cleans confirmed descendants",
             `Quick,
             test_unexpected_supervisor_death_cleans_confirmed_descendants );
           ( "unexpected supervisor death coordinates with termination",
             `Quick,
             test_unexpected_supervisor_death_coordinates_with_termination );
           ( "release retires ownership before later termination",
             `Quick,
             test_release_retires_ownership_before_later_termination );
           ( "premature release retains ownership until switch cleanup",
             `Quick,
             test_premature_release_retains_ownership_until_switch_cleanup );
          ( "failed RELEASE after external death has no group signal",
            `Quick,
            test_failed_release_after_external_supervisor_death_has_no_group_signal );
          ( "failed RELEASE uses direct cleanup without group signal",
            `Quick,
            test_failed_release_uses_direct_cleanup_without_group_signal );
           ( "terminating target cannot release ownership",
              `Quick,
              test_terminating_target_cannot_release_ownership );
           ( "unexpected death immediately escalates and retires",
             `Quick,
             test_unexpected_supervisor_death_immediately_escalates_and_retires );
           ( "observer takes over sleeping termination leader",
             `Quick,
             test_observer_takes_over_sleeping_termination_leader );
           ( "invalid signal log is best effort",
             `Quick,
             test_invalid_signal_log_is_best_effort );
           ( "FIFO signal log is nonblocking",
             `Quick,
             test_fifo_signal_log_is_nonblocking );
           ( "cleanup failures still kill, retire, and resolve",
             `Quick,
             test_cleanup_failures_still_kill_retire_and_resolve );
           ( "catastrophic TERM and logging failures still KILL",
             `Quick,
             test_catastrophic_term_and_logging_failures_still_kill );
           ( "unconfirmed timer and await failures do not strand claim",
             `Quick,
             test_unconfirmed_timer_and_await_failures_do_not_strand_claim );
           ( "failed RELEASE timer and await failures do not strand claim",
             `Quick,
             test_failed_release_timer_and_await_failures_do_not_strand_claim );
           ( "follower and switch shutdown do not deadlock",
             `Quick,
             test_follower_and_simultaneous_switch_shutdown_do_not_deadlock );
           ( "reentrant diagnostics and group-KILL failure do not deadlock",
             `Quick,
             test_reentrant_diagnostics_and_group_kill_failure_do_not_deadlock );
           ( "direct-KILL failure retires followers and switch",
             `Quick,
             test_direct_kill_failure_retires_followers_and_switch );
         ] );
    ]
