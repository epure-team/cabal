(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Portable real-process helper linked into process-related test executables.
    A test binary invoked with [--process-descendant-helper] becomes a small
    OCaml child program before it initializes Alcotest. *)

let launcher_path () =
  let test_dir = Filename.dirname (Unix.realpath Sys.executable_name) in
  let build_dir = Filename.dirname test_dir in
  Filename.concat build_dir "bin/process_group_launcher.exe"

let install_launcher () =
  let launcher = launcher_path () in
  if not (Sys.file_exists launcher) then
    failwith ("process-group launcher was not built at " ^ launcher) ;
  Unix.putenv "CABAL_PROCESS_GROUP_LAUNCHER" launcher

let write_file path text =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      output_string channel text ;
      flush channel)

let descriptor_of_int (fd : int) : Unix.file_descr = Obj.magic fd

let write_all descriptor text =
  let rec loop offset =
    if offset < String.length text then
      try
        let written =
          Unix.write_substring
            descriptor
            text
            offset
            (String.length text - offset)
        in
        loop (offset + written)
      with Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
  in
  loop 0

let read_line descriptor =
  let byte = Bytes.create 1 in
  let buffer = Buffer.create 16 in
  let rec loop () =
    try
      match Unix.read descriptor byte 0 1 with
      | 0 -> None
      | _ ->
          let character = Bytes.get byte 0 in
          if character = '\n' then Some (Buffer.contents buffer)
          else begin
            Buffer.add_char buffer character ;
            loop ()
          end
    with Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let rec sleep_forever () =
  Unix.sleep 1 ;
  sleep_forever ()

let run_child marker =
  write_file marker "started\n" ;
  write_file (marker ^ ".pid") (string_of_int (Unix.getpid ())) ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         write_file (marker ^ ".terminated") "terminated\n" ;
         exit 0)) ;
  sleep_forever ()

let run_timestamped_child marker =
  write_file marker "started\n" ;
  write_file (marker ^ ".pid") (string_of_int (Unix.getpid ())) ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         write_file
           (marker ^ ".terminated")
           (Printf.sprintf "%.6f\n" (Unix.gettimeofday ())) ;
         exit 0)) ;
  sleep_forever ()

let spawn_child marker =
  let executable = Sys.executable_name in
  let child_pid =
    Unix.create_process
      executable
      [|executable; "--process-descendant-helper"; "child"; marker|]
      Unix.stdin
      Unix.stdout
      Unix.stderr
  in
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         ignore (Unix.waitpid [] child_pid) ;
         exit 0)) ;
  Printf.printf "child-started\n%!" ;
  prerr_endline "parent-stderr" ;
  sleep_forever ()

let spawn_exit_child marker =
  let executable = Sys.executable_name in
  let _child_pid =
    Unix.create_process
      executable
      [|executable; "--process-descendant-helper"; "child"; marker|]
      Unix.stdin
      Unix.stdout
      Unix.stderr
  in
  Printf.printf "child-started\n%!" ;
  prerr_endline "parent-exited" ;
  exit 0

let run_term_ignoring_child marker =
  write_file marker "started\n" ;
  write_file (marker ^ ".pid") (string_of_int (Unix.getpid ())) ;
  Sys.set_signal Sys.sigterm Sys.Signal_ignore ;
  sleep_forever ()

let run_term_observing_child marker =
  write_file marker "started\n" ;
  write_file (marker ^ ".pid") (string_of_int (Unix.getpid ())) ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ -> write_file (marker ^ ".terminated") "terminated\n")) ;
  sleep_forever ()

let run_term_recording_ignoring_child marker =
  write_file (marker ^ ".pid") (string_of_int (Unix.getpid ())) ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         write_file
           (marker ^ ".terminated")
           (Printf.sprintf "%.6f\n" (Unix.gettimeofday ())))) ;
  (* The marker is the parent's readiness boundary: publish it only after the
     TERM handler is installed so an immediate control record is deterministic. *)
  write_file marker "started\n" ;
  sleep_forever ()

let wait_for_path path =
  let rec loop () =
    if Sys.file_exists path then ()
    else begin
      (try ignore (Unix.select [] [] [] 0.01)
       with Unix.Unix_error (Unix.EINTR, _, _) -> ()) ;
      loop ()
    end
  in
  loop ()

let wait_for_supervisor_death_gate marker =
  wait_for_path (marker ^ ".allow-supervisor-death")

let run_gated_kill_supervisor_ignoring_term marker =
  write_file marker "started\n" ;
  write_file (marker ^ ".pid") (string_of_int (Unix.getpid ())) ;
  Sys.set_signal Sys.sigterm Sys.Signal_ignore ;
  wait_for_supervisor_death_gate marker ;
  Unix.kill (Unix.getppid ()) Sys.sigkill ;
  sleep_forever ()

let run_gated_kill_supervisor_recording_term marker =
  write_file marker "started\n" ;
  write_file (marker ^ ".pid") (string_of_int (Unix.getpid ())) ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ -> write_file (marker ^ ".term-observed") "term\n")) ;
  wait_for_supervisor_death_gate marker ;
  Unix.kill (Unix.getppid ()) Sys.sigkill ;
  sleep_forever ()

let run_gated_kill_supervisor_exiting_on_term marker =
  write_file marker "started\n" ;
  write_file (marker ^ ".pid") (string_of_int (Unix.getpid ())) ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         write_file (marker ^ ".terminated") "terminated\n" ;
         exit 0)) ;
  wait_for_supervisor_death_gate marker ;
  Unix.kill (Unix.getppid ()) Sys.sigkill ;
  sleep_forever ()

let fake_launcher_with_closed_control_ignoring_term marker =
  let handshake = descriptor_of_int 3 in
  let control = descriptor_of_int 4 in
  ignore (Unix.write_substring handshake "PGID 1\n" 0 7) ;
  Unix.close handshake ;
  Unix.close control ;
  write_file marker "started\n" ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
          write_file
            (marker ^ ".terminated")
            (Printf.sprintf "%.6f\n" (Unix.gettimeofday ())))) ;
  sleep_forever ()

let spawn_term_ignoring_child marker =
  let executable = Sys.executable_name in
  let _child_pid =
    Unix.create_process
      executable
      [|
        executable; "--process-descendant-helper"; "term-ignoring-child"; marker;
      |]
      Unix.stdin
      Unix.stdout
      Unix.stderr
  in
  Printf.printf "term-ignoring-child-started\n%!" ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         write_file (marker ^ ".parent-terminated") "terminated\n" ;
         exit 0)) ;
  sleep_forever ()

let spawn_term_observing_child marker =
  let executable = Sys.executable_name in
  let _child_pid =
    Unix.create_process
      executable
      [|
        executable;
        "--process-descendant-helper";
        "term-observing-child";
        marker;
      |]
      Unix.stdin
      Unix.stdout
      Unix.stderr
  in
  sleep_forever ()

let spawn_term_recording_ignoring_child marker =
  let executable = Sys.executable_name in
  let _child_pid =
    Unix.create_process
      executable
      [|
        executable;
        "--process-descendant-helper";
        "term-recording-ignoring-child";
        marker;
      |]
      Unix.stdin
      Unix.stdout
      Unix.stderr
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> exit 0)) ;
  sleep_forever ()

let fake_launcher_pgid_one marker =
  let handshake = descriptor_of_int 3 in
  ignore (Unix.write_substring handshake "PGID 1\n" 0 7) ;
  Unix.close handshake ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         write_file (marker ^ ".terminated") "terminated\n" ;
         exit 0)) ;
  sleep_forever ()

let fake_launcher_handshake mode =
  let handshake = descriptor_of_int 3 in
  let pgid = Unix.setsid () in
  let record =
    match mode with
    | "bare-pgid" -> Printf.sprintf "PGID %d\n" pgid
    | "duplicate-exec" -> Printf.sprintf "PGID %d\nEXEC\nEXEC\n" pgid
    | _ -> invalid_arg "unknown fake launcher handshake mode"
  in
  ignore (Unix.write_substring handshake record 0 (String.length record)) ;
  Unix.close handshake ;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> exit 0)) ;
  sleep_forever ()

let fake_launcher_missing_exec_with_zero_status marker =
  let handshake = descriptor_of_int 3 in
  let control = descriptor_of_int 4 in
  let status = descriptor_of_int 5 in
  let pgid = Unix.setsid () in
  write_all handshake (Printf.sprintf "PGID %d\n" pgid) ;
  (match read_line control with
  | Some "ACK" -> ()
  | Some command -> failwith ("expected ACK, received " ^ command)
  | None -> failwith "control FD closed before ACK") ;
  write_file marker "started\n" ;
  write_file (marker ^ ".pid") (string_of_int (Unix.getpid ())) ;
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         write_file (marker ^ ".terminated") "terminated\n" ;
         exit 0)) ;
  close_out_noerr stdout ;
  close_out_noerr stderr ;
  write_all status "EXIT 0\n" ;
  Unix.close status ;
  (* A matching PGID was acknowledged, but EOF without EXEC must never allow
     the reported zero status to become backend success. *)
  Unix.close handshake ;
  let rec wait_for_cleanup () =
    match read_line control with
    | Some "RELEASE" ->
        write_file (marker ^ ".released") "released\n" ;
        exit 0
    | Some command when String.starts_with ~prefix:"TERM " command ->
        write_file (marker ^ ".terminated") "terminated\n" ;
        exit 0
    | Some _ -> wait_for_cleanup ()
    | None ->
        write_file (marker ^ ".control-closed") "closed\n" ;
        exit 0
  in
  wait_for_cleanup ()

let write_after_term () =
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         output_string stdout (String.make (1024 * 1024) 't') ;
         output_string stdout "\nTERM-WRITE-COMPLETE\n" ;
         flush stdout ;
         exit 0)) ;
  sleep_forever ()

let write_large_no_newline () =
  output_string stdout (String.make (1024 * 1024) 'x') ;
  flush stdout

let self_signal signal = Unix.kill (Unix.getpid ()) signal

let action_from_argv () =
  match Array.to_list Sys.argv with
  | _ :: "--process-descendant-helper" :: action :: arguments ->
      Some (action, arguments)
  | _ :: "--" :: _ :: "--process-descendant-helper" :: action :: arguments ->
      Some (action, arguments)
  | _ -> None

let run_if_requested () =
  match action_from_argv () with
  | Some ("success", _) ->
      print_endline "helper-stdout" ;
      prerr_endline "helper-stderr" ;
      exit 0
  | Some ("sleep", _) | Some ("nonreading", _) | Some ("broken-launcher", _) ->
      sleep_forever ()
  | Some ("spawn-child", marker :: _) -> spawn_child marker
  | Some ("spawn-exit-child", marker :: _) -> spawn_exit_child marker
  | Some ("child", marker :: _) -> run_child marker
  | Some ("timestamped-child", marker :: _) -> run_timestamped_child marker
  | Some ("spawn-term-ignoring-child", marker :: _) ->
      spawn_term_ignoring_child marker
  | Some ("term-ignoring-child", marker :: _) -> run_term_ignoring_child marker
  | Some ("term-observing-child", marker :: _) ->
      run_term_observing_child marker
  | Some ("spawn-term-observing-child", marker :: _) ->
      spawn_term_observing_child marker
  | Some ("spawn-term-recording-ignoring-child", marker :: _) ->
      spawn_term_recording_ignoring_child marker
  | Some ("term-recording-ignoring-child", marker :: _) ->
        run_term_recording_ignoring_child marker
  | Some ("gated-kill-supervisor-ignoring-term", marker :: _) ->
        run_gated_kill_supervisor_ignoring_term marker
  | Some ("gated-kill-supervisor-recording-term", marker :: _) ->
        run_gated_kill_supervisor_recording_term marker
  | Some ("gated-kill-supervisor-exiting-on-term", marker :: _) ->
        run_gated_kill_supervisor_exiting_on_term marker
  | Some ("fake-launcher-pgid-one", marker :: _) ->
       fake_launcher_pgid_one marker
  | Some ("fake-launcher-handshake", mode :: _) -> fake_launcher_handshake mode
  | Some ("fake-launcher-missing-exec-zero", marker :: _) ->
        fake_launcher_missing_exec_with_zero_status marker
  | Some
      ( ( "fake-launcher-closed-control"
        | "fake-launcher-closed-control-ignoring-term" ),
        marker :: _ ) ->
        fake_launcher_with_closed_control_ignoring_term marker
  | Some ("large-no-newline", _) -> write_large_no_newline ()
  | Some ("write-after-term", _) -> write_after_term ()
  | Some ("self-term", _) -> self_signal Sys.sigterm
  | Some ("self-kill", _) -> self_signal Sys.sigkill
  | Some _ ->
      prerr_endline "unexpected process helper command" ;
      exit 64
  | None -> ()
