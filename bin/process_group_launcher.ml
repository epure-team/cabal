(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** POSIX session-leader supervisor for Cabal backend commands.

    FD 3 is the bounded launch handshake: [PGID <n>] followed by [EXEC] and EOF
    confirms backend exec, while [ERROR <message>] reports a pre-exec failure. FD 4
    receives [ACK], [TERM <finite-seconds>], and [RELEASE] commands from the
    parent. The launcher will not fork until it has received [ACK] after its
    PGID record. The first valid TERM grace wins; malformed records, EOF, and
    direct signals use the fixed parent-loss fallback. FD 5 carries the backend child's actual
    exit status. The supervisor keeps the session and process group anchored
    after the backend exits until the parent releases it or terminates it. *)

let handshake_fd = 3

let control_fd = 4

let status_fd = 5

let liveness_fd = 6

let max_error_bytes = 1024

let max_control_bytes = 256

(* OCaml 5.3, Cabal's minimum and CI compiler, does not yet expose
   [Unix.clock_gettime] or [Unix.CLOCK_MONOTONIC]. The companion C stub binds
   that POSIX API directly; it is available on both Linux and supported macOS. *)
external monotonic_now : unit -> float = "cabal_monotonic_clock_now"

(* POSIX OCaml runtimes represent [Unix.file_descr] as an integer.  This is
   confined to the Unix-only launcher; Cabal's public library never exposes
   this representation. *)
let descriptor_of_int (fd : int) : Unix.file_descr = Obj.magic fd

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

let write_all fd text =
  let rec loop offset =
    if offset < String.length text then
      try
        let written =
          Unix.write_substring fd text offset (String.length text - offset)
        in
        loop (offset + written)
      with Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
  in
  loop 0

let write_noerr fd text = try write_all fd text with Unix.Unix_error _ -> ()

let one_line text =
  String.map
    (fun c ->
      let code = Char.code c in
      if c = '\n' || c = '\r' || code < 32 || code = 127 then ' ' else c)
    text

let bounded_error text =
  let text = one_line text in
  if String.length text <= max_error_bytes then text
  else String.sub text 0 max_error_bytes

let report_error_message fd message =
  write_noerr fd ("ERROR " ^ bounded_error message ^ "\n")

let report_error fd exn = report_error_message fd (Printexc.to_string exn)

let command () =
  match Array.to_list Sys.argv with
  | _ :: "--" :: executable :: arguments -> executable :: arguments
  | _ -> invalid_arg "expected -- followed by a backend command"

type control = {
  mutable fd : Unix.file_descr option;
  pending : Buffer.t;
  mutable acknowledged : bool;
  mutable term_requested : bool;
  mutable requested_grace_seconds : float option;
  mutable deadline_started : bool;
  mutable release_requested : bool;
  mutable term_state_event_count : int;
}

let report_ack_failure fd control =
  let reason =
    if control.release_requested then "RELEASE received before ACK"
    else if control.term_requested then "control loss or TERM received before ACK"
    else "timed out waiting for ACK"
  in
  report_error_message fd reason

let finite_non_negative seconds =
  seconds >= 0.0
  &&
  match classify_float seconds with FP_normal | FP_subnormal | FP_zero -> true | _ -> false

type term_state_event =
  | Term_accepted of float
  | Term_ignored of float
  | Deadline_created of float

let max_term_state_events = 8

let term_state_record_bytes = 64

let term_state_event_text = function
  | Term_accepted seconds -> Printf.sprintf "TERM_ACCEPT grace=%.17g" seconds
  | Term_ignored seconds -> Printf.sprintf "TERM_IGNORE grace=%.17g" seconds
  | Deadline_created seconds ->
      Printf.sprintf "DEADLINE_CREATE grace=%.17g" seconds

let fixed_term_state_record event =
  let payload = term_state_event_text event in
  let payload_bytes = term_state_record_bytes - 1 in
  let payload =
    if String.length payload <= payload_bytes then payload
    else String.sub payload 0 payload_bytes
  in
  payload ^ String.make (payload_bytes - String.length payload) ' ' ^ "\n"

let log_term_state control event =
  match Sys.getenv_opt "CABAL_PROCESS_GROUP_TEST_TERM_STATE_LOG" with
  | None | Some "" -> ()
  | Some path when control.term_state_event_count < max_term_state_events ->
      control.term_state_event_count <- control.term_state_event_count + 1 ;
      (try
         let fd =
           Unix.openfile
             path
             [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_NONBLOCK]
             0o600
         in
         Fun.protect
           ~finally:(fun () -> close_noerr fd)
           (fun () ->
             let record = fixed_term_state_record event in
             ignore (Unix.write_substring fd record 0 (String.length record)))
       with _ -> ())
  | Some _ -> ()

let request_term ?grace_seconds control =
  match (control.term_requested, grace_seconds) with
  | false, _ ->
      control.term_requested <- true ;
      control.requested_grace_seconds <- grace_seconds ;
      Option.is_some grace_seconds
  | true, Some seconds
    when Option.is_none control.requested_grace_seconds
         && not control.deadline_started ->
      (* Parent TERM is written before its group broadcast. A queued valid
         record can therefore enrich an already-observed SIGTERM before the
         deadline is fixed. *)
      control.requested_grace_seconds <- Some seconds ;
      true
  | true, None | true, Some _ -> false

let install_term_handler control =
  let rec handle _ =
    ignore (request_term control) ;
    (* Some Unix signal implementations reset a handler after delivery. Keep
       repeated direct and group-delivered TERM requests on the same state
       machine instead of falling back to the default immediate termination. *)
    Sys.set_signal Sys.sigterm (Sys.Signal_handle handle)
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handle)

let handle_control_line control line =
  match String.split_on_char ' ' line with
  | ["TERM"; seconds] -> (
      match float_of_string_opt seconds with
      | Some seconds when finite_non_negative seconds ->
          let event =
            if request_term ~grace_seconds:seconds control then
              Term_accepted seconds
            else Term_ignored seconds
          in
          log_term_state control event
      | Some _ | None -> ignore (request_term control))
  | ["ACK"]
    when not control.acknowledged
         && not control.term_requested
         && not control.release_requested ->
      control.acknowledged <- true
  | ["ACK"] -> ignore (request_term control)
  | ["RELEASE"] -> control.release_requested <- true
  | _ ->
      (* A malformed command cannot safely leave an owned group running after
         its parent has failed. *)
      ignore (request_term control)

let consume_control_lines control =
  let text = Buffer.contents control.pending in
  let lines = String.split_on_char '\n' text in
  match List.rev lines with
  | incomplete :: complete_reversed ->
      Buffer.clear control.pending ;
      Buffer.add_string control.pending incomplete ;
      List.rev complete_reversed
      |> List.iter (fun line -> handle_control_line control line)
  | [] -> assert false

let service_control control =
  match control.fd with
  | None -> ()
  | Some fd -> (
      let bytes = Bytes.create 64 in
      try
        match Unix.read fd bytes 0 (Bytes.length bytes) with
        | 0 ->
            control.fd <- None ;
            ignore (request_term control)
        | count ->
            if Buffer.length control.pending + count > max_control_bytes then
              ignore (request_term control)
            else begin
              Buffer.add_subbytes control.pending bytes 0 count ;
              consume_control_lines control
            end
      with
      | Unix.Unix_error (Unix.EINTR, _, _) -> ()
      | Unix.Unix_error _ ->
          control.fd <- None ;
          ignore (request_term control))

let select_retry ?(on_eintr = fun () -> ()) read_fds timeout =
  let deadline = monotonic_now () +. timeout in
  let rec loop first_attempt =
    let remaining = deadline -. monotonic_now () in
    if (not first_attempt) && remaining <= 0.0 then ([], [], [])
    else
      try Unix.select read_fds [] [] (max 0.0 remaining)
      with Unix.Unix_error (Unix.EINTR, _, _) ->
        on_eintr () ;
        loop false
  in
  loop true

let wait_for_control ?(on_tick = fun () -> ()) control timeout =
  match control.fd with
  | None -> ignore (select_retry ~on_eintr:on_tick [] timeout)
  | Some fd ->
      let readable, _, _ = select_retry ~on_eintr:on_tick [fd] timeout in
      if readable <> [] then service_control control

let wait_for_ack control =
  let deadline = monotonic_now () +. 1.0 in
  let rec loop () =
    if control.acknowledged then true
    else if control.term_requested || control.release_requested then false
    else
      let remaining = deadline -. monotonic_now () in
      if remaining <= 0.0 then false
      else begin
        wait_for_control control (min remaining 0.05) ;
        loop ()
      end
  in
  loop ()

let wait_for_exec_error ~control ~on_tick error_fd =
  let buffer = Buffer.create 64 in
  let bytes = Bytes.create 128 in
  let rec loop () =
    on_tick () ;
    let readable, _, _ =
      match control.fd with
      | Some control_fd ->
          select_retry ~on_eintr:on_tick [error_fd; control_fd] 0.05
      | None -> select_retry ~on_eintr:on_tick [error_fd] 0.05
    in
    if List.memq error_fd readable then
      try
        match Unix.read error_fd bytes 0 (Bytes.length bytes) with
        | 0 -> Buffer.contents buffer
        | count ->
            if Buffer.length buffer + count > max_error_bytes then
              invalid_arg "backend exec error exceeds its size bound" ;
            Buffer.add_subbytes buffer bytes 0 count ;
            loop ()
      with Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
    else begin
      (match control.fd with
      | Some control_fd when List.memq control_fd readable ->
          service_control control
      | _ -> ()) ;
      loop ()
    end
  in
  loop ()

let wait_for_child ~control ~on_tick pid =
  let rec loop () =
    on_tick () ;
    try
      match Unix.waitpid [Unix.WNOHANG] pid with
      | 0, _ ->
          wait_for_control ~on_tick control 0.05 ;
          loop ()
      | _, ((Unix.WEXITED _ | Unix.WSIGNALED _) as status) -> status
      | _, Unix.WSTOPPED _ -> loop ()
    with Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let terminate_group pgid =
  (try Unix.kill (-pgid) Sys.sigterm with Unix.Unix_error _ -> ()) ;
  try Unix.kill (-pgid) Sys.sigkill with Unix.Unix_error _ -> ()

let close_supervisor_standard_fds () =
  List.iter close_noerr [Unix.stdin; Unix.stdout; Unix.stderr]

let send_backend_status fd = function
  | Unix.WEXITED code -> write_noerr fd (Printf.sprintf "EXIT %d\n" code)
  | Unix.WSIGNALED signal ->
      write_noerr fd (Printf.sprintf "SIGNAL %d\n" signal)
  | Unix.WSTOPPED _ -> assert false

let exit_with_child_status = function
  | Unix.WEXITED code -> exit code
  | Unix.WSIGNALED signal ->
      (* [signal] is deliberately reused verbatim.  OCaml's portable signal
         constants may be negative, so deriving a shell-style [128 + n] would
         lose the actual wait status. The supervisor handles only SIGTERM;
         attempting to install a handler for an uncatchable signal such as
         SIGKILL would fail. *)
      Sys.set_signal Sys.sigterm Sys.Signal_default ;
      Unix.kill (Unix.getpid ()) signal ;
      exit 127
  | Unix.WSTOPPED _ -> assert false

let test_handshake_delay_seconds () =
  match Sys.getenv_opt "CABAL_PROCESS_GROUP_TEST_HANDSHAKE_DELAY_SECONDS" with
  | Some value -> (
      match float_of_string_opt value with
      | Some seconds when seconds > 0.0 -> seconds
      | _ -> 0.0)
  | None -> 0.0

let test_delay_seconds variable =
  match Sys.getenv_opt variable with
  | Some value -> (
      match float_of_string_opt value with
      | Some seconds when seconds > 0.0 -> seconds
      | _ -> 0.0)
  | None -> 0.0

let test_enabled variable =
  match Sys.getenv_opt variable with Some "1" | Some "true" -> true | _ -> false

let write_test_marker variable =
  match Sys.getenv_opt variable with
  | None | Some "" -> ()
  | Some path -> (
      try
        let fd =
          Unix.openfile
            path
            [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK]
            0o600
        in
        Fun.protect
          ~finally:(fun () -> close_noerr fd)
          (fun () -> write_all fd "started\n")
      with Unix.Unix_error _ -> ())

let handshake_pgid_record established_pgid =
  match Sys.getenv_opt "CABAL_PROCESS_GROUP_TEST_HANDSHAKE_PGID" with
  | Some value ->
      if value <> "" then value else string_of_int established_pgid
  | None -> string_of_int established_pgid

let parent_loss_grace_seconds =
  let default = 2.0 in
  match Sys.getenv_opt "CABAL_PROCESS_GROUP_TEST_TERM_GRACE_SECONDS" with
  | Some value -> (
      match float_of_string_opt value with
      | Some seconds when seconds > 0.0 -> seconds
      | _ -> default)
  | None -> default

let create_termination_deadline control term_deadline =
  control.deadline_started <- true ;
  let grace_seconds =
    Option.value
      ~default:parent_loss_grace_seconds
      control.requested_grace_seconds
  in
  let deadline = monotonic_now () +. grace_seconds in
  term_deadline := Some deadline ;
  (* This event follows the assignment above, so a recorded creation always
     corresponds to the actual write-once deadline state. *)
  log_term_state control (Deadline_created grace_seconds) ;
  deadline

let delay_with_control ~control ~on_tick seconds =
  let deadline = monotonic_now () +. seconds in
  let rec loop () =
    let remaining = deadline -. monotonic_now () in
    if remaining > 0.0 then begin
      on_tick () ;
      wait_for_control ~on_tick control (min remaining 0.05) ;
      loop ()
    end
  in
  loop ()

(* A test gate may only shorten this hard maximum; inherited test environment
   can never turn normal RELEASE into an unbounded wait. *)
let default_test_release_gate_timeout_seconds = 2.0

let test_release_gate_timeout_seconds () =
  match
    Sys.getenv_opt "CABAL_PROCESS_GROUP_TEST_RELEASE_GATE_TIMEOUT_SECONDS"
  with
  | Some value -> (
      match float_of_string_opt value with
      | Some seconds
        when finite_non_negative seconds
             && seconds > 0.0
             && seconds <= default_test_release_gate_timeout_seconds ->
          seconds
      | Some _ | None -> default_test_release_gate_timeout_seconds)
  | None -> default_test_release_gate_timeout_seconds

let wait_for_test_gate ~control ~on_tick variable =
  match Sys.getenv_opt variable with
  | None | Some "" -> ()
  | Some path ->
      let deadline =
        monotonic_now () +. test_release_gate_timeout_seconds ()
      in
      let rec loop () =
        on_tick () ;
        if not (Sys.file_exists path) then
          let remaining = deadline -. monotonic_now () in
          if remaining > 0.0 then begin
            wait_for_control ~on_tick control (min remaining 0.05) ;
            loop ()
          end
      in
      loop ()

let () =
  let handshake = descriptor_of_int handshake_fd in
  let control_fd = descriptor_of_int control_fd in
  let status = descriptor_of_int status_fd in
  let liveness = descriptor_of_int liveness_fd in
  let child = ref None in
  let pgid = ref None in
  let forked = ref false in
  let control_state = ref None in
  let term_deadline = ref None in
  let post_fork_cleanup () =
    match (!pgid, !control_state) with
    | Some established_pgid, Some control when !forked ->
        wait_for_control control 0.0 ;
        ignore (request_term control) ;
        Option.iter
          (fun child_pid ->
            try Unix.kill child_pid Sys.sigterm with Unix.Unix_error _ -> ())
          !child ;
        (try Unix.kill (-established_pgid) Sys.sigterm
         with Unix.Unix_error _ -> ()) ;
        let deadline =
          match !term_deadline with
          | Some deadline -> deadline
          | None -> create_termination_deadline control term_deadline
        in
        let kill_after_grace () =
          if monotonic_now () >= deadline then begin
            terminate_group established_pgid ;
            exit 128
          end
        in
        let rec wait_for_grace_or_reap () =
          kill_after_grace () ;
          Option.iter
            (fun child_pid ->
              try
                match Unix.waitpid [Unix.WNOHANG] child_pid with
                | 0, _ -> ()
                | _, (Unix.WEXITED _ | Unix.WSIGNALED _) -> child := None
                | _, Unix.WSTOPPED _ -> ()
              with
              | Unix.Unix_error (Unix.EINTR, _, _) -> ()
              | Unix.Unix_error (Unix.ECHILD, _, _) -> child := None)
            !child ;
          wait_for_control ~on_tick:kill_after_grace control 0.05 ;
          wait_for_grace_or_reap ()
        in
        wait_for_grace_or_reap ()
    | Some established_pgid, _ -> terminate_group established_pgid
    | None, _ -> ()
  in
  try
    let cmd = command () in
    let established_pgid = Unix.setsid () in
    pgid := Some established_pgid ;
    (* SIGTERM must be blocked while the child is created.  The handler is
       installed before [fork], so a parent cancellation cannot kill the
       supervisor in the vulnerable post-fork window. *)
    ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigterm]) ;
    let control =
      {
        fd = Some control_fd;
        pending = Buffer.create 32;
        acknowledged = false;
        term_requested = false;
        requested_grace_seconds = None;
        deadline_started = false;
        release_requested = false;
        term_state_event_count = 0;
      }
    in
    control_state := Some control ;
    install_term_handler control ;
    (* The group must be published before any backend exists.  A closed
       handshake reader turns this into EPIPE, which exits through the outer
       fail-closed cleanup while [child] is still [None]. *)
    let before_pgid_delay =
      test_delay_seconds "CABAL_PROCESS_GROUP_TEST_BEFORE_PGID_DELAY_SECONDS"
    in
    if before_pgid_delay > 0.0 then ignore (select_retry [] before_pgid_delay) ;
    write_all
      handshake
      (Printf.sprintf "PGID %s\n" (handshake_pgid_record established_pgid)) ;
    let before_ack_delay =
      test_delay_seconds "CABAL_PROCESS_GROUP_TEST_BEFORE_ACK_DELAY_SECONDS"
    in
    if before_ack_delay > 0.0 then ignore (select_retry [] before_ack_delay) ;
    (* The parent validates this exact PGID and ACKs ownership before the
       backend exists. Any control loss, TERM, malformed record, or timeout
       before ACK reports a controlled failure and exits with no backend child. *)
    if not (wait_for_ack control) then begin
      report_ack_failure handshake control ;
      terminate_group established_pgid ;
      exit 128
    end ;
    let error_r, error_w = Unix.pipe () in
    let child_pid =
      match Unix.fork () with
      | 0 -> (
          close_noerr error_r ;
          close_noerr handshake ;
           close_noerr control_fd ;
           close_noerr status ;
           close_noerr liveness ;
          Unix.set_close_on_exec error_w ;
          Sys.set_signal Sys.sigterm Sys.Signal_default ;
          ignore (Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigterm]) ;
          try
            match cmd with
            | executable :: _ -> Unix.execvp executable (Array.of_list cmd)
            | [] -> assert false
          with exn ->
            write_noerr error_w (bounded_error (Printexc.to_string exn)) ;
            exit 127)
      | child_pid -> child_pid
    in
    child := Some child_pid ;
    forked := true ;
    close_noerr error_w ;
    close_supervisor_standard_fds () ;
    ignore (Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigterm]) ;
    let term_relayed = ref false in
    let relay_term () =
      if control.term_requested && not !term_relayed then begin
        term_relayed := true ;
        (* The direct child is known-owned even if the group broadcast races
           with a concurrent exit. *)
        (try Unix.kill child_pid Sys.sigterm with Unix.Unix_error _ -> ()) ;
        try Unix.kill (-established_pgid) Sys.sigterm
        with Unix.Unix_error _ -> ()
      end
    in
    let enforce_termination_deadline () =
      if control.term_requested then begin
        if
          Option.is_none control.requested_grace_seconds
          && not control.deadline_started
        then
          wait_for_control control 0.0 ;
        relay_term () ;
        let deadline =
          match !term_deadline with
          | Some deadline -> deadline
          | None -> create_termination_deadline control term_deadline
        in
        if monotonic_now () >= deadline then begin
          (* The supervisor remains the confirmed group leader until this
             broadcast.  Killing it with the group is safe: all descendants
             have already received TERM and no parent remains to release us. *)
          terminate_group established_pgid ;
          exit 128
        end
      end
    in
    let exec_error =
      Fun.protect
        ~finally:(fun () -> close_noerr error_r)
        (fun () ->
          wait_for_exec_error
            ~control
            ~on_tick:enforce_termination_deadline
            error_r)
    in
    if exec_error <> "" then report_error_message handshake exec_error
    else begin
      delay_with_control
        ~control
        ~on_tick:enforce_termination_deadline
        (test_handshake_delay_seconds ()) ;
      (* EOF alone is ambiguous: a pre-ACK exit or external supervisor death
         can leave only a PGID record. This explicit record is emitted only
         after the exec-error pipe closes successfully. *)
      write_all handshake "EXEC\n" ;
      if test_enabled "CABAL_PROCESS_GROUP_TEST_CLOSE_CONTROL_AFTER_EXEC" then begin
        close_noerr control_fd ;
        control.fd <- None
      end
    end ;
    close_noerr handshake ;
    let child_status =
      wait_for_child ~control ~on_tick:enforce_termination_deadline child_pid
    in
    child := None ;
    send_backend_status status child_status ;
    close_noerr status ;
    let rec remain_anchor () =
      enforce_termination_deadline () ;
      if control.term_requested then begin
        wait_for_control ~on_tick:enforce_termination_deadline control 0.05 ;
        remain_anchor ()
      end
      else if control.release_requested then begin
        write_test_marker "CABAL_PROCESS_GROUP_TEST_RELEASE_STARTED_MARKER" ;
        wait_for_test_gate
          ~control
          ~on_tick:enforce_termination_deadline
          "CABAL_PROCESS_GROUP_TEST_RELEASE_GATE" ;
        delay_with_control
          ~control
          ~on_tick:enforce_termination_deadline
          (test_delay_seconds "CABAL_PROCESS_GROUP_TEST_RELEASE_DELAY_SECONDS") ;
        exit_with_child_status child_status
      end
      else begin
        wait_for_control ~on_tick:enforce_termination_deadline control 0.05 ;
        remain_anchor ()
      end
    in
    remain_anchor ()
  with exn ->
    post_fork_cleanup () ;
    report_error handshake exn ;
    close_noerr status ;
    exit 127
