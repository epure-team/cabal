(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type handshake =
  | Established of int
  | Launcher_failed of {pgid : int option; message : string}
  | Timed_out
  | Invalid of string

type lifecycle =
  | Owned of int option
  | Terminating of {
      grace_seconds : float;
      pgid : int option;
      control_delivered : bool;
      cleanup_claim : int option;
      kill_sent : bool;
    }
  | Releasing
  | Retired

type t = {
  process : Eio_unix.Process.ty Eio.Resource.t;
  pid : int;
  mutable handshake : handshake;
  backend_status : Eio.Process.exit_status Eio.Promise.t;
  resolve_backend_status : Eio.Process.exit_status Eio.Promise.u;
  supervisor_status : Eio.Process.exit_status Eio.Promise.t;
  resolve_supervisor_status : Eio.Process.exit_status Eio.Promise.u;
  direct_supervisor_status : Eio.Process.exit_status Eio.Promise.t;
  resolve_direct_supervisor_status : Eio.Process.exit_status Eio.Promise.u;
  control : Eio_unix.sink_ty Eio.Resource.t;
  status : Eio_unix.source_ty Eio.Resource.t;
  liveness : Eio_unix.source_ty Eio.Resource.t;
  mutable lifecycle : lifecycle;
  stop_owner : unit Eio.Promise.u;
  lifecycle_lock : Eio.Mutex.t;
  wait_for_cleanup : float -> bool;
  mutable next_cleanup_claim : int;
}

let default_launcher = "cabal-process-group-launcher"

let default_handshake_timeout_seconds = 1.0

let default_grace_seconds = 2.0

(* The launcher uses this fixed grace for direct-SIGTERM and control-loss
   cleanup. It bounds custom launchers that do not implement the ACK protocol;
   an official launcher cannot fork before ownership is confirmed. *)
let launcher_fallback_grace_seconds = 2.0

let termination_scheduling_margin_seconds = 0.2

let handshake_fd = 3

let control_fd = 4

let status_fd = 5

let liveness_fd = 6

let max_handshake_bytes = 4096

let max_status_bytes = 128

let launcher_path () =
  match Sys.getenv_opt "CABAL_PROCESS_GROUP_LAUNCHER" with
  | Some path when path <> "" -> path
  | _ -> default_launcher

let pid t = t.pid

let handshake t = t.handshake

let group_id t =
  Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
      match t.lifecycle with
      | Owned (Some pgid) | Terminating {pgid = Some pgid; _} -> Some pgid
      | Owned None | Terminating {pgid = None; _} | Releasing | Retired -> None)

let awaited_status t = Eio.Promise.peek t.direct_supervisor_status

let parse_pgid line =
  match String.split_on_char ' ' line with
  | ["PGID"; value] -> (
      match int_of_string_opt value with
      | Some n when n > 0 -> Some n
      | _ -> None)
  | _ -> None

let parse_error line =
  match String.split_on_char ' ' line with
  | "ERROR" :: message when message <> [] -> Some (String.concat " " message)
  | _ -> None

let parse_handshake ~launcher_pid text =
  match String.split_on_char '\n' text with
  | [pgid_line; "EXEC"; ""] -> (
      match parse_pgid pgid_line with
      | Some pgid when pgid = launcher_pid -> Established pgid
      | Some _ -> Invalid "launcher reported a PGID that does not match its PID"
      | None -> Invalid "launcher sent EXEC without a valid PGID")
  | [pgid_line; error_line; ""] -> (
      match (parse_pgid pgid_line, parse_error error_line) with
      | Some pgid, Some message when pgid = launcher_pid ->
          Launcher_failed {pgid = Some pgid; message}
      | Some _, Some message -> Launcher_failed {pgid = None; message}
      | _ -> Invalid "launcher sent an invalid handshake sequence")
  | [pgid_line; ""] when Option.is_some (parse_pgid pgid_line) ->
      Invalid "launcher closed the handshake FD after PGID without EXEC"
  | [error_line; ""] -> (
      match parse_error error_line with
      | Some message -> Launcher_failed {pgid = None; message}
      | None -> Invalid "launcher closed the handshake FD with an invalid record")
  | _ -> Invalid "launcher sent an unexpected handshake sequence"

let parse_backend_status text =
  let lines =
    String.split_on_char '\n' text |> List.filter (fun line -> line <> "")
  in
  match lines with
  | [line] -> (
      match String.split_on_char ' ' line with
      | ["EXIT"; value] -> (
          match int_of_string_opt value with
          | Some code when code >= 0 -> `Exited code
          | _ -> invalid_arg "launcher sent an invalid backend exit status")
      | ["SIGNAL"; value] -> (
          match int_of_string_opt value with
          | Some signal when signal <> 0 -> `Signaled signal
          | _ -> invalid_arg "launcher sent an invalid backend signal status")
      | _ -> invalid_arg "launcher sent an invalid backend status record")
  | _ -> invalid_arg "launcher sent an unexpected backend status sequence"

let read_handshake ~launcher_pid ~on_pgid flow =
  let buffer = Buffer.create 64 in
  let chunk = Cstruct.create 128 in
  let pgid_reported = ref false in
  let inspect_first_line () =
    if not !pgid_reported then
      match String.index_opt (Buffer.contents buffer) '\n' with
      | None -> ()
      | Some newline ->
          let line = Buffer.sub buffer 0 newline in
          (match parse_pgid line with
          | Some pgid when pgid = launcher_pid -> on_pgid pgid
          | Some _ | None -> ()) ;
          pgid_reported := true
  in
  let rec loop () =
    match Eio.Flow.single_read flow chunk with
    | count ->
        if Buffer.length buffer + count > max_handshake_bytes then
          raise (Invalid_argument "launcher handshake exceeds its size bound") ;
        Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 count)) ;
        inspect_first_line () ;
        loop ()
    | exception End_of_file -> Buffer.contents buffer
  in
  loop ()

let read_backend_status flow =
  let buffer = Buffer.create 32 in
  let chunk = Cstruct.create 64 in
  let rec loop () =
    match Eio.Flow.single_read flow chunk with
    | count -> (
        if Buffer.length buffer + count > max_status_bytes then
          raise
            (Invalid_argument "launcher backend status exceeds its size bound") ;
        Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 count)) ;
        let text = Buffer.contents buffer in
        match String.index_opt text '\n' with
        | Some newline when newline + 1 = String.length text -> text
        | Some _ -> invalid_arg "launcher sent multiple backend status records"
        | None -> loop ())
    | exception End_of_file -> Buffer.contents buffer
  in
  loop ()

let try_resolve resolver status =
  try ignore (Eio.Promise.try_resolve resolver status) with _ -> ()

let close_noerr flow = try Eio.Flow.close flow with _ -> ()

let warn_noerr fmt =
  Printf.ksprintf
    (fun message -> try Diagnostics.warn "%s" message with _ -> ())
    fmt

let record_warning warnings fmt =
  Printf.ksprintf (fun message -> warnings := message :: !warnings) fmt

let emit_warnings warnings =
  List.rev !warnings |> List.iter (fun message -> warn_noerr "%s" message)

let test_enabled variable =
  (* Narrow fault-injection hooks used only by process-group regressions. *)
  match Sys.getenv_opt variable with Some "1" | Some "true" -> true | _ -> false

let write_test_marker variable =
  match Sys.getenv_opt variable with
  | None | Some "" -> ()
  | Some path ->
      let descriptor =
        try
          Some
            (Unix.openfile
               path
               [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK]
               0o600)
        with _ -> None
      in
      Option.iter
        (fun descriptor ->
          Fun.protect
            ~finally:(fun () -> try Unix.close descriptor with _ -> ())
            (fun () ->
              try ignore (Unix.write_substring descriptor "confirmed\n" 0 10)
              with _ -> ()))
        descriptor

let wait_for_test_gate ~clock variable =
  match Sys.getenv_opt variable with
  | None | Some "" -> ()
  | Some path ->
      let rec loop () =
        if Sys.file_exists path then ()
        else begin
          Eio.Time.sleep clock 0.01 ;
          loop ()
        end
      in
      loop ()

let next_cleanup_claim t =
  let claim = t.next_cleanup_claim in
  t.next_cleanup_claim <- claim + 1 ;
  claim

let complete_reaped_status t status =
  close_noerr t.control ;
  close_noerr t.status ;
  close_noerr t.liveness ;
  try_resolve t.resolve_backend_status status ;
  try_resolve t.resolve_direct_supervisor_status status ;
  try_resolve t.resolve_supervisor_status status

let finish_retirement t status =
  complete_reaped_status t status ;
  try_resolve t.stop_owner ()

let start_owner_finalization t =
  (* Logical retirement has already hidden the PGID and invalidated the claim.
     Closing protocol flows prevents more work, while [stop_owner] starts Eio's
     process-resource release hook. Public completion remains unresolved until
     that hook has performed its non-cancellable waitpid. *)
  close_noerr t.control ;
  close_noerr t.status ;
  try_resolve t.stop_owner ()

let retire t status =
  let committed =
    Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
        match t.lifecycle with
        | Retired -> false
        | Owned _ | Terminating _ | Releasing ->
            t.lifecycle <- Retired ;
            true)
  in
  if committed then finish_retirement t status

let retire_claim t claim status =
  let committed =
    Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
        match t.lifecycle with
        | Terminating {cleanup_claim = Some current; _} when current = claim ->
            t.lifecycle <- Retired ;
            true
        | Owned _ | Terminating _ | Releasing | Retired -> false)
  in
  if committed then finish_retirement t status

let retire_claim_after_owner_reap t claim =
  let committed =
    Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
        match t.lifecycle with
        | Terminating {cleanup_claim = Some current; _} when current = claim ->
            t.lifecycle <- Retired ;
            true
        | Owned _ | Terminating _ | Releasing | Retired -> false)
  in
  if committed then start_owner_finalization t ;
  (* This wait cannot form a cycle: [start_owner_finalization] wakes the owner
     switch first; only its completed process-resource finalizer resolves this
     promise. A lost claim waits for whichever actual-reap path won. *)
  ignore (Eio.Promise.await t.supervisor_status)

let await_supervisor_once t =
  Eio.Promise.await t.supervisor_status

let await_direct_supervisor_once t =
  Eio.Promise.await t.direct_supervisor_status

(* Awaiting must not hold [lifecycle_lock]: a concurrent terminator needs that
   lock to send TERM/KILL. Eio serializes concurrent waits for the underlying
   process exit status. *)
let await_supervisor t = await_supervisor_once t

type group_signal_outcome =
  | Group_signaled
  | Group_missing
  | Group_signal_failed of exn

let attempt_group_signal pgid signal =
  try
    if signal = Sys.sigterm && test_enabled "CABAL_PROCESS_GROUP_TEST_FAIL_GROUP_TERM"
    then raise (Unix.Unix_error (Unix.EPERM, "kill", string_of_int (-pgid))) ;
    if signal = Sys.sigkill && test_enabled "CABAL_PROCESS_GROUP_TEST_FAIL_GROUP_KILL"
    then raise (Unix.Unix_error (Unix.EPERM, "kill", string_of_int (-pgid))) ;
    Unix.kill (-pgid) signal ;
    Group_signaled
  with
  | Unix.Unix_error (Unix.ESRCH, _, _) -> Group_missing
  | exn -> Group_signal_failed exn

let record_group_signal_outcome warnings pgid signal = function
  | Group_signaled | Group_missing -> ()
  | Group_signal_failed (Unix.Unix_error (error, _, _)) ->
      record_warning
        warnings
        "Process-group signal %d to PGID %d failed: %s"
        signal
        pgid
        (Unix.error_message error)
  | Group_signal_failed exn ->
      record_warning
        warnings
        "Process-group signal %d to PGID %d failed: %s"
        signal
        pgid
        (Printexc.to_string exn)

let attempt_direct_signal t signal =
  try
    if signal = Sys.sigkill && test_enabled "CABAL_PROCESS_GROUP_TEST_FAIL_DIRECT_KILL"
    then begin
      write_test_marker "CABAL_PROCESS_GROUP_TEST_DIRECT_KILL_ATTEMPT_MARKER" ;
      raise (Unix.Unix_error (Unix.EPERM, "kill", string_of_int t.pid))
    end ;
    Eio.Process.signal t.process signal ;
    Group_signaled
  with exn -> Group_signal_failed exn

let record_direct_signal_outcome warnings t signal = function
  | Group_signaled | Group_missing -> ()
  | Group_signal_failed exn ->
      record_warning
        warnings
      "Direct process signal %d to PID %d failed: %s"
      signal
      t.pid
      (Printexc.to_string exn)

let send_command t command =
  try
    Eio.Flow.copy_string command t.control ;
    true
  with _ -> false

let valid_grace_seconds seconds =
  seconds >= 0.0
  &&
  match classify_float seconds with FP_normal | FP_subnormal | FP_zero -> true | _ -> false

let valid_handshake_timeout_seconds seconds =
  seconds > 0.0
  &&
  match classify_float seconds with FP_normal | FP_subnormal -> true | _ -> false

let signal_log pgid signal =
  match Sys.getenv_opt "CABAL_PROCESS_GROUP_TEST_SIGNAL_LOG" with
  | None | Some "" -> ()
  | Some path ->
      let descriptor =
        try
          Some
            (Unix.openfile
               path
               [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_NONBLOCK]
               0o600)
        with _ -> None
      in
      Option.iter
        (fun descriptor ->
          Fun.protect
            ~finally:(fun () -> try Unix.close descriptor with _ -> ())
            (fun () ->
              try
                Unix.set_close_on_exec descriptor ;
                let record = Printf.sprintf "%d %d\n" pgid signal in
                ignore
                  (Unix.write_substring descriptor record 0 (String.length record))
              with _ -> ()))
        descriptor

let record_group_attempt warnings pgid signal outcome =
  signal_log pgid signal ;
  record_group_signal_outcome warnings pgid signal outcome

type wait_outcome = Cleanup_completed | Deadline_elapsed | Wait_failed

let wait_for_cleanup_noerr ~warnings ~context ~wait seconds =
  try
    if test_enabled "CABAL_PROCESS_GROUP_TEST_FAIL_CLEANUP_SLEEP" then
      failwith "injected cleanup timer failure" ;
    if wait seconds then Cleanup_completed else Deadline_elapsed
  with exn ->
    record_warning warnings "%s timer failed: %s" context (Printexc.to_string exn) ;
    Wait_failed

let await_direct_supervisor_noerr ~warnings t =
  match Eio.Promise.peek t.direct_supervisor_status with
  | Some status -> Some status
  | None -> (
      try
        if test_enabled "CABAL_PROCESS_GROUP_TEST_FAIL_DIRECT_AWAIT" then
          failwith "injected direct-supervisor await failure" ;
        Some (await_direct_supervisor_once t)
      with exn ->
        record_warning
          warnings
          "Direct-supervisor await for PID %d failed: %s"
          t.pid
          (Printexc.to_string exn) ;
        None)

let signal_group_for_claim t claim pgid signal =
  Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
      match t.lifecycle with
      | Terminating
          {
            grace_seconds;
            pgid = current_pgid;
            control_delivered;
            cleanup_claim = Some current;
            kill_sent;
          }
        when current = claim ->
          if signal = Sys.sigkill && not kill_sent then
            t.lifecycle <-
              Terminating
                {
                  grace_seconds;
                  pgid = current_pgid;
                  control_delivered;
                  cleanup_claim = Some current;
                  kill_sent = true;
                } ;
          Some (attempt_group_signal pgid signal)
      | Owned _ | Terminating _ | Releasing | Retired -> None)

let signal_catastrophic_group_for_claim t claim pgid =
  Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
      match t.lifecycle with
      | Terminating
          {
            grace_seconds;
            pgid = current_pgid;
            control_delivered;
            cleanup_claim = Some current;
            kill_sent = _;
          }
        when current = claim ->
          t.lifecycle <-
            Terminating
              {
                grace_seconds;
                pgid = current_pgid;
                control_delivered;
                cleanup_claim = Some current;
                kill_sent = true;
              } ;
          (* The raw signal attempts are the first side effects after the state
             transition and are consecutive. Logging and diagnostics happen
             only after ownership retirement. *)
          let term_outcome = attempt_group_signal pgid Sys.sigterm in
          let kill_outcome = attempt_group_signal pgid Sys.sigkill in
          Some (term_outcome, kill_outcome)
      | Owned _ | Terminating _ | Releasing | Retired -> None)

let signal_direct_for_claim t claim signal =
  Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
      match t.lifecycle with
      | Terminating
          {
            grace_seconds;
            pgid;
            control_delivered;
            cleanup_claim = Some current;
            kill_sent;
          }
        when current = claim ->
          if signal = Sys.sigkill && not kill_sent then
            t.lifecycle <-
              Terminating
                {
                  grace_seconds;
                  pgid;
                  control_delivered;
                  cleanup_claim = Some current;
                  kill_sent = true;
                } ;
          Some (attempt_direct_signal t signal)
      | Owned _ | Terminating _ | Releasing | Retired -> None)

let kill_direct_and_retire_claim ~warnings t claim =
  match Eio.Promise.peek t.direct_supervisor_status with
  | Some status -> retire_claim t claim status
  | None -> (
      match signal_direct_for_claim t claim Sys.sigkill with
      | None -> ignore (await_supervisor_once t)
      | Some outcome ->
          let reaped_status =
            match outcome with
            | Group_signaled -> await_direct_supervisor_noerr ~warnings t
            | Group_missing | Group_signal_failed _ -> None
          in
          (match reaped_status with
          | Some status -> retire_claim t claim status
          | None -> retire_claim_after_owner_reap t claim) ;
          record_direct_signal_outcome warnings t Sys.sigkill outcome)

let finish_group_cleanup ~clock t ~claim ~pgid ~grace_seconds =
  let warnings = ref [] in
  let record signal = function
    | None -> None
    | Some outcome ->
        record_group_attempt warnings pgid signal outcome ;
        Some outcome
  in
  let safest_escalation () =
    match record Sys.sigkill (signal_group_for_claim t claim pgid Sys.sigkill) with
    | None -> ignore (await_supervisor_once t)
    | Some outcome ->
        let reaped_status =
          match outcome with
          | Group_signaled -> await_direct_supervisor_noerr ~warnings t
          | Group_missing -> None
          | Group_signal_failed _ -> (
              (* The private TERM command may still let the official launcher
                 perform its own group KILL. Give its polling loop one bounded
                 scheduling margin before retirement stops the owner switch;
                 otherwise the process-resource fallback would kill the
                 supervisor first and could orphan a TERM-ignoring child. *)
              match
                wait_for_cleanup_noerr
                  ~warnings
                  ~context:"Failed group-KILL supervisor fallback"
                  ~wait:t.wait_for_cleanup
                  termination_scheduling_margin_seconds
              with
              | Cleanup_completed -> Eio.Promise.peek t.direct_supervisor_status
              | Deadline_elapsed | Wait_failed -> None)
        in
        (match reaped_status with
        | Some status -> retire_claim t claim status
        | None -> retire_claim_after_owner_reap t claim)
  in
  (* The direct launcher remains the normal-path anchor through the requested
     grace. If its observer retires or takes over, this wait resolves and the
     stale claim can no longer signal. *)
  Eio.Cancel.protect (fun () ->
      Fun.protect
        ~finally:(fun () -> emit_warnings warnings)
        (fun () ->
          try
            ignore
              (record Sys.sigterm
                 (signal_group_for_claim t claim pgid Sys.sigterm)) ;
            let outcome =
              wait_for_cleanup_noerr
                ~warnings
                ~context:"Confirmed process-group cleanup"
                ~wait:(fun seconds ->
                  match
                    Eio.Time.with_timeout clock seconds (fun () ->
                        ignore (await_supervisor_once t) ;
                        Ok ())
                  with
                  | Ok () -> true
                  | Error `Timeout -> false)
                grace_seconds
            in
            (match outcome with
            | Cleanup_completed -> ()
            | Deadline_elapsed | Wait_failed -> safest_escalation ())
          with exn ->
            record_warning
              warnings
              "Confirmed process-group cleanup leader failed: %s"
              (Printexc.to_string exn) ;
            safest_escalation ()))

let finish_catastrophic_group_cleanup t ~claim ~pgid ~status =
  let warnings = ref [] in
  let record_attempts = function
    | None -> ()
    | Some (term_outcome, kill_outcome) ->
        record_group_attempt warnings pgid Sys.sigterm term_outcome ;
        record_group_attempt warnings pgid Sys.sigkill kill_outcome
  in
  Eio.Cancel.protect (fun () ->
      Fun.protect
        ~finally:(fun () -> emit_warnings warnings)
        (fun () ->
          try
            let attempts = signal_catastrophic_group_for_claim t claim pgid in
            retire_claim t claim status ;
            record_attempts attempts
          with exn ->
            record_warning
              warnings
              "Catastrophic process-group cleanup leader failed: %s"
              (Printexc.to_string exn) ;
            let kill_attempt = signal_group_for_claim t claim pgid Sys.sigkill in
            retire_claim t claim status ;
            Option.iter
              (record_group_attempt warnings pgid Sys.sigkill)
              kill_attempt))

let finish_unconfirmed_cleanup ~clock t ~claim ~grace_seconds ~control_delivered
    ~initial_direct_term =
  let warnings = ref [] in
  let backstop_delay =
    if control_delivered then
      grace_seconds +. termination_scheduling_margin_seconds
    else
      max grace_seconds launcher_fallback_grace_seconds
      +. termination_scheduling_margin_seconds
  in
  Eio.Cancel.protect (fun () ->
      Fun.protect
        ~finally:(fun () -> emit_warnings warnings)
        (fun () ->
          Option.iter
            (record_direct_signal_outcome warnings t Sys.sigterm)
            initial_direct_term ;
          try
            let outcome =
              wait_for_cleanup_noerr
                ~warnings
                ~context:"Unconfirmed direct-process cleanup"
                ~wait:(fun seconds ->
                  match
                    Eio.Time.with_timeout clock seconds (fun () ->
                        ignore (await_supervisor_once t) ;
                        Ok ())
                  with
                  | Ok () -> true
                  | Error `Timeout -> false)
                backstop_delay
            in
            (match outcome with
            | Cleanup_completed -> ()
            | Deadline_elapsed | Wait_failed ->
                kill_direct_and_retire_claim ~warnings t claim)
          with exn ->
            record_warning
              warnings
              "Unconfirmed direct-process cleanup leader failed: %s"
              (Printexc.to_string exn) ;
            kill_direct_and_retire_claim ~warnings t claim))

let terminate ?(grace_seconds = default_grace_seconds) ~clock t =
  if not (valid_grace_seconds grace_seconds) then
    invalid_arg "Process_group.terminate: grace must be finite and non-negative" ;
  Eio.Cancel.protect (fun () ->
      let action =
        Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
            match Eio.Promise.peek t.supervisor_status with
            | Some _ -> `Done
            | None -> (
                match t.lifecycle with
                | Releasing -> `Done
                | Retired -> `Follower
                | Terminating {cleanup_claim = Some _; _} -> `Follower
                | Terminating
                    {
                      grace_seconds;
                      pgid = Some pgid;
                      control_delivered;
                      cleanup_claim = None;
                      kill_sent;
                    } ->
                    let claim = next_cleanup_claim t in
                    t.lifecycle <-
                      Terminating
                        {
                          grace_seconds;
                          pgid = Some pgid;
                          control_delivered;
                          cleanup_claim = Some claim;
                          kill_sent;
                        } ;
                    `Group_leader (claim, pgid, grace_seconds)
                | Terminating
                    {
                      grace_seconds;
                      pgid = None;
                      control_delivered;
                      cleanup_claim = None;
                      kill_sent;
                    } ->
                    let claim = next_cleanup_claim t in
                    t.lifecycle <-
                      Terminating
                        {
                          grace_seconds;
                          pgid = None;
                          control_delivered;
                          cleanup_claim = Some claim;
                          kill_sent;
                        } ;
                    `Unconfirmed_leader
                      (claim, grace_seconds, control_delivered, None)
                | Owned (Some pgid) ->
                    let claim = next_cleanup_claim t in
                    t.lifecycle <-
                      Terminating
                        {
                          grace_seconds;
                          pgid = Some pgid;
                          control_delivered = false;
                          cleanup_claim = Some claim;
                          kill_sent = false;
                        } ;
                    (* The queued record preserves the requested deadline if
                       the supervisor wakes later. The parent broadcasts TERM
                       itself before this grace starts. *)
                    ignore
                      (send_command
                         t
                         (Printf.sprintf "TERM %.17g\n" grace_seconds)) ;
                    `Group_leader (claim, pgid, grace_seconds)
                | Owned None ->
                    (* Mark termination before FD4 is written: a simultaneous
                       PGID report must not be ACKed. *)
                    let claim = next_cleanup_claim t in
                    t.lifecycle <-
                      Terminating
                        {
                          grace_seconds;
                          pgid = None;
                          control_delivered = false;
                          cleanup_claim = Some claim;
                          kill_sent = false;
                        } ;
                    let control_delivered =
                      send_command t (Printf.sprintf "TERM %.17g\n" grace_seconds)
                    in
                    t.lifecycle <-
                      Terminating
                        {
                          grace_seconds;
                          pgid = None;
                          control_delivered;
                          cleanup_claim = Some claim;
                          kill_sent = false;
                        } ;
                    let initial_direct_term =
                      if control_delivered then None
                      else Some (attempt_direct_signal t Sys.sigterm)
                    in
                    `Unconfirmed_leader
                      (claim, grace_seconds, control_delivered, initial_direct_term)))
      in
      match action with
      | `Done -> ()
      | `Group_leader (claim, pgid, effective_grace_seconds) ->
          finish_group_cleanup
            ~clock
            t
            ~claim
            ~pgid
            ~grace_seconds:effective_grace_seconds
      | `Unconfirmed_leader
          (claim, effective_grace_seconds, control_delivered, initial_direct_term)
        ->
          finish_unconfirmed_cleanup
            ~clock
            t
            ~claim
            ~grace_seconds:effective_grace_seconds
            ~control_delivered
            ~initial_direct_term
      | `Follower -> (
          try ignore (await_supervisor_once t)
          with exn ->
            warn_noerr
              "Process-group cleanup follower failed: %s"
              (Printexc.to_string exn)))

let finish_failed_release_cleanup t claim =
  let warnings = ref [] in
  let direct_kill () = kill_direct_and_retire_claim ~warnings t claim in
  Eio.Cancel.protect (fun () ->
      Fun.protect
        ~finally:(fun () -> emit_warnings warnings)
        (fun () ->
          try
            match signal_direct_for_claim t claim Sys.sigterm with
            | None -> ()
            | Some term_outcome ->
                let delivered =
                  match term_outcome with
                  | Group_signaled -> true
                  | Group_missing | Group_signal_failed _ -> false
                in
                if not delivered then direct_kill ()
                else
                  let outcome =
                    wait_for_cleanup_noerr
                      ~warnings
                      ~context:"Failed RELEASE direct-process cleanup"
                      ~wait:t.wait_for_cleanup
                      (launcher_fallback_grace_seconds
                      +. termination_scheduling_margin_seconds)
                  in
                  (match outcome with
                  | Cleanup_completed -> ()
                  | Deadline_elapsed | Wait_failed -> direct_kill ()) ;
                record_direct_signal_outcome warnings t Sys.sigterm term_outcome
          with exn ->
            record_warning
              warnings
              "Failed RELEASE cleanup leader failed: %s"
              (Printexc.to_string exn) ;
            direct_kill ()))

let release t =
  Eio.Cancel.protect (fun () ->
      let failed_release_claim =
        Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
            match Eio.Promise.peek t.supervisor_status with
            | Some _ -> None
            | None -> (
                match t.lifecycle with
                | Retired | Releasing | Terminating _ -> None
                | Owned _ when Option.is_none (Eio.Promise.peek t.backend_status) ->
                    (* RELEASE transfers ownership to a supervisor that exits
                       only after its backend child has completed. Before the
                       status promise resolves, retaining ownership is required
                       so caller-switch shutdown can still terminate the group. *)
                    None
                | Owned _ ->
                    if send_command t "RELEASE\n" then begin
                      (* The successful write and ownership retirement are one
                         locked transition; later terminators are no-ops. *)
                      t.lifecycle <- Releasing ;
                      None
                    end
                    else begin
                      (* A failed write cannot establish that the old group
                         leader still owns [pgid]. Clear it before direct
                         cleanup: a stale negative PID could target an unrelated
                         reused group. *)
                      let claim = next_cleanup_claim t in
                      t.lifecycle <-
                        Terminating
                          {
                            grace_seconds = default_grace_seconds;
                            pgid = None;
                            control_delivered = false;
                            cleanup_claim = Some claim;
                            kill_sent = false;
                          } ;
                      Some claim
                    end))
      in
      Option.iter (finish_failed_release_cleanup t) failed_release_claim)

let observe_direct_supervisor_exit t status =
  (* FD6 proves only that the direct supervisor is gone. A confirmed process
     group can retain descendants after that event, so the observer records the
     direct wait status separately and lets exactly one cleanup leader retire
     public ownership only after TERM/KILL escalation. *)
  try_resolve t.resolve_direct_supervisor_status status ;
  let action =
    Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
        match (Eio.Promise.peek t.supervisor_status, t.lifecycle) with
        | Some _, _ -> `Done
        | None, Retired -> `Done
        | None, (Releasing | Owned None | Terminating {pgid = None; _}) -> `Retire
        | None, Owned (Some pgid) ->
            let claim = next_cleanup_claim t in
            t.lifecycle <-
              Terminating
                {
                  grace_seconds = default_grace_seconds;
                  pgid = Some pgid;
                  control_delivered = false;
                  cleanup_claim = Some claim;
                  kill_sent = false;
                } ;
            `Catastrophic_group_leader (claim, pgid)
        | None, Terminating {pgid = Some _; kill_sent = true; _} -> `Retire
        | None,
          Terminating
            {
              grace_seconds;
              pgid = Some pgid;
              control_delivered;
              cleanup_claim = _;
              kill_sent = false;
            } ->
            (* Direct-supervisor loss invalidates any sleeping normal leader's
               claim before the catastrophic signal pair is attempted. *)
            let claim = next_cleanup_claim t in
            t.lifecycle <-
              Terminating
                {
                  grace_seconds;
                  pgid = Some pgid;
                  control_delivered;
                  cleanup_claim = Some claim;
                  kill_sent = false;
                } ;
            `Catastrophic_group_leader (claim, pgid))
  in
  match action with
  | `Done -> ()
  | `Retire -> retire t status
  | `Catastrophic_group_leader (claim, pgid) ->
      finish_catastrophic_group_cleanup t ~claim ~pgid ~status

let await_backend t = Eio.Promise.await t.backend_status

let await t =
  let status = await_backend t in
  release t ;
  ignore (await_supervisor t) ;
  status

type acknowledgement = Delivered | Skipped | Failed

let acknowledge_reported_group t pgid =
  let should_ack =
    Eio.Mutex.use_rw ~protect:true t.lifecycle_lock (fun () ->
        match (Eio.Promise.peek t.supervisor_status, t.lifecycle) with
        | None, Owned None ->
            (* Ownership is committed before ACK is written. A concurrent
               terminator can therefore safely signal [-pgid] before the
               launcher has permission to fork. *)
            t.lifecycle <- Owned (Some pgid) ;
            true
        | _ -> false)
  in
  if should_ack then
    if send_command t "ACK\n" then Delivered else Failed
  else Skipped

let launcher_executable launcher =
  if String.contains launcher '/' then
    let host_path =
      if Filename.is_relative launcher then
        Filename.concat (Sys.getcwd ()) launcher
      else launcher
    in
    Some (Unix.realpath host_path)
  else None

let spawn ~sw ~clock ~mgr ?cwd ?env ?stdin ?stdout ?stderr ?launcher
    ?(handshake_timeout_seconds = default_handshake_timeout_seconds) cmd =
  if not (valid_handshake_timeout_seconds handshake_timeout_seconds) then
    invalid_arg "Process_group.spawn: handshake timeout must be finite and positive" ;
  match cmd with
  | [] -> invalid_arg "Process_group.spawn: empty command"
  | _ ->
      let launcher = Option.value ~default:(launcher_path ()) launcher in
      (* Resolve explicit launcher paths in the host before allocating any
          resources or changing the backend's cwd in [spawn_unix]. *)
      let executable = launcher_executable launcher in
      let ready, resolve_ready = Eio.Promise.create () in
      let target_ref = ref None in
      (* Eio attaches a process reaper daemon to the switch supplied to
          [spawn_unix].  Keep that daemon on a protected owner switch: the
          caller switch may cancel its observers, but its cancellation finalizer
          can still await a live process reaper before any caller release hooks
          run. *)
      Eio.Fiber.fork_daemon ~sw (fun () ->
          try Eio.Fiber.await_cancel ()
          with Eio.Cancel.Cancelled _ ->
            Eio.Cancel.protect (fun () ->
                match !target_ref with
                | Some target -> terminate ~clock target
                | None -> (
                    match Eio.Promise.await ready with
                    | Ok (target, _) -> terminate ~clock target
                    | Error _ -> ())) ;
            `Stop_daemon) ;
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Fun.protect
            ~finally:(fun () ->
              (* [run_protected] returns only after the process release hook has
                 killed any live direct process and completed waitpid. Resolve
                 public completion from that actual status, never from an
                 inferred signal outcome that merely started finalization. *)
              Eio.Cancel.protect (fun () ->
                  Option.iter
                    (fun target ->
                      let status = Eio.Process.await target.process in
                      complete_reaped_status target status)
                    !target_ref))
            (fun () ->
              Eio.Switch.run_protected (fun owner_sw ->
                try
                let handshake_r, handshake_w = Eio_unix.pipe owner_sw in
                let control_r, control_w = Eio_unix.pipe owner_sw in
                let status_r, status_w = Eio_unix.pipe owner_sw in
                let liveness_r, liveness_w = Eio_unix.pipe owner_sw in
                let stdin_fd =
                  match stdin with
                  | Some flow -> Eio_unix.Resource.fd flow
                  | None -> Eio_unix.Fd.stdin
                in
                let stdout_fd =
                  match stdout with
                  | Some flow -> Eio_unix.Resource.fd flow
                  | None -> Eio_unix.Fd.stdout
                in
                let stderr_fd =
                  match stderr with
                  | Some flow -> Eio_unix.Resource.fd flow
                  | None -> Eio_unix.Fd.stderr
                in
                let args = launcher :: "--" :: cmd in
                let fds =
                  [
                    (0, stdin_fd, `Blocking);
                    (1, stdout_fd, `Blocking);
                    (2, stderr_fd, `Blocking);
                    (handshake_fd, Eio_unix.Resource.fd handshake_w, `Blocking);
                    (control_fd, Eio_unix.Resource.fd control_r, `Blocking);
                    (status_fd, Eio_unix.Resource.fd status_w, `Blocking);
                    (liveness_fd, Eio_unix.Resource.fd liveness_w, `Blocking);
                  ]
                in
                let process =
                  match executable with
                  | Some executable ->
                      Eio_unix.Process.spawn_unix
                        ~sw:owner_sw
                        mgr
                        ?cwd
                        ?env
                        ~fds
                        ~executable
                        args
                  | None ->
                      Eio_unix.Process.spawn_unix
                        ~sw:owner_sw
                        mgr
                        ?cwd
                        ?env
                        ~fds
                        args
                in
                close_noerr handshake_w ;
                close_noerr control_r ;
                close_noerr status_w ;
                close_noerr liveness_w ;
                let backend_status, resolve_backend_status =
                  Eio.Promise.create ()
                in
                let supervisor_status, resolve_supervisor_status =
                  Eio.Promise.create ()
                in
                let direct_supervisor_status, resolve_direct_supervisor_status =
                  Eio.Promise.create ()
                in
                let owner_stop, stop_owner = Eio.Promise.create () in
                 let target =
                  {
                    process;
                    pid = Eio.Process.pid process;
                    handshake = Timed_out;
                    backend_status;
                    resolve_backend_status;
                    supervisor_status;
                    resolve_supervisor_status;
                    direct_supervisor_status;
                    resolve_direct_supervisor_status;
                    control = control_w;
                    status = status_r;
                    liveness = liveness_r;
                     lifecycle = Owned None;
                     stop_owner;
                     lifecycle_lock = Eio.Mutex.create ();
                     wait_for_cleanup =
                       (fun seconds ->
                         match
                           Eio.Time.with_timeout clock seconds (fun () ->
                               ignore (Eio.Promise.await supervisor_status) ;
                               Ok ())
                         with
                         | Ok () -> true
                         | Error `Timeout -> false);
                     next_cleanup_claim = 1;
                   }
                 in
                 target_ref := Some target ;
                  (* Reap the direct supervisor exactly once on the protected
                     owner switch. This observer must be a daemon: if an
                     explicit direct KILL fails, logical retirement stops the
                     owner switch, whose process release hook supplies the
                     final SIGKILL and non-cancellable waitpid. A non-daemon
                     liveness read would prevent those resource hooks from
                     running and strand caller-switch teardown. *)
                  Eio.Fiber.fork_daemon ~sw:owner_sw (fun () ->
                      Fun.protect
                        ~finally:(fun () -> close_noerr target.liveness)
                        (fun () ->
                          let chunk = Cstruct.create 1 in
                          let rec wait_for_liveness_eof () =
                            match Eio.Flow.single_read target.liveness chunk with
                            | _ -> wait_for_liveness_eof ()
                            | exception End_of_file -> ()
                          in
                          wait_for_liveness_eof () ;
                          let status = Eio.Process.await process in
                          observe_direct_supervisor_exit target status) ;
                      `Stop_daemon) ;
                  try_resolve resolve_ready (Ok (target, handshake_r)) ;
                  Eio.Promise.await owner_stop ;
                  write_test_marker
                    "CABAL_PROCESS_GROUP_TEST_OWNER_FINALIZATION_STARTED_MARKER" ;
                  wait_for_test_gate
                    ~clock
                    "CABAL_PROCESS_GROUP_TEST_OWNER_FINALIZATION_GATE"
                with exn ->
                  try_resolve resolve_ready (Error exn) ;
                  raise exn)) ;
          `Stop_daemon) ;
      let target, handshake_r =
        match Eio.Promise.await ready with
        | Ok target -> target
        | Error exn -> raise exn
      in
      let complete_handshake () =
        Fun.protect
          ~finally:(fun () -> close_noerr handshake_r)
          (fun () ->
            let outcome =
              Eio.Time.with_timeout clock handshake_timeout_seconds (fun () ->
                  Ok
                     (read_handshake
                        ~launcher_pid:target.pid
                        ~on_pgid:(fun pgid ->
                          match acknowledge_reported_group target pgid with
                          | Delivered | Skipped -> ()
                          | Failed -> terminate ~clock target)
                        handshake_r))
            in
            match outcome with
            | Error `Timeout -> Timed_out
            | Ok text -> parse_handshake ~launcher_pid:target.pid text)
      in
      (try
          let outcome = complete_handshake () in
          target.handshake <- outcome ;
          (match outcome with
          | Established _ ->
              write_test_marker "CABAL_PROCESS_GROUP_TEST_EXEC_CONFIRMED_MARKER"
          | Launcher_failed _ | Timed_out | Invalid _ -> ()) ;
          (* A handshake timeout before the ACK cannot leave an official
             launcher waiting indefinitely. Closing FD3 makes its PGID write
             fail; TERM on FD4 is the independent fail-closed backstop. *)
          (match outcome with
          | Timed_out ->
              let awaiting_ack =
                Eio.Mutex.use_rw ~protect:true target.lifecycle_lock (fun () ->
                    match target.lifecycle with Owned None -> true | _ -> false)
              in
              if awaiting_ack then terminate ~clock target
          | Established _ | Launcher_failed _ | Invalid _ -> ())
        with exn ->
         terminate ~clock target ;
         raise exn) ;
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Fun.protect
            ~finally:(fun () -> close_noerr target.status)
            (fun () ->
              try
                let status =
                  read_backend_status target.status |> parse_backend_status
                in
                try_resolve target.resolve_backend_status status
              with
              | Eio.Cancel.Cancelled _ as exn -> raise exn
              | _ ->
                  (* A malformed or closed status FD is a launcher failure. Keep
                   the anchor alive until the normal termination path has
                   killed and reaped the group. *)
                  terminate ~clock target) ;
          `Stop_daemon) ;
      target
