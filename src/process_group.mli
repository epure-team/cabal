(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Process-tree ownership for backend commands.

    A process group is created by the installed [cabal-process-group-launcher]
    executable. The launcher is a session-leader supervisor: it calls [setsid],
    reports its PGID, waits for Cabal's private acknowledgement before it starts
    the backend child, emits an explicit [EXEC] record only after child [exec]
    succeeds, and remains alive after TERM so termination can
    KILL a still-anchored process group after its grace period. The
    implementation uses only POSIX process groups and Eio's portable Unix
    process API; it has no dependency on a Cabal host or backend. *)

(** Outcome of the launcher's bounded handshake. *)
type handshake =
  | Established of int
       (** The launcher established and reported this process-group ID, which
           was verified to equal its direct process ID, then emitted the exact
           [PGID <pid>\nEXEC\n] handshake sequence after successful backend
           [exec]. *)
  | Launcher_failed of {pgid : int option; message : string}
      (** The launcher reported a failure before [exec].  When [pgid] is
           present, it was verified to equal the direct launcher process ID and
           is safe to terminate as the established group. *)
  | Timed_out
      (** The launcher did not complete its handshake within the configured
           bound. A [PGID] record received before the timeout remains owned only
           when it matches the direct launcher process ID. *)
  | Invalid of string
       (** The launcher closed the handshake FD without an exact successful or
             controlled-failure protocol. A bare [PGID] followed by EOF is never
             successful. No group signal is sent unless a prior PID-matching
             [PGID] record established ownership. *)

(** An owned supervisor/backend process and, while it is live, its process
    group. Backend completion and supervisor retirement are deliberately
    separate: the supervisor remains the group leader until Cabal releases or
    terminates it. *)
type t

(** Installed launcher command used when [spawn] is not given [~launcher].
    [CABAL_PROCESS_GROUP_LAUNCHER] overrides this for deployments and tests. *)
val default_launcher : string

(** [spawn ~sw ~clock ~mgr cmd] starts [cmd] through the process-group
    launcher.  The dedicated handshake FD is separate from standard input,
     output, and error.  It carries a [PGID] record after [setsid] and before
      the backend is forked. Cabal validates that it equals the direct launcher
      PID, records ownership under its lifecycle lock, and only then sends the
       private acknowledgement that permits the fork. If either side loses the
       handshake or control channel before that acknowledgement, the launcher
       exits without creating a backend. After a successful fork and [exec], it
       writes [EXEC] before closing FD3; EOF alone never confirms [exec]. The
       handshake is bounded by a finite, strictly-positive
      [handshake_timeout_seconds] (default [1.0]); invalid values are rejected
      before any process or pipe is allocated.

     [stdin], [stdout], and [stderr] must be Unix-backed Eio flows.  They are
     intentionally explicit because the launcher needs arbitrary FD mappings.
     An explicit slash-containing relative [launcher] path is resolved against
     the host [Sys.getcwd ()] before pipes are allocated and before [cwd] is
     applied to the backend process.
     On cancellation while waiting for the handshake, the owned target is
     terminated before the exception is propagated. Caller-switch cancellation
     also terminates and reaps through a protected owner switch before ordinary
     switch release can kill only the direct supervisor. *)
val spawn :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  mgr:_ Eio_unix.Process.mgr ->
  ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
  ?env:string array ->
  ?stdin:_ Eio_unix.source ->
  ?stdout:_ Eio_unix.sink ->
  ?stderr:_ Eio_unix.sink ->
  ?launcher:string ->
  ?handshake_timeout_seconds:float ->
  string list ->
  t

(** [pid t] is the direct launcher/backend process ID. *)
val pid : t -> int

(** [handshake t] is the immutable result of the launcher handshake. *)
val handshake : t -> handshake

(** [group_id t] is [Some pgid] only while Cabal owns a confirmed, live process
    group. It remains present after the backend child completes, but becomes
     [None] immediately when a successful {!release} transfers ownership and
     permanently after cleanup completes. If an owned supervisor dies
     unexpectedly, Cabal retains the confirmed PGID only through its immediate
     fail-closed group TERM/KILL attempts rather than treating direct exit as
     proof that descendants are gone. *)
val group_id : t -> int option

(** [await_backend t] waits for the backend child's reported exit status without
    releasing the supervisor. This lets callers wait for stdout/stderr EOF while
    the confirmed process group remains anchored. *)
val await_backend : t -> Eio.Process.exit_status

(** [release t] asks a normally-completed supervisor to exit. A successful
    protocol write atomically transfers ownership to the supervisor only after
    [await_backend t] can resolve. A premature call retains ownership, allowing
    later {!terminate} or caller-switch shutdown to terminate and reap a still
    running backend. After transfer, later or concurrent {!terminate} calls
    become no-ops and [group_id t] is [None]. It is idempotent and does not
    terminate descendants; timeout and cancellation must use {!terminate}
    instead. A failed protocol write remains fail-closed: Cabal clears cached
    group ownership, sends TERM only to the direct owned supervisor, and uses a
    bounded direct-KILL backstop if it does not retire. It never negative-signals
    a PGID after RELEASE write failure; a live official launcher performs its
    own bounded group cleanup. *)
val release : t -> unit

(** [await t] waits for the backend child, releases the supervisor, waits until
    the supervisor is reaped, and returns the backend exit status. Repeated and
    concurrent calls share both phases. *)
val await : t -> Eio.Process.exit_status

(** [awaited_status t] returns the cached direct-supervisor exit status, if it
     has already been reaped. It can become available before public cleanup
     completes for unexpected owned-supervisor death. *)
val awaited_status : t -> Eio.Process.exit_status option

(** [terminate ?grace_seconds ~clock t] is idempotent. It sends a private
    termination request to the owned supervisor (or, only if no group was
    established, [SIGTERM] to the direct process). [grace_seconds] must be
    finite and non-negative; the first termination request's value is conveyed
    to the confirmed supervisor and wins over later requests. Cabal directly
    broadcasts [SIGTERM] to a confirmed group before starting that grace, while
    the queued command preserves the launcher's deadline if it wakes later. The
     supervisor anchors the PGID for that value. If it exits unexpectedly, Cabal
     replaces any normal cleanup claim with catastrophic cleanup, because
     descendants may remain. Before a group is confirmed, an
     official launcher has not yet received permission to fork, so cancellation
     cannot orphan an official backend. Cabal still bounds arbitrary custom
     launchers: it first sends the private request and allows its requested
      grace plus a bounded scheduling margin; if delivery fails, it falls back
      to direct [SIGTERM] and waits the launcher's fixed fallback grace plus that
      margin before direct [SIGKILL]. This can exceed the requested grace.
      Direct signals, malformed control records, and parent loss use the
      launcher's fixed fallback grace. If an explicit direct [SIGKILL] attempt
      fails, logical retirement still stops the protected owner switch; its Eio
      process-resource finalizer supplies a final kill and non-cancellable reap
      instead of leaving the liveness observer or caller switch stranded.
      Public completion and concurrent termination followers resolve only after
      that finalizer has reaped the direct process; malformed or missing backend
      status falls back to this actual reaped status. Already-exited targets are
      tolerated. *)
val terminate : ?grace_seconds:float -> clock:_ Eio.Time.clock -> t -> unit

(** {b Catastrophic supervisor-loss behavior.} Unexpected direct-supervisor
    death is deliberately distinct from normal {!terminate} behavior. Cabal
    atomically takes over the confirmed-group cleanup claim, attempts group TERM
    and then group KILL immediately back-to-back with no Eio sleep or scheduling
    margin, retires the PGID, and resolves concurrent cleanup followers. This
    fail-closed path may forfeit all remaining graceful-shutdown time. A normal
    termination leader that was already waiting for its full grace loses its
    claim and cannot issue a delayed signal after that retirement. *)
