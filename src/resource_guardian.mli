(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Lightweight Linux memory and CPU monitor.

    Runs as an Eio background fiber, polling [/proc/meminfo] and [/proc/stat].
    Tracks child PIDs spawned by the build system and can SIGTERM/SIGKILL them
    when memory usage exceeds a configurable threshold.

    {2 Usage}

    {[
      let guardian = Resource_guardian.create Resource_guardian.default_config in
      Eio.Fiber.fork ~sw (fun () ->
          Resource_guardian.run guardian ~clock:(Eio.Stdenv.clock env)) ;
      (* ... later, when spawning a child process: *)
      Resource_guardian.register_pid guardian (Eio.Process.pid proc) ;
      (* ... on process exit: *)
      Resource_guardian.unregister_pid guardian (Eio.Process.pid proc)
    ]} *)

(** Guardian configuration. *)
type config = {
  warn_percent : int;  (** Log warning at this memory usage (default 80) *)
  kill_percent : int;  (** Kill children at this threshold (default 90) *)
  poll_interval_s : float;  (** Seconds between checks (default 5.0) *)
}

(** Opaque guardian state. *)
type t

(** Current resource statistics. *)
type stats = {memory_percent : float; cpu_percent : float}

(** Resource usage history (most recent last, up to 60 samples). *)
type stats_history = {memory : float list; cpu : float list}

(** Default configuration: warn at 80%, kill at 90%, poll every 5s.

    {pre}
    (none)

    {post}
    Returns a [config] with [warn_percent = 80], [kill_percent = 90], and
    [poll_interval_s = 5.0].

    {violators}
    (none)

    {violates}
    (none) *)
val default_config : config

(** [create config] creates a new guardian with the given configuration.
    The guardian is inert until {!run} is called.

    {pre}
    [config.kill_percent] must be greater than [config.warn_percent].
    [config.poll_interval_s] must be positive.

    {post}
    Returns a new, inert [t]; no background polling is started until [run]
    is called.

    {violators}
    (none)

    {violates}
    (none) *)
val create : config -> t

(** [register_pid t pid] starts tracking a child PID for potential killing
    under memory pressure.

    {pre}
    [pid] must be a valid, running process ID.

    {post}
    Adds [pid] to the set of tracked PIDs; subsequent memory-pressure events
    may send SIGTERM/SIGKILL to this PID.

    {violators}
    (none)

    {violates}
    (none) *)
val register_pid : t -> int -> unit

(** [unregister_pid t pid] stops tracking a child PID.

    {pre}
    (none)

    {post}
    Removes [pid] from the tracked set; silently succeeds if [pid] was not
    registered.

    {violators}
    (none)

    {violates}
    (none) *)
val unregister_pid : t -> int -> unit

(** [run t ~clock] starts the background polling loop. Reads [/proc/meminfo]
    and [/proc/stat] every [config.poll_interval_s] seconds. Logs warnings at
    [warn_percent] and kills registered children at [kill_percent].
    Runs until the switch is cancelled.

    {pre}
    Must be called inside an [Eio.Fiber.fork] or equivalent; the fiber's
    switch controls the loop lifetime.

    {post}
    Blocks indefinitely, polling resources and killing children as needed;
    returns only when the enclosing switch is cancelled.

    {violators}
    (none)

    {violates}
    (none) *)
val run : t -> clock:Eio.Time.clock -> unit

(** [memory_pressure t] returns [true] if current memory usage exceeds
    [warn_percent]. Can be checked before spawning new processes.

    {pre}
    (none)

    {post}
    Returns [true] if the most recently sampled memory usage is at or above
    [config.warn_percent], [false] otherwise.

    {violators}
    (none)

    {violates}
    (none) *)
val memory_pressure : t -> bool

(** [current_stats t] returns the latest memory and CPU readings.

    {pre}
    (none)

    {post}
    Returns the most recently sampled [stats]; values are 0.0 before the
    first poll completes.

    {violators}
    (none)

    {violates}
    (none) *)
val current_stats : t -> stats

(** [history t] returns the usage history (up to 60 samples).

    {pre}
    (none)

    {post}
    Returns a [stats_history] with up to 60 most-recent memory and CPU
    samples (most recent last); both lists are empty before the first poll.

    {violators}
    (none)

    {violates}
    (none) *)
val history : t -> stats_history
