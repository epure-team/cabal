(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type config = {warn_percent : int; kill_percent : int; poll_interval_s : float}

type stats = {memory_percent : float; cpu_percent : float}

type stats_history = {memory : float list; cpu : float list}

(** Previous CPU sample from /proc/stat for computing deltas. *)
type cpu_sample = {total : int; idle : int}

type t = {
  config : config;
  mutable pids : int list;
  mutable current : stats;
  mutable mem_history : float list;
  mutable cpu_history : float list;
  mutable prev_cpu : cpu_sample option;
  mutable warned : bool;  (** Hysteresis: true while above warn_percent *)
  mutable proc_available : bool option;
      (** None = not checked yet, Some false = /proc unavailable *)
}

let max_history = 60

let default_parallel_max_mem =
  match Sys.getenv_opt "EPURE_VALIDATOR_PARALLEL_MAX_MEM" with
  | Some s -> (
      match int_of_string_opt s with
      | Some n when n > 0 && n <= 100 -> n
      | _ -> 85)
  | None -> 85

let default_config =
  {
    warn_percent = default_parallel_max_mem;
    kill_percent = 90;
    poll_interval_s = 5.0;
  }

let create config =
  {
    config;
    pids = [];
    current = {memory_percent = 0.0; cpu_percent = 0.0};
    mem_history = [];
    cpu_history = [];
    prev_cpu = None;
    warned = false;
    proc_available = None;
  }

let register_pid t pid = t.pids <- pid :: t.pids

let unregister_pid t pid = t.pids <- List.filter (fun p -> p <> pid) t.pids

let registered_pids t = t.pids

let memory_pressure t =
  t.current.memory_percent >= float_of_int t.config.warn_percent

let current_stats t = t.current

let history t = {memory = t.mem_history; cpu = t.cpu_history}

(** Append a value to a history list, keeping at most [max_history] entries. *)
let push_history lst v =
  let lst = lst @ [v] in
  let len = List.length lst in
  if len > max_history then
    (* Drop oldest entries *)
    List.filteri (fun i _ -> i >= len - max_history) lst
  else lst

(** Parse /proc/meminfo and return (total_kb, available_kb).
    Returns None if the file cannot be read or parsed. *)
let read_meminfo () =
  try
    let ic = open_in "/proc/meminfo" in
    let total = ref None in
    let available = ref None in
    (try
       while !total = None || !available = None do
         let line = input_line ic in
         if String.length line > 10 then
           let prefix_9 = String.sub line 0 9 in
           let prefix_13 =
             if String.length line > 14 then String.sub line 0 13 else ""
           in
           if prefix_9 = "MemTotal:" then
             Scanf.sscanf line "MemTotal: %d kB" (fun v -> total := Some v)
           else if prefix_13 = "MemAvailable:" then
             Scanf.sscanf line "MemAvailable: %d kB" (fun v ->
                 available := Some v)
       done
     with
    | End_of_file -> ()
    | Scanf.Scan_failure _ -> ()) ;
    close_in ic ;
    match (!total, !available) with Some t, Some a -> Some (t, a) | _ -> None
  with Sys_error _ -> None

(** [read_meminfo_with_log t] reads /proc/meminfo and logs once if unavailable. *)
let read_meminfo_with_log t =
  match read_meminfo () with
  | Some _ as r ->
      (match t.proc_available with
      | None -> t.proc_available <- Some true
      | _ -> ()) ;
      r
  | None ->
      (match t.proc_available with
      | None | Some true ->
          Diagnostics.warn
            "Resource guardian: /proc/meminfo unavailable, memory monitoring \
             disabled" ;
          t.proc_available <- Some false
      | Some false -> ()) ;
      None

(** Parse /proc/stat first line and return a cpu_sample.
    Format: cpu user nice system idle iowait irq softirq steal guest guest_nice *)
let read_cpu_stat () =
  try
    let ic = open_in "/proc/stat" in
    let line = input_line ic in
    close_in ic ;
    (* Parse "cpu  user nice system idle iowait irq softirq ..." *)
    let parts =
      String.split_on_char ' ' line |> List.filter (fun s -> s <> "")
    in
    match parts with
    | "cpu" :: values ->
        let nums = List.filter_map int_of_string_opt values in
        let total = List.fold_left ( + ) 0 nums in
        (* idle is the 4th value (0-indexed: user=0, nice=1, system=2, idle=3) *)
        let idle = match nums with _ :: _ :: _ :: i :: _ -> i | _ -> 0 in
        Some {total; idle}
    | _ -> None
  with Sys_error _ | End_of_file -> None

(** Compute CPU usage percentage from two samples. *)
let compute_cpu_percent prev curr =
  let total_diff = curr.total - prev.total in
  let idle_diff = curr.idle - prev.idle in
  if total_diff > 0 then
    100.0 *. float_of_int (total_diff - idle_diff) /. float_of_int total_diff
  else 0.0

(** Try to kill a PID with the given signal. Ignores ESRCH (already exited)
    but logs EPERM and other unexpected errors. *)
let try_kill pid signal =
  try Unix.kill pid signal
  with Unix.Unix_error (errno, _, _) ->
    if errno <> Unix.ESRCH then
      Diagnostics.warn
        "Resource guardian: kill(%d, %d) failed: %s"
        pid
        signal
        (Unix.error_message errno)

(** Kill all registered PIDs: SIGTERM first, wait, then SIGKILL survivors. *)
let kill_registered_pids t ~clock =
  let pids = t.pids in
  if pids <> [] then begin
    Diagnostics.error
      "Memory threshold exceeded (%.0f%% >= %d%%), killing %d child processes"
      t.current.memory_percent
      t.config.kill_percent
      (List.length pids) ;
    (* SIGTERM all *)
    List.iter (fun pid -> try_kill pid Sys.sigterm) pids ;
    (* Wait 2 seconds for graceful shutdown *)
    Eio.Time.sleep clock 2.0 ;
    (* SIGKILL survivors *)
    List.iter (fun pid -> try_kill pid Sys.sigkill) pids
  end

let run t ~clock =
  (* Take initial CPU sample so the first poll produces a real delta *)
  (match read_cpu_stat () with Some s -> t.prev_cpu <- Some s | None -> ()) ;
  let poll_once () =
    (* Read memory *)
    let mem_pct =
      match read_meminfo_with_log t with
      | Some (total, available) ->
          if total > 0 then
            100.0 -. (float_of_int available *. 100.0 /. float_of_int total)
          else 0.0
      | None -> t.current.memory_percent
    in
    (* Read CPU *)
    let cpu_sample = read_cpu_stat () in
    let cpu_pct =
      match (t.prev_cpu, cpu_sample) with
      | Some prev, Some curr -> compute_cpu_percent prev curr
      | _ -> t.current.cpu_percent
    in
    (match cpu_sample with Some s -> t.prev_cpu <- Some s | None -> ()) ;
    (* Update state *)
    t.current <- {memory_percent = mem_pct; cpu_percent = cpu_pct} ;
    t.mem_history <- push_history t.mem_history mem_pct ;
    t.cpu_history <- push_history t.cpu_history cpu_pct ;
    (* Check thresholds with hysteresis *)
    if mem_pct >= float_of_int t.config.kill_percent then
      kill_registered_pids t ~clock
    else if mem_pct >= float_of_int t.config.warn_percent then begin
      if not t.warned then begin
        Diagnostics.warn "Memory pressure: %.0f%% used" mem_pct ;
        t.warned <- true
      end
    end
    else if t.warned then
      (* Clear warning state once memory drops below warn threshold *)
      t.warned <- false
  in
  (* Polling loop — runs until the switch is cancelled *)
  while true do
    poll_once () ;
    Eio.Time.sleep clock t.config.poll_interval_s
  done
