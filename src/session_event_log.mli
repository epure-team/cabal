(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Per-session append-only NDJSON event log for live observability.

    All write functions are best-effort: they swallow exceptions so a log
    failure never blocks an agentic turn or build pipeline.

    All I/O uses [Eio.Path] (no blocking stdlib calls).

    [session_id] values are validated before any file access: only
    [[a-zA-Z0-9_-]] characters are allowed.  Any value failing this check is
    silently ignored on write and causes an empty result on read, preventing
    path traversal. *)

(** [is_safe_session_id s] returns [true] iff [s] is a safe, non-empty
    filename component ([a-zA-Z0-9_-] characters only, no path separators). *)
val is_safe_session_id : string -> bool

(** [write_session_start ~fs ~session_logs_dir ~session_id ~backend ~story_id
    ~agent_role ()] appends a [session_start] lifecycle event to the session
    log.  Best-effort: no exception propagates to the caller. *)
val write_session_start :
  fs:_ Eio.Path.t ->
  session_logs_dir:string ->
  session_id:string ->
  backend:string ->
  story_id:int ->
  agent_role:string ->
  unit ->
  unit

(** [write_session_end ~fs ~session_logs_dir ~session_id ~backend ~status ~cost ()]
    appends a [session_end] event with final status and optional cost summary. *)
val write_session_end :
  fs:_ Eio.Path.t ->
  session_logs_dir:string ->
  session_id:string ->
  backend:string ->
  status:string ->
  cost:Backend_types.cost option ->
  unit ->
  unit

(** [write_turn_start ~fs ~session_logs_dir ~session_id ~backend ~turn_number
    ~agent_role ()] appends a [turn_started] event. *)
val write_turn_start :
  fs:_ Eio.Path.t ->
  session_logs_dir:string ->
  session_id:string ->
  backend:string ->
  turn_number:int ->
  agent_role:string ->
  unit ->
  unit

(** [write_turn_end ~fs ~session_logs_dir ~session_id ~backend ~turn_number
    ~agent_role ~status ~cost ()] appends a [turn_completed] event with
    normalized token usage when available. *)
val write_turn_end :
  fs:_ Eio.Path.t ->
  session_logs_dir:string ->
  session_id:string ->
  backend:string ->
  turn_number:int ->
  agent_role:string ->
  status:string ->
  cost:Backend_types.cost option ->
  unit ->
  unit

(** [write_turn_failed ~fs ~session_logs_dir ~session_id ~backend ~turn_number
    ~agent_role ~error ()] appends a [turn_failed] event. *)
val write_turn_failed :
  fs:_ Eio.Path.t ->
  session_logs_dir:string ->
  session_id:string ->
  backend:string ->
  turn_number:int ->
  agent_role:string ->
  error:string ->
  unit ->
  unit

(** [write_raw_event ~fs ~session_logs_dir ~session_id ~backend ~turn_number line]
    parses [line] as JSON and appends it as a [raw] event.  Non-parseable
    lines are silently dropped.  Called at the stream-json seam for Claude Code
    backend captures (Story #466). *)
val write_raw_event :
  fs:_ Eio.Path.t ->
  session_logs_dir:string ->
  session_id:string ->
  backend:string ->
  turn_number:int ->
  string ->
  unit

(** [list_sessions ~fs ~session_logs_dir ()] returns session IDs (filenames without
    [.ndjson] extension) found in [session_logs_dir].  Returns [[]] on
    missing directory or any error.  Entries failing [is_safe_session_id] are
    filtered out.  Order is descending by filename string. *)
val list_sessions :
  fs:_ Eio.Path.t -> session_logs_dir:string -> unit -> string list

(** [read_events ~fs ~session_logs_dir ~session_id ()] reads all NDJSON lines from
    the session log and returns parsed JSON values.  Returns [[]] when the file
    is absent, unreadable, or [session_id] fails [is_safe_session_id].  Uses
    [Eio.Path.with_open_in] — no separate stat, no TOCTOU. *)
val read_events :
  fs:_ Eio.Path.t ->
  session_logs_dir:string ->
  session_id:string ->
  unit ->
  Yojson.Safe.t list
