(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** tmux-backed live agentic CLI sessions.

    Agentic CLIs behave differently in their interactive TUI than in
    [--print]/headless mode (plan mode, permission prompts, todos, slash
    commands, different system prompts).  To exercise the real behaviour and to
    let a human attach and take over, this module keeps a CLI alive inside a
    detached tmux session and drives it {b coarsely}: it injects whole turns and
    captures the pane, but does not finely parse the TUI.  Structured signal is
    expected to come from the client's own on-disk transcript (see
    {!Session_ingest}), not from screen scraping.

    The driver is intentionally minimal — open, send a turn, capture, list,
    close — and each running session is human-attachable via [tmux attach -t]
    {!target}. *)

(** A handle to a tmux-backed session (identified by its tmux session name). *)
type t

(** [of_name name] is a handle to the tmux session called [name], whether or
    not it currently exists.  Use it to address a session opened elsewhere
    (e.g. by a previous process); pair with {!has_session} to check liveness. *)
val of_name : string -> t

(** {1 Pure command builders}

    Exposed for testing and for callers that want to drive tmux themselves.
    None of these run anything. *)

(** [new_session_argv ~name ?size ?working_dir cmd] builds the argv for a
    detached tmux session named [name] running [cmd].  [size] forces the pane
    to [(width, height)] (headless has no real terminal size); [working_dir]
    sets the start directory. *)
val new_session_argv :
  name:string ->
  ?size:int * int ->
  ?working_dir:string ->
  string ->
  string list

(** [set_buffer_argv ~name text] loads [text] into a named tmux buffer.  Using
    a named buffer + {!paste_buffer_argv} injects arbitrary multi-line text
    without per-line submission or key-name reinterpretation. *)
val set_buffer_argv : name:string -> string -> string list

(** [paste_buffer_argv ~name] pastes the named buffer into session [name] and
    deletes the buffer afterwards. *)
val paste_buffer_argv : name:string -> string list

(** [enter_argv ~name] sends a single [Enter] key to session [name] (submits
    the pasted turn). *)
val enter_argv : name:string -> string list

(** [capture_argv ~name] builds the argv that prints session [name]'s current
    pane as plain text. *)
val capture_argv : name:string -> string list

(** [has_session_argv ~name] builds the argv that exits 0 iff session [name]
    exists. *)
val has_session_argv : name:string -> string list

(** [kill_session_argv ~name] builds the argv that kills session [name]. *)
val kill_session_argv : name:string -> string list

(** [list_sessions_argv ()] builds the argv that lists session names, one per
    line. *)
val list_sessions_argv : unit -> string list

(** {1 Effectful operations} *)

(** [target t] is the tmux session name; a human attaches with
    [tmux attach -t <target t>]. *)
val target : t -> string

(** [open_ ~env ?size ?working_dir ~name cmd] starts a detached tmux session
    running [cmd] and returns its handle.  Best-effort: if tmux fails the
    handle is still returned and {!has_session} will report [false].

    {pre} tmux must be installed for the session to actually start.
    {post} Returns a handle whose {!target} is [name].
    {violators} (none)
    {violates} (none) *)
val open_ :
  env:Eio_unix.Stdenv.base ->
  ?size:int * int ->
  ?working_dir:string ->
  name:string ->
  string ->
  t

(** [send ~env t text] injects [text] as one turn (buffer + paste + Enter).
    Handles arbitrary multi-line text.

    {pre} (none)
    {post} The text is submitted to the session's pane; no-op effect if the
    session does not exist.
    {violators} (none)
    {violates} (none) *)
val send : env:Eio_unix.Stdenv.base -> t -> string -> unit

(** [capture ~env t] returns the current pane contents as plain text, or [""]
    if the session does not exist.

    {pre} (none)
    {post} Returns pane text on success, [""] on any failure.
    {violators} (none)
    {violates} (none) *)
val capture : env:Eio_unix.Stdenv.base -> t -> string

(** [has_session ~env t] returns whether the tmux session exists.

    {pre} (none)
    {post} [true] iff tmux reports the session present; [false] on any error.
    {violators} (none)
    {violates} (none) *)
val has_session : env:Eio_unix.Stdenv.base -> t -> bool

(** [close ~env t] kills the tmux session (no-op if already gone).

    {pre} (none)
    {post} The session is terminated; never raises.
    {violators} (none)
    {violates} (none) *)
val close : env:Eio_unix.Stdenv.base -> t -> unit

(** [list ~env ()] returns the names of all live tmux sessions, or [[]] if no
    tmux server is running.

    {pre} (none)
    {post} Returns one entry per live session; [[]] on any error.
    {violators} (none)
    {violates} (none) *)
val list : env:Eio_unix.Stdenv.base -> unit -> string list
