(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Backend-owned diagnostics surface.

    Backend modules use this API for diagnostic logs and human-facing warnings
    without depending on Épure application libraries. Applications embedding the
    backend may install a handler that bridges events into their own logging or
    UI surface.
*)

(** Diagnostic severity for backend log events. *)
type level = Debug | Info | Warn | Error

(** Rendered diagnostic event delivered to the installed handler. *)
type event = Log of level * string | User_warning of string

(** Callback that consumes rendered backend diagnostic events. *)
type handler = event -> unit

(** Install a diagnostics handler.

    {pre}
    The handler must not raise for normal diagnostic input.

    {post}
    Subsequent diagnostics are delivered to [handler] as rendered strings.

    {violators}
    (none)

    {violates}
    (none) *)
val set_handler : handler -> unit

(** Restore the standalone backend default handler.

    {pre}
    (none)

    {post}
    Debug/info logs are ignored; warnings/errors and user warnings are written
    to stderr.

    {violators}
    (none)

    {violates}
    (none) *)
val reset_handler : unit -> unit

(** Emit a formatted backend log event at [level].

    {pre}
    The format string and arguments must be valid for [Printf.ksprintf].

    {post}
    The active handler receives [Log (level, rendered_message)].

    {violators}
    (none)

    {violates}
    (none) *)
val log : level -> ('a, unit, string, unit) format4 -> 'a

(** Emit a formatted debug log event.

    {pre}
    The format string and arguments must be valid for [Printf.ksprintf].

    {post}
    The active handler receives a [Debug] log event.

    {violators}
    (none)

    {violates}
    (none) *)
val debug : ('a, unit, string, unit) format4 -> 'a

(** Emit a formatted info log event.

    {pre}
    The format string and arguments must be valid for [Printf.ksprintf].

    {post}
    The active handler receives an [Info] log event.

    {violators}
    (none)

    {violates}
    (none) *)
val info : ('a, unit, string, unit) format4 -> 'a

(** Emit a formatted warning log event.

    {pre}
    The format string and arguments must be valid for [Printf.ksprintf].

    {post}
    The active handler receives a [Warn] log event.

    {violators}
    (none)

    {violates}
    (none) *)
val warn : ('a, unit, string, unit) format4 -> 'a

(** Emit a formatted error log event.

    {pre}
    The format string and arguments must be valid for [Printf.ksprintf].

    {post}
    The active handler receives an [Error] log event.

    {violators}
    (none)

    {violates}
    (none) *)
val error : ('a, unit, string, unit) format4 -> 'a

(** Emit a formatted human-facing backend warning.

    {pre}
    The format string and arguments must be valid for [Printf.ksprintf].

    {post}
    The active handler receives [User_warning rendered_message].

    {violators}
    (none)

    {violates}
    (none) *)
val user_warning : ('a, unit, string, unit) format4 -> 'a
