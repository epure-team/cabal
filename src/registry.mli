(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Backend registry for managing agentic backends.

    The registry provides a central place to register, discover, and access
    agentic backends. Backends register themselves at startup, and the
    orchestrator queries the registry to find available backends.

    See DESIGN.md Section 6 "Agentic Backend Layer". *)

(** {1 Registration} *)

(** [register backend] adds a backend to the registry. If a backend with
    the same ID already exists, it is replaced (with a warning).

    {pre}
    (none)

    {post}
    Adds [backend] to the global registry; replaces any existing backend
    with the same ID and logs a warning.

    {violators}
    (none)

    {violates}
    (none) *)
val register : Agentic_backend.t -> unit

(** {1 Lookup} *)

(** [get id] returns the backend with the given ID, or [None] if not found.

    {pre}
    (none)

    {post}
    Returns [Some backend] if a backend with [id] is registered, [None]
    otherwise.

    {violators}
    (none)

    {violates}
    (none) *)
val get : string -> Agentic_backend.t option

(** [get_exn id] returns the backend with the given ID.
    @raise Not_found if no backend with that ID is registered.

    {pre}
    A backend with [id] must be registered.

    {post}
    Returns the registered backend for [id].

    {violators}
    (none)

    {violates}
    (none) *)
val get_exn : string -> Agentic_backend.t

(** [list ()] returns all registered backends.

    {pre}
    (none)

    {post}
    Returns a list of all currently registered backends in registration
    order.

    {violators}
    (none)

    {violates}
    (none) *)
val list : unit -> Agentic_backend.t list

(** [list_ids ()] returns the IDs of all registered backends.

    {pre}
    (none)

    {post}
    Returns a list of backend ID strings in registration order.

    {violators}
    (none)

    {violates}
    (none) *)
val list_ids : unit -> string list

(** {1 Availability} *)

(** [available ~sw ~env] returns all backends that are currently available
    (installed, configured, etc.). This checks each registered backend.

    {pre}
    [sw] must be active.

    {post}
    Returns the subset of registered backends that report themselves as
    available (e.g., CLI tool found in PATH).

    {violators}
    (none)

    {violates}
    (none) *)
val available :
  sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> Agentic_backend.t list

(** [first_available ~sw ~env] returns the first available backend, or [None]
    if no backends are available. Useful for auto-selection.

    {pre}
    [sw] must be active.

    {post}
    Returns [Some backend] for the first registered backend that is
    available, or [None] if none are available.

    {violators}
    (none)

    {violates}
    (none) *)
val first_available :
  sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> Agentic_backend.t option

(** {1 Testing Support} *)

(** [clear ()] removes all registered backends. For testing only.

    {pre}
    (none)

    {post}
    Empties the global registry; subsequent calls to [list] return [[]].

    {violators}
    (none)

    {violates}
    (none) *)
val clear : unit -> unit
