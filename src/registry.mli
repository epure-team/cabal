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

    See DESIGN.md Section 6 "Agentic Backend Layer".

    This is process-global startup state for a single OCaml domain. Register
    backends before concurrent task execution; mutation and lookup are not
    synchronized across domains. *)

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

(** [register_from_adapter_loader backend] installs or replaces a YAML runtime
    without emitting replacement diagnostics. This narrow startup primitive is
    reserved for {!Adapter_loader}'s descriptor/runtime pair transaction, after
    descriptor ownership validation has succeeded. *)
val register_from_adapter_loader : Agentic_backend.t -> unit

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

(** Origin of a backend's resolved model list. *)
type models_source =
  | Probe
      (** The backend's runtime probe returned a non-empty list and that list
          is now the source of truth. *)
  | Static
      (** The probe was absent, errored, raised, or returned an empty list, so
          the backend's declared static list is in use. *)
  | Hybrid
      (** Reserved for a future merge of static + live entries.  The current
          implementation never emits this tag; it exists so external code can
          pattern-match the contract without breaking on a future addition. *)

(** [resolved_models backend_id] returns [Some (models, source)] for any
    registered backend, where [source] documents whether the list comes from a
    successful probe ({!Probe}) or from the static declaration ({!Static}).

    {pre}
    (none)

    {post}
    Returns [Some (models, src)] when [backend_id] is registered, or [None]
    otherwise. The [models] list is identical to [Agentic_backend.models b]
    when [src = Static] and contains the probe output otherwise.

    {violators}
    (none)

    {violates}
    (none) *)
val resolved_models : string -> (string list * models_source) option

(** [list_models backend_id] returns [Some models] when [backend_id] is
    registered.  Returns [None] when the backend is not registered.  Returns
    [Some []] when the backend is registered but declares no models and any
    probe also returned no models.

    The list is the {i resolved} view: if {!Adapter_loader.register_all} ran
    a probe successfully, that output is what surfaces here; otherwise the
    static declaration is returned.  Equivalent to
    [Option.map fst (resolved_models backend_id)].

    {pre}
    (none)

    {post}
    Returns [Some models] when [backend_id] is in the registry, or [None]
    otherwise.

    {violators}
    (none)

    {violates}
    (none) *)
val list_models : string -> string list option

(** [set_resolved_models id pair] records the resolved view for [id].

    Intended for the adapter-loader to publish probe outcomes.  Hosts should
    treat this as adapter-loader internal and prefer {!resolved_models} or
    {!list_models} as the query API.

    {pre}
    A backend with [id] should already be registered.

    {post}
    Subsequent calls to {!resolved_models}[ id] return [Some pair].

    {violators}
    (none)

    {violates}
    (none) *)
val set_resolved_models : string -> string list * models_source -> unit

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
