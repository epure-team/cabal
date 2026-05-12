(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Two-tier backend system for fast/cheap vs smart/expensive model selection.

    Usage tiers:
    - Fast: Talos chat, quick checks, mechanical validation
    - Smart: Deep reasoning, strategist personas, architecture review

    Tier specs are "backend:model" (e.g., "claude-code:haiku", "codex:gpt-4o-mini"). *)

(** A tier specification: backend name and optional model. *)
type tier_spec = {backend_name : string; model : string option}

(** Parse a tier spec from string like "claude-code:haiku" or "codex".

    {pre}
    [s] must be a non-empty string; the backend name is the portion before the
    first colon (if any).

    {post}
    Returns a [tier_spec] with [backend_name] set to the portion before any
    colon and [model] set to [Some suffix] if a colon is present, [None]
    otherwise.

    {violators}
    (none)

    {violates}
    (none) *)
val parse_tier_spec : string -> tier_spec

(** Convert tier spec back to string.

    {pre}
    (none)

    {post}
    Returns a string of the form ["backend_name"] when [model] is [None], or
    ["backend_name:model"] when [model] is [Some model].

    {violators}
    (none)

    {violates}
    (none) *)
val tier_spec_to_string : tier_spec -> string

(** Default tier configurations. *)
val default_fast : tier_spec

val default_smart : tier_spec

(** Map abstract model names ("fast", "smart", "deep") to backend-specific models.

    {pre}
    (none)

    {post}
    Returns [Some backend_model] when [model] is a known alias for
    [backend_name], or [model] unchanged when no mapping exists.

    {violators}
    (none)

    {violates}
    (none) *)
val map_model_for_backend :
  backend_name:string -> model:string option -> string option

(** Get effective model for a tier spec (with alias mapping).

    {pre}
    (none)

    {post}
    Returns the resolved model string (after alias expansion) for the tier
    spec, or [None] if no model is configured.

    {violators}
    (none)

    {violates}
    (none) *)
val effective_model : tier_spec -> string option

(** Initialize tiers from command-line args or environment.
    Environment: EPURE_BACKEND_FAST, EPURE_BACKEND_SMART

    {pre}
    Must be called before any [get_fast] / [get_smart] / convenience
    accessors are used.

    {post}
    Sets the global fast and smart tier specs; subsequent calls to
    [get_fast] / [get_smart] will reflect the new values.

    {violators}
    (none)

    {violates}
    (none) *)
val init : ?fast:string -> ?smart:string -> unit -> unit

(** Get the current fast tier spec.

    {pre}
    [init] must have been called at least once.

    {post}
    Returns the currently configured fast [tier_spec].

    {violators}
    (none)

    {violates}
    (none) *)
val get_fast : unit -> tier_spec

(** Get the current smart tier spec.

    {pre}
    [init] must have been called at least once.

    {post}
    Returns the currently configured smart [tier_spec].

    {violators}
    (none)

    {violates}
    (none) *)
val get_smart : unit -> tier_spec

(** Convenience: get fast tier backend name.

    {pre}
    [init] must have been called at least once.

    {post}
    Returns the [backend_name] field of the current fast tier spec.

    {violators}
    (none)

    {violates}
    (none) *)
val fast_backend : unit -> string

(** Convenience: get fast tier model (with alias mapping).

    {pre}
    [init] must have been called at least once.

    {post}
    Returns the resolved model for the fast tier after alias expansion, or
    [None] if no model is set.

    {violators}
    (none)

    {violates}
    (none) *)
val fast_model : unit -> string option

(** Convenience: get smart tier backend name.

    {pre}
    [init] must have been called at least once.

    {post}
    Returns the [backend_name] field of the current smart tier spec.

    {violators}
    (none)

    {violates}
    (none) *)
val smart_backend : unit -> string

(** Convenience: get smart tier model (with alias mapping).

    {pre}
    [init] must have been called at least once.

    {post}
    Returns the resolved model for the smart tier after alias expansion, or
    [None] if no model is set.

    {violators}
    (none)

    {violates}
    (none) *)
val smart_model : unit -> string option
