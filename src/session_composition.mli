(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Composition algebra over {!Portable_session} events.

    Composition is how a session is {b continued in a different client}: the
    past events are filtered, deduplicated, reordered, merged across sources,
    and optionally compacted into a curated context that is then rendered and
    re-seeded (see {!Session_render}).  The deterministic transforms here are
    pure and total; model-driven steps (compaction/amplification) are carried
    as a {!Compact} stage holding an injected callback, so this module needs
    no backend dependency and stays fully unit-testable. *)

open Portable_session

(** A pure event-list transform. *)
type transform = event list -> event list

(** A declarative composition step. *)
type stage =
  | Filter of (event -> bool)  (** Keep events satisfying the predicate. *)
  | Dedup  (** Drop later events with a duplicate (role, normalized text). *)
  | Reorder
      (** Stable chronological sort by {!Portable_session.event.timestamp}. *)
  | Take of int  (** Keep the first [n] events (clamped to [0]). *)
  | Drop of int  (** Drop the first [n] events (clamped to [0]). *)
  | Compact of transform
      (** Model-driven step: an injected summarizer/re-ranker.  The algebra
          does not interpret it beyond applying it. *)

(** [filter p] keeps events satisfying [p], preserving order. *)
val filter : (event -> bool) -> transform

(** [dedup] removes every event whose [(role, normalized_text)] pair has
    already appeared earlier in the list, keeping the first occurrence and
    preserving order.

    {pre} (none)
    {post} The result is a sublist of the input with duplicates removed;
    order of surviving events is preserved.
    {violators} (none)
    {violates} (none) *)
val dedup : transform

(** [reorder] stably sorts events by ISO-8601 [timestamp] (lexical order is
    chronological).  Events with no timestamp sort before timestamped ones and
    otherwise keep their relative order.

    {pre} (none)
    {post} Returns a permutation of the input; stable for equal keys.
    {violators} (none)
    {violates} (none) *)
val reorder : transform

(** [take n] keeps the first [max 0 n] events. *)
val take : int -> transform

(** [drop n] drops the first [max 0 n] events. *)
val drop : int -> transform

(** [merge sources] concatenates several event lists into one, preserving each
    event's provenance.  Order is [sources] order then within-source order;
    follow with {!reorder} for a chronological merge.

    {pre} (none)
    {post} Length equals the sum of source lengths; every input event appears
    exactly once with its provenance unchanged.
    {violators} (none)
    {violates} (none) *)
val merge : event list list -> event list

(** [run stages evs] applies each stage to [evs] left-to-right.

    {pre} (none)
    {post} Returns the result of folding the stages over [evs]; an empty stage
    list is the identity.
    {violators} (none)
    {violates} (none) *)
val run : stage list -> event list -> event list
