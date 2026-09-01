(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** One absolute monotonic deadline for a complete task and all retries. *)

type t

type error = Invalid_timeout

(** Start a deadline. Negative and NaN values are invalid. Values too large for
    safe monotonic-span arithmetic, including [max_float], are unbounded. *)
val create : _ Eio.Time.Mono.t -> float -> (t, error) result

(** Monotonic elapsed seconds since creation. *)
val elapsed : t -> float

(** Non-negative remaining seconds, or [None] when unbounded. *)
val remaining : t -> float option

(** Run under the single timeout established at creation. *)
val run : t -> (unit -> 'a) -> [ `Completed of 'a | `Timeout ]
