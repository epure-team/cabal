(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the dynamic-probe layer added on top of static model enumeration.

    This module is a test executable; it exposes no public values.

    {pre}  Run via [dune runtest libs/cabal/test/].
    {post} Exercises probe success/failure modes, cache and accessor contract,
           and built-in adapter resolution via [Adapter_loader.register_all].
           All test cases must pass.
    {violators} None — test executable only.
    {violates}  None. *)
