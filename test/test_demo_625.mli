(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #625 — Native JSON schema wiring for the first supporting
    backend.

    This module is a test executable; it exposes no public values.

    {pre}  Run via [dune runtest libs/cabal/test/].
    {post} All Story #625 acceptance criteria are exercised:
           AC-N1 native path skips validate-and-retry;
           AC-N2 native backend failure returns Error immediately (fail-fast);
           AC-N3 pass-through unchanged when [json_schema = None];
           AC-N4 session_id propagated from the native result;
           AC-N5 [json_schema = Some _] is preserved in the spec passed to
           the native backend.
    {violators} None — test executable only.
    {violates}  None. *)
