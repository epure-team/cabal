(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** E2E tests for Story #627 — real backend CLIs, manual, CI-excluded.

    This module is a test executable; it exposes no public values.

    {pre}  Run with [CABAL_E2E_TESTS=1 dune runtest libs/cabal/test/] or
           [CABAL_E2E_TESTS=1 dune build @e2e].  Also requires
           [CABAL_E2E_BACKEND] and [CABAL_E2E_MODEL] to be set; otherwise
           every test case is skipped.
    {post} Story #627 acceptance criteria are exercised end-to-end against a
           real backend CLI:
           AC-E1 the enforcer reaches schema compliance via the first attempt
                 or the corrective re-invocation;
           AC-E2 the binary is excluded from CI when [CABAL_E2E_TESTS] is
                 unset ([(enabled_if ...)] stanza);
           AC-E3 a [(alias e2e)] stanza makes [dune build @e2e] discoverable;
           AC-E4 [CABAL_E2E_BACKEND] and [CABAL_E2E_MODEL] select the backend
                 and model at runtime; missing vars cause a documented skip.
    {violators} None — test executable only; no production code is exported.
    {violates}  None. *)
