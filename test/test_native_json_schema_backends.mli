(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Generic native-path E2E tests for Story #628 — capability evidence record
    and verification gates for [native_json_schema_output].

    This module is a test executable; it exposes no public values.

    {b Gate}: built and executed only when [CABAL_E2E_TESTS=1].

    {pre}  Run with
           [CABAL_E2E_TESTS=1 CABAL_E2E_MODEL=haiku dune runtest libs/cabal/test/]
           or [CABAL_E2E_TESTS=1 CABAL_E2E_MODEL=haiku dune build @e2e].
    {post} For every backend in [Backend_registry.all ()] with
           [native_json_schema_output = true]:
           (1) If the required credential env var is absent, the backend is
               skipped with a diagnostic naming the missing var.
           (2) If the credential is present, the native path is exercised via
               [Json_schema_enforcer.run_task] against a small model.
           (3) Version-drift detection is performed (advisory only):
               installed version < baseline → warning;
               baseline ≤ installed ≤ tested_at_version → no message;
               installed > tested_at_version → debug log.
    {violators} None — test executable only; no production code is exported.
    {violates}  None. *)
