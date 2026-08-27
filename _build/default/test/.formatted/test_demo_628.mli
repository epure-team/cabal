(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Structural CI tests for Story #628 — Capability evidence record and
    verification gates for [native_json_schema_output].

    This module is a test executable; it exposes no public values.

    {pre}  Run with [dune runtest libs/cabal/test/] (no env-var gate; always
           compiled and executed in CI).
    {post} Story #628 acceptance criteria are verified structurally against
           [Backend_registry.all ()]:
           AC1  [capability_evidence] has [tested_at_version],
                [json_schema_draft], and [test_method] fields.
           AC2  [test_method] has [E2e_test] and [Manual_probe of string]
                constructors.
           AC3  Every backend with [native_json_schema_output = true] carries
                [native_json_schema_output_evidence = Some _].
           AC4  Evidence fields are non-empty; [Manual_probe] payload
                documents the exact invocation used.
    {violators} None — test executable only; no production code is exported.
    {violates}  None. *)
