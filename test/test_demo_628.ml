(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #628 — Capability evidence record and verification gates
    for native_json_schema_output.

    Covers:
    - AC1: [capability_evidence] has exactly three fields:
           [tested_at_version], [json_schema_draft], [test_method].
    - AC2: [test_method] is a variant type with constructors [E2e_test] and
           [Manual_probe of string], both defined in [Backend_types].
    - AC3: Structural CI gate — every backend in [Backend_registry.all ()] that
           declares [native_json_schema_output = true] carries a non-[None]
           [native_json_schema_output_evidence] record.
    - AC4: For each such backend the evidence record fields are valid:
           [tested_at_version] and [json_schema_draft] are non-empty strings,
           and when [test_method = Manual_probe s] the string [s] is non-empty. *)

open Cabal

(** {1 AC1 & AC2 — capability_evidence type shape and test_method variant}

    These are compile-time checks: if [Backend_types.test_method] or its
    constructors are missing, or if [capability_evidence] has different fields,
    this module fails to compile and CI fails immediately. *)

let test_test_method_e2e_test () =
  let (_ : Backend_types.test_method) = Backend_types.E2e_test in
  ()

let test_test_method_manual_probe () =
  let (_ : Backend_types.test_method) =
    Backend_types.Manual_probe "claude --version 2>/dev/null | head -1"
  in
  ()

let test_capability_evidence_tested_at_version () =
  let ev : Backend_types.capability_evidence =
    {
      Backend_types.tested_at_version = "2.1.117";
      json_schema_draft = "2020-12";
      test_method = Backend_types.E2e_test;
    }
  in
  Alcotest.(check string)
    "tested_at_version field accessible"
    "2.1.117"
    ev.Backend_types.tested_at_version

let test_capability_evidence_json_schema_draft () =
  let ev : Backend_types.capability_evidence =
    {
      Backend_types.tested_at_version = "2.1.117";
      json_schema_draft = "2020-12";
      test_method = Backend_types.E2e_test;
    }
  in
  Alcotest.(check string)
    "json_schema_draft field accessible"
    "2020-12"
    ev.Backend_types.json_schema_draft

let test_capability_evidence_test_method_e2e () =
  let ev : Backend_types.capability_evidence =
    {
      Backend_types.tested_at_version = "2.1.117";
      json_schema_draft = "2020-12";
      test_method = Backend_types.E2e_test;
    }
  in
  Alcotest.(check bool)
    "test_method = E2e_test"
    true
    (match ev.Backend_types.test_method with
    | Backend_types.E2e_test -> true
    | Backend_types.Manual_probe _ -> false)

let test_capability_evidence_test_method_manual_probe () =
  let probe_invocation = "claude --version 2>/dev/null | head -1  # verified 2.1.117" in
  let ev : Backend_types.capability_evidence =
    {
      Backend_types.tested_at_version = "2.1.117";
      json_schema_draft = "2020-12";
      test_method = Backend_types.Manual_probe probe_invocation;
    }
  in
  match ev.Backend_types.test_method with
  | Backend_types.E2e_test -> Alcotest.fail "expected Manual_probe"
  | Backend_types.Manual_probe s ->
      Alcotest.(check string) "Manual_probe payload" probe_invocation s

(** {1 AC3 — Structural gate: every native backend carries evidence = Some _}

    This is the machine-checkable CI paper trail required by Story #628.
    Adding [native_json_schema_output = true] to a descriptor without a
    co-located [Some _] evidence record causes this test to fail and blocks
    the story from shipping. *)

let test_every_native_backend_has_evidence () =
  List.iter
    (fun (d : Backend_registry.descriptor) ->
      if d.capabilities.native_json_schema_output then
        Alcotest.(check bool)
          (d.id
         ^ ": native_json_schema_output = true requires \
            native_json_schema_output_evidence = Some _")
          true
          (d.capabilities.native_json_schema_output_evidence <> None))
    (Backend_registry.all ())

(** {1 AC4 — Evidence record field validity for all native backends}

    Every field must be non-empty; [Manual_probe] payload must document an
    exact invocation so it is not just a boilerplate placeholder. *)

let test_native_backend_evidence_fields_valid () =
  List.iter
    (fun (d : Backend_registry.descriptor) ->
      let cap = d.capabilities in
      if cap.native_json_schema_output then
        match cap.native_json_schema_output_evidence with
        | None ->
            Alcotest.failf "%s: native backend has None evidence (AC3 gate)" d.id
        | Some ev ->
            Alcotest.(check bool)
              (d.id ^ ": tested_at_version must be non-empty")
              true
              (ev.Backend_types.tested_at_version <> "") ;
            Alcotest.(check bool)
              (d.id ^ ": json_schema_draft must be non-empty")
              true
              (ev.Backend_types.json_schema_draft <> "") ;
            (match ev.Backend_types.test_method with
            | Backend_types.E2e_test -> ()
            | Backend_types.Manual_probe s ->
                Alcotest.(check bool)
                  (d.id ^ ": Manual_probe string must be non-empty")
                  true
                  (s <> "")))
    (Backend_registry.all ())

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story_628"
    [
      ( "AC1 & AC2 — capability_evidence type shape and test_method variant",
        [
          Alcotest.test_case
            "E2e_test constructor exists in test_method"
            `Quick
            test_test_method_e2e_test;
          Alcotest.test_case
            "Manual_probe constructor exists in test_method"
            `Quick
            test_test_method_manual_probe;
          Alcotest.test_case
            "capability_evidence has tested_at_version field"
            `Quick
            test_capability_evidence_tested_at_version;
          Alcotest.test_case
            "capability_evidence has json_schema_draft field"
            `Quick
            test_capability_evidence_json_schema_draft;
          Alcotest.test_case
            "capability_evidence test_method = E2e_test"
            `Quick
            test_capability_evidence_test_method_e2e;
          Alcotest.test_case
            "capability_evidence test_method = Manual_probe of string"
            `Quick
            test_capability_evidence_test_method_manual_probe;
        ] );
      ( "AC3 — structural gate: evidence = Some _ for all native backends",
        [
          Alcotest.test_case
            "every native_json_schema_output=true backend has evidence = Some _"
            `Quick
            test_every_native_backend_has_evidence;
        ] );
      ( "AC4 — evidence record field validity",
        [
          Alcotest.test_case
            "native backend evidence fields are valid and non-empty"
            `Quick
            test_native_backend_evidence_fields_valid;
        ] );
    ]
