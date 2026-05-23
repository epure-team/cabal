(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #622 — Extend task_spec and capabilities with JSON schema
    fields.

    Covers:
    - AC1: task_spec carries [json_schema : Yojson.Safe.t option] (default None)
    - AC2: capabilities exposes [native_json_schema_output : bool] and
           [native_json_schema_output_evidence : capability_evidence option]
    - AC3: All initially shipped built-in backends declare
           [native_json_schema_output = false] *)

open Cabal

let all_ids =
  List.map
    (fun (d : Backend_registry.descriptor) -> d.id)
    (Backend_registry.all ())

let find_desc id =
  match Backend_registry.find id with
  | Some d -> d
  | None -> Alcotest.failf "backend descriptor not found for id=%s" id

(** {1 AC1 — task_spec carries json_schema field} *)

let test_task_spec_json_schema_default_none () =
  let spec =
    Backend_types.make_task_spec ~prompt:"test" ~working_dir:"/tmp" ()
  in
  Alcotest.(check bool)
    "json_schema defaults to None"
    true
    (spec.Backend_types.json_schema = None)

let test_task_spec_json_schema_set () =
  let schema = `Assoc [("type", `String "object")] in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"test"
      ~working_dir:"/tmp"
      ~json_schema:schema
      ()
  in
  Alcotest.(check bool)
    "json_schema is propagated"
    true
    (spec.Backend_types.json_schema = Some schema)

let test_task_spec_json_schema_roundtrip () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ("properties", `Assoc [("name", `Assoc [("type", `String "string")])]);
      ]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"roundtrip"
      ~working_dir:"/tmp"
      ~json_schema:schema
      ()
  in
  let json = Backend_types.task_spec_to_yojson spec in
  match Backend_types.task_spec_of_yojson json with
  | Error e ->
      Alcotest.failf "task_spec with json_schema roundtrip failed: %s" e
  | Ok roundtripped ->
      Alcotest.(check bool)
        "json_schema survives JSON round-trip"
        true
        (roundtripped.Backend_types.json_schema = Some schema)

let test_task_spec_legacy_json_without_json_schema () =
  let json =
    `Assoc
      [
        ("prompt", `String "legacy");
        ("instructions", `String "");
        ("mcp_servers", `List []);
        ("working_dir", `String "/tmp/project");
        ("timeout", `Float 1.0);
        ("expected_outputs", `List []);
        ("read_only", `Bool false);
      ]
  in
  match Backend_types.task_spec_of_yojson json with
  | Error e -> Alcotest.failf "legacy task_spec decode failed: %s" e
  | Ok spec ->
      Alcotest.(check bool)
        "json_schema defaults to None when absent from JSON"
        true
        (spec.Backend_types.json_schema = None)

(** {1 AC2 — capabilities exposes native_json_schema_output fields} *)

let test_capabilities_has_native_json_schema_output () =
  let d = find_desc "claude-code" in
  let cap = d.Backend_registry.capabilities in
  let _ : bool = cap.Backend_registry.native_json_schema_output in
  ()

let test_capabilities_has_native_json_schema_output_evidence () =
  let d = find_desc "claude-code" in
  let cap = d.Backend_registry.capabilities in
  let _ : Backend_types.capability_evidence option =
    cap.Backend_registry.native_json_schema_output_evidence
  in
  ()

(** {1 AC3 — All built-in backends declare native_json_schema_output = false} *)

let test_all_backends_native_json_schema_output_false () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ ": native_json_schema_output = false")
        false
        d.Backend_registry.capabilities
          .Backend_registry.native_json_schema_output)
    all_ids

let test_all_backends_native_json_schema_output_evidence_none () =
  List.iter
    (fun id ->
      let d = find_desc id in
      Alcotest.(check bool)
        (id ^ ": native_json_schema_output_evidence = None")
        true
        (d.Backend_registry.capabilities
           .Backend_registry.native_json_schema_output_evidence = None))
    all_ids

(** Structural invariant: every backend with [native_json_schema_output = true]
    must carry [native_json_schema_output_evidence = Some _].

    Currently all built-in backends have [native_json_schema_output = false], so
    this test passes trivially.  It will catch any future story that sets the
    flag to [true] without providing evidence. *)
let test_native_json_schema_evidence_required_when_true () =
  List.iter
    (fun id ->
      let d = find_desc id in
      let cap = d.Backend_registry.capabilities in
      if cap.Backend_registry.native_json_schema_output then
        Alcotest.(check bool)
          (id ^ ": native_json_schema_output = true requires evidence = Some _")
          true
          (cap.Backend_registry.native_json_schema_output_evidence <> None))
    all_ids

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story_622"
    [
      ( "AC1 task_spec json_schema",
        [
          Alcotest.test_case
            "json_schema defaults to None"
            `Quick
            test_task_spec_json_schema_default_none;
          Alcotest.test_case
            "json_schema can be set via make_task_spec"
            `Quick
            test_task_spec_json_schema_set;
          Alcotest.test_case
            "json_schema survives JSON round-trip"
            `Quick
            test_task_spec_json_schema_roundtrip;
          Alcotest.test_case
            "legacy JSON without json_schema decodes with None"
            `Quick
            test_task_spec_legacy_json_without_json_schema;
        ] );
      ( "AC2 capabilities native_json_schema_output",
        [
          Alcotest.test_case
            "capabilities has native_json_schema_output field"
            `Quick
            test_capabilities_has_native_json_schema_output;
          Alcotest.test_case
            "capabilities has native_json_schema_output_evidence field"
            `Quick
            test_capabilities_has_native_json_schema_output_evidence;
        ] );
      ( "AC3 all built-in backends declare native_json_schema_output = false",
        [
          Alcotest.test_case
            "native_json_schema_output = false for all backends"
            `Quick
            test_all_backends_native_json_schema_output_false;
          Alcotest.test_case
            "native_json_schema_output_evidence = None for all backends"
            `Quick
            test_all_backends_native_json_schema_output_evidence_none;
          Alcotest.test_case
            "native_json_schema_output = true requires evidence = Some _"
            `Quick
            test_native_json_schema_evidence_required_when_true;
        ] );
    ]
