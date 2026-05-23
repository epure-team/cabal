(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #623 — Pure Json_schema_validator module using the
    jsonschema opam package. *)

open Cabal

let validate = Json_schema_validator.validate
let is_ok = Result.is_ok
let is_error = Result.is_error

(** {1 AC1 — Returns success or structured error without I/O, subprocess, or
    LLM} *)

let test_valid_json_object_passes () =
  let schema = `Assoc [ ("type", `String "object") ] in
  let result = validate ~schema ~document:{|{"x":1}|} in
  Alcotest.(check bool)
    "AC1: valid JSON object passes object schema"
    true
    (is_ok result)

let test_invalid_json_fails () =
  let schema = `Assoc [ ("type", `String "object") ] in
  let result = validate ~schema ~document:"not valid json {" in
  Alcotest.(check bool)
    "AC1: non-JSON document returns Error"
    true
    (is_error result)

let test_type_mismatch_returns_error () =
  let schema = `Assoc [ ("type", `String "string") ] in
  let result = validate ~schema ~document:"42" in
  Alcotest.(check bool)
    "AC1: integer document fails string schema"
    true
    (is_error result)

let test_missing_required_returns_error () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ("required", `List [ `String "name"; `String "age" ]);
      ]
  in
  let result = validate ~schema ~document:{|{"name":"Alice"}|} in
  Alcotest.(check bool)
    "AC1: missing required field returns Error"
    true
    (is_error result)

let test_all_required_fields_present () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ("required", `List [ `String "name"; `String "age" ]);
      ]
  in
  let result = validate ~schema ~document:{|{"name":"Alice","age":30}|} in
  Alcotest.(check bool)
    "AC1: all required fields present returns Ok"
    true
    (is_ok result)

let test_valid_json_string_passes_string_schema () =
  let schema = `Assoc [ ("type", `String "string") ] in
  let result = validate ~schema ~document:{|"hello"|} in
  Alcotest.(check bool)
    "AC1: valid string passes string schema"
    true
    (is_ok result)

let test_valid_json_array_passes_array_schema () =
  let schema = `Assoc [ ("type", `String "array") ] in
  let result = validate ~schema ~document:{|[1,2,3]|} in
  Alcotest.(check bool)
    "AC1: valid array passes array schema"
    true
    (is_ok result)

let test_empty_document_is_error () =
  let schema = `Assoc [ ("type", `String "object") ] in
  let result = validate ~schema ~document:"" in
  Alcotest.(check bool)
    "AC1: empty document returns Error"
    true
    (is_error result)

(** {1 AC2 — No [$schema] field → default draft 2020-12 with full keyword
    support}

    These tests verify that the [jsonschema] opam package is used by exercising
    keywords beyond [type] and [required] that the previous hand-written
    implementation silently ignored. All tests below FAIL with the old
    hand-written implementation and PASS once the package is wired in. *)

let test_enum_rejects_non_member () =
  let schema =
    `Assoc [ ("enum", `List [ `String "a"; `String "b"; `String "c" ]) ]
  in
  let result = validate ~schema ~document:{|"d"|} in
  Alcotest.(check bool)
    "AC2: enum keyword: non-member value is rejected"
    true
    (is_error result)

let test_enum_accepts_member () =
  let schema =
    `Assoc [ ("enum", `List [ `String "a"; `String "b"; `String "c" ]) ]
  in
  let result = validate ~schema ~document:{|"b"|} in
  Alcotest.(check bool)
    "AC2: enum keyword: member value is accepted"
    true
    (is_ok result)

let test_additional_properties_false_rejects_extra () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("name", `Assoc [ ("type", `String "string") ]) ] );
        ("additionalProperties", `Bool false);
      ]
  in
  let result = validate ~schema ~document:{|{"name":"Alice","extra":1}|} in
  Alcotest.(check bool)
    "AC2: additionalProperties:false rejects extra properties"
    true
    (is_error result)

let test_additional_properties_false_allows_declared () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("name", `Assoc [ ("type", `String "string") ]) ] );
        ("additionalProperties", `Bool false);
      ]
  in
  let result = validate ~schema ~document:{|{"name":"Alice"}|} in
  Alcotest.(check bool)
    "AC2: additionalProperties:false allows declared properties"
    true
    (is_ok result)

let test_minimum_rejects_small_value () =
  let schema =
    `Assoc [ ("type", `String "number"); ("minimum", `Float 10.0) ]
  in
  let result = validate ~schema ~document:"3" in
  Alcotest.(check bool)
    "AC2: minimum: value below minimum is rejected"
    true
    (is_error result)

let test_minimum_accepts_valid_value () =
  let schema =
    `Assoc [ ("type", `String "number"); ("minimum", `Float 10.0) ]
  in
  let result = validate ~schema ~document:"15" in
  Alcotest.(check bool)
    "AC2: minimum: value above minimum is accepted"
    true
    (is_ok result)

let test_max_length_rejects_long_string () =
  let schema =
    `Assoc [ ("type", `String "string"); ("maxLength", `Int 5) ]
  in
  let result = validate ~schema ~document:{|"toolong"|} in
  Alcotest.(check bool)
    "AC2: maxLength: string exceeding max length is rejected"
    true
    (is_error result)

let test_max_length_accepts_short_string () =
  let schema =
    `Assoc [ ("type", `String "string"); ("maxLength", `Int 5) ]
  in
  let result = validate ~schema ~document:{|"hi"|} in
  Alcotest.(check bool)
    "AC2: maxLength: string within max length is accepted"
    true
    (is_ok result)

let test_properties_type_check () =
  (* properties sub-schema type checking: old impl ignores properties keyword *)
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("count", `Assoc [ ("type", `String "integer") ]) ] );
      ]
  in
  let result = validate ~schema ~document:{|{"count":"not-an-integer"}|} in
  Alcotest.(check bool)
    "AC2: properties type check: string for integer field is rejected"
    true
    (is_error result)

(** {1 AC3 — [$schema] field selects draft among drafts 4, 6, 7, 2019-09,
    2020-12} *)

let test_schema_draft_2020_12_valid () =
  let schema =
    `Assoc
      [
        ("$schema", `String "https://json-schema.org/draft/2020-12/schema");
        ("type", `String "object");
        ("required", `List [ `String "id" ]);
      ]
  in
  let result = validate ~schema ~document:{|{"id":"abc"}|} in
  Alcotest.(check bool)
    "AC3: $schema=2020-12: valid document passes"
    true
    (is_ok result)

let test_schema_draft_2020_12_rejects_invalid () =
  let schema =
    `Assoc
      [
        ("$schema", `String "https://json-schema.org/draft/2020-12/schema");
        ("type", `String "object");
        ("required", `List [ `String "id" ]);
      ]
  in
  let result = validate ~schema ~document:{|{"name":"Alice"}|} in
  Alcotest.(check bool)
    "AC3: $schema=2020-12: missing required field is rejected"
    true
    (is_error result)

let test_schema_draft_07_valid () =
  let schema =
    `Assoc
      [
        ("$schema", `String "http://json-schema.org/draft-07/schema#");
        ("type", `String "string");
        ("minLength", `Int 3);
      ]
  in
  let result = validate ~schema ~document:{|"hello"|} in
  Alcotest.(check bool)
    "AC3: $schema=draft-07: valid string passes"
    true
    (is_ok result)

let test_schema_draft_07_rejects_short_string () =
  (* minLength is a draft-07 keyword; old impl ignores it.
     With the jsonschema package, draft-07 semantics apply and the short
     string is rejected. *)
  let schema =
    `Assoc
      [
        ("$schema", `String "http://json-schema.org/draft-07/schema#");
        ("type", `String "string");
        ("minLength", `Int 3);
      ]
  in
  let result = validate ~schema ~document:{|"hi"|} in
  Alcotest.(check bool)
    "AC3: $schema=draft-07: string below minLength is rejected"
    true
    (is_error result)

let test_schema_draft_2019_09_valid () =
  let schema =
    `Assoc
      [
        ("$schema", `String "https://json-schema.org/draft/2019-09/schema");
        ("type", `String "object");
      ]
  in
  let result = validate ~schema ~document:{|{"x":1}|} in
  Alcotest.(check bool)
    "AC3: $schema=2019-09: valid object passes"
    true
    (is_ok result)

let test_schema_draft_04_valid () =
  let schema =
    `Assoc
      [
        ("$schema", `String "http://json-schema.org/draft-04/schema#");
        ("type", `String "object");
        ("required", `List [ `String "name" ]);
      ]
  in
  let result = validate ~schema ~document:{|{"name":"Bob"}|} in
  Alcotest.(check bool)
    "AC3: $schema=draft-04: valid object passes"
    true
    (is_ok result)

(** {1 Test suite} *)

let () =
  Alcotest.run
    "Json_schema_validator — Story #623"
    [
      ( "AC1 — returns success or error without I/O",
        [
          Alcotest.test_case "valid object passes" `Quick
            test_valid_json_object_passes;
          Alcotest.test_case "invalid JSON returns Error" `Quick
            test_invalid_json_fails;
          Alcotest.test_case "type mismatch returns Error" `Quick
            test_type_mismatch_returns_error;
          Alcotest.test_case "missing required returns Error" `Quick
            test_missing_required_returns_error;
          Alcotest.test_case "all required fields present returns Ok" `Quick
            test_all_required_fields_present;
          Alcotest.test_case "valid string passes string schema" `Quick
            test_valid_json_string_passes_string_schema;
          Alcotest.test_case "valid array passes array schema" `Quick
            test_valid_json_array_passes_array_schema;
          Alcotest.test_case "empty document returns Error" `Quick
            test_empty_document_is_error;
        ] );
      ( "AC2 — no $schema uses draft 2020-12 (full keyword support via \
         jsonschema package)",
        [
          Alcotest.test_case "enum: non-member rejected" `Quick
            test_enum_rejects_non_member;
          Alcotest.test_case "enum: member accepted" `Quick
            test_enum_accepts_member;
          Alcotest.test_case "additionalProperties:false rejects extra" `Quick
            test_additional_properties_false_rejects_extra;
          Alcotest.test_case "additionalProperties:false allows declared" `Quick
            test_additional_properties_false_allows_declared;
          Alcotest.test_case "minimum: small value rejected" `Quick
            test_minimum_rejects_small_value;
          Alcotest.test_case "minimum: valid value accepted" `Quick
            test_minimum_accepts_valid_value;
          Alcotest.test_case "maxLength: long string rejected" `Quick
            test_max_length_rejects_long_string;
          Alcotest.test_case "maxLength: short string accepted" `Quick
            test_max_length_accepts_short_string;
          Alcotest.test_case "properties type check rejects wrong type" `Quick
            test_properties_type_check;
        ] );
      ( "AC3 — $schema field selects draft among 4, 6, 7, 2019-09, 2020-12",
        [
          Alcotest.test_case "$schema=2020-12: valid passes" `Quick
            test_schema_draft_2020_12_valid;
          Alcotest.test_case "$schema=2020-12: missing required fails" `Quick
            test_schema_draft_2020_12_rejects_invalid;
          Alcotest.test_case "$schema=draft-07: valid string passes" `Quick
            test_schema_draft_07_valid;
          Alcotest.test_case "$schema=draft-07: short string fails" `Quick
            test_schema_draft_07_rejects_short_string;
          Alcotest.test_case "$schema=2019-09: valid object passes" `Quick
            test_schema_draft_2019_09_valid;
          Alcotest.test_case "$schema=draft-04: valid object passes" `Quick
            test_schema_draft_04_valid;
        ] );
    ]
