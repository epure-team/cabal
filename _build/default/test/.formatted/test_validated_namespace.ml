(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for [Backend_types.validated_namespace] / [validate_namespace].

    These are smoke tests for the type-level enforcement contract: the only
    way to obtain a [validated_namespace] is through [validate_namespace], and
    the smart constructor rejects the same inputs as the unit-returning
    [validate_managed_namespace]. *)

open Cabal.Backend_types

let test_default_namespace_validates () =
  match validate_namespace default_managed_namespace with
  | Ok v ->
      (* Round-trip: coerce back to managed_namespace and confirm fields match. *)
      let m = (v :> managed_namespace) in
      Alcotest.(check string) "id" default_managed_namespace.id m.id ;
      Alcotest.(check string)
        "display_name"
        default_managed_namespace.display_name
        m.display_name
  | Error e -> Alcotest.failf "default namespace must validate, got: %s" e

let test_validate_rejects_bad_id () =
  let bad = {default_managed_namespace with id = "Has Spaces!"} in
  match validate_namespace bad with
  | Ok _ -> Alcotest.fail "expected Error on invalid id"
  | Error _ -> ()

let test_validate_rejects_traversal_in_config_dir () =
  let bad = {default_managed_namespace with config_dir = "../etc/secrets"} in
  match validate_namespace bad with
  | Ok _ -> Alcotest.fail "expected Error on '..' segment"
  | Error _ -> ()

let test_validate_rejects_empty_display_name () =
  let bad = {default_managed_namespace with display_name = "  "} in
  match validate_namespace bad with
  | Ok _ -> Alcotest.fail "expected Error on empty display_name"
  | Error _ -> ()

(* Compile-time contract: code below would fail to type-check, demonstrating
   that the private alias blocks direct record construction. We keep this
   as a comment so the test file stays buildable.

   {[
     let _bad : validated_namespace =
       { id = "x"; display_name = "x"; config_dir = ".x" }
   ]} *)

let () =
  Alcotest.run
    "Validated_namespace"
    [
      ( "smart_constructor",
        [
          Alcotest.test_case
            "default namespace validates"
            `Quick
            test_default_namespace_validates;
          Alcotest.test_case
            "rejects invalid id"
            `Quick
            test_validate_rejects_bad_id;
          Alcotest.test_case
            "rejects path traversal in config_dir"
            `Quick
            test_validate_rejects_traversal_in_config_dir;
          Alcotest.test_case
            "rejects empty display_name"
            `Quick
            test_validate_rejects_empty_display_name;
        ] );
    ]
