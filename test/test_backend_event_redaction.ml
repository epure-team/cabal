(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Backend_event_redaction.

    Pinpoints the redaction policy: which fields are always redacted, which
    are always preserved, and which structural patterns (URL credentials,
    JWT-shaped tokens) trigger value-level redaction even in non-sensitive
    fields. *)

module R = Cabal.Backend_event_redaction

let json_eq =
  Alcotest.testable (Fmt.of_to_string Yojson.Safe.to_string) Yojson.Safe.equal

(** Helper: extract sanitized JSON from a redaction result. *)
let sanitize (json : Yojson.Safe.t) : Yojson.Safe.t =
  (R.redact_event ~backend_id:"test" json).sanitized

(** True iff the sanitized output, serialised to a string, still contains
    [needle].  Used to assert leakage absence. *)
let leaks needle json =
  let s = Yojson.Safe.to_string (sanitize json) in
  let rec scan i =
    if i > String.length s - String.length needle then false
    else if String.sub s i (String.length needle) = needle then true
    else scan (i + 1)
  in
  scan 0

(* ---- baseline behaviour kept intact -------------------------------------- *)

let test_prompt_still_redacted () =
  let j =
    `Assoc [("type", `String "user_message"); ("prompt", `String "hello world")]
  in
  Alcotest.(check bool) "prompt leaks" false (leaks "hello world" j)

let test_event_type_preserved () =
  let j = `Assoc [("type", `String "assistant")] in
  Alcotest.check json_eq "type kept" j (sanitize j)

(* ---- newly-covered sensitive fields -------------------------------------- *)

let assert_redacted ~field ~secret =
  let j = `Assoc [(field, `String secret)] in
  Alcotest.(check bool)
    (Printf.sprintf "field %s containing %S leaks" field secret)
    false
    (leaks secret j)

let test_environment_field_redacted () =
  assert_redacted ~field:"environment" ~secret:"API_KEY=sk-1234567890abcdef"

let test_env_vars_field_redacted () =
  assert_redacted ~field:"env_vars" ~secret:"AWS_SECRET=wJalrXUtnFEMI/K7MDENG"

let test_oauth_token_field_redacted () =
  assert_redacted ~field:"oauth_token" ~secret:"ya29.A0ARrdaM_some_oauth_token"

let test_jwt_field_redacted () =
  assert_redacted
    ~field:"jwt"
    ~secret:"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJqb2UifQ.sig"

let test_bearer_field_redacted () =
  assert_redacted ~field:"bearer" ~secret:"abcdef1234567890"

let test_connection_string_redacted () =
  assert_redacted
    ~field:"connection_string"
    ~secret:"postgresql://admin:hunter2@db.local/prod"

let test_dsn_redacted () =
  assert_redacted ~field:"dsn" ~secret:"https://abc:def@sentry.io/123"

let test_cookie_redacted () =
  assert_redacted ~field:"cookie" ~secret:"sessionId=secretvalue; HttpOnly"

let test_signature_redacted () =
  assert_redacted ~field:"signature" ~secret:"AKIAIOSFODNN7EXAMPLE-deadbeef"

let test_client_secret_redacted () =
  assert_redacted ~field:"client_secret" ~secret:"sk-abc123"

(* ---- pattern-based redaction in otherwise-safe fields -------------------- *)

let test_url_with_credentials_redacted () =
  (* "url" used to be on the safe-string list, allowing
     postgres://user:pass@host/db to leak in a non-sensitive context. *)
  let j =
    `Assoc [("url", `String "postgresql://admin:hunter2@db.local/prod")]
  in
  Alcotest.(check bool) "credential URL leaks" false (leaks "hunter2" j)

let test_url_without_credentials_kept () =
  (* Non-credential URLs are common in events (model endpoints, doc links).
     They should still be readable for observability. *)
  let j = `Assoc [("url", `String "https://api.anthropic.com/v1/messages")] in
  Alcotest.(check bool) "plain URL kept" true (leaks "anthropic.com" j)

let test_jwt_shaped_value_in_arbitrary_field_redacted () =
  (* A JWT can leak through a field that's not in the sensitive list (e.g. a
     "user" payload).  The pattern-based fallback should still catch it. *)
  let jwt =
    "eyJhbGciOiJIUzI1NiJ9."
    ^ "eyJzdWIiOiJ1c2VyMSIsIm5hbWUiOiJBbGljZSIsImlhdCI6MTUxNjIzOTAyMn0."
    ^ "deadbeefdeadbeefdeadbeefdeadbeef0123"
  in
  let j = `Assoc [("extra", `String jwt)] in
  Alcotest.(check bool) "JWT-shaped token leaks" false (leaks jwt j)

(* ---- nested-container coverage ------------------------------------------- *)

let test_nested_prompt_inside_array_redacted () =
  let j =
    `Assoc
      [
        ( "messages",
          `List
            [
              `Assoc
                [("role", `String "user"); ("prompt", `String "deep secret")];
            ] );
      ]
  in
  Alcotest.(check bool) "nested prompt leaks" false (leaks "deep secret" j)

let test_nested_environment_inside_object_redacted () =
  let j =
    `Assoc
      [
        ( "process",
          `Assoc [("environment", `String "GITHUB_TOKEN=ghp_secrettoken")] );
      ]
  in
  Alcotest.(check bool) "nested env leaks" false (leaks "ghp_secrettoken" j)

let test_extended_yojson_containers_are_exhaustive () =
  let tuple = `Tuple [`Assoc [("prompt", `String "tuple-secret")]] in
  let variant =
    `Variant ("Envelope", Some (`Assoc [("token", `String "variant-secret")]))
  in
  Alcotest.(check bool) "tuple secret redacted" false (leaks "tuple-secret" tuple) ;
  Alcotest.(check bool)
    "variant secret redacted"
    false
    (leaks "variant-secret" variant)

(* ---- error fields ---------------------------------------------------------*)

let test_error_field_with_secret_redacted () =
  (* "error" used to be safe-listed, so an error message echoing a secret
     would leak unredacted. *)
  let j =
    `Assoc
      [
        ( "error",
          `String
            "failed to call postgresql://admin:hunter2@db.local: connection \
             refused" );
      ]
  in
  Alcotest.(check bool) "error with cred URL leaks" false (leaks "hunter2" j)

let () =
  Alcotest.run
    "Backend_event_redaction"
    [
      ( "baseline",
        [
          Alcotest.test_case
            "prompt still redacted"
            `Quick
            test_prompt_still_redacted;
          Alcotest.test_case
            "event type preserved"
            `Quick
            test_event_type_preserved;
        ] );
      ( "sensitive_fields",
        [
          Alcotest.test_case "environment" `Quick test_environment_field_redacted;
          Alcotest.test_case "env_vars" `Quick test_env_vars_field_redacted;
          Alcotest.test_case
            "oauth_token"
            `Quick
            test_oauth_token_field_redacted;
          Alcotest.test_case "jwt" `Quick test_jwt_field_redacted;
          Alcotest.test_case "bearer" `Quick test_bearer_field_redacted;
          Alcotest.test_case
            "connection_string"
            `Quick
            test_connection_string_redacted;
          Alcotest.test_case "dsn" `Quick test_dsn_redacted;
          Alcotest.test_case "cookie" `Quick test_cookie_redacted;
          Alcotest.test_case "signature" `Quick test_signature_redacted;
          Alcotest.test_case "client_secret" `Quick test_client_secret_redacted;
        ] );
      ( "patterns",
        [
          Alcotest.test_case
            "url with credentials redacted"
            `Quick
            test_url_with_credentials_redacted;
          Alcotest.test_case
            "url without credentials kept"
            `Quick
            test_url_without_credentials_kept;
          Alcotest.test_case
            "jwt-shaped token in arbitrary field"
            `Quick
            test_jwt_shaped_value_in_arbitrary_field_redacted;
          Alcotest.test_case
            "error with credential URL"
            `Quick
            test_error_field_with_secret_redacted;
        ] );
      ( "nesting",
        [
          Alcotest.test_case
            "nested prompt inside array"
            `Quick
            test_nested_prompt_inside_array_redacted;
          Alcotest.test_case
            "nested environment inside object"
            `Quick
            test_nested_environment_inside_object_redacted;
          Alcotest.test_case
            "tuple and variant containers"
            `Quick
            test_extended_yojson_containers_are_exhaustive;
        ] );
    ]
