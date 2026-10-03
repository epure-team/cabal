(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Codex [--output-schema] must only carry schemas OpenAI strict structured
    outputs accept.

    Regression: on 2026-10-03 [codex exec --output-schema] exited 1 with
    [invalid_json_schema] ("In context=('properties', 'thinking'),
    'additionalProperties' is required to be supplied and to be false") for
    the strategist schema, because Codex_cli passed every schema to the flag.
    The shapes below are the ones observed accepted / rejected by
    codex-cli 0.160.0 with gpt-6.1-sol. *)

open Cabal

let obj ?(extra = []) props =
  `Assoc
    ([
       ("type", `String "object");
       ("additionalProperties", `Bool false);
       ("required", `List (List.map (fun (k, _) -> `String k) props));
       ("properties", `Assoc props);
     ]
    @ extra)

let str = `Assoc [("type", `String "string")]

let is_ok = function Ok () -> true | Error _ -> false

let check_accepts name schema =
  match Openai_strict_schema.check schema with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: expected strict, got %s" name e

let check_rejects name ~mentions schema =
  match Openai_strict_schema.check schema with
  | Ok () -> Alcotest.failf "%s: expected rejection" name
  | Error e ->
      let contains =
        let n = String.length mentions and l = String.length e in
        let rec go i =
          i + n <= l && (String.sub e i n = mentions || go (i + 1))
        in
        go 0
      in
      if not contains then
        Alcotest.failf "%s: reason %S does not mention %S" name e mentions

(* {1 Checker — accepted shapes} *)

let test_accepts_observed_ok_shapes () =
  check_accepts "plain" (obj [("a", str)]) ;
  check_accepts
    "$schema at root"
    (obj
       ~extra:
         [("$schema", `String "https://json-schema.org/draft/2020-12/schema")]
       [("a", str)]) ;
  check_accepts
    "anyOf"
    (obj
       [
         ( "a",
           `Assoc [("anyOf", `List [str; `Assoc [("type", `String "integer")]])]
         );
       ]) ;
  check_accepts
    "bounded int"
    (obj
       [
         ( "a",
           `Assoc
             [
               ("type", `String "integer");
               ("minimum", `Int 0);
               ("maximum", `Int 5);
             ] );
       ]) ;
  check_accepts
    "bounded string"
    (obj
       [
         ( "a",
           `Assoc
             [
               ("type", `String "string");
               ("maxLength", `Int 5);
               ("minLength", `Int 1);
               ("pattern", `String "\\S");
             ] );
       ]) ;
  check_accepts
    "nullable"
    (obj [("a", `Assoc [("type", `List [`String "string"; `String "null"])])]) ;
  check_accepts
    "enum + array"
    (obj
       [
         ( "a",
           `Assoc
             [
               ("type", `String "array");
               ("maxItems", `Int 3);
               ( "items",
                 `Assoc
                   [
                     ("type", `String "string");
                     ("enum", `List [`String "x"; `String "y"]);
                   ] );
             ] );
       ])

(* {1 Checker — rejected shapes (each observed as an API 400)} *)

let test_rejects_free_form_object () =
  (* The exact shape of the 2026-10-03 failure: a property typed only as
     "object". *)
  check_rejects
    "free-form object"
    ~mentions:"properties.thinking"
    (obj [("thinking", `Assoc [("type", `String "object")])])

let test_rejects_missing_additional_properties () =
  check_rejects
    "additionalProperties absent"
    ~mentions:"additionalProperties"
    (`Assoc
       [
         ("type", `String "object");
         ("required", `List [`String "a"]);
         ("properties", `Assoc [("a", str)]);
       ]) ;
  check_rejects
    "additionalProperties true"
    ~mentions:"additionalProperties"
    (`Assoc
       [
         ("type", `String "object");
         ("additionalProperties", `Bool true);
         ("required", `List [`String "a"]);
         ("properties", `Assoc [("a", str)]);
       ])

let test_rejects_optional_property () =
  check_rejects
    "optional property"
    ~mentions:"required omits property b"
    (`Assoc
       [
         ("type", `String "object");
         ("additionalProperties", `Bool false);
         ("required", `List [`String "a"]);
         ("properties", `Assoc [("a", str); ("b", str)]);
       ])

let test_rejects_one_of () =
  check_rejects
    "oneOf"
    ~mentions:"oneOf"
    (obj [("a", `Assoc [("oneOf", `List [str])])])

let test_rejects_const_without_type () =
  check_rejects
    "const"
    ~mentions:"const"
    (obj [("a", `Assoc [("const", `String "x")])])

let test_rejects_untyped_and_non_object_root () =
  check_rejects
    "untyped"
    ~mentions:"neither type nor anyOf"
    (obj [("a", `Assoc [])]) ;
  check_rejects
    "root array"
    ~mentions:"root"
    (`Assoc [("type", `String "array")]) ;
  check_rejects
    "array without items"
    ~mentions:"items"
    (obj [("a", `Assoc [("type", `String "array")])])

let test_rejects_excessive_depth () =
  let rec nest n = if n = 0 then str else obj [("x", nest (n - 1))] in
  Alcotest.(check bool)
    "depth = max accepted"
    true
    (is_ok
       (Openai_strict_schema.check (nest (Openai_strict_schema.max_depth - 1)))) ;
  check_rejects
    "depth > max"
    ~mentions:"nesting"
    (nest (Openai_strict_schema.max_depth + 1))

(* {1 Codex_cli.build_command wiring} *)

let rec flag_value flag = function
  | f :: v :: _ when String.equal f flag -> Some v
  | _ :: rest -> flag_value flag rest
  | [] -> None

let contains_sub s sub =
  let n = String.length sub and l = String.length s in
  let rec go i = i + n <= l && (String.sub s i n = sub || go (i + 1)) in
  go 0

let free_form_schema =
  `Assoc
    [
      ("type", `String "object");
      ("additionalProperties", `Bool false);
      ("properties", `Assoc [("thinking", `Assoc [("type", `String "object")])]);
    ]

let test_build_command_omits_flag_for_non_strict () =
  let spec =
    Backend_types.make_task_spec
      ~prompt:"PROMPT"
      ~working_dir:"/tmp"
      ~json_schema:free_form_schema
      ()
  in
  let cmd, stdin = Codex_cli.build_command ~mcp_config_path:None spec in
  Alcotest.(check (option string))
    "no --output-schema"
    None
    (flag_value "--output-schema" cmd) ;
  Alcotest.(check bool)
    "schema still delivered in prompt"
    true
    (contains_sub stdin (Yojson.Safe.to_string ~std:true free_form_schema)) ;
  Alcotest.(check bool)
    "under the shared heading"
    true
    (contains_sub stdin "## Required output schema")

let test_build_command_keeps_flag_for_strict () =
  let schema = obj [("answer", str)] in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"PROMPT"
      ~working_dir:"/tmp"
      ~json_schema:schema
      ()
  in
  let cmd, stdin = Codex_cli.build_command ~mcp_config_path:None spec in
  (match flag_value "--output-schema" cmd with
  | None -> Alcotest.fail "--output-schema missing for a strict schema"
  | Some path -> ( try Sys.remove path with Sys_error _ -> ())) ;
  Alcotest.(check bool)
    "prompt not inflated when native"
    false
    (contains_sub stdin "## Required output schema")

(* {1 Json_schema_enforcer routing} *)

let make_backend ~id ~native ~responses =
  let calls = ref [] in
  let module M = struct
    let id = id

    let name = id

    let models = []

    let models_probe = None

    let available ~sw:_ ~env:_ = true

    let supports_session_resume = false

    let native_json_schema_output = native

    let is_resume_failure _ = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "mock"

    let run_task ~sw:_ ~env:_ ?on_raw_line:_ (spec : Backend_types.task_spec) =
      let i = List.length !calls in
      calls := spec :: !calls ;
      match List.nth_opt responses i with
      | Some text ->
          Backend_types.make_task_result
            ~status:Backend_types.Success
            ~agent_text:text
            ()
      | None ->
          Backend_types.make_task_result
            ~status:(Backend_types.Failed "no more responses")
            ()
  end in
  ((module M : Agentic_backend.S), calls)

let test_routing_decision () =
  let codex, _ = make_backend ~id:Codex_cli.id ~native:true ~responses:[] in
  let other, _ = make_backend ~id:"other-native" ~native:true ~responses:[] in
  let plain, _ = make_backend ~id:"plain" ~native:false ~responses:[] in
  let strict = obj [("a", str)] in
  let check name expected backend schema =
    Alcotest.(check bool)
      name
      expected
      (Json_schema_enforcer.native_path_applies ~backend schema)
  in
  check "codex + strict → native" true codex strict ;
  check "codex + free-form → validate-and-retry" false codex free_form_schema ;
  check "other native + free-form → native" true other free_form_schema ;
  check "non-native → validate-and-retry" false plain strict

let test_codex_non_strict_is_validated_against_original () =
  (* The free-form schema accepts any object under "thinking"; a first answer
     that is not JSON must be caught and retried — enforcement is not dropped
     when the native flag is. *)
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let backend, calls =
    make_backend
      ~id:Codex_cli.id
      ~native:true
      ~responses:["not json"; {|{"thinking":{"free":"form"}}|}]
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"P"
      ~working_dir:"/tmp"
      ~json_schema:free_form_schema
      ()
  in
  match Json_schema_enforcer.run_task ~sw ~env ~backend spec with
  | Error e -> Alcotest.failf "expected recovery on retry, got %s" e
  | Ok result ->
      Alcotest.(check int)
        "two calls (validate-and-retry)"
        2
        (List.length !calls) ;
      Alcotest.(check string)
        "second answer kept"
        {|{"thinking":{"free":"form"}}|}
        result.Backend_types.agent_text ;
      let first = List.nth !calls 1 in
      Alcotest.(check bool)
        "first call still carries the schema for the prompt"
        true
        (Option.is_some first.Backend_types.json_schema)

let () =
  Alcotest.run
    "openai_strict_schema"
    [
      ( "checker",
        [
          ("accepts observed-ok shapes", `Quick, test_accepts_observed_ok_shapes);
          ("rejects free-form object", `Quick, test_rejects_free_form_object);
          ( "rejects missing additionalProperties: false",
            `Quick,
            test_rejects_missing_additional_properties );
          ("rejects optional property", `Quick, test_rejects_optional_property);
          ("rejects oneOf", `Quick, test_rejects_one_of);
          ("rejects const without type", `Quick, test_rejects_const_without_type);
          ( "rejects untyped / non-object root",
            `Quick,
            test_rejects_untyped_and_non_object_root );
          ("rejects excessive depth", `Quick, test_rejects_excessive_depth);
        ] );
      ( "codex build_command",
        [
          ( "non-strict: no flag, schema in prompt",
            `Quick,
            test_build_command_omits_flag_for_non_strict );
          ("strict: flag kept", `Quick, test_build_command_keeps_flag_for_strict);
        ] );
      ( "enforcer routing",
        [
          ("native_path_applies", `Quick, test_routing_decision);
          ( "codex non-strict validated against original",
            `Quick,
            test_codex_non_strict_is_validated_against_original );
        ] );
    ]
