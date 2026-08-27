(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the Gemini CLI backend. *)

open Cabal

(** {1 Module Identity Tests} *)

let test_id () = Alcotest.(check string) "id" "gemini-cli" Gemini_cli.id

let test_name () = Alcotest.(check string) "name" "Gemini CLI" Gemini_cli.name

let identity_tests =
  [
    ("id is gemini-cli", `Quick, test_id);
    ("name is Gemini CLI", `Quick, test_name);
  ]

(** {1 JSON Output Parsing Tests} *)

let test_parse_gemini_json_output_with_response () =
  let json =
    `Assoc
      [
        ("response", `String "Analysis complete");
        ( "usageMetadata",
          `Assoc
            [("promptTokenCount", `Int 150); ("candidatesTokenCount", `Int 80)]
        );
      ]
  in
  let text, cost = Gemini_cli.parse_gemini_json_output json in
  Alcotest.(check string) "response text" "Analysis complete" text ;
  Alcotest.(check bool) "cost present" true (Option.is_some cost) ;
  match cost with
  | Some c ->
      Alcotest.(check (option int)) "input_tokens" (Some 150) c.tokens_input ;
      Alcotest.(check (option int)) "output_tokens" (Some 80) c.tokens_output
  | None -> Alcotest.fail "Expected cost to be Some"

let test_parse_gemini_json_output_with_result () =
  let json =
    `Assoc
      [
        ("result", `String "Done");
        ("usage", `Assoc [("input_tokens", `Int 50); ("output_tokens", `Int 25)]);
      ]
  in
  let text, cost = Gemini_cli.parse_gemini_json_output json in
  Alcotest.(check string) "result text" "Done" text ;
  Alcotest.(check bool) "cost present" true (Option.is_some cost) ;
  match cost with
  | Some c ->
      Alcotest.(check (option int)) "input_tokens" (Some 50) c.tokens_input ;
      Alcotest.(check (option int)) "output_tokens" (Some 25) c.tokens_output
  | None -> Alcotest.fail "Expected cost to be Some"

let test_parse_gemini_json_output_no_usage () =
  let json = `Assoc [("response", `String "Simple result")] in
  let text, cost = Gemini_cli.parse_gemini_json_output json in
  Alcotest.(check string) "response text" "Simple result" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_gemini_json_output_malformed () =
  let json = `Assoc [("unexpected", `String "field")] in
  let text, cost = Gemini_cli.parse_gemini_json_output json in
  (* Falls back to stringifying the JSON *)
  Alcotest.(check bool) "text is non-empty" true (String.length text > 0) ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let json_output_tests =
  [
    ( "parse with response and usageMetadata",
      `Quick,
      test_parse_gemini_json_output_with_response );
    ( "parse with result and usage",
      `Quick,
      test_parse_gemini_json_output_with_result );
    ("parse without usage", `Quick, test_parse_gemini_json_output_no_usage);
    ("parse malformed", `Quick, test_parse_gemini_json_output_malformed);
  ]

(** {1 Availability Tests} *)

let test_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Just verify available runs without raising; check logic tested in
     test_backend_process.ml via check_available *)
  let (_ : bool) = Gemini_cli.available ~sw ~env in
  ()

let availability_tests = [("available check", `Quick, test_available)]

(** {1 Command Construction Tests} *)

let minimal_spec ?model () =
  Backend_types.make_task_spec ~prompt:"test" ~working_dir:"/tmp" ?model ()

let has_adjacent_args flag value cmd =
  let rec loop = function
    | first :: second :: _ when first = flag && second = value -> true
    | _ :: rest -> loop rest
    | [] -> false
  in
  loop cmd

let test_build_command_includes_model_flag () =
  let cmd, _stdin =
    Gemini_cli.build_command
      ~mcp_config_path:None
      (minimal_spec ~model:"gemini-3-flash-preview" ())
  in
  Alcotest.(check bool)
    "includes -m model"
    true
    (has_adjacent_args "-m" "gemini-3-flash-preview" cmd)

let test_build_command_includes_skip_trust () =
  let cmd, _stdin =
    Gemini_cli.build_command ~mcp_config_path:None (minimal_spec ())
  in
  Alcotest.(check bool)
    "includes --skip-trust"
    true
    (List.mem "--skip-trust" cmd)

let command_tests =
  [
    ( "build_command includes model flag",
      `Quick,
      test_build_command_includes_model_flag );
    ( "build_command includes skip-trust",
      `Quick,
      test_build_command_includes_skip_trust );
  ]

(** {1 Backend Interface Compliance Tests} *)

let test_implements_agentic_backend () =
  let backend = (module Gemini_cli : Agentic_backend.S) in
  Alcotest.(check string)
    "id via interface"
    "gemini-cli"
    (Agentic_backend.id backend) ;
  Alcotest.(check string)
    "name via interface"
    "Gemini CLI"
    (Agentic_backend.name backend)

let interface_tests =
  [("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend)]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Gemini_cli"
    [
      ("Identity", identity_tests);
      ("JSON Output", json_output_tests);
      ("Availability", availability_tests);
      ("Command", command_tests);
      ("Interface", interface_tests);
    ]
