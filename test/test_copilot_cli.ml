(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the Copilot CLI backend. *)

open Cabal

(** {1 Module Identity Tests} *)

let test_id () = Alcotest.(check string) "id" "copilot-cli" Copilot_cli.id

let test_name () =
  Alcotest.(check string) "name" "GitHub Copilot" Copilot_cli.name

let identity_tests =
  [
    ("id is copilot-cli", `Quick, test_id);
    ("name is GitHub Copilot", `Quick, test_name);
  ]

(** {1 Availability Tests} *)

let test_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Just verify available runs without raising; check logic tested in
     test_backend_process.ml via check_available *)
  let (_ : bool) = Copilot_cli.available ~sw ~env in
  ()

let availability_tests = [("available check", `Quick, test_available)]

(** {1 Command Construction Tests} *)

let minimal_spec () : Backend_types.task_spec =
  {
    prompt = "test";
    instructions = "";
    mcp_servers = [];
    managed_namespace = Backend_types.default_managed_namespace;
    lsp_servers = [];
    working_dir = "/tmp";
    timeout = 60.0;
    expected_outputs = [];
    model = None;
    resume_session_id = None;
    max_turns = None;
    read_only = false;
    json_schema = None;
  }

let test_build_command_allows_project_custom_instructions () =
  let cmd, _stdin =
    Copilot_cli.build_command ~mcp_config_path:None (minimal_spec ())
  in
  Alcotest.(check bool)
    "does not disable generated .github/copilot-instructions.md"
    false
    (List.mem "--no-custom-instructions" cmd)

let has_adjacent_args flag value cmd =
  let rec loop = function
    | first :: second :: _ when first = flag && second = value -> true
    | _ :: rest -> loop rest
    | [] -> false
  in
  loop cmd

let contains_substr s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let test_build_command_includes_model_flag () =
  let spec =
    {(minimal_spec ()) with Backend_types.model = Some "gpt-5.4-mini"}
  in
  let cmd, _stdin = Copilot_cli.build_command ~mcp_config_path:None spec in
  Alcotest.(check bool)
    "includes --model"
    true
    (has_adjacent_args "--model" "gpt-5.4-mini" cmd)

let test_build_command_passes_prompt_as_argument () =
  let spec =
    {
      (minimal_spec ()) with
      Backend_types.prompt = "Reply exactly: EPURE_SMOKE_OK";
      instructions = "Project instruction";
    }
  in
  let expected_prompt =
    "Reply exactly: EPURE_SMOKE_OK\n\n\
     ---\n\
     Project Instructions:\n\
     Project instruction"
  in
  let cmd, stdin = Copilot_cli.build_command ~mcp_config_path:None spec in
  Alcotest.(check bool)
    "passes prompt to -p/--prompt"
    true
    (has_adjacent_args "-p" expected_prompt cmd
    || has_adjacent_args "--prompt" expected_prompt cmd) ;
  Alcotest.(check string) "stdin is empty" "" stdin

let rec ensure_dir path =
  if path <> "." && path <> Filename.dirname path then begin
    ensure_dir (Filename.dirname path) ;
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path content =
  ensure_dir (Filename.dirname path) ;
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let with_tmpdir f =
  let dir = Filename.temp_dir "epure_test_copilot_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let test_project_config_validation_prioritizes_invalid_generated_config () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun project_dir ->
      write_file
        (Filename.concat project_dir ".github/copilot-instructions.md")
        "user-owned instructions\n" ;
      let setup_result =
        Backend_config_writer.setup_artifacts
          ~project_dir
          ~force:false
          (Copilot_cli.project_config_artifacts
             ~managed_namespace:Backend_types.default_managed_namespace
             ~mcp_servers:[]
             ~lsp_servers:[])
      in
      write_file
        (Filename.concat project_dir ".github/mcp.json")
        "{ invalid json\n" ;
      match
        Copilot_cli.check_project_config ~sw ~env ~project_dir ~setup_result
      with
      | Agentic_backend.Config_invalid msg ->
          Alcotest.(check bool)
            "reports invalid generated MCP config"
            true
            (String.starts_with
               ~prefix:"Copilot MCP config is not strict JSON:"
               msg)
      | Agentic_backend.Config_check_unsupported reason ->
          Alcotest.failf
            "unsupported user artifact masked invalid generated MCP config: %s"
            reason
      | Agentic_backend.Config_valid ->
          Alcotest.fail "invalid generated MCP config should fail validation")

let test_project_config_validation_accepts_custom_namespace_marker () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun project_dir ->
      let managed_namespace : Backend_types.managed_namespace =
        {
          id = "crucible";
          display_name = "Crucible";
          config_dir = ".crucible/backend-config";
        }
      in
      let setup_result =
        Backend_config_writer.setup_artifacts
          ~project_dir
          ~force:false
          (Copilot_cli.project_config_artifacts
             ~managed_namespace
             ~mcp_servers:[]
             ~lsp_servers:[])
      in
      let instructions =
        let ic =
          open_in_bin
            (Filename.concat project_dir ".github/copilot-instructions.md")
        in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> really_input_string ic (in_channel_length ic))
      in
      Alcotest.(check bool)
        "generated custom marker exists"
        true
        (contains_substr instructions "crucible-managed") ;
      match
        Copilot_cli.check_project_config ~sw ~env ~project_dir ~setup_result
      with
      | Agentic_backend.Config_valid -> ()
      | Agentic_backend.Config_check_unsupported reason ->
          Alcotest.failf
            "expected custom namespace config to validate: %s"
            reason
      | Agentic_backend.Config_invalid msg ->
          Alcotest.failf "expected custom namespace config to validate: %s" msg)

let test_run_task_rejects_invalid_namespace_before_write () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun project_dir ->
      let invalid_namespace : Backend_types.managed_namespace =
        {
          id = "../bad";
          display_name = "Bad";
          config_dir = ".epure/backend-config";
        }
      in
      let spec =
        {
          (Backend_types.make_task_spec
             ~prompt:"test"
             ~working_dir:project_dir
             ())
          with
          Backend_types.managed_namespace = invalid_namespace;
        }
      in
      let result = Copilot_cli.run_task ~sw ~env spec in
      (match result.Backend_types.status with
      | Backend_types.Failed msg ->
          Alcotest.(check bool)
            "invalid namespace failure"
            true
            (String.length msg > 0)
      | _ -> Alcotest.fail "expected Failed status for invalid namespace") ;
      Alcotest.(check bool)
        "no project config directory written"
        false
        (Sys.file_exists (Filename.concat project_dir ".github")))

let command_tests =
  [
    ( "build_command allows project custom instructions",
      `Quick,
      test_build_command_allows_project_custom_instructions );
    ( "build_command includes model flag",
      `Quick,
      test_build_command_includes_model_flag );
    ( "build_command passes prompt as argument",
      `Quick,
      test_build_command_passes_prompt_as_argument );
    ( "project config validation prioritizes invalid generated config",
      `Quick,
      test_project_config_validation_prioritizes_invalid_generated_config );
    ( "project config validation accepts custom namespace marker",
      `Quick,
      test_project_config_validation_accepts_custom_namespace_marker );
    ( "run_task rejects invalid namespace before write",
      `Quick,
      test_run_task_rejects_invalid_namespace_before_write );
  ]

(** {1 Backend Interface Compliance Tests} *)

let test_implements_agentic_backend () =
  let backend = (module Copilot_cli : Agentic_backend.S) in
  Alcotest.(check string)
    "id via interface"
    "copilot-cli"
    (Agentic_backend.id backend) ;
  Alcotest.(check string)
    "name via interface"
    "GitHub Copilot"
    (Agentic_backend.name backend)

let interface_tests =
  [("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend)]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Copilot_cli"
    [
      ("Identity", identity_tests);
      ("Availability", availability_tests);
      ("Command", command_tests);
      ("Interface", interface_tests);
    ]
