(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the OpenCode CLI backend. *)

open Cabal

(** {1 Helpers} *)

let with_tmpdir f =
  let dir = Filename.temp_dir "epure_test_oc_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let write_file path content =
  let oc = open_out path in
  output_string oc content ;
  close_out oc

let rec ensure_parent_dir path =
  let dir = Filename.dirname path in
  if dir = path || dir = "." then ()
  else begin
    ensure_parent_dir dir ;
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755
  end

let write_file_creating_dirs path content =
  ensure_parent_dir path ;
  write_file path content

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n ;
  close_in ic ;
  Bytes.to_string buf

let contains_str s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let strip_line_comment_headers content =
  content |> String.split_on_char '\n'
  |> List.filter (fun line ->
      not (String.starts_with ~prefix:"//" (String.trim line)))
  |> String.concat "\n"

(** {1 Module Identity Tests} *)

let test_id () = Alcotest.(check string) "id" "opencode" Opencode_cli.id

let test_name () = Alcotest.(check string) "name" "OpenCode" Opencode_cli.name

let identity_tests =
  [("id is opencode", `Quick, test_id); ("name is OpenCode", `Quick, test_name)]

(** {1 JSON Events Parsing Tests} *)

let test_parse_json_events_with_content () =
  let input =
    {|{"type":"step_start","part":{"type":"step-start"}}
{"type":"text","part":{"text":"First chunk"}}
{"type":"text","part":{"text":" second chunk"}}
{"type":"step_finish","part":{"tokens":{"input":300,"output":120},"cost":0.005}}|}
  in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "concatenated text" "First chunk second chunk" text ;
  Alcotest.(check bool) "cost present" true (Option.is_some cost) ;
  match cost with
  | Some c ->
      Alcotest.(check (option int)) "input_tokens" (Some 300) c.tokens_input ;
      Alcotest.(check (option int)) "output_tokens" (Some 120) c.tokens_output ;
      Alcotest.(check bool) "cost_usd present" true (Option.is_some c.cost_usd)
  | None -> Alcotest.fail "Expected cost to be Some"

let test_parse_json_events_text_field () =
  let input = {|{"type":"text","part":{"text":"Hello from OpenCode"}}|} in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "text field" "Hello from OpenCode" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_json_events_no_usage () =
  let input = {|{"type":"text","part":{"text":"Simple response"}}|} in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "content text" "Simple response" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_json_events_empty () =
  let text, cost = Opencode_cli.parse_json_events "" in
  Alcotest.(check string) "empty fallback" "" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let test_parse_json_events_malformed () =
  let input = "garbage data\n{not valid json" in
  let text, cost = Opencode_cli.parse_json_events input in
  Alcotest.(check string) "malformed fallback" "" text ;
  Alcotest.(check bool) "no cost" true (Option.is_none cost)

let json_events_tests =
  [
    ("parse with content and usage", `Quick, test_parse_json_events_with_content);
    ("parse text field", `Quick, test_parse_json_events_text_field);
    ("parse without usage", `Quick, test_parse_json_events_no_usage);
    ("parse empty output", `Quick, test_parse_json_events_empty);
    ("parse malformed output", `Quick, test_parse_json_events_malformed);
  ]

(** {1 Availability Tests} *)

let test_available () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Just verify available runs without raising; check logic tested in
     test_backend_process.ml via check_available *)
  let (_ : bool) = Opencode_cli.available ~sw ~env in
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
    Opencode_cli.build_command
      ~mcp_config_path:None
      (minimal_spec ~model:"openai/gpt-4o-mini" ())
  in
  Alcotest.(check bool)
    "includes -m model"
    true
    (has_adjacent_args "-m" "openai/gpt-4o-mini" cmd)

let command_tests =
  [
    ( "build_command includes model flag",
      `Quick,
      test_build_command_includes_model_flag );
  ]

(** {1 Backend Interface Compliance Tests} *)

let test_implements_agentic_backend () =
  let backend = (module Opencode_cli : Agentic_backend.S) in
  Alcotest.(check string)
    "id via interface"
    "opencode"
    (Agentic_backend.id backend) ;
  Alcotest.(check string)
    "name via interface"
    "OpenCode"
    (Agentic_backend.name backend)

let interface_tests =
  [("implements AGENTIC_BACKEND.S", `Quick, test_implements_agentic_backend)]

(** {1 OpenCode config mutation helpers} *)

let epure_mcp_server () =
  Backend_types.make_mcp_server_config
    ~name:"epure"
    ~command:"epure"
    ~args:["mcp"]
    ()

let task_spec_with_mcp ?managed_namespace dir =
  Backend_types.make_task_spec
    ~prompt:"test"
    ~working_dir:dir
    ~mcp_servers:[epure_mcp_server ()]
    ?managed_namespace
    ()

let custom_managed_namespace =
  {
    Backend_types.id = "acme";
    display_name = "Acme";
    config_dir = ".acme/backend-config";
  }

let test_ensure_mcp_preserves_jsonc_managed_header () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"opencode"
           ~project_dir:dir
           ~force:false) ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let content = read_file (Filename.concat dir "opencode.json") in
      Alcotest.(check bool)
        "managed attribution comment preserved"
        true
        (contains_str content "Generated by Cabal"
        && contains_str content "cabal-managed"
        && contains_str content "cabal-hash") ;
      Alcotest.(check bool)
        "legacy _epure schema keys are absent"
        false
        (contains_str content "_epure_") ;
      let json = Yojson.Safe.from_string (strip_line_comment_headers content) in
      match json with
      | `Assoc fields ->
          Alcotest.(check bool)
            "mcp key present in schema-valid JSON body"
            true
            (List.mem_assoc "mcp" fields)
      | _ -> Alcotest.fail "expected opencode config object")

let test_ensure_mcp_removes_legacy_epure_keys () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      write_file
        path
        {|{
  "_epure_attribution": "Generated by Epure — do not edit manually",
  "_epure-managed": true,
  "_epure-hash": "d41d8cd98f00b204e9800998ecf8427e",
  "_epure_custom": "keep-me",
  "model": "anthropic/claude-sonnet-4.5"
}
|} ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let content = read_file path in
      List.iter
        (fun key ->
          Alcotest.(check bool)
            ("legacy schema key removed: " ^ key)
            false
            (contains_str content key))
        ["_epure_attribution"; "_epure-managed"; "_epure-hash"] ;
      Alcotest.(check bool)
        "non-legacy _epure-like field preserved"
        true
        (contains_str content "_epure_custom" && contains_str content "keep-me") ;
      Alcotest.(check bool)
        "user model field preserved"
        true
        (contains_str content "anthropic/claude-sonnet-4.5") ;
      let json = Yojson.Safe.from_string (strip_line_comment_headers content) in
      match json with
      | `Assoc fields ->
          Alcotest.(check bool)
            "mcp key present"
            true
            (List.mem_assoc "mcp" fields)
      | _ -> Alcotest.fail "expected opencode config object")

let test_ensure_mcp_parses_block_comments_and_trailing_commas () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      write_file
        path
        {|{
  /* user-authored JSONC comment */
  "model": "anthropic/claude-sonnet-4.5",
  "mcp": {
    "user-server": {
      "type": "local",
      "command": ["user-mcp"],
      "enabled": true,
      "environment": {},
    },
  },
}
|} ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let content = read_file path in
      Alcotest.(check bool)
        "user model preserved after JSONC parse"
        true
        (contains_str content "anthropic/claude-sonnet-4.5") ;
      Alcotest.(check bool)
        "user MCP server preserved after JSONC parse"
        true
        (contains_str content "user-server") ;
      Alcotest.(check bool)
        "epure MCP server added"
        true
        (contains_str content "\"epure\"") ;
      ignore (Yojson.Safe.from_string content : Yojson.Safe.t))

let test_ensure_mcp_parse_failure_preserves_file () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original =
        "{\n  \"model\": \"anthropic/claude-sonnet-4.5\",\n  /* unterminated"
      in
      write_file path original ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      Alcotest.(check string)
        "parse failure leaves opencode.json byte-for-byte unchanged"
        original
        (read_file path))

let test_ensure_mcp_runtime_entry_has_local_schema () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let content = read_file path in
      let json = Yojson.Safe.from_string (strip_line_comment_headers content) in
      let expected =
        `Assoc
          [
            ("type", `String "local");
            ("command", `List [`String "epure"; `String "mcp"]);
            ("enabled", `Bool true);
            ("environment", `Assoc []);
          ]
      in
      match json with
      | `Assoc fields -> (
          match List.assoc_opt "mcp" fields with
          | Some (`Assoc mcp_fields) -> (
              match List.assoc_opt "epure" mcp_fields with
              | Some actual ->
                  Alcotest.(check string)
                    "runtime MCP entry schema"
                    (Yojson.Safe.to_string expected)
                    (Yojson.Safe.to_string actual)
              | _ -> Alcotest.fail "expected epure MCP server")
          | _ -> Alcotest.fail "expected mcp object")
      | _ -> Alcotest.fail "expected opencode config object")

let test_ensure_mcp_after_legacy_migration_keeps_setup_idempotent () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let body_without_mcp =
        "{\n  \"model\": \"anthropic/claude-sonnet-4.5\"\n}"
      in
      let hash = Digest.to_hex (Digest.string body_without_mcp) in
      write_file
        path
        (Printf.sprintf
           {|{
  "_epure_attribution": "Generated by Epure — do not edit manually",
  "_epure-managed": true,
  "_epure-hash": "%s",
  "model": "anthropic/claude-sonnet-4.5"
}
|}
           hash) ;
      let first_setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      (match first_setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Written _) -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "legacy setup refused before MCP merge: %s" msg
      | Some _ -> Alcotest.fail "expected legacy setup to migrate"
      | None -> Alcotest.fail "expected opencode setup outcome") ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json ~env (task_spec_with_mcp dir) ;
      let second_setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      match second_setup.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "setup refused after MCP merge: %s" msg
      | Some _ ->
          Alcotest.fail "expected setup after MCP merge to be idempotent"
      | None -> Alcotest.fail "expected opencode setup outcome")

let test_ensure_mcp_custom_namespace_marks_and_stays_idempotent () =
  with_tmpdir (fun dir ->
      let ns = custom_managed_namespace in
      let path = Filename.concat dir "opencode.json" in
      let first_setup =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:ns
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
          ()
      in
      (match first_setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Written _) -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "custom setup refused before MCP merge: %s" msg
      | Some _ -> Alcotest.fail "expected custom setup to write opencode.json"
      | None -> Alcotest.fail "expected opencode setup outcome") ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json
        ~env
        (task_spec_with_mcp ~managed_namespace:ns dir) ;
      let content = read_file path in
      List.iter
        (fun marker ->
          Alcotest.(check bool)
            ("custom marker present: " ^ marker)
            true
            (contains_str content marker))
        ["Generated by Acme"; "acme-managed"; "acme-hash"] ;
      List.iter
        (fun marker ->
          Alcotest.(check bool)
            ("default-namespace marker absent: " ^ marker)
            false
            (contains_str content marker))
        [
          "Generated by Cabal";
          "cabal-managed";
          "cabal-hash";
          "Generated by Epure";
          "epure-managed";
          "epure-hash";
        ] ;
      let second_setup =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:ns
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
          ()
      in
      match second_setup.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "custom setup refused after MCP merge: %s" msg
      | Some _ -> Alcotest.fail "expected custom setup to remain idempotent"
      | None -> Alcotest.fail "expected opencode setup outcome")

let test_ensure_mcp_custom_namespace_marks_new_file () =
  with_tmpdir (fun dir ->
      let ns = custom_managed_namespace in
      let path = Filename.concat dir "opencode.json" in
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json
        ~env
        (task_spec_with_mcp ~managed_namespace:ns dir) ;
      let content = read_file path in
      Alcotest.(check bool)
        "new file uses custom managed header"
        true
        (contains_str content "Generated by Acme"
        && contains_str content "acme-managed"
        && contains_str content "acme-hash") ;
      Alcotest.(check bool)
        "new file does not use default-namespace or legacy Epure markers"
        false
        (contains_str content "Generated by Cabal"
        || contains_str content "cabal-managed"
        || contains_str content "cabal-hash"
        || contains_str content "Generated by Epure"
        || contains_str content "epure-managed"
        || contains_str content "epure-hash") ;
      let setup =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:ns
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
          ()
      in
      match setup.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "custom setup refused after direct MCP write: %s" msg
      | Some _ -> Alcotest.fail "expected direct MCP write to be idempotent"
      | None -> Alcotest.fail "expected opencode setup outcome")

let test_ensure_mcp_custom_namespace_migrates_legacy_epure_header () =
  with_tmpdir (fun dir ->
      let ns = custom_managed_namespace in
      let path = Filename.concat dir "opencode.json" in
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"opencode"
           ~project_dir:dir
           ~force:false) ;
      Eio_posix.run @@ fun env ->
      Opencode_cli.ensure_mcp_in_opencode_json
        ~env
        (task_spec_with_mcp ~managed_namespace:ns dir) ;
      let content = read_file path in
      Alcotest.(check bool)
        "default-namespace header migrated to custom marker"
        true
        (contains_str content "acme-managed" && contains_str content "acme-hash") ;
      Alcotest.(check bool)
        "default-namespace and legacy Epure markers no longer emitted"
        false
        (contains_str content "cabal-managed"
        || contains_str content "cabal-hash"
        || contains_str content "Generated by Cabal"
        || contains_str content "epure-managed"
        || contains_str content "epure-hash"
        || contains_str content "Generated by Epure") ;
      let setup =
        Backend_config_gen.setup_project_config_with_options
          ~managed_namespace:ns
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
          ()
      in
      match setup.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | Some (Backend_config_gen.Refused_hash_mismatch msg) ->
          Alcotest.failf "custom setup refused migrated header: %s" msg
      | Some _ -> Alcotest.fail "expected migrated header to be idempotent"
      | None -> Alcotest.fail "expected opencode setup outcome")

let test_ensure_mcp_after_skipped_setup_preserves_user_file () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original =
        {|{
  // user comment must survive byte-for-byte
  "model": "anthropic/claude-sonnet-4.5"
}
|}
      in
      write_file_creating_dirs path original ;
      let setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      (match setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Skipped_user_content _) -> ()
      | _ -> Alcotest.fail "expected setup to skip user-authored opencode.json") ;
      Eio_posix.run @@ fun env ->
      match
        Opencode_cli.ensure_mcp_if_config_applied
          ~env
          ~setup_outcome:setup.Backend_config_gen.write_outcome
          (task_spec_with_mcp dir)
      with
      | Ok () -> Alcotest.fail "expected MCP merge to be blocked after skip"
      | Error msg ->
          Alcotest.(check bool)
            "error mentions opencode.json"
            true
            (contains_str msg "opencode.json") ;
          Alcotest.(check string)
            "user-authored opencode.json unchanged"
            original
            (read_file path))

let test_ensure_mcp_after_refused_setup_preserves_hash_mismatch_file () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"opencode"
           ~project_dir:dir
           ~force:false) ;
      let path = Filename.concat dir "opencode.json" in
      let modified = read_file path ^ "// user modification\n" in
      write_file path modified ;
      let setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"opencode"
          ~project_dir:dir
          ~force:false
      in
      (match setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Refused_hash_mismatch _) -> ()
      | _ ->
          Alcotest.fail "expected setup to refuse hash-mismatched opencode.json") ;
      Eio_posix.run @@ fun env ->
      match
        Opencode_cli.ensure_mcp_if_config_applied
          ~env
          ~setup_outcome:setup.Backend_config_gen.write_outcome
          (task_spec_with_mcp dir)
      with
      | Ok () -> Alcotest.fail "expected MCP merge to be blocked after refusal"
      | Error msg ->
          Alcotest.(check bool)
            "error mentions hash mismatch"
            true
            (contains_str msg "hash" || contains_str msg "refused") ;
          Alcotest.(check string)
            "hash-mismatched opencode.json unchanged"
            modified
            (read_file path))

let config_update_tests =
  [
    ( "ensure_mcp preserves managed JSONC header",
      `Quick,
      test_ensure_mcp_preserves_jsonc_managed_header );
    ( "ensure_mcp removes legacy _epure keys",
      `Quick,
      test_ensure_mcp_removes_legacy_epure_keys );
    ( "ensure_mcp parses block comments and trailing commas",
      `Quick,
      test_ensure_mcp_parses_block_comments_and_trailing_commas );
    ( "ensure_mcp parse failure preserves file",
      `Quick,
      test_ensure_mcp_parse_failure_preserves_file );
    ( "ensure_mcp runtime entry has local schema",
      `Quick,
      test_ensure_mcp_runtime_entry_has_local_schema );
    ( "ensure_mcp after legacy migration keeps setup idempotent",
      `Quick,
      test_ensure_mcp_after_legacy_migration_keeps_setup_idempotent );
    ( "ensure_mcp custom namespace marks and stays idempotent",
      `Quick,
      test_ensure_mcp_custom_namespace_marks_and_stays_idempotent );
    ( "ensure_mcp custom namespace marks new file",
      `Quick,
      test_ensure_mcp_custom_namespace_marks_new_file );
    ( "ensure_mcp custom namespace migrates legacy Epure header",
      `Quick,
      test_ensure_mcp_custom_namespace_migrates_legacy_epure_header );
    ( "ensure_mcp blocked after skipped setup preserves user file",
      `Quick,
      test_ensure_mcp_after_skipped_setup_preserves_user_file );
    ( "ensure_mcp blocked after refused setup preserves file",
      `Quick,
      test_ensure_mcp_after_refused_setup_preserves_hash_mismatch_file );
  ]

(** {1 Test Runner} *)

(** {1 Mutation Guard Tests — Story #515} *)

(** {2 AC4/AC5: user_owned_backup_needed predicate} *)

let test_predicate_skipped_user_content () =
  Alcotest.(check bool)
    "Skipped_user_content arms guard"
    true
    (Opencode_cli.user_owned_backup_needed
       (Some (Backend_config_gen.Skipped_user_content "/some/path")))

let test_predicate_already_current () =
  Alcotest.(check bool)
    "Already_current skips guard (AC4)"
    false
    (Opencode_cli.user_owned_backup_needed
       (Some Backend_config_gen.Already_current))

let test_predicate_written () =
  Alcotest.(check bool)
    "Written skips guard (AC4)"
    false
    (Opencode_cli.user_owned_backup_needed
       (Some (Backend_config_gen.Written "/p")))

let test_predicate_refused_hash_mismatch () =
  Alcotest.(check bool)
    "Refused_hash_mismatch skips guard (AC5)"
    false
    (Opencode_cli.user_owned_backup_needed
       (Some (Backend_config_gen.Refused_hash_mismatch "msg")))

let test_predicate_backed_up () =
  Alcotest.(check bool)
    "Backed_up_and_written skips guard (AC4)"
    false
    (Opencode_cli.user_owned_backup_needed
       (Some
          (Backend_config_gen.Backed_up_and_written
             {path = "/p"; backup_path = "/b"})))

let test_predicate_none () =
  Alcotest.(check bool)
    "None skips guard"
    false
    (Opencode_cli.user_owned_backup_needed None)

let predicate_tests =
  [
    ( "Skipped_user_content arms guard",
      `Quick,
      test_predicate_skipped_user_content );
    ("Already_current skips guard (AC4)", `Quick, test_predicate_already_current);
    ("Written skips guard (AC4)", `Quick, test_predicate_written);
    ( "Refused_hash_mismatch skips guard (AC5)",
      `Quick,
      test_predicate_refused_hash_mismatch );
    ("Backed_up_and_written skips guard (AC4)", `Quick, test_predicate_backed_up);
    ("None skips guard", `Quick, test_predicate_none);
  ]

(** {2 AC1/AC3: read_opencode_backup} *)

let test_read_backup_success () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let content = {|{"model":"claude-sonnet","theme":"dark"}|} in
      write_file path content ;
      Eio_posix.run @@ fun env ->
      match Opencode_cli.read_opencode_backup ~env ~config_path:path with
      | Ok bytes ->
          Alcotest.(check string)
            "backup bytes match file content"
            content
            bytes
      | Error msg -> Alcotest.failf "Expected Ok but got Error: %s" msg)

let test_read_backup_failure_missing_file () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "nonexistent.json" in
      Eio_posix.run @@ fun env ->
      match Opencode_cli.read_opencode_backup ~env ~config_path:path with
      | Ok _ -> Alcotest.fail "Expected Error for missing file but got Ok"
      | Error _ -> ())

let backup_tests =
  [
    ("read backup success (AC1)", `Quick, test_read_backup_success);
    ( "read backup error on missing file (AC3)",
      `Quick,
      test_read_backup_failure_missing_file );
  ]

(** {2 AC2: check_opencode_mutation} *)

let test_check_no_mutation () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let content = {|{"model":"claude-sonnet"}|} in
      write_file path content ;
      Eio_posix.run @@ fun env ->
      let mock_result =
        Backend_types.make_task_result
          ~status:Backend_types.Success
          ~stdout:"some output"
          ~session_id:"sess-123"
          ()
      in
      let result =
        Opencode_cli.check_opencode_mutation
          ~env
          ~config_path:path
          ~backup:content
          mock_result
      in
      Alcotest.(check string)
        "stdout unchanged"
        "some output"
        result.Backend_types.stdout ;
      Alcotest.(check (option string))
        "session_id unchanged"
        (Some "sess-123")
        result.Backend_types.session_id)

let test_check_mutation_detected () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original = {|{"model":"claude-sonnet"}|} in
      let mutated = {|{"model":"gpt-4","theme":"dark","extra":"injected"}|} in
      write_file path original ;
      Eio_posix.run @@ fun env ->
      let mock_result =
        Backend_types.make_task_result
          ~status:Backend_types.Success
          ~stdout:"some output"
          ~session_id:"sess-abc"
          ()
      in
      write_file path mutated ;
      let result =
        Opencode_cli.check_opencode_mutation
          ~env
          ~config_path:path
          ~backup:original
          mock_result
      in
      (match result.Backend_types.status with
      | Backend_types.Failed msg ->
          Alcotest.(check bool)
            "failure message mentions mutation"
            true
            (let len = String.length msg and sub = "mutated" in
             let nlen = String.length sub in
             let rec f i =
               i + nlen <= len && (String.sub msg i nlen = sub || f (i + 1))
             in
             f 0)
      | _ -> Alcotest.fail "Expected Failed status") ;
      Alcotest.(check string) "stdout discarded" "" result.Backend_types.stdout ;
      Alcotest.(check (option string))
        "session_id discarded"
        None
        result.Backend_types.session_id ;
      let restored = read_file path in
      Alcotest.(check string)
        "file content restored to original"
        original
        restored)

let test_check_deletion_detected () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original = {|{"model":"claude-sonnet"}|} in
      write_file path original ;
      Eio_posix.run @@ fun env ->
      let mock_result =
        Backend_types.make_task_result
          ~status:Backend_types.Success
          ~stdout:"some output"
          ~session_id:"sess-del"
          ()
      in
      (* Simulate OpenCode deleting the file *)
      Sys.remove path ;
      let result =
        Opencode_cli.check_opencode_mutation
          ~env
          ~config_path:path
          ~backup:original
          mock_result
      in
      match result.Backend_types.status with
      | Backend_types.Failed _ -> ()
      | _ -> Alcotest.fail "Expected Failed status when file is deleted")

let test_check_mutation_restore_fails () =
  with_tmpdir (fun dir ->
      let path = Filename.concat dir "opencode.json" in
      let original = {|{"model":"claude-sonnet"}|} in
      let mutated = {|{"model":"gpt-4","injected":true}|} in
      write_file path original ;
      Eio_posix.run @@ fun env ->
      let mock_result =
        Backend_types.make_task_result
          ~status:Backend_types.Success
          ~stdout:"some output"
          ~session_id:"sess-fail"
          ()
      in
      (* Mutate the file, then make the file itself read-only so save fails *)
      write_file path mutated ;
      Unix.chmod path 0o444 ;
      let result =
        Fun.protect
          ~finally:(fun () -> Unix.chmod path 0o644)
          (fun () ->
            Opencode_cli.check_opencode_mutation
              ~env
              ~config_path:path
              ~backup:original
              mock_result)
      in
      let contains sub s =
        let sub_len = String.length sub and s_len = String.length s in
        let rec loop i =
          i + sub_len <= s_len && (String.sub s i sub_len = sub || loop (i + 1))
        in
        loop 0
      in
      (match result.Backend_types.status with
      | Backend_types.Failed msg ->
          Alcotest.(check bool)
            "failure message mentions restore failed (not falsely 'restored')"
            true
            (contains "restore failed" msg)
      | _ -> Alcotest.fail "Expected Failed status when mutation detected") ;
      Alcotest.(check string) "stdout discarded" "" result.Backend_types.stdout ;
      Alcotest.(check (option string))
        "session_id discarded"
        None
        result.Backend_types.session_id)

let mutation_tests =
  [
    ("no mutation passes result through (AC2)", `Quick, test_check_no_mutation);
    ( "mutation detected: restore and failure (AC2)",
      `Quick,
      test_check_mutation_detected );
    ( "deletion detected: returns Failed (AC2)",
      `Quick,
      test_check_deletion_detected );
    ( "mutation with restore failure: message says restore failed (AC2)",
      `Quick,
      test_check_mutation_restore_fails );
  ]

let () =
  Alcotest.run
    "Opencode_cli"
    [
      ("Identity", identity_tests);
      ("JSON Events", json_events_tests);
      ("Availability", availability_tests);
      ("Command", command_tests);
      ("Interface", interface_tests);
      ("Config Update", config_update_tests);
      ("Mutation Guard Predicate", predicate_tests);
      ("Backup Read", backup_tests);
      ("Mutation Detection", mutation_tests);
    ]
