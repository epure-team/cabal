(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the Backend_process shared module. *)

open Cabal

(** {1 MCP Config Tests} *)

let test_write_mcp_config_single_server () =
  Eio_posix.run @@ fun env ->
  let tmp_path = Filename.temp_file "epure-test-bp-mcp-" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp_path with _ -> ())
    (fun () ->
      let config =
        Backend_types.make_mcp_server_config
          ~name:"test-server"
          ~command:"/usr/bin/test-server"
          ~args:["--port"; "8080"]
          ~env:[("API_KEY", "secret")]
          ()
      in
      Backend_process.write_mcp_config ~env ~path:tmp_path [config] ;
      let fs = Eio.Stdenv.fs env in
      let content = Eio.Path.load Eio.Path.(fs / tmp_path) in
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let servers = json |> member "mcpServers" in
      Alcotest.(check bool) "mcpServers exists" true (servers <> `Null) ;
      let server = servers |> member "test-server" in
      Alcotest.(check string)
        "command"
        "/usr/bin/test-server"
        (server |> member "command" |> to_string) ;
      let args = server |> member "args" |> to_list |> List.map to_string in
      Alcotest.(check (list string)) "args" ["--port"; "8080"] args ;
      let env_obj = server |> member "env" in
      Alcotest.(check string)
        "env API_KEY"
        "secret"
        (env_obj |> member "API_KEY" |> to_string))

let test_write_mcp_config_empty () =
  Eio_posix.run @@ fun env ->
  let tmp_path = Filename.temp_file "epure-test-bp-mcp-" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp_path with _ -> ())
    (fun () ->
      Backend_process.write_mcp_config ~env ~path:tmp_path [] ;
      let fs = Eio.Stdenv.fs env in
      let content = Eio.Path.load Eio.Path.(fs / tmp_path) in
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let servers = json |> member "mcpServers" in
      Alcotest.(check bool)
        "mcpServers is empty assoc"
        true
        (servers = `Assoc []))

let mcp_config_tests =
  [
    ("write single server config", `Quick, test_write_mcp_config_single_server);
    ("write empty config", `Quick, test_write_mcp_config_empty);
  ]

(** {1 Setup/Cleanup MCP Config Tests} *)

let test_setup_mcp_config_no_servers () =
  Eio_posix.run @@ fun env ->
  let spec =
    Backend_types.make_task_spec
      ~prompt:"test"
      ~working_dir:"/tmp"
      ~mcp_servers:[]
      ()
  in
  let result = Backend_process.setup_mcp_config ~env spec in
  Alcotest.(check bool) "no config when no servers" true (Option.is_none result)

let test_setup_mcp_config_with_servers () =
  Eio_posix.run @@ fun env ->
  let server =
    Backend_types.make_mcp_server_config ~name:"test" ~command:"test-cmd" ()
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"test"
      ~working_dir:"/tmp"
      ~mcp_servers:[server]
      ()
  in
  let result = Backend_process.setup_mcp_config ~env spec in
  Alcotest.(check bool) "config created" true (Option.is_some result) ;
  (* Clean up *)
  match result with
  | Some path -> Backend_process.cleanup_mcp_config ~env path
  | None -> ()

let test_setup_mcp_config_uses_managed_namespace_prefix () =
  Eio_posix.run @@ fun env ->
  let server =
    Backend_types.make_mcp_server_config ~name:"test" ~command:"test-cmd" ()
  in
  let managed_namespace : Backend_types.managed_namespace =
    {
      id = "crucible";
      display_name = "Crucible";
      config_dir = ".crucible/backend-config";
    }
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:"test"
      ~working_dir:"/tmp"
      ~mcp_servers:[server]
      ~managed_namespace
      ()
  in
  match Backend_process.setup_mcp_config ~env spec with
  | None -> Alcotest.fail "expected MCP config path"
  | Some path ->
      Alcotest.(check bool)
        "temp file uses namespace id prefix"
        true
        (String.starts_with
           ~prefix:(Filename.concat "/tmp" ".crucible-mcp-config-")
           path) ;
      Backend_process.cleanup_mcp_config ~env path

let invalid_namespace : Backend_types.managed_namespace =
  {id = "../bad"; display_name = "Bad"; config_dir = ".epure/backend-config"}

let test_setup_mcp_config_rejects_invalid_namespace_before_temp_file () =
  Eio_posix.run @@ fun env ->
  let server =
    Backend_types.make_mcp_server_config ~name:"test" ~command:"test-cmd" ()
  in
  let spec =
    {
      (Backend_types.make_task_spec
         ~prompt:"test"
         ~working_dir:"/tmp"
         ~mcp_servers:[server]
         ())
      with
      Backend_types.managed_namespace = invalid_namespace;
    }
  in
  try
    ignore (Backend_process.setup_mcp_config ~env spec) ;
    Alcotest.fail "expected invalid namespace to be rejected before temp file"
  with Invalid_argument msg ->
    Alcotest.(check bool) "explicit namespace error" true (String.length msg > 0)

let test_run_task_with_invalid_namespace_fails_before_command () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec =
    {
      (Backend_types.make_task_spec ~prompt:"test" ~working_dir:"/tmp" ()) with
      Backend_types.managed_namespace = invalid_namespace;
    }
  in
  let result =
    Backend_process.run_task_with
      ~sw
      ~env
      ~spec
      ~build_command:(fun ~mcp_config_path:_ _ ->
        Alcotest.fail "build_command must not be called for invalid namespace")
      ()
  in
  match result.Backend_types.status with
  | Backend_types.Failed msg ->
      Alcotest.(check bool)
        "failed before backend command"
        true
        (String.length msg > 0)
  | _ -> Alcotest.fail "expected Failed status for invalid namespace"

let setup_cleanup_tests =
  [
    ( "setup returns None when no servers",
      `Quick,
      test_setup_mcp_config_no_servers );
    ( "setup creates config with servers",
      `Quick,
      test_setup_mcp_config_with_servers );
    ( "setup uses managed namespace prefix",
      `Quick,
      test_setup_mcp_config_uses_managed_namespace_prefix );
    ( "setup rejects invalid namespace before temp file",
      `Quick,
      test_setup_mcp_config_rejects_invalid_namespace_before_temp_file );
    ( "run_task_with rejects invalid namespace before command",
      `Quick,
      test_run_task_with_invalid_namespace_fails_before_command );
  ]

(** {1 Git Diff Tests} *)

let test_get_git_diff_in_git_repo () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let working_dir = Sys.getcwd () in
  let files = Backend_process.get_git_diff ~sw ~env ~working_dir in
  (* Verify all returned paths are non-empty strings.
     Also check each path doesn't contain newlines (well-formed). *)
  List.iter
    (fun f ->
      Alcotest.(check bool)
        (Printf.sprintf "non-empty path: %s" f)
        true
        (String.length f > 0) ;
      Alcotest.(check bool)
        (Printf.sprintf "no newlines in path: %s" f)
        true
        (not (String.contains f '\n')))
    files

let test_get_git_diff_not_git_repo () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let files = Backend_process.get_git_diff ~sw ~env ~working_dir:"/tmp" in
  Alcotest.(check (list string)) "empty for non-git dir" [] files

let test_get_git_diff_includes_untracked () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Create a temporary git repo with an untracked file *)
  let tmp_dir = Filename.temp_dir "epure-test-git-" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" tmp_dir)))
    (fun () ->
      ignore (Sys.command (Printf.sprintf "git init %s" tmp_dir)) ;
      (* Create initial commit so HEAD exists *)
      ignore
        (Sys.command
           (Printf.sprintf
              "cd %s && git config user.email test@test && git config \
               user.name test && touch .keep && git add .keep && git commit -m \
               init"
              tmp_dir)) ;
      (* Create an untracked file *)
      let oc = open_out (Filename.concat tmp_dir "new_file.ml") in
      output_string oc "let x = 42\n" ;
      close_out oc ;
      (* get_git_diff should include the untracked file *)
      let files = Backend_process.get_git_diff ~sw ~env ~working_dir:tmp_dir in
      Alcotest.(check bool)
        "untracked file in list"
        true
        (List.mem "new_file.ml" files) ;
      (* get_git_diff_content should include the file content *)
      let diff =
        Backend_process.get_git_diff_content ~sw ~env ~working_dir:tmp_dir
      in
      Alcotest.(check bool) "diff is non-empty" true (String.length diff > 0) ;
      let has_new_file =
        let pat = "new_file.ml" in
        let plen = String.length pat and dlen = String.length diff in
        let rec find i =
          if i + plen > dlen then false
          else if String.sub diff i plen = pat then true
          else find (i + 1)
        in
        find 0
      in
      Alcotest.(check bool) "diff mentions new_file.ml" true has_new_file)

let git_diff_tests =
  [
    ("git diff in git repo", `Quick, test_get_git_diff_in_git_repo);
    ("git diff in non-git dir", `Quick, test_get_git_diff_not_git_repo);
    ( "git diff includes untracked files",
      `Quick,
      test_get_git_diff_includes_untracked );
  ]

(** {1 Availability Check Tests} *)

let test_check_available_true () =
  Eio_posix.run @@ fun env ->
  (* 'true' is always available on Unix *)
  let result = Backend_process.check_available ~env ["true"] in
  Alcotest.(check bool) "true is available" true result

let test_check_available_false () =
  Eio_posix.run @@ fun env ->
  let result =
    Backend_process.check_available ~env ["nonexistent-command-xyz-12345"]
  in
  Alcotest.(check bool) "nonexistent not available" false result

(** A hanging binary on the PATH (or [sleep 30]) must not freeze the
    availability check: [check_available] should honour [~timeout_seconds]
    and return [false] rather than blocking the host. *)
let test_check_available_times_out_on_hang () =
  if not (Sys.file_exists "/bin/sleep" || Sys.file_exists "/usr/bin/sleep") then
    Alcotest.skip ()
  else
    Eio_posix.run @@ fun env ->
    let started = Unix.gettimeofday () in
    let result =
      Backend_process.check_available ~env ~timeout_seconds:0.5 ["sleep"; "30"]
    in
    let elapsed = Unix.gettimeofday () -. started in
    Alcotest.(check bool)
      "hanging binary reported unavailable on timeout"
      false
      result ;
    Alcotest.(check bool)
      "check returned within ~1s, not 30s"
      true
      (elapsed < 2.0)

let test_capture_version_output_times_out_on_hang () =
  if not (Sys.file_exists "/bin/sleep" || Sys.file_exists "/usr/bin/sleep") then
    Alcotest.skip ()
  else
    Eio_posix.run @@ fun env ->
    let started = Unix.gettimeofday () in
    let result =
      Backend_process.capture_version_output
        ~env
        ~timeout_seconds:0.5
        ["sleep"; "30"]
    in
    let elapsed = Unix.gettimeofday () -. started in
    Alcotest.(check bool)
      "hanging --version probe returns Error on timeout"
      true
      (Result.is_error result) ;
    Alcotest.(check bool)
      "probe returned within ~1s, not 30s"
      true
      (elapsed < 2.0)

let availability_tests =
  [
    ("check_available with true", `Quick, test_check_available_true);
    ("check_available with nonexistent", `Quick, test_check_available_false);
    ( "check_available timeout on hang",
      `Quick,
      test_check_available_times_out_on_hang );
    ( "capture_version_output timeout on hang",
      `Quick,
      test_capture_version_output_times_out_on_hang );
  ]

(** {1 Test Runner} *)

let () =
  Alcotest.run
    "Backend_process"
    [
      ("MCP Config", mcp_config_tests);
      ("Setup/Cleanup", setup_cleanup_tests);
      ("Git Diff", git_diff_tests);
      ("Availability", availability_tests);
    ]
