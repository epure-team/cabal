(** Tests for Session_event_log.

    Covers:
    - file mode (session NDJSON files must be user-only, 0o600)
    - directory mode (must not be world-accessible) *)

let rm_rf path =
  let rec aux p =
    match Unix.lstat p with
    | { st_kind = Unix.S_DIR; _ } ->
      Sys.readdir p |> Array.iter (fun n -> aux (Filename.concat p n)) ;
      Unix.rmdir p
    | _ -> Unix.unlink p
    | exception Unix.Unix_error _ -> ()
  in
  aux path

let with_tmp_dir f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base (Printf.sprintf "cabal-sel-%d" (Unix.getpid ()))
  in
  rm_rf dir ;
  Unix.mkdir dir 0o755 ;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let write_one ~fs ~dir ~session_id =
  Cabal.Session_event_log.write_session_start
    ~fs
    ~session_logs_dir:(Filename.concat dir "sessions")
    ~session_id
    ~backend:"test-backend"
    ~story_id:0
    ~agent_role:"test"
    ()

let test_file_mode_is_user_only () =
  with_tmp_dir (fun dir ->
    let session_id = "mode-test-001" in
    Eio_main.run (fun env ->
      let fs = Eio.Stdenv.fs env in
      write_one ~fs ~dir ~session_id) ;
    let path =
      Filename.concat (Filename.concat dir "sessions") (session_id ^ ".ndjson")
    in
    let perm = (Unix.stat path).Unix.st_perm land 0o777 in
    Alcotest.(check int)
      "session ndjson file must be user-only (0o600)"
      0o600
      perm)

let test_dir_not_world_accessible () =
  with_tmp_dir (fun dir ->
    Eio_main.run (fun env ->
      let fs = Eio.Stdenv.fs env in
      write_one ~fs ~dir ~session_id:"dir-mode-test-001") ;
    let dpath = Filename.concat dir "sessions" in
    let perm = (Unix.stat dpath).Unix.st_perm land 0o777 in
    Alcotest.(check bool)
      "session log dir must not be world-accessible"
      false
      (perm land 0o007 <> 0))

let () =
  Alcotest.run
    "Session_event_log"
    [ ( "file_mode"
      , [ Alcotest.test_case
            "ndjson file is user-only (0o600)"
            `Quick
            test_file_mode_is_user_only
        ; Alcotest.test_case
            "log dir is not world-accessible"
            `Quick
            test_dir_not_world_accessible ] ) ]
