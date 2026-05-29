(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Session_event_log.

    Covers:
    - file mode (session NDJSON files must be user-only, 0o600)
    - directory mode (must not be world-accessible) *)

let rm_rf path =
  let rec aux p =
    match Unix.lstat p with
    | {st_kind = Unix.S_DIR; _} ->
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
      Eio_posix.run (fun env ->
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

(* ---- Diagnostic surfacing for swallowed errors -------------------------- *)

let with_captured_diagnostics f =
  let events = ref [] in
  let handler ev = events := ev :: !events in
  Cabal.Diagnostics.set_handler handler ;
  Fun.protect
    ~finally:(fun () -> Cabal.Diagnostics.reset_handler ())
    (fun () ->
      f () ;
      List.rev !events)

let warn_count events =
  List.fold_left
    (fun acc ev ->
      match ev with Cabal.Diagnostics.Log (Warn, _) -> acc + 1 | _ -> acc)
    0
    events

let test_write_raw_event_warns_on_malformed_json () =
  with_tmp_dir (fun dir ->
      let events =
        with_captured_diagnostics (fun () ->
            Eio_posix.run (fun env ->
                let fs = Eio.Stdenv.fs env in
                Cabal.Session_event_log.write_raw_event
                  ~fs
                  ~session_logs_dir:(Filename.concat dir "sessions")
                  ~session_id:"raw-warn-test"
                  ~backend:"test-backend"
                  ~turn_number:0
                  "{this is not valid json"))
      in
      Alcotest.(check int)
        "write_raw_event emits one Warn for a malformed line"
        1
        (warn_count events))

let test_read_events_warns_on_malformed_line () =
  with_tmp_dir (fun dir ->
      (* Pre-populate a session log file with one good line, one bad line. *)
      let sessions = Filename.concat dir "sessions" in
      Unix.mkdir sessions 0o755 ;
      let path = Filename.concat sessions "mixed.ndjson" in
      let oc = open_out path in
      output_string oc {|{"type":"session_start","session_id":"mixed"}
|} ;
      output_string oc "{this is not valid json\n" ;
      close_out oc ;
      let events =
        with_captured_diagnostics (fun () ->
            Eio_posix.run (fun env ->
                let fs = Eio.Stdenv.fs env in
                let parsed =
                  Cabal.Session_event_log.read_events
                    ~fs
                    ~session_logs_dir:sessions
                    ~session_id:"mixed"
                    ()
                in
                (* Sanity: the good line is parsed *)
                Alcotest.(check int)
                  "one good line survives"
                  1
                  (List.length parsed)))
      in
      Alcotest.(check int)
        "read_events emits one Warn for the malformed line"
        1
        (warn_count events))

let test_dir_not_world_accessible () =
  with_tmp_dir (fun dir ->
      Eio_posix.run (fun env ->
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
    [
      ( "file_mode",
        [
          Alcotest.test_case
            "ndjson file is user-only (0o600)"
            `Quick
            test_file_mode_is_user_only;
          Alcotest.test_case
            "log dir is not world-accessible"
            `Quick
            test_dir_not_world_accessible;
        ] );
      ( "diagnostics",
        [
          Alcotest.test_case
            "write_raw_event warns on malformed JSON"
            `Quick
            test_write_raw_event_warns_on_malformed_json;
          Alcotest.test_case
            "read_events warns on malformed line"
            `Quick
            test_read_events_warns_on_malformed_line;
        ] );
    ]
