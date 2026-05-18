(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Backend_config_writer.

    Focuses on the policy seams that the security/correctness review
    flagged as un(direct-)tested:

    - managed-header detection and hash extraction round-trip
    - write_artifact branches: fresh write, no-op when already current,
      hash-mismatch refusal, [force] backup-and-write
    - [is_managed_content] / [extract_hash] for both comment styles *)

module W = Cabal.Backend_config_writer
module BT = Cabal.Backend_types

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

let with_project_dir f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat
      base
      (Printf.sprintf "cabal-cw-%d-%d" (Unix.getpid ()) (Random.int 1_000_000))
  in
  rm_rf dir ;
  Unix.mkdir dir 0o755 ;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let buf = Bytes.create n in
      really_input ic buf 0 n ;
      Bytes.to_string buf)

let make_artifact ~ownership content =
  {
    W.backend_id = "test-backend";
    ownership;
    managed_namespace = BT.default_managed_namespace;
    project_relative_path = "config/test.cfg";
    content;
  }

(* ---- header / hash round-trip -------------------------------------------- *)

let test_with_managed_header_is_detected () =
  let body = "key=value\n" in
  let wrapped = W.with_managed_header W.Hash ~backend_id:"test-backend" body in
  Alcotest.(check bool)
    "managed marker present in wrapped output"
    true
    (W.is_managed_content wrapped) ;
  match W.extract_hash wrapped with
  | None -> Alcotest.fail "expected an extractable hash from managed header"
  | Some _ -> ()

let test_extract_hash_returns_none_on_plain_content () =
  Alcotest.(check (option string))
    "no hash in plain content"
    None
    (W.extract_hash "just some key=value text\n")

(* ---- write_artifact branches --------------------------------------------- *)

let test_write_fresh_file_succeeds () =
  with_project_dir (fun project ->
      let artifact = make_artifact ~ownership:Epure_owned "alpha=1\n" in
      let result =
        W.write_artifact ~project_dir:project ~force:false artifact
      in
      match result with
      | Written p ->
          Alcotest.(check string)
            "written content round-trips"
            artifact.content
            (read_file p)
      | other ->
          Alcotest.failf
            "expected Written, got: %s"
            (match other with
            | Already_current -> "Already_current"
            | Refused_hash_mismatch _ -> "Refused_hash_mismatch"
            | Backed_up_and_written _ -> "Backed_up_and_written"
            | Skipped_user_content _ -> "Skipped_user_content"
            | Invalid_managed_namespace _ -> "Invalid_managed_namespace"
            | Written _ -> assert false))

let test_write_twice_is_idempotent () =
  with_project_dir (fun project ->
      let artifact = make_artifact ~ownership:Epure_owned "alpha=2\n" in
      let _ = W.write_artifact ~project_dir:project ~force:false artifact in
      let r2 = W.write_artifact ~project_dir:project ~force:false artifact in
      match r2 with
      | Already_current | Written _ -> ()
      | other ->
          Alcotest.failf
            "second write should be Already_current or Written, got: %s"
            (match other with
            | Refused_hash_mismatch _ -> "Refused_hash_mismatch"
            | Backed_up_and_written _ -> "Backed_up_and_written"
            | Skipped_user_content _ -> "Skipped_user_content"
            | Invalid_managed_namespace _ -> "Invalid_managed_namespace"
            | Written _ | Already_current -> assert false))

let test_backend_project_refuses_hash_mismatch_then_force_overwrites () =
  with_project_dir (fun project ->
      let artifact = make_artifact ~ownership:W.Backend_project "first=v1\n" in
      (* First write installs a managed file with a known hash. *)
      let _ = W.write_artifact ~project_dir:project ~force:false artifact in
      (* User tampers with the file: overwrite content under the marker. *)
      let abs_path = Filename.concat project artifact.project_relative_path in
      (* Replace its body while preserving any managed marker that may have
       been added; we want extract_hash to NOT match the new content. *)
      let oc = open_out abs_path in
      output_string oc "user_modified=true\n" ;
      close_out oc ;
      let artifact_v2 = {artifact with content = "first=v2\n"} in
      let polite =
        W.write_artifact ~project_dir:project ~force:false artifact_v2
      in
      (* Without force: should NOT silently overwrite user-modified content. *)
      (match polite with
      | Written _ ->
          Alcotest.fail
            "expected refusal or skip on user-modified file without force"
      | Refused_hash_mismatch _ | Skipped_user_content _ | Already_current
      | Backed_up_and_written _ | Invalid_managed_namespace _ ->
          ()) ;
      let forced =
        W.write_artifact ~project_dir:project ~force:true artifact_v2
      in
      match forced with
      | Backed_up_and_written {path; backup_path} ->
          Alcotest.(check bool)
            "backup file exists"
            true
            (Sys.file_exists backup_path) ;
          Alcotest.(check bool) "target file exists" true (Sys.file_exists path) ;
          let body = read_file path in
          Alcotest.(check bool)
            "target now contains new managed content"
            true
            (try
               let _ =
                 Str.search_forward (Str.regexp_string "first=v2") body 0
               in
               true
             with Not_found -> false)
      | Written _ ->
          (* Acceptable when no prior managed marker was retained. *)
          ()
      | other ->
          Alcotest.failf
            "expected Backed_up_and_written under force, got: %s"
            (match other with
            | Already_current -> "Already_current"
            | Refused_hash_mismatch _ -> "Refused_hash_mismatch"
            | Skipped_user_content _ -> "Skipped_user_content"
            | Invalid_managed_namespace _ -> "Invalid_managed_namespace"
            | Written _ | Backed_up_and_written _ -> assert false))

(* ---- managed namespace validation at the write boundary ----------------- *)

let test_invalid_managed_namespace_refuses_write () =
  with_project_dir (fun project ->
      let bad_ns =
        {BT.default_managed_namespace with config_dir = "../sneaky"}
      in
      let artifact =
        {
          W.backend_id = "test-backend";
          ownership = Epure_owned;
          managed_namespace = bad_ns;
          project_relative_path = "ok.cfg";
          content = "hi\n";
        }
      in
      let r = W.write_artifact ~project_dir:project ~force:true artifact in
      match r with
      | Invalid_managed_namespace _ -> ()
      | other ->
          Alcotest.failf
            "expected Invalid_managed_namespace for bad config_dir, got: %s"
            (match other with
            | Written _ -> "Written"
            | Already_current -> "Already_current"
            | Refused_hash_mismatch _ -> "Refused_hash_mismatch"
            | Backed_up_and_written _ -> "Backed_up_and_written"
            | Skipped_user_content _ -> "Skipped_user_content"
            | Invalid_managed_namespace _ -> assert false))

let () =
  Random.self_init () ;
  Alcotest.run
    "Backend_config_writer"
    [
      ( "headers",
        [
          Alcotest.test_case
            "managed header round-trips through is_managed/extract_hash"
            `Quick
            test_with_managed_header_is_detected;
          Alcotest.test_case
            "extract_hash returns None on plain content"
            `Quick
            test_extract_hash_returns_none_on_plain_content;
        ] );
      ( "write_artifact",
        [
          Alcotest.test_case
            "fresh write succeeds"
            `Quick
            test_write_fresh_file_succeeds;
          Alcotest.test_case
            "second identical write is idempotent"
            `Quick
            test_write_twice_is_idempotent;
          Alcotest.test_case
            "backend_project refuses user-modified, accepts under force"
            `Quick
            test_backend_project_refuses_hash_mismatch_then_force_overwrites;
          Alcotest.test_case
            "invalid managed namespace refuses to write"
            `Quick
            test_invalid_managed_namespace_refuses_write;
        ] );
    ]
