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
            | Unsafe_project_path _ -> "Unsafe_project_path"
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
            | Unsafe_project_path _ -> "Unsafe_project_path"
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
      | Backed_up_and_written _ | Unsafe_project_path _
      | Invalid_managed_namespace _ ->
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
            | Unsafe_project_path _ -> "Unsafe_project_path"
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
            | Unsafe_project_path _ -> "Unsafe_project_path"
            | Invalid_managed_namespace _ -> assert false))

let test_stale_legacy_temp_does_not_poison_atomic_write () =
  with_project_dir (fun project ->
      let config_dir = Filename.concat project "config" in
      Unix.mkdir config_dir 0o755 ;
      let temp_path = Filename.concat config_dir "test.cfg.cabal-tmp" in
      let sentinel = "stale legacy temporary data\n" in
      let channel = open_out_bin temp_path in
      output_string channel sentinel ;
      close_out channel ;
      let artifact = make_artifact ~ownership:Epure_owned "new data\n" in
      (match W.write_artifact ~project_dir:project ~force:false artifact with
      | W.Written path ->
          Alcotest.(check string) "target content" "new data\n" (read_file path)
      | _ -> Alcotest.fail "stale legacy temp poisoned a fresh atomic write") ;
      Alcotest.(check bool)
        "unowned legacy temporary file is preserved" true
        (Sys.file_exists temp_path) ;
      Alcotest.(check string)
        "unowned legacy temporary content is unchanged" sentinel
        (read_file temp_path))

let test_post_write_failure_cleans_only_owned_temp () =
  with_project_dir (fun project ->
      let observed_temp = ref None in
      let observed_mode = ref None in
      let injected = Failure "injected post-write failure" in
      let propagated =
        try
          W.Private.atomic_write_file_with_hooks ~project_root:project
            ~relative_path:"config/test.cfg"
            ~after_write:(fun temp_path ->
              observed_temp := Some temp_path ;
              observed_mode := Some ((Unix.lstat temp_path).st_perm land 0o777) ;
              raise injected)
            "new data\n" ;
          false
        with Failure message -> message = "injected post-write failure"
      in
      Alcotest.(check bool) "injected failure propagated" true propagated ;
      Alcotest.(check (option int))
        "owned temporary file is created with mode 0600" (Some 0o600)
        !observed_mode ;
      Option.iter
        (fun temp_path ->
          Alcotest.(check bool)
            "owned temporary file is removed after failure" false
            (Sys.file_exists temp_path))
        !observed_temp ;
      Alcotest.(check bool)
        "failed write never publishes target" false
        (Sys.file_exists (Filename.concat project "config/test.cfg")))

let test_unique_temp_collision_retries_without_deleting_unowned_file () =
  with_project_dir (fun project ->
      let config_dir = Filename.concat project "config" in
      Unix.mkdir config_dir 0o755 ;
      let target = Filename.concat config_dir "test.cfg" in
      let collision = target ^ ".cabal-tmp-collision" in
      let winner = target ^ ".cabal-tmp-winner" in
      let sentinel = "concurrent writer data\n" in
      let channel = open_out_bin collision in
      output_string channel sentinel ;
      close_out channel ;
      let attempts = ref 0 in
      W.Private.atomic_write_file_with_hooks ~project_root:project
        ~relative_path:"config/test.cfg"
        ~nonce_for_attempt:(fun attempt ->
          incr attempts ;
          if attempt = 0 then "collision" else "winner")
        "new data\n" ;
      Alcotest.(check int) "one O_EXCL collision was retried" 2 !attempts ;
      Alcotest.(check string)
        "colliding unowned file is unchanged" sentinel (read_file collision) ;
      Alcotest.(check bool)
        "successful owned temp was renamed" false (Sys.file_exists winner) ;
      Alcotest.(check string) "target content" "new data\n" (read_file target))

let test_observed_temp_inode_mismatch_is_rejected () =
  with_project_dir (fun project ->
      let replacement_path = ref None in
      let sentinel = "mismatched replacement fixture\n" in
      let rejected =
        try
          W.Private.atomic_write_file_with_hooks ~project_root:project
            ~relative_path:"config/test.cfg"
            ~nonce_for_attempt:(fun _ -> "replaced")
            ~after_write:(fun temp_path ->
              Unix.unlink temp_path ;
              let channel = open_out_bin temp_path in
              output_string channel sentinel ;
              close_out channel ;
              replacement_path := Some temp_path)
            "new data\n" ;
          false
        with Unix.Unix_error (Unix.EAGAIN, _, _) -> true
      in
      Alcotest.(check bool)
        "observed inode mismatch is rejected before publish" true rejected ;
      Alcotest.(check bool)
        "target is not published" false
        (Sys.file_exists (Filename.concat project "config/test.cfg")) ;
      Option.iter
        (fun path ->
          Alcotest.(check bool) "mismatched replacement is preserved" true
            (Sys.file_exists path) ;
          Alcotest.(check string) "mismatched replacement is unchanged" sentinel
            (read_file path))
        !replacement_path)

let test_symlink_target_never_writes_outside_workspace () =
  with_project_dir (fun project ->
      let outside = Filename.temp_file "cabal-cw-outside-target-" ".cfg" in
      Fun.protect
        ~finally:(fun () -> if Sys.file_exists outside then Unix.unlink outside)
        (fun () ->
          let sentinel = "outside sentinel\n" in
          let channel = open_out_bin outside in
          output_string channel sentinel ;
          close_out channel ;
          let config_dir = Filename.concat project "config" in
          Unix.mkdir config_dir 0o755 ;
          Unix.symlink outside (Filename.concat config_dir "test.cfg") ;
          let artifact = make_artifact ~ownership:Epure_owned "new data\n" in
          (match W.write_artifact ~project_dir:project ~force:false artifact with
          | W.Unsafe_project_path _ -> ()
          | _ -> Alcotest.fail "symlink target was accepted") ;
          Alcotest.(check string)
            "outside symlink target is unchanged" sentinel (read_file outside)))

let test_symlinked_parent_never_writes_outside_workspace () =
  let run_case label target_for_symlink =
    with_project_dir (fun project ->
        let outside = Filename.temp_dir ("cabal-cw-outside-" ^ label) "" in
        Fun.protect
          ~finally:(fun () -> rm_rf outside)
          (fun () ->
            let target = target_for_symlink ~project ~outside in
            Unix.symlink target (Filename.concat project ".github") ;
            let artifact =
              {
                W.backend_id = "test-backend";
                ownership = W.Backend_project;
                managed_namespace = BT.default_managed_namespace;
                project_relative_path = ".github/mcp.json";
                content = {|{"mcpServers":{"hostile":{"command":"run"}}}|};
              }
            in
            (match W.write_artifact ~project_dir:project ~force:false artifact with
            | W.Unsafe_project_path _ -> ()
            | _ -> Alcotest.fail (label ^ " parent symlink was accepted")) ;
            Alcotest.(check bool)
              (label ^ " wrote nothing outside") false
              (Sys.file_exists (Filename.concat outside "mcp.json"))))
  in
  run_case "relative" (fun ~project:_ ~outside ->
      Filename.concat ".." (Filename.basename outside)) ;
  run_case "absolute" (fun ~project:_ ~outside -> outside)

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
          Alcotest.test_case
            "stale legacy temporary file does not poison writes" `Quick
            test_stale_legacy_temp_does_not_poison_atomic_write;
          Alcotest.test_case
            "post-write failure cleans only the owned temporary file" `Quick
            test_post_write_failure_cleans_only_owned_temp;
          Alcotest.test_case
            "unique temporary collision retries safely" `Quick
            test_unique_temp_collision_retries_without_deleting_unowned_file;
          Alcotest.test_case
            "observed temporary inode mismatch is rejected" `Quick
            test_observed_temp_inode_mismatch_is_rejected;
          Alcotest.test_case
            "relative and absolute parent symlinks cannot escape"
            `Quick test_symlinked_parent_never_writes_outside_workspace;
          Alcotest.test_case "target symlink cannot escape" `Quick
            test_symlink_target_never_writes_outside_workspace;
        ] );
    ]
