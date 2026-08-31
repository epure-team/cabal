(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

let png = "\x89PNG\r\n\x1a\npayload"

let jpeg = "\xff\xd8\xff\xe0payload"

let digest content =
  Digestif.SHA256.(to_hex (digest_string content))

let default_limits : Task_preflight.limits =
  {
    max_attachments = 4;
    max_file_size_bytes = 1024;
    max_total_size_bytes = 2048;
  }

let contains haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec loop index =
    index + needle_length <= haystack_length
    && (String.sub haystack index needle_length = needle || loop (index + 1))
  in
  needle_length = 0 || loop 0

let write_file path content =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let remove_tree path =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote path)))

let with_workspace f =
  let workspace = Filename.temp_dir "cabal-preflight-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree workspace)
    (fun () -> f workspace)

let attachment ?(id = "image") ?(path = "image.png") ?(media_type = Backend_types.Png)
    ?sha256 ?size_bytes content : Backend_types.media_attachment =
  {
    id;
    path;
    media_type;
    sha256 = Option.value ~default:(digest content) sha256;
    size_bytes = Option.value ~default:(String.length content) size_bytes;
  }

let spec ?(attachments = []) ?(web_access = Backend_types.Web_disabled)
    ?(read_only = false) ?resume_session_id ?json_schema working_dir =
  Backend_types.make_task_spec
    ~prompt:"test"
    ~working_dir
    ~attachments
    ~web_access
    ~read_only
    ?resume_session_id
    ?json_schema
    ()

let expect_input_error expected result =
  match result with
  | Error (Task_preflight.Input error) when expected error -> ()
  | Error error ->
      Alcotest.failf
        "unexpected preflight error: %s"
        (Task_preflight.render_error error)
  | Ok () -> Alcotest.fail "expected input validation to fail"

let expect_capability_error expected result =
  match result with
  | Error (Task_preflight.Capability error) when expected error -> ()
  | Error error ->
      Alcotest.failf
        "unexpected capability error: %s"
        (Task_preflight.render_error error)
  | Ok () -> Alcotest.fail "expected capability validation to fail"

let check_inputs_ok ~limits task =
  match Task_preflight.validate_inputs ~limits task with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf
        "expected valid inputs, got: %s"
        (Task_preflight.render_error error)

let test_valid_png () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "image.png") png ;
  check_inputs_ok
    ~limits:default_limits
    (spec workspace ~attachments:[attachment png])

let test_valid_jpeg () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "image.jpeg") jpeg ;
  check_inputs_ok
    ~limits:default_limits
    (spec
       workspace
       ~attachments:
         [attachment ~path:"image.jpeg" ~media_type:Backend_types.Jpeg jpeg])

let test_exact_multi_chunk_read_budget () =
  with_workspace @@ fun workspace ->
  let content = png ^ String.make 65536 'x' in
  write_file (Filename.concat workspace "image.png") content ;
  let size = String.length content in
  let limits : Task_preflight.limits =
    {
      max_attachments = 1;
      max_file_size_bytes = size;
      max_total_size_bytes = size;
    }
  in
  check_inputs_ok
    ~limits
    (spec workspace ~attachments:[attachment content])

let test_zero_size_stops_without_probe_read () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "image.png") "" ;
  let zero_limits : Task_preflight.limits =
    {
      max_attachments = 1;
      max_file_size_bytes = 0;
      max_total_size_bytes = 0;
    }
  in
  expect_input_error
    (function Task_preflight.Media_type_mismatch _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:zero_limits
       (spec workspace ~attachments:[attachment ""]))

let test_missing_attachment () =
  with_workspace @@ fun workspace ->
  expect_input_error
    (function Task_preflight.Attachment_missing _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec workspace ~attachments:[attachment png]))

let test_absolute_attachment_path () =
  with_workspace @@ fun workspace ->
  let path = Filename.concat workspace "image.png" in
  write_file path png ;
  expect_input_error
    (function Task_preflight.Absolute_attachment_path _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec workspace ~attachments:[attachment ~path png]))

let test_non_regular_attachment () =
  with_workspace @@ fun workspace ->
  Unix.mkdir (Filename.concat workspace "image.png") 0o700 ;
  expect_input_error
    (function Task_preflight.Attachment_not_regular _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec workspace ~attachments:[attachment png]))

let test_unreadable_attachment () =
  with_workspace @@ fun workspace ->
  let path = Filename.concat workspace "image.png" in
  write_file path png ;
  if Unix.geteuid () = 0 then Alcotest.skip ()
  else
    let original_permissions = (Unix.stat path).st_perm in
    let permissions_changed = ref false in
    Fun.protect
      ~finally:(fun () ->
        if !permissions_changed then Unix.chmod path original_permissions)
      (fun () ->
        let chmod_succeeded =
          try
            Unix.chmod path 0o000 ;
            permissions_changed := true ;
            true
          with Unix.Unix_error _ -> false
        in
        if not chmod_succeeded then Alcotest.skip ()
        else
          let unreadable =
            try
              let descriptor = Unix.openfile path [Unix.O_RDONLY] 0 in
              Unix.close descriptor ;
              false
            with
            | Unix.Unix_error ((Unix.EACCES | Unix.EPERM), _, _) -> true
            | Unix.Unix_error _ -> Alcotest.skip ()
          in
          if not unreadable then Alcotest.skip ()
          else
            expect_input_error
              (function
                | Task_preflight.Attachment_unreadable _ -> true
                | _ -> false)
              (Task_preflight.validate_inputs
                 ~limits:default_limits
                 (spec workspace ~attachments:[attachment png])))

let with_outside_file content f =
  let outside = Filename.temp_file "cabal-preflight-outside-" ".png" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists outside then Sys.remove outside)
    (fun () ->
      write_file outside content ;
      f outside)

let test_parent_path_outside_workspace () =
  with_workspace @@ fun workspace ->
  with_outside_file png @@ fun outside ->
  let path = Filename.concat ".." (Filename.basename outside) in
  expect_input_error
    (function
      | Task_preflight.Attachment_outside_workspace _ -> true
      | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec workspace ~attachments:[attachment ~path png]))

let test_symlink_escape_rejected () =
  with_workspace @@ fun workspace ->
  with_outside_file png @@ fun outside ->
  Unix.symlink outside (Filename.concat workspace "image.png") ;
  expect_input_error
    (function
      | Task_preflight.Attachment_outside_workspace _ -> true
      | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec workspace ~attachments:[attachment png]))

let test_intra_workspace_symlink_accepted () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "target.png") png ;
  Unix.symlink "target.png" (Filename.concat workspace "image.png") ;
  check_inputs_ok
    ~limits:default_limits
    (spec workspace ~attachments:[attachment png])

let test_absolute_intra_workspace_symlink_accepted () =
  with_workspace @@ fun workspace ->
  let target = Filename.concat workspace "target.png" in
  write_file target png ;
  Unix.symlink target (Filename.concat workspace "image.png") ;
  check_inputs_ok
    ~limits:default_limits
    (spec workspace ~attachments:[attachment png])

let same_file_identity first second =
  first.Unix.st_dev = second.Unix.st_dev && first.st_ino = second.st_ino

(* Linux-only race synchronization: observe that the attachment descriptor's
   offset has advanced, stop the validator process, mutate the namespace or
   file, then resume it. This proves the mutation happens after authorization
   and after content streaming has begun without a production test hook. *)
let read_descriptor_position path =
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let rec loop () =
        match input_line channel with
        | line ->
            if String.starts_with ~prefix:"pos:" line then
              Some
                (int_of_string
                   (String.trim
                      (String.sub line 4 (String.length line - 4))))
            else loop ()
        | exception End_of_file -> None
      in
      loop ())

let find_streaming_descriptor ~parent_pid target_stat =
  let descriptor_dir = Printf.sprintf "/proc/%d/fd" parent_pid in
  let descriptor_info_dir = Printf.sprintf "/proc/%d/fdinfo" parent_pid in
  Sys.readdir descriptor_dir
  |> Array.find_map (fun entry ->
         if
           entry = ""
           || not
                (String.for_all
                   (function '0' .. '9' -> true | _ -> false)
                   entry)
         then None
         else
           let descriptor_path = Filename.concat descriptor_dir entry in
           try
             if same_file_identity target_stat (Unix.stat descriptor_path) then
               match
                 read_descriptor_position
                   (Filename.concat descriptor_info_dir entry)
               with
               | Some position when position > 0 -> Some ()
               | Some _ | None -> None
             else None
           with
           | Unix.Unix_error _ | Sys_error _ | Failure _ -> None)

let write_ready descriptor =
  let rec loop () =
    try ignore (Unix.write_substring descriptor "r" 0 1) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let await_ready descriptor =
  let buffer = Bytes.create 1 in
  let rec loop () =
    match Unix.read descriptor buffer 0 1 with
    | 1 -> ()
    | 0 -> Alcotest.fail "stream mutation monitor failed to initialize"
    | _ -> loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let wait_for_stream_progress ~parent_pid target_stat =
  let deadline = Unix.gettimeofday () +. 10.0 in
  let rec loop () =
    match find_streaming_descriptor ~parent_pid target_stat with
    | Some () -> ()
    | None when Unix.gettimeofday () < deadline ->
        Unix.sleepf 0.0005 ;
        loop ()
    | None -> exit 3
  in
  loop ()

let run_during_attachment_stream ~target mutate validate =
  if
    not
      (Sys.file_exists "/proc/self/fd"
      && Sys.file_exists "/proc/self/fdinfo")
  then Alcotest.skip ()
  else
    let target_stat = Unix.stat target in
    let parent_pid = Unix.getpid () in
    let ready_read, ready_write = Unix.pipe ~cloexec:true () in
    match Unix.fork () with
    | 0 ->
        Unix.close ready_read ;
        let parent_stopped = ref false in
        let exit_code =
          try
            write_ready ready_write ;
            Unix.close ready_write ;
            wait_for_stream_progress ~parent_pid target_stat ;
            Unix.kill parent_pid Sys.sigstop ;
            parent_stopped := true ;
            mutate () ;
            Unix.kill parent_pid Sys.sigcont ;
            parent_stopped := false ;
            0
          with _ ->
            if !parent_stopped then Unix.kill parent_pid Sys.sigcont ;
            2
        in
        exit exit_code
    | child_pid ->
        Unix.close ready_write ;
        let child_reaped = ref false in
        Fun.protect
          ~finally:(fun () ->
            Unix.close ready_read ;
            if not !child_reaped then (
              (try Unix.kill child_pid Sys.sigkill
               with Unix.Unix_error (Unix.ESRCH, _, _) -> ()) ;
              ignore (Unix.waitpid [] child_pid)))
          (fun () ->
            await_ready ready_read ;
            let result = validate () in
            let _, status = Unix.waitpid [] child_pid in
            child_reaped := true ;
            (match status with
            | Unix.WEXITED 0 -> ()
            | Unix.WEXITED code ->
                Alcotest.failf
                  "stream mutation monitor exited with status %d"
                  code
            | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
                Alcotest.failf
                  "stream mutation monitor received signal %d"
                  signal) ;
            result)

let streaming_png = png ^ String.make (32 * 1024 * 1024) 'x'

let streaming_png_digest = digest streaming_png

let streaming_limits : Task_preflight.limits =
  {
    max_attachments = 1;
    max_file_size_bytes = String.length streaming_png;
    max_total_size_bytes = String.length streaming_png;
  }

let streaming_attachment () =
  attachment ~sha256:streaming_png_digest streaming_png

let expect_stream_mutation_rejected result =
  expect_input_error
    (function
      | Task_preflight.Attachment_changed_during_validation _ -> true
      | _ -> false)
    result

let test_attachment_ancestor_rename_during_stream_rejected () =
  with_workspace @@ fun workspace ->
  let ancestor = Filename.concat workspace "images" in
  let moved = Filename.concat workspace "moved-images" in
  Unix.mkdir ancestor 0o700 ;
  let target = Filename.concat ancestor "image.png" in
  write_file target streaming_png ;
  run_during_attachment_stream
    ~target
    (fun () -> Unix.rename ancestor moved)
    (fun () ->
      Task_preflight.validate_inputs
        ~limits:streaming_limits
        (spec
           workspace
           ~attachments:
             [attachment
                ~path:"images/image.png"
                ~sha256:streaming_png_digest
                streaming_png]))
  |> expect_stream_mutation_rejected

let test_workspace_rename_during_stream_rejected () =
  with_workspace @@ fun workspace ->
  let moved_workspace = workspace ^ "-moved" in
  Fun.protect
    ~finally:(fun () -> remove_tree moved_workspace)
    (fun () ->
      let target = Filename.concat workspace "image.png" in
      write_file target streaming_png ;
      run_during_attachment_stream
        ~target
        (fun () -> Unix.rename workspace moved_workspace)
        (fun () ->
          Task_preflight.validate_inputs
            ~limits:streaming_limits
            (spec workspace ~attachments:[streaming_attachment ()]))
      |> expect_stream_mutation_rejected)

let test_content_mutation_during_stream_rejected () =
  with_workspace @@ fun workspace ->
  let target = Filename.concat workspace "image.png" in
  write_file target streaming_png ;
  let old_timestamp = Unix.gettimeofday () -. 3600.0 in
  Unix.utimes target old_timestamp old_timestamp ;
  let mutate () =
    let descriptor = Unix.openfile target [Unix.O_WRONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close descriptor)
      (fun () ->
        ignore (Unix.lseek descriptor 8 Unix.SEEK_SET) ;
        ignore (Unix.write_substring descriptor "P" 0 1))
  in
  run_during_attachment_stream
    ~target
    mutate
    (fun () ->
      Task_preflight.validate_inputs
        ~limits:streaming_limits
        (spec workspace ~attachments:[streaming_attachment ()]))
  |> expect_stream_mutation_rejected

let proc_path_for_open_descriptor descriptor_stat =
  let proc_fds = "/proc/self/fd" in
  if not (Sys.file_exists proc_fds) then Alcotest.skip ()
  else
    match
      Sys.readdir proc_fds
      |> Array.to_list
      |> List.find_opt (fun entry ->
             entry <> ""
             && String.for_all (function '0' .. '9' -> true | _ -> false) entry
             &&
             let path = Filename.concat proc_fds entry in
             try same_file_identity descriptor_stat (Unix.stat path)
             with Unix.Unix_error _ -> false)
    with
    | Some entry -> Filename.concat proc_fds entry
    | None -> Alcotest.fail "could not resolve the test attachment descriptor"

let test_deleted_open_descriptor_rejected () =
  with_workspace @@ fun workspace ->
  let target = Filename.concat workspace "target.png" in
  write_file target png ;
  let descriptor = Unix.openfile target [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close descriptor)
    (fun () ->
      let descriptor_stat = Unix.fstat descriptor in
      Sys.remove target ;
      let descriptor_path = proc_path_for_open_descriptor descriptor_stat in
      Unix.symlink descriptor_path (Filename.concat workspace "image.png") ;
      expect_input_error
        (function
          | Task_preflight.Attachment_changed_during_validation _ -> true
          | _ -> false)
        (Task_preflight.validate_inputs
           ~limits:default_limits
           (spec workspace ~attachments:[attachment png])))

let test_workspace_prefix_collision_rejected () =
  with_workspace @@ fun workspace ->
  let sibling = workspace ^ "-outside" in
  Fun.protect
    ~finally:(fun () -> remove_tree sibling)
    (fun () ->
      Unix.mkdir sibling 0o700 ;
      let outside = Filename.concat sibling "outside.png" in
      write_file outside png ;
      Unix.symlink outside (Filename.concat workspace "image.png") ;
      expect_input_error
        (function
          | Task_preflight.Attachment_outside_workspace _ -> true
          | _ -> false)
        (Task_preflight.validate_inputs
           ~limits:default_limits
           (spec workspace ~attachments:[attachment png])))

let test_empty_attachment_id () =
  with_workspace @@ fun workspace ->
  expect_input_error
    (function Task_preflight.Empty_attachment_id -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec workspace ~attachments:[attachment ~id:"" png]))

let test_duplicate_attachment_ids () =
  with_workspace @@ fun workspace ->
  let first = attachment ~id:"same" ~path:"one.png" png in
  let second = attachment ~id:"same" ~path:"two.png" png in
  expect_input_error
    (function Task_preflight.Duplicate_attachment_id _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec workspace ~attachments:[first; second]))

let test_negative_limits_rejected () =
  with_workspace @@ fun workspace ->
  let limits = {default_limits with max_attachments = -1} in
  expect_input_error
    (function Task_preflight.Negative_limit _ -> true | _ -> false)
    (Task_preflight.validate_inputs ~limits (spec workspace))

let test_incoherent_limits_rejected () =
  with_workspace @@ fun workspace ->
  let limits : Task_preflight.limits =
    {
      max_attachments = 1;
      max_file_size_bytes = 10;
      max_total_size_bytes = 5;
    }
  in
  expect_input_error
    (function Task_preflight.Incoherent_limits -> true | _ -> false)
    (Task_preflight.validate_inputs ~limits (spec workspace))

let test_attachment_count_limit () =
  with_workspace @@ fun workspace ->
  let limits = {default_limits with max_attachments = 0} in
  expect_input_error
    (function Task_preflight.Too_many_attachments _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits
       (spec workspace ~attachments:[attachment png]))

let test_per_file_size_limit () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "image.png") png ;
  let limits =
    {default_limits with max_file_size_bytes = String.length png - 1}
  in
  expect_input_error
    (function Task_preflight.Attachment_too_large _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits
       (spec workspace ~attachments:[attachment png]))

let test_total_size_limit () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "one.png") png ;
  write_file (Filename.concat workspace "two.png") png ;
  let attachments =
    [attachment ~id:"one" ~path:"one.png" png; attachment ~id:"two" ~path:"two.png" png]
  in
  let limits =
    {
      default_limits with
      max_file_size_bytes = String.length png;
      max_total_size_bytes = (2 * String.length png) - 1;
    }
  in
  expect_input_error
    (function Task_preflight.Total_size_too_large _ -> true | _ -> false)
    (Task_preflight.validate_inputs ~limits (spec workspace ~attachments))

let test_declared_size_mismatch () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "image.png") png ;
  expect_input_error
    (function Task_preflight.Attachment_size_mismatch _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec
          workspace
          ~attachments:[attachment ~size_bytes:(String.length png + 1) png]))

let test_file_growth_since_declaration () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "image.png") (png ^ "growth") ;
  expect_input_error
    (function Task_preflight.Attachment_size_mismatch _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec workspace ~attachments:[attachment png]))

let test_malformed_digest () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "image.png") png ;
  expect_input_error
    (function Task_preflight.Malformed_sha256 _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec
          workspace
          ~attachments:
            [attachment ~sha256:(String.uppercase_ascii (digest png)) png]))

let test_wrong_digest () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "image.png") png ;
  expect_input_error
    (function Task_preflight.Digest_mismatch _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec
          workspace
          ~attachments:[attachment ~sha256:(String.make 64 '0') png]))

let test_magic_mismatch () =
  with_workspace @@ fun workspace ->
  write_file (Filename.concat workspace "image.png") jpeg ;
  expect_input_error
    (function Task_preflight.Media_type_mismatch _ -> true | _ -> false)
    (Task_preflight.validate_inputs
       ~limits:default_limits
       (spec workspace ~attachments:[attachment jpeg]))

let test_rendered_errors_do_not_leak_paths_or_bytes () =
  with_workspace @@ fun workspace ->
  let secret_path = Filename.concat workspace "private-cover.png" in
  let secret_bytes = "TOP-SECRET-ATTACHMENT-BYTES" in
  let absolute_error =
    Task_preflight.validate_inputs
      ~limits:default_limits
      (spec workspace ~attachments:[attachment ~path:secret_path png])
  in
  let absolute_rendered =
    match absolute_error with
    | Error error -> Task_preflight.render_error error
    | Ok () -> Alcotest.fail "expected absolute path rejection"
  in
  Alcotest.(check bool)
    "absolute path not rendered"
    false
    (contains absolute_rendered secret_path) ;
  write_file (Filename.concat workspace "image.png") secret_bytes ;
  let bytes_error =
    Task_preflight.validate_inputs
      ~limits:default_limits
      (spec workspace ~attachments:[attachment secret_bytes])
  in
  let bytes_rendered =
    match bytes_error with
    | Error error -> Task_preflight.render_error error
    | Ok () -> Alcotest.fail "expected magic mismatch"
  in
  Alcotest.(check bool)
    "attachment bytes not rendered"
    false
    (contains bytes_rendered secret_bytes) ;
  let race_rendered =
    Task_preflight.render_error
      (Task_preflight.Input
         (Task_preflight.Attachment_changed_during_validation secret_path))
  in
  Alcotest.(check bool)
    "raced attachment identifier not rendered"
    false
    (contains race_rendered secret_path)

let base_descriptor () =
  match Backend_registry.find "opencode" with
  | Some descriptor -> descriptor
  | None -> Alcotest.fail "opencode descriptor missing"

let feature_evidence : Backend_types.feature_evidence =
  {
    tested_at_version = "1.2.3";
    test_method = Backend_types.E2e_test;
    evidence_url = None;
    notes = "Reproduced by test/test_task_preflight.ml";
  }

let validate_capabilities descriptor task =
  Task_preflight.validate_capabilities ~descriptor task

let descriptor_with_media_evidence evidence =
  let base = base_descriptor () in
  {
    base with
    capabilities =
      {
        base.capabilities with
        media_support =
          {media_types = [Backend_types.Png]; evidence = Some evidence};
      };
  }

let descriptor_with_web_evidence evidence =
  let base = base_descriptor () in
  {
    base with
    capabilities =
      {
        base.capabilities with
        web_support =
          {maximum = Backend_types.Web_search; evidence = Some evidence};
      };
  }

let malformed_feature_evidence_cases =
  [
    ( "empty tested_at_version",
      {feature_evidence with tested_at_version = " "} );
    ( "malformed tested_at_version",
      {feature_evidence with tested_at_version = "release-latest"} );
    ( "empty E2E notes/test reference",
      {feature_evidence with notes = " "} );
    ( "empty Manual_probe command",
      {
        feature_evidence with
        test_method = Backend_types.Manual_probe " ";
      } );
    ( "empty evidence_url",
      {feature_evidence with evidence_url = Some " "} );
  ]

let valid_feature_evidence_cases =
  [
    ("E2e_test", feature_evidence);
    ( "Manual_probe",
      {
        Backend_types.tested_at_version = "1.2.3";
        test_method =
          Backend_types.Manual_probe "backend --version && backend --probe";
        evidence_url = Some "https://example.test/backend-evidence";
        notes = "Reproduced by the documented manual probe.";
      } );
  ]

let test_malformed_media_feature_evidence () =
  List.iter
    (fun (label, evidence) ->
      let result =
        validate_capabilities
          (descriptor_with_media_evidence evidence)
          (spec "/tmp")
      in
      Alcotest.(check bool)
        (label ^ " rejected for media")
        true
        (match result with
        | Error
            (Task_preflight.Capability
              ( Invalid_media_support_evidence
              | Invalid_media_support_evidence_version )) ->
            true
        | _ -> false))
    malformed_feature_evidence_cases

let test_malformed_web_feature_evidence () =
  List.iter
    (fun (label, evidence) ->
      let result =
        validate_capabilities
          (descriptor_with_web_evidence evidence)
          (spec "/tmp")
      in
      Alcotest.(check bool)
        (label ^ " rejected for web")
        true
        (match result with
        | Error
            (Task_preflight.Capability
              ( Invalid_web_support_evidence
              | Invalid_web_support_evidence_version )) ->
            true
        | _ -> false))
    malformed_feature_evidence_cases

let test_valid_feature_evidence () =
  List.iter
    (fun (label, evidence) ->
      Alcotest.(check bool)
        (label ^ " accepted for media")
        true
        (validate_capabilities
           (descriptor_with_media_evidence evidence)
           (spec "/tmp")
        = Ok ()) ;
      Alcotest.(check bool)
        (label ^ " accepted for web")
        true
        (validate_capabilities
           (descriptor_with_web_evidence evidence)
           (spec "/tmp")
        = Ok ()))
    valid_feature_evidence_cases

let test_positive_media_support_requires_evidence () =
  let base = base_descriptor () in
  let descriptor =
    {
      base with
      capabilities =
        {
          base.capabilities with
          media_support = {media_types = [Backend_types.Png]; evidence = None};
        };
    }
  in
  expect_capability_error
    (function
      | Task_preflight.Media_support_without_evidence -> true
      | _ -> false)
    (validate_capabilities descriptor (spec "/tmp"))

let test_positive_web_support_requires_evidence () =
  let base = base_descriptor () in
  let descriptor =
    {
      base with
      capabilities =
        {
          base.capabilities with
          web_support = {maximum = Backend_types.Web_search; evidence = None};
        };
    }
  in
  expect_capability_error
    (function Task_preflight.Web_support_without_evidence -> true | _ -> false)
    (validate_capabilities descriptor (spec "/tmp"))

let test_native_schema_support_still_requires_evidence () =
  let base = base_descriptor () in
  let descriptor =
    {
      base with
      capabilities =
        {
          base.capabilities with
          native_json_schema_output = true;
          native_json_schema_output_evidence = None;
        };
    }
  in
  expect_capability_error
    (function
      | Task_preflight.Native_json_schema_support_without_evidence -> true
      | _ -> false)
    (validate_capabilities descriptor (spec "/tmp"))

let test_malformed_native_evidence_version_is_rejected () =
  let descriptor =
    match Backend_registry.find "claude-code" with
    | Some descriptor -> descriptor
    | None -> Alcotest.fail "claude-code descriptor missing"
  in
  let evidence =
    match descriptor.capabilities.native_json_schema_output_evidence with
    | Some evidence ->
        {evidence with Backend_types.tested_at_version = "release-latest"}
    | None -> Alcotest.fail "claude-code evidence missing"
  in
  let descriptor =
    {
      descriptor with
      capabilities =
        {
          descriptor.capabilities with
          native_json_schema_output_evidence = Some evidence;
        };
    }
  in
  expect_capability_error
    (function
      | Task_preflight.Invalid_native_json_schema_evidence_version -> true
      | _ -> false)
    (validate_capabilities descriptor (spec "/tmp"))

let test_every_requested_media_type_is_required () =
  let base = base_descriptor () in
  let descriptor =
    {
      base with
      capabilities =
        {
          base.capabilities with
          media_support =
            {media_types = [Backend_types.Png]; evidence = Some feature_evidence};
        };
    }
  in
  let attachments =
    [attachment png; attachment ~id:"jpeg" ~media_type:Backend_types.Jpeg jpeg]
  in
  Alcotest.(check bool)
    "supported media type accepted"
    true
    (validate_capabilities
       descriptor
       (spec "/tmp" ~attachments:[attachment png])
    = Ok ()) ;
  expect_capability_error
    (function Task_preflight.Unsupported_media_type Backend_types.Jpeg -> true | _ -> false)
    (validate_capabilities descriptor (spec "/tmp" ~attachments))

let test_web_access_hierarchy () =
  let base = base_descriptor () in
  let with_maximum maximum =
    {
      base with
      capabilities =
        {
          base.capabilities with
          web_support = {maximum; evidence = Some feature_evidence};
        };
    }
  in
  let fetch_descriptor = with_maximum Backend_types.Web_search_and_fetch in
  Alcotest.(check bool)
    "search accepted by search-and-fetch support"
    true
    (validate_capabilities
       fetch_descriptor
       (spec "/tmp" ~web_access:Backend_types.Web_search)
    = Ok ()) ;
  let search_descriptor = with_maximum Backend_types.Web_search in
  Alcotest.(check bool)
    "search accepted by search support"
    true
    (validate_capabilities
       search_descriptor
       (spec "/tmp" ~web_access:Backend_types.Web_search)
    = Ok ()) ;
  expect_capability_error
    (function Task_preflight.Unsupported_web_access _ -> true | _ -> false)
    (validate_capabilities
       search_descriptor
       (spec "/tmp" ~web_access:Backend_types.Web_search_and_fetch))

let test_read_only_gate () =
  let descriptor = base_descriptor () in
  let supported =
    {
      descriptor with
      capabilities = {descriptor.capabilities with read_only_support = true};
    }
  in
  Alcotest.(check bool)
    "read-only request accepted when supported"
    true
    (validate_capabilities supported (spec "/tmp" ~read_only:true) = Ok ()) ;
  expect_capability_error
    (function Task_preflight.Read_only_unsupported -> true | _ -> false)
    (validate_capabilities descriptor (spec "/tmp" ~read_only:true))

let test_session_resume_gate () =
  let descriptor = base_descriptor () in
  let supported =
    {
      descriptor with
      capabilities = {descriptor.capabilities with session_resume = true};
    }
  in
  Alcotest.(check bool)
    "session resume accepted when supported"
    true
    (validate_capabilities
       supported
       (spec "/tmp" ~resume_session_id:"session-1")
    = Ok ()) ;
  expect_capability_error
    (function Task_preflight.Session_resume_unsupported -> true | _ -> false)
    (validate_capabilities
       descriptor
       (spec "/tmp" ~resume_session_id:"session-1"))

let test_non_native_json_schema_uses_fallback () =
  let descriptor = base_descriptor () in
  let schema = `Assoc [("type", `String "object")] in
  Alcotest.(check bool)
    "non-native JSON schema does not fail capability preflight"
    true
    (validate_capabilities descriptor (spec "/tmp" ~json_schema:schema) = Ok ())

let input_tests =
  [
    ("valid PNG", `Quick, test_valid_png);
    ("valid JPEG", `Quick, test_valid_jpeg);
    ("exact multi-chunk read budget", `Quick, test_exact_multi_chunk_read_budget);
    ("zero-size input stops without probe", `Quick, test_zero_size_stops_without_probe_read);
    ("missing attachment", `Quick, test_missing_attachment);
    ("absolute attachment path", `Quick, test_absolute_attachment_path);
    ("non-regular attachment", `Quick, test_non_regular_attachment);
    ("unreadable attachment", `Quick, test_unreadable_attachment);
    ("parent path outside workspace", `Quick, test_parent_path_outside_workspace);
    ("absolute symlink outside rejected", `Quick, test_symlink_escape_rejected);
    ("relative symlink inside accepted", `Quick, test_intra_workspace_symlink_accepted);
    ("absolute symlink inside accepted", `Quick, test_absolute_intra_workspace_symlink_accepted);
    ("deleted opened descriptor rejected", `Quick, test_deleted_open_descriptor_rejected);
    ("attachment ancestor rename during stream", `Quick, test_attachment_ancestor_rename_during_stream_rejected);
    ("workspace rename during stream", `Quick, test_workspace_rename_during_stream_rejected);
    ("content mutation during stream", `Quick, test_content_mutation_during_stream_rejected);
    ("workspace prefix collision rejected", `Quick, test_workspace_prefix_collision_rejected);
    ("empty attachment ID", `Quick, test_empty_attachment_id);
    ("duplicate attachment IDs", `Quick, test_duplicate_attachment_ids);
    ("negative limits rejected", `Quick, test_negative_limits_rejected);
    ("incoherent limits rejected", `Quick, test_incoherent_limits_rejected);
    ("attachment count limit", `Quick, test_attachment_count_limit);
    ("per-file size limit", `Quick, test_per_file_size_limit);
    ("total size limit", `Quick, test_total_size_limit);
    ("declared size mismatch", `Quick, test_declared_size_mismatch);
    ("file growth since declaration", `Quick, test_file_growth_since_declaration);
    ("malformed digest", `Quick, test_malformed_digest);
    ("wrong digest", `Quick, test_wrong_digest);
    ("magic mismatch", `Quick, test_magic_mismatch);
    ("rendered errors are sanitized", `Quick, test_rendered_errors_do_not_leak_paths_or_bytes);
  ]

let capability_tests =
  [
    ("positive media support requires evidence", `Quick, test_positive_media_support_requires_evidence);
    ("positive web support requires evidence", `Quick, test_positive_web_support_requires_evidence);
    ("malformed media feature evidence", `Quick, test_malformed_media_feature_evidence);
    ("malformed web feature evidence", `Quick, test_malformed_web_feature_evidence);
    ("valid feature evidence", `Quick, test_valid_feature_evidence);
    ("native schema support still requires evidence", `Quick, test_native_schema_support_still_requires_evidence);
    ( "malformed native evidence version is rejected",
      `Quick,
      test_malformed_native_evidence_version_is_rejected );
    ("every requested media type is required", `Quick, test_every_requested_media_type_is_required);
    ("web access hierarchy", `Quick, test_web_access_hierarchy);
    ("read-only gate", `Quick, test_read_only_gate);
    ("session-resume gate", `Quick, test_session_resume_gate);
    ("non-native JSON schema fallback", `Quick, test_non_native_json_schema_uses_fallback);
  ]

let () =
  Alcotest.run
    "Task_preflight"
    [("input validation", input_tests); ("capability validation", capability_tests)]
