(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type limits = {
  max_attachments : int;
  max_file_size_bytes : int;
  max_total_size_bytes : int;
}

type limit_name = Max_attachments | Max_file_size_bytes | Max_total_size_bytes

type input_error =
  | Negative_limit of limit_name
  | Incoherent_limits
  | Too_many_attachments of { maximum : int; actual : int }
  | Empty_attachment_id
  | Duplicate_attachment_id of string
  | Absolute_attachment_path of string
  | Workspace_unavailable
  | Attachment_missing of string
  | Attachment_outside_workspace of string
  | Attachment_not_regular of string
  | Attachment_unreadable of string
  | Attachment_changed_during_validation of string
  | Attachment_staging_failed
  | Attachment_cleanup_failed
  | Attachment_size_mismatch of {
      attachment_id : string;
      declared : int;
      actual : int;
    }
  | Attachment_too_large of {
      attachment_id : string;
      maximum : int;
      actual : int;
    }
  | Total_size_too_large of { maximum : int; actual : int }
  | Malformed_sha256 of string
  | Digest_mismatch of string
  | Media_type_mismatch of {
      attachment_id : string;
      media_type : Backend_types.media_type;
    }

type capability_error =
  | Native_json_schema_support_without_evidence
  | Native_json_schema_evidence_without_support
  | Invalid_native_json_schema_evidence
  | Invalid_native_json_schema_evidence_version
  | Media_support_without_evidence
  | Web_support_without_evidence
  | Invalid_media_support_evidence
  | Invalid_web_support_evidence
  | Invalid_media_support_evidence_version
  | Invalid_web_support_evidence_version
  | Unsupported_media_type of Backend_types.media_type
  | Unsupported_web_access of {
      requested : Backend_types.web_access;
      maximum : Backend_types.web_access;
    }
  | Read_only_unsupported
  | Session_resume_unsupported

type error = Input of input_error | Capability of capability_error

let render_input_error = function
  | Negative_limit Max_attachments ->
      "attachment count limit must be nonnegative"
  | Negative_limit Max_file_size_bytes ->
      "per-file attachment size limit must be nonnegative"
  | Negative_limit Max_total_size_bytes ->
      "total attachment size limit must be nonnegative"
  | Incoherent_limits ->
      "per-file attachment size limit must not exceed the total size limit"
  | Too_many_attachments { maximum; actual } ->
      Printf.sprintf
        "attachment count exceeds caller limit (maximum %d, actual %d)" maximum
        actual
  | Empty_attachment_id -> "attachment ID must be non-empty"
  | Duplicate_attachment_id _ -> "attachment IDs must be unique"
  | Absolute_attachment_path _ ->
      "attachment path must be relative to the workspace"
  | Workspace_unavailable ->
      "workspace cannot be resolved as a readable directory"
  | Attachment_missing _ -> "attachment target does not exist"
  | Attachment_outside_workspace _ ->
      "attachment target resolves outside the workspace"
  | Attachment_not_regular _ -> "attachment target must be a regular file"
  | Attachment_unreadable _ -> "attachment target is not readable"
  | Attachment_changed_during_validation _ ->
      "attachment changed during validation"
  | Attachment_staging_failed ->
      "attachment could not be sealed for backend transport"
  | Attachment_cleanup_failed ->
      "sealed attachment transport artifacts could not be removed"
  | Attachment_size_mismatch { declared; actual; _ } ->
      Printf.sprintf
        "attachment declared size does not match the file (declared %d, actual \
         %d)"
        declared actual
  | Attachment_too_large { maximum; actual; _ } ->
      Printf.sprintf
        "attachment exceeds per-file size limit (maximum %d, actual %d)" maximum
        actual
  | Total_size_too_large { maximum; actual } ->
      Printf.sprintf
        "attachments exceed total size limit (maximum %d, actual %d)" maximum
        actual
  | Malformed_sha256 _ ->
      "attachment SHA-256 must be 64 lowercase hexadecimal characters"
  | Digest_mismatch _ -> "attachment SHA-256 does not match the file"
  | Media_type_mismatch { media_type = Backend_types.Png; _ } ->
      "attachment content does not have the declared PNG signature"
  | Media_type_mismatch { media_type = Backend_types.Jpeg; _ } ->
      "attachment content does not have the declared JPEG signature"

let render_capability_error = function
  | Native_json_schema_support_without_evidence ->
      "backend descriptor claims native JSON schema output without evidence"
  | Native_json_schema_evidence_without_support ->
      "backend descriptor carries native JSON schema evidence for disabled \
       support"
  | Invalid_native_json_schema_evidence ->
      "backend native JSON schema evidence is incomplete or not reproducible"
  | Invalid_native_json_schema_evidence_version ->
      "backend native JSON schema evidence has an invalid tested version"
  | Media_support_without_evidence ->
      "backend descriptor claims media support without evidence"
  | Web_support_without_evidence ->
      "backend descriptor claims web support without evidence"
  | Invalid_media_support_evidence ->
      "backend media evidence is incomplete or not reproducible"
  | Invalid_web_support_evidence ->
      "backend web evidence is incomplete or not reproducible"
  | Invalid_media_support_evidence_version ->
      "backend media evidence has an invalid tested version"
  | Invalid_web_support_evidence_version ->
      "backend web evidence has an invalid tested version"
  | Unsupported_media_type Backend_types.Png ->
      "backend does not support requested PNG attachments"
  | Unsupported_media_type Backend_types.Jpeg ->
      "backend does not support requested JPEG attachments"
  | Unsupported_web_access _ ->
      "backend does not support the requested web access level"
  | Read_only_unsupported ->
      "backend does not support the requested read-only mode"
  | Session_resume_unsupported ->
      "backend does not support the requested session resume"

let render_error = function
  | Input error -> render_input_error error
  | Capability error -> render_capability_error error

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let validate_limits limits =
  if limits.max_attachments < 0 then
    Error (Input (Negative_limit Max_attachments))
  else if limits.max_file_size_bytes < 0 then
    Error (Input (Negative_limit Max_file_size_bytes))
  else if limits.max_total_size_bytes < 0 then
    Error (Input (Negative_limit Max_total_size_bytes))
  else if
    limits.max_attachments > 0
    && limits.max_file_size_bytes > 0
    && limits.max_total_size_bytes > 0
    && limits.max_file_size_bytes > limits.max_total_size_bytes
  then Error (Input Incoherent_limits)
  else Ok ()

let validate_count limits attachments =
  let actual = List.length attachments in
  if actual > limits.max_attachments then
    Error
      (Input (Too_many_attachments { maximum = limits.max_attachments; actual }))
  else Ok ()

let validate_ids attachments =
  let rec loop seen = function
    | [] -> Ok ()
    | attachment :: rest ->
        if attachment.Backend_types.id = "" then
          Error (Input Empty_attachment_id)
        else if List.mem attachment.id seen then
          Error (Input (Duplicate_attachment_id attachment.id))
        else loop (attachment.id :: seen) rest
  in
  loop [] attachments

let validate_relative_paths attachments =
  match
    List.find_opt
      (fun attachment ->
        not (Filename.is_relative attachment.Backend_types.path))
      attachments
  with
  | None -> Ok ()
  | Some attachment -> Error (Input (Absolute_attachment_path attachment.id))

external openat_readonly : Unix.file_descr -> string -> Unix.file_descr
  = "cabal_task_preflight_openat"

external descriptor_path : Unix.file_descr -> string
  = "cabal_task_preflight_descriptor_path"

type workspace = { descriptor : Unix.file_descr; resolved_path : string }

type prepared_attachment = {
  reference : Backend_types.media_attachment;
  staged_path : string;
}

type cleanup_state = Cleanup_pending | Cleanup_in_progress | Cleanup_complete

type prepared_inputs = {
  staging_directory : string option;
  staged_attachments : prepared_attachment list;
  transport_revoked : bool Atomic.t;
  cleanup_state : cleanup_state Atomic.t;
  on_cleanup_attempt : unit -> unit;
}

type staging_hooks = {
  on_staging_directory : string -> unit;
  on_staged_file : string -> Unix.file_descr -> unit;
  on_cleanup_attempt : unit -> unit;
}

let no_staging_hooks =
  {
    on_staging_directory = (fun _ -> ());
    on_staged_file = (fun _ _ -> ());
    on_cleanup_attempt = (fun () -> ());
  }

let deleted_path_suffix = " (deleted)"

let descriptor_path_is_deleted path =
  String.ends_with ~suffix:deleted_path_suffix path

let with_workspace_descriptor working_dir f =
  let descriptor =
    try
      Ok
        (Unix.openfile working_dir
           [ Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK ]
           0)
    with Unix.Unix_error _ | Invalid_argument _ ->
      Error (Input Workspace_unavailable)
  in
  let* descriptor = descriptor in
  let close_descriptor () =
    try Unix.close descriptor with Unix.Unix_error _ -> ()
  in
  Fun.protect ~finally:close_descriptor (fun () ->
      try
        let stat = Unix.fstat descriptor in
        if stat.Unix.st_kind <> Unix.S_DIR || stat.st_nlink = 0 then
          Error (Input Workspace_unavailable)
        else
          let resolved_path = descriptor_path descriptor in
          if
            Filename.is_relative resolved_path
            || descriptor_path_is_deleted resolved_path
          then Error (Input Workspace_unavailable)
          else f { descriptor; resolved_path }
      with Unix.Unix_error _ -> Error (Input Workspace_unavailable))

let path_is_contained ~workspace candidate =
  candidate = workspace
  ||
  let prefix =
    if workspace = Filename.dir_sep then workspace
    else workspace ^ Filename.dir_sep
  in
  String.starts_with ~prefix candidate

let canonical_sha256 value =
  String.length value = 64
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       value

type streamed_attachment = {
  digest : string;
  magic : string;
  observed_size : int;
}

let staging_failed = Error (Input Attachment_staging_failed)

let size_mismatch attachment actual =
  Error
    (Input
       (Attachment_size_mismatch
          {
            attachment_id = attachment.Backend_types.id;
            declared = attachment.size_bytes;
            actual;
          }))

let attachment_too_large ~limits attachment actual =
  Error
    (Input
       (Attachment_too_large
          {
            attachment_id = attachment.Backend_types.id;
            maximum = limits.max_file_size_bytes;
            actual;
          }))

let saturating_add left right =
  if right > max_int - left then max_int else left + right

let total_size_too_large ~limits ~total observed =
  Error
    (Input
       (Total_size_too_large
          {
            maximum = limits.max_total_size_bytes;
            actual = saturating_add total observed;
          }))

let remaining_allowance ~limit ~used = if used >= limit then 0 else limit - used

let next_read_budget ~buffer_capacity ~declared_size ~per_file_limit
    ~remaining_total ~observed_size =
  min buffer_capacity
    (min
       (remaining_allowance ~limit:declared_size ~used:observed_size)
       (min
          (remaining_allowance ~limit:per_file_limit ~used:observed_size)
          (remaining_allowance ~limit:remaining_total ~used:observed_size)))

let read_digest_and_magic ~limits ~total attachment descriptor staged_channel =
  let buffer = Bytes.create 65536 in
  let magic = Bytes.create 8 in
  let remaining_total = limits.max_total_size_bytes - total in
  let rec read length =
    try Ok (Unix.read descriptor buffer 0 length) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> read length
    | Unix.Unix_error _ ->
        Error (Input (Attachment_unreadable attachment.Backend_types.id))
  in
  let stage count =
    try
      output staged_channel buffer 0 count;
      Ok ()
    with Sys_error _ -> staging_failed
  in
  let rec loop context magic_length observed_size =
    if observed_size = attachment.Backend_types.size_bytes then
      Ok
        {
          digest = Digestif.SHA256.(to_hex (get context));
          magic = Bytes.sub_string magic 0 magic_length;
          observed_size;
        }
    else
      let budget =
        next_read_budget ~buffer_capacity:(Bytes.length buffer)
          ~declared_size:attachment.size_bytes
          ~per_file_limit:limits.max_file_size_bytes ~remaining_total
          ~observed_size
      in
      if budget = 0 then
        if attachment.size_bytes > limits.max_file_size_bytes then
          attachment_too_large ~limits attachment attachment.size_bytes
        else if attachment.size_bytes > remaining_total then
          total_size_too_large ~limits ~total attachment.size_bytes
        else size_mismatch attachment observed_size
      else
        let* count = read budget in
        if count = 0 then size_mismatch attachment observed_size
        else
          let* () = stage count in
          let observed_size = saturating_add observed_size count in
          if observed_size > attachment.size_bytes then
            size_mismatch attachment observed_size
          else if observed_size > limits.max_file_size_bytes then
            attachment_too_large ~limits attachment observed_size
          else if observed_size > remaining_total then
            total_size_too_large ~limits ~total observed_size
          else
            let copy_count = min count (Bytes.length magic - magic_length) in
            if copy_count > 0 then
              Bytes.blit buffer 0 magic magic_length copy_count;
            loop
              (Digestif.SHA256.feed_bytes context ~off:0 ~len:count buffer)
              (magic_length + copy_count)
              observed_size
  in
  loop Digestif.SHA256.empty 0 0

let has_expected_magic media_type magic =
  match media_type with
  | Backend_types.Png -> String.starts_with ~prefix:"\x89PNG\r\n\x1a\n" magic
  | Backend_types.Jpeg -> String.starts_with ~prefix:"\xff\xd8\xff" magic

let same_file_identity first second =
  first.Unix.st_dev = second.Unix.st_dev && first.st_ino = second.st_ino

let same_open_file_state first second =
  same_file_identity first second
  && first.Unix.st_kind = second.Unix.st_kind
  && first.st_nlink = second.st_nlink
  && first.st_size = second.st_size
  && first.st_mtime = second.st_mtime
  && first.st_ctime = second.st_ctime

let attachment_changed attachment =
  Error
    (Input (Attachment_changed_during_validation attachment.Backend_types.id))

let media_suffix = function
  | Backend_types.Png -> ".png"
  | Backend_types.Jpeg -> ".jpg"

let close_staged_channel ?expected_size ~path channel =
  try
    flush channel;
    let descriptor = Unix.descr_of_out_channel channel in
    Unix.fchmod descriptor 0o600;
    Option.iter
      (fun expected_size ->
        let stat = Unix.fstat descriptor in
        if
          stat.Unix.st_kind <> Unix.S_REG
          || stat.st_nlink <> 1
          || stat.st_size <> expected_size
          || descriptor_path descriptor <> path
        then raise (Sys_error "sealed attachment changed"))
      expected_size;
    close_out channel;
    Ok ()
  with Sys_error _ | Unix.Unix_error _ ->
    close_out_noerr channel;
    staging_failed

let create_staged_file ~hooks ~staging_directory attachment =
  let opened = ref None in
  let cleanup_opened () =
    Option.iter
      (fun (path, channel) ->
        close_out_noerr channel;
        try Sys.remove path with Sys_error _ -> ())
      !opened
  in
  try
    let path, channel =
      Filename.open_temp_file ~mode:[ Open_binary ] ~perms:0o600
        ~temp_dir:staging_directory "attachment-"
        (media_suffix attachment.Backend_types.media_type)
    in
    opened := Some (path, channel);
    Unix.fchmod (Unix.descr_of_out_channel channel) 0o600;
    hooks.on_staged_file path (Unix.descr_of_out_channel channel);
    opened := None;
    Ok (path, channel)
  with
  | (Out_of_memory | Stack_overflow | Sys.Break) as fatal ->
      cleanup_opened ();
      raise fatal
  | _ ->
      cleanup_opened ();
      staging_failed

let open_attachment workspace attachment =
  try
    Ok (openat_readonly workspace.descriptor attachment.Backend_types.path)
  with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR | Unix.ELOOP), _, _) ->
      Error (Input (Attachment_missing attachment.id))
  | Unix.Unix_error _ | Invalid_argument _ ->
      Error (Input (Attachment_unreadable attachment.id))

let authorize_open_attachment ~workspace attachment descriptor =
  try
    let current_workspace_path = descriptor_path workspace.descriptor in
    if
      current_workspace_path <> workspace.resolved_path
      || descriptor_path_is_deleted current_workspace_path
    then attachment_changed attachment
    else
      let attachment_path = descriptor_path descriptor in
      if descriptor_path_is_deleted attachment_path then
        attachment_changed attachment
      else if
        path_is_contained ~workspace:workspace.resolved_path attachment_path
      then Ok attachment_path
      else Error (Input (Attachment_outside_workspace attachment.id))
  with Unix.Unix_error _ | Invalid_argument _ -> attachment_changed attachment

let validate_authorized_paths ~workspace ~authorized_attachment_path attachment
    descriptor =
  try
    let current_workspace_path = descriptor_path workspace.descriptor in
    let current_attachment_path = descriptor_path descriptor in
    if
      current_workspace_path <> workspace.resolved_path
      || descriptor_path_is_deleted current_workspace_path
      || current_attachment_path <> authorized_attachment_path
      || descriptor_path_is_deleted current_attachment_path
      || not
           (path_is_contained ~workspace:workspace.resolved_path
              current_attachment_path)
    then attachment_changed attachment
    else Ok ()
  with Unix.Unix_error _ | Invalid_argument _ -> attachment_changed attachment

let validate_open_attachment ~limits ~total ~workspace ~staging_directory ~hooks
    attachment =
  let* descriptor = open_attachment workspace attachment in
  let close_descriptor () =
    try Unix.close descriptor with Unix.Unix_error _ -> ()
  in
  Fun.protect ~finally:close_descriptor (fun () ->
      try
        let* authorized_attachment_path =
          authorize_open_attachment ~workspace attachment descriptor
        in
        let initial_stat = Unix.fstat descriptor in
        if initial_stat.st_nlink = 0 then attachment_changed attachment
        else if initial_stat.Unix.st_kind <> Unix.S_REG then
          Error (Input (Attachment_not_regular attachment.id))
        else
          let actual = initial_stat.st_size in
          if attachment.size_bytes <> actual then
            size_mismatch attachment actual
          else if actual > limits.max_file_size_bytes then
            attachment_too_large ~limits attachment actual
          else if actual > limits.max_total_size_bytes - total then
            total_size_too_large ~limits ~total actual
          else if not (canonical_sha256 attachment.sha256) then
            Error (Input (Malformed_sha256 attachment.id))
          else
            let* staged_path, staged_channel =
              create_staged_file ~hooks ~staging_directory attachment
            in
            let streamed =
              read_digest_and_magic ~limits ~total attachment descriptor
                staged_channel
            in
            let close_result =
              match streamed with
              | Ok streamed ->
                  close_staged_channel ~expected_size:streamed.observed_size
                    ~path:staged_path staged_channel
              | Error _ -> close_staged_channel ~path:staged_path staged_channel
            in
            let* streamed = streamed in
            let* () = close_result in
            let final_stat = Unix.fstat descriptor in
            if
              (not (same_open_file_state initial_stat final_stat))
              || final_stat.st_size <> streamed.observed_size
            then attachment_changed attachment
            else
              let* () =
                validate_authorized_paths ~workspace ~authorized_attachment_path
                  attachment descriptor
              in
              if streamed.digest <> attachment.sha256 then
                Error (Input (Digest_mismatch attachment.id))
              else if
                not (has_expected_magic attachment.media_type streamed.magic)
              then
                Error
                  (Input
                     (Media_type_mismatch
                        {
                          attachment_id = attachment.id;
                          media_type = attachment.media_type;
                        }))
              else
                Ok
                  ( total + streamed.observed_size,
                    { reference = attachment; staged_path } )
      with Unix.Unix_error _ ->
        Error (Input (Attachment_unreadable attachment.id)))

let validate_attachments ~limits ~workspace ~staging_directory ~hooks
    attachments =
  let rec loop total staged = function
    | [] -> Ok (List.rev staged)
    | attachment :: rest ->
        let* total, prepared =
          validate_open_attachment ~limits ~total ~workspace ~staging_directory
            ~hooks attachment
        in
        loop total (prepared :: staged) rest
  in
  loop 0 [] attachments

let remove_file path =
  try
    Unix.unlink path;
    Ok ()
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  | Unix.Unix_error _ -> Error ()

let remove_directory path =
  try
    Unix.rmdir path;
    Ok ()
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  | Unix.Unix_error _ -> Error ()

let cleanup_staged_paths staging_directory staged_attachments =
  let files_removed =
    List.fold_left
      (fun removed attachment ->
        Result.is_ok (remove_file attachment.staged_path) && removed)
      true staged_attachments
  in
  let directory_removed = Result.is_ok (remove_directory staging_directory) in
  if files_removed && directory_removed then Ok ()
  else Error (Input Attachment_cleanup_failed)

let cleanup_staging_directory staging_directory =
  let entries_removed =
    try
      Sys.readdir staging_directory
      |> Array.fold_left
           (fun removed name ->
             Result.is_ok (remove_file (Filename.concat staging_directory name))
             && removed)
           true
    with Sys_error _ -> false
  in
  let directory_removed = Result.is_ok (remove_directory staging_directory) in
  if entries_removed && directory_removed then Ok ()
  else Error (Input Attachment_cleanup_failed)

let rec release_inputs prepared =
  Atomic.set prepared.transport_revoked true;
  match Atomic.get prepared.cleanup_state with
  | Cleanup_complete -> Ok ()
  | Cleanup_in_progress -> Error (Input Attachment_cleanup_failed)
  | Cleanup_pending ->
      if
        not
          (Atomic.compare_and_set prepared.cleanup_state Cleanup_pending
             Cleanup_in_progress)
      then release_inputs prepared
      else
        let result =
          try
            prepared.on_cleanup_attempt ();
            match prepared.staging_directory with
            | None -> Ok ()
            | Some staging_directory ->
                cleanup_staged_paths staging_directory
                  prepared.staged_attachments
          with
          | (Out_of_memory | Stack_overflow | Sys.Break) as fatal ->
              Atomic.set prepared.cleanup_state Cleanup_pending;
              raise fatal
          | _ -> Error (Input Attachment_cleanup_failed)
        in
        match result with
        | Ok () ->
            Atomic.set prepared.cleanup_state Cleanup_complete;
            Ok ()
        | Error _ as error ->
            Atomic.set prepared.cleanup_state Cleanup_pending;
            error

let create_staging_directory ~workspace ~hooks =
  let created = ref None in
  let cleanup_created () =
    Option.iter (fun path -> ignore (cleanup_staging_directory path)) !created
  in
  try
    let path = Filename.temp_dir ~perms:0o700 "cabal-task-inputs-" "" in
    created := Some path;
    Unix.chmod path 0o700;
    let descriptor =
      Unix.openfile path [ Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK ] 0
    in
    let resolved_path =
      Fun.protect
        ~finally:(fun () -> Unix.close descriptor)
        (fun () -> descriptor_path descriptor)
    in
    if path_is_contained ~workspace:workspace.resolved_path resolved_path then begin
      ignore (remove_directory resolved_path);
      created := None;
      staging_failed
    end
    else begin
      hooks.on_staging_directory resolved_path;
      created := None;
      Ok resolved_path
    end
  with
  | (Out_of_memory | Stack_overflow | Sys.Break) as fatal ->
      cleanup_created ();
      raise fatal
  | _ ->
      cleanup_created ();
      staging_failed

let prepare_inputs_with_hooks ~hooks ~limits spec =
  let attachments = spec.Backend_types.attachments in
  let* () = validate_limits limits in
  let* () = validate_count limits attachments in
  let* () = validate_ids attachments in
  let* () = validate_relative_paths attachments in
  if attachments = [] then
    with_workspace_descriptor spec.working_dir (fun _workspace ->
        Ok
          {
            staging_directory = None;
            staged_attachments = [];
            transport_revoked = Atomic.make false;
            cleanup_state = Atomic.make Cleanup_pending;
            on_cleanup_attempt = hooks.on_cleanup_attempt;
          })
  else
    with_workspace_descriptor spec.working_dir (fun workspace ->
        let* staging_directory = create_staging_directory ~workspace ~hooks in
        let result =
          try
            validate_attachments ~limits ~workspace ~staging_directory ~hooks
              attachments
          with error ->
            ignore (cleanup_staging_directory staging_directory);
            raise error
        in
        match result with
        | Ok staged_attachments ->
            Ok
              {
                staging_directory = Some staging_directory;
                staged_attachments;
                transport_revoked = Atomic.make false;
                cleanup_state = Atomic.make Cleanup_pending;
                on_cleanup_attempt = hooks.on_cleanup_attempt;
              }
        | Error _ as error -> (
            match cleanup_staging_directory staging_directory with
            | Ok () -> error
            | Error cleanup_error -> Error cleanup_error))

let prepare_inputs ~limits spec =
  prepare_inputs_with_hooks ~hooks:no_staging_hooks ~limits spec

let validate_inputs ~limits spec =
  let* prepared = prepare_inputs ~limits spec in
  release_inputs prepared

let nonempty value = String.trim value <> ""

let valid_feature_evidence (evidence : Backend_types.feature_evidence) =
  nonempty evidence.notes
  && (match evidence.evidence_url with
    | None -> true
    | Some url -> nonempty url)
  &&
  match evidence.test_method with
  | Backend_types.E2e_test -> true
  | Backend_types.Manual_probe command -> nonempty command

let valid_native_schema_evidence (evidence : Backend_types.capability_evidence)
    =
  nonempty evidence.json_schema_draft
  &&
  match evidence.test_method with
  | Backend_types.E2e_test -> true
  | Backend_types.Manual_probe command -> nonempty command

let validate_descriptor_evidence (capabilities : Backend_registry.capabilities)
    =
  let* () =
    if capabilities.Backend_registry.native_json_schema_output then
      match capabilities.native_json_schema_output_evidence with
      | None -> Error (Capability Native_json_schema_support_without_evidence)
      | Some evidence
        when not
               (Backend_version.is_valid_version_string
                  evidence.Backend_types.tested_at_version) ->
          Error (Capability Invalid_native_json_schema_evidence_version)
      | Some evidence when not (valid_native_schema_evidence evidence) ->
          Error (Capability Invalid_native_json_schema_evidence)
      | Some _ -> Ok ()
    else if Option.is_some capabilities.native_json_schema_output_evidence then
      Error (Capability Native_json_schema_evidence_without_support)
    else Ok ()
  in
  let media_support = capabilities.media_support in
  if media_support.media_types <> [] && media_support.evidence = None then
    Error (Capability Media_support_without_evidence)
  else
    match media_support.evidence with
    | Some evidence
      when not
             (Backend_version.is_valid_version_string
                evidence.Backend_types.tested_at_version) ->
        Error (Capability Invalid_media_support_evidence_version)
    | Some evidence when not (valid_feature_evidence evidence) ->
        Error (Capability Invalid_media_support_evidence)
    | _ -> (
        let web_support = capabilities.web_support in
        if
          web_support.maximum <> Backend_types.Web_disabled
          && web_support.evidence = None
        then Error (Capability Web_support_without_evidence)
        else
          match web_support.evidence with
          | Some evidence
            when not
                   (Backend_version.is_valid_version_string
                      evidence.Backend_types.tested_at_version) ->
              Error (Capability Invalid_web_support_evidence_version)
          | Some evidence when not (valid_feature_evidence evidence) ->
              Error (Capability Invalid_web_support_evidence)
          | _ -> Ok ())

let validate_descriptor descriptor =
  validate_descriptor_evidence descriptor.Backend_registry.capabilities

let web_access_rank = function
  | Backend_types.Web_disabled -> 0
  | Backend_types.Web_search -> 1
  | Backend_types.Web_search_and_fetch -> 2

let validate_requested_media capabilities attachments =
  match
    List.find_opt
      (fun attachment ->
        not
          (List.mem attachment.Backend_types.media_type
             capabilities.Backend_registry.media_support.media_types))
      attachments
  with
  | None -> Ok ()
  | Some attachment ->
      Error (Capability (Unsupported_media_type attachment.media_type))

let validate_capabilities ~descriptor spec =
  let capabilities = descriptor.Backend_registry.capabilities in
  let* () = validate_descriptor descriptor in
  let* () =
    validate_requested_media capabilities spec.Backend_types.attachments
  in
  if
    web_access_rank spec.web_access
    > web_access_rank capabilities.web_support.maximum
  then
    Error
      (Capability
         (Unsupported_web_access
            {
              requested = spec.web_access;
              maximum = capabilities.web_support.maximum;
            }))
  else if spec.read_only && not capabilities.read_only_support then
    Error (Capability Read_only_unsupported)
  else if
    Option.is_some spec.resume_session_id && not capabilities.session_resume
  then Error (Capability Session_resume_unsupported)
  else Ok ()

module Private = struct
  let staged_attachments prepared =
    List.map
      (fun attachment -> (attachment.reference, attachment.staged_path))
      prepared.staged_attachments

  let staging_directory prepared = prepared.staging_directory
  let active prepared = not (Atomic.get prepared.transport_revoked)

  let prepare_inputs_with_hooks ?(on_staging_directory = fun _ -> ())
      ?(on_staged_file = fun _ _ -> ()) ?(on_cleanup_attempt = fun () -> ())
      ~limits spec =
    prepare_inputs_with_hooks
      ~hooks:{ on_staging_directory; on_staged_file; on_cleanup_attempt }
      ~limits spec
end
