(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Backend_types

type process_result = {
  status : result_status;
  stdout : string;
  stderr : string;
  exit_code : int;
  elapsed : duration;
  cost : cost option;
  session_id : string option;
}

let shell_quote_argv argv = argv |> String.concat " "

let invalid_managed_namespace_result msg =
  make_task_result ~status:(Failed msg) ~stderr:msg ~exit_code:1 ()

let validate_task_namespace (spec : task_spec) =
  match validate_managed_namespace spec.managed_namespace with
  | Ok () -> None
  | Error msg -> Some (invalid_managed_namespace_result msg)

(* Run a command and capture its stdout as a string.
   Returns [Ok output] when stdout is non-empty regardless of exit code;
   returns [Error _] only when the binary is not found or produces no output.
   This is the version-detection primitive: callers should never crash on
   a non-zero exit from a --version command. *)

(* Write MCP server configuration to a JSON file *)
let write_mcp_config ~env ~path configs =
  let fs = Eio.Stdenv.fs env in
  let servers =
    List.map
      (fun (cfg : mcp_server_config) ->
        let env_obj =
          `Assoc (List.map (fun (k, v) -> (k, `String v)) cfg.env)
        in
        ( cfg.name,
          `Assoc
            [
              ("command", `String cfg.command);
              ("args", `List (List.map (fun a -> `String a) cfg.args));
              ("env", env_obj);
            ] ))
      configs
  in
  let json = `Assoc [("mcpServers", `Assoc servers)] in
  let content = Yojson.Safe.pretty_to_string json in
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(fs / path) content

(* Create a temporary MCP config file if needed.
   Uses a unique filename to avoid collisions from concurrent runs. *)
let setup_mcp_config ~env spec =
  match spec.mcp_servers with
  | [] -> None
  | servers ->
      (match validate_managed_namespace spec.managed_namespace with
      | Ok () -> ()
      | Error msg -> invalid_arg msg) ;
      let path =
        Filename.temp_file
          ~temp_dir:spec.working_dir
          ("." ^ spec.managed_namespace.id ^ "-mcp-config-")
          ".json"
      in
      write_mcp_config ~env ~path servers ;
      Some path

(* Clean up temporary MCP config file *)
let cleanup_mcp_config ~env path =
  let fs = Eio.Stdenv.fs env in
  try Eio.Path.unlink Eio.Path.(fs / path) with _ -> ()

(* Run a git command and return stdout, or empty string on error. *)
let run_git ~env ~working_dir args =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  try
    let stdout_buf = Buffer.create 4096 in
    let stderr_buf = Buffer.create 64 in
    Eio.Process.run
      proc_mgr
      ~cwd:Eio.Path.(fs / working_dir)
      ~stdout:(Eio.Flow.buffer_sink stdout_buf)
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      ("git" :: args) ;
    Buffer.contents stdout_buf
  with Eio.Io _ -> ""

(* Split output into non-empty lines. *)
let lines_of_output output =
  output |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length (String.trim s) > 0)

(** Pathspec exclusions appended to every [git diff] call. These directories are
    build artefacts that should never appear in the diff shown to review agents
    — they inflate context, cause context-window overflows, and contain no
    meaningful code changes. The ":!" pathspec magic works for both commit-range
    diffs and HEAD diffs. *)
let diff_exclude_pathspecs =
  [
    ":(exclude)node_modules";
    ":(exclude)_build";
    ":(exclude).vite";
    ":(exclude)dist";
    ":(exclude)vendor";
  ]

(* Get list of changed files: tracked changes + untracked new files. *)
let get_git_diff ~sw:_ ~env ~working_dir =
  let tracked =
    run_git
      ~env
      ~working_dir
      (["diff"; "--name-only"; "HEAD"; "--"] @ diff_exclude_pathspecs)
  in
  let untracked =
    run_git ~env ~working_dir ["ls-files"; "--others"; "--exclude-standard"]
  in
  let tracked_files = lines_of_output tracked in
  let untracked_files = lines_of_output untracked in
  tracked_files @ untracked_files

(* Generate a unified diff for an untracked file by diffing /dev/null against it.
   git diff --no-index exits with code 1 when files differ, so we
   capture output even on non-zero exit. *)
let diff_untracked_file ~env ~working_dir path =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let stdout_buf = Buffer.create 4096 in
  let stderr_buf = Buffer.create 64 in
  (try
     Eio.Process.run
       proc_mgr
       ~cwd:Eio.Path.(fs / working_dir)
       ~stdout:(Eio.Flow.buffer_sink stdout_buf)
       ~stderr:(Eio.Flow.buffer_sink stderr_buf)
       ["git"; "diff"; "--no-index"; "--"; "/dev/null"; path]
   with Eio.Io _ ->
     (* git diff --no-index exits 1 when files differ — expected *) ()) ;
  Buffer.contents stdout_buf

(* Get full diff content: tracked changes + untracked file contents as
   pseudo-diffs so that reviewers can see new file content. *)
let get_git_diff_content ~sw:_ ~env ~working_dir =
  let tracked_diff =
    run_git ~env ~working_dir (["diff"; "HEAD"; "--"] @ diff_exclude_pathspecs)
  in
  let untracked =
    run_git ~env ~working_dir ["ls-files"; "--others"; "--exclude-standard"]
  in
  let untracked_files = lines_of_output untracked in
  if untracked_files = [] then tracked_diff
  else
    let untracked_diffs =
      List.filter_map
        (fun path ->
          let d = diff_untracked_file ~env ~working_dir path in
          if String.length d > 0 then Some d else None)
        untracked_files
    in
    let parts =
      (if String.length tracked_diff > 0 then [tracked_diff] else [])
      @ untracked_diffs
    in
    String.concat "\n" parts

let close_noerr flow = try Eio.Flow.close flow with _ -> ()

let without_trailing_cr line =
  let length = String.length line in
  if length > 0 && line.[length - 1] = '\r' then String.sub line 0 (length - 1)
  else line

(* Read a flow in chunks so timeout/cancellation never discards bytes that were
   already delivered by Eio. Streaming callbacks retain the existing line-based
   contract while the captured buffer retains the original bytes verbatim. *)
let read_output ?on_stdout ~max_bytes flow output =
  let pending_line = Buffer.create 128 in
  let emit_completed_lines chunk =
    match on_stdout with
    | None -> ()
    | Some callback ->
        let start = ref 0 in
        let length = String.length chunk in
        for index = 0 to length - 1 do
          if chunk.[index] = '\n' then begin
            Buffer.add_substring pending_line chunk !start (index - !start) ;
            callback (without_trailing_cr (Buffer.contents pending_line)) ;
            Buffer.clear pending_line ;
            start := index + 1
          end
        done ;
        if !start < length then
          Buffer.add_substring pending_line chunk !start (length - !start)
  in
  let chunk_buffer = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read flow chunk_buffer with
    | count ->
        if Buffer.length output + count > max_bytes then
          raise Eio.Buf_read.Buffer_limit_exceeded ;
        let chunk = Cstruct.to_string (Cstruct.sub chunk_buffer 0 count) in
        Buffer.add_string output chunk ;
        emit_completed_lines chunk ;
        loop ()
    | exception End_of_file -> (
        match on_stdout with
        | Some callback when Buffer.length pending_line > 0 ->
            callback (without_trailing_cr (Buffer.contents pending_line))
        | _ -> ())
  in
  loop ()

let cleanup_drain_timeout_seconds = 3.0

let handshake_failure_message = function
  | Process_group.Established _ -> None
  | Process_group.Timed_out ->
      Some "Backend launch handshake timed out before EXEC confirmation"
  | Process_group.Invalid message ->
      Some ("Backend launch handshake invalid before EXEC confirmation: " ^ message)
  | Process_group.Launcher_failed {message; _} ->
      Some ("Backend launcher failed before EXEC confirmation: " ^ message)

let drain_output ~clock ~max_bytes flow output =
  Eio.Cancel.protect (fun () ->
      try
        ignore
          (Eio.Time.with_timeout clock cleanup_drain_timeout_seconds (fun () ->
               read_output ~max_bytes flow output ;
               Ok ()))
      with _ -> ())

(* Run a subprocess and capture output with timeout. Stderr is captured and
   returned in process_result for the caller to display appropriately (toast in
   TUI, print in CLI). If on_stdout is provided, it is called for each line of
   stdout as it arrives. *)
let run_process ~sw ~env ~cmd ?(stdin_content = None) ~working_dir
    ~timeout_seconds ?parse_cost ?on_stdout ?guardian () =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let start_time = Eio.Time.now clock in
  let stdout_r, stdout_w = Eio_unix.pipe sw in
  let stderr_r, stderr_w = Eio_unix.pipe sw in
  let stdin_r, stdin_w =
    match stdin_content with
    | Some _ ->
        let reader, writer = Eio_unix.pipe sw in
        (Some reader, Some writer)
    | None -> (None, None)
  in
  let stdout_buf = Buffer.create 4096 in
  let stderr_buf = Buffer.create 1024 in
  let target = ref None in
  let guardian_registered = ref false in
  let released = ref false in
  let cleanup () =
    Option.iter
      (fun process_group ->
        if !guardian_registered then
          Option.iter
            (fun g -> Resource_guardian.unregister_target g process_group)
            guardian ;
        guardian_registered := false ;
        if not !released then begin
          (* Start draining before TERM so a handler that emits a full pipe can
             finish and exit during the launcher's bounded grace period. The
             three fibres are cancel-protected and each pipe has exactly one
             reader: the timeout's I/O fibres have already been cancelled. *)
          Eio.Cancel.protect (fun () ->
              Eio.Fiber.all
                [
                  (fun () ->
                    try Process_group.terminate ~clock process_group
                    with _ -> ());
                  (fun () ->
                    drain_output
                      ~clock
                      ~max_bytes:(128 * 1024 * 1024)
                      stdout_r
                      stdout_buf);
                  (fun () ->
                    drain_output
                      ~clock
                      ~max_bytes:(16 * 1024 * 1024)
                      stderr_r
                      stderr_buf);
                ])
        end)
      !target ;
    close_noerr stdout_r ;
    close_noerr stdout_w ;
    close_noerr stderr_r ;
    close_noerr stderr_w ;
    Option.iter close_noerr stdin_r ;
    Option.iter close_noerr stdin_w
  in
  let result = ref None in
  let timeout_result =
    Fun.protect ~finally:cleanup (fun () ->
        let process_group =
          Process_group.spawn
            ~sw
            ~clock
            ~mgr:proc_mgr
            ~cwd:Eio.Path.(fs / working_dir)
            ?stdin:stdin_r
            ~stdout:stdout_w
            ~stderr:stderr_w
            cmd
        in
        target := Some process_group ;
        (* The child owns these ends after spawn. *)
        close_noerr stdout_w ;
        close_noerr stderr_w ;
        Option.iter close_noerr stdin_r ;
        match handshake_failure_message (Process_group.handshake process_group) with
        | Some message -> Error (`Handshake_failed message)
        | None ->
            Option.iter
              (fun g ->
                Resource_guardian.register_target g process_group ;
                guardian_registered := true)
              guardian ;
            let io_result =
              Eio.Time.with_timeout clock timeout_seconds (fun () ->
                  Eio.Fiber.all
                    [
                      (fun () ->
                        match (stdin_content, stdin_w) with
                        | Some content, Some writer ->
                            Fun.protect
                              ~finally:(fun () -> close_noerr writer)
                              (fun () ->
                                try Eio.Flow.copy_string content writer
                                with Eio.Io _ -> ())
                        | _ -> ());
                      (fun () ->
                        read_output
                          ?on_stdout
                          ~max_bytes:(128 * 1024 * 1024)
                          stdout_r
                          stdout_buf);
                      (fun () ->
                        (* Stderr is deliberately not streamed to avoid corrupting a
                            TUI. The captured bytes are returned to the caller. *)
                        read_output
                          ~max_bytes:(16 * 1024 * 1024)
                          stderr_r
                          stderr_buf);
                      (fun () ->
                        result := Some (Process_group.await_backend process_group));
                    ] ;
                  Ok ())
            in
            match io_result with
            | Error `Timeout -> Error `Timeout
            | Ok () ->
                (* This is the normal-completion commit boundary. Once all I/O and
                   backend status have completed, RELEASE and supervisor reaping are
                   cancel-protected and cannot be reclassified as a task timeout. *)
                Eio.Cancel.protect (fun () ->
                    Process_group.release process_group ;
                    ignore (Process_group.await process_group) ;
                    released := true ;
                    Ok ()))
  in
  let elapsed_seconds = Eio.Time.now clock -. start_time in
  let stdout_str = Buffer.contents stdout_buf in
  let stderr_str = Buffer.contents stderr_buf in
  let elapsed = duration_of_seconds elapsed_seconds in
  match timeout_result with
  | Error `Timeout ->
      {
        status = Timeout;
        stdout = stdout_str;
        stderr = stderr_str;
        exit_code = -1;
        elapsed;
        cost = None;
        session_id = None;
      }
  | Error (`Handshake_failed message) ->
      {
        status = Failed message;
        stdout = stdout_str;
        stderr = stderr_str;
        exit_code = -1;
        elapsed;
        cost = None;
        session_id = None;
      }
  | Ok () -> (
      match !result with
      | Some (`Exited 0) ->
          let cost =
            match parse_cost with Some f -> f stdout_str | None -> None
          in
          {
            status = Success;
            stdout = stdout_str;
            stderr = stderr_str;
            exit_code = 0;
            elapsed;
            cost;
            session_id = None;
          }
      | Some (`Exited n) ->
          {
            status = Failed (Printf.sprintf "Process exited with code %d" n);
            stdout = stdout_str;
            stderr = stderr_str;
            exit_code = n;
            elapsed;
            cost = None;
            session_id = None;
          }
      | Some (`Signaled n) ->
          {
            status = Failed (Printf.sprintf "Process killed by signal %d" n);
            stdout = stdout_str;
            stderr = stderr_str;
            exit_code = 128 + n;
            elapsed;
            cost = None;
            session_id = None;
          }
      | None ->
          {
            status = Failed "Unknown error";
            stdout = stdout_str;
            stderr = stderr_str;
            exit_code = -1;
            elapsed;
            cost = None;
            session_id = None;
          })

(** Default cap for version / availability probes. Five seconds is long enough
    for cold-start CLIs (large Node binaries on slow disks) but short enough
    that a hung backend cannot freeze registry initialisation for the whole
    host. *)
let default_probe_timeout_seconds = 5.0

let run_probe ~env ~timeout_seconds cmd =
  Eio.Switch.run @@ fun sw ->
  run_process ~sw ~env ~cmd ~working_dir:(Sys.getcwd ()) ~timeout_seconds ()

type version_probe_result = {
  command_available : bool;
  output : string option;
  timed_out : bool;
}

let probe_version_command ~env
    ?(timeout_seconds = default_probe_timeout_seconds) cmd =
  try
    let result = run_probe ~env ~timeout_seconds cmd in
    let timed_out = result.status = Timeout in
    let output =
      if timed_out then None
      else if result.stdout <> "" then Some result.stdout
      else if result.stderr <> "" then Some result.stderr
      else None
    in
    {command_available = result.status = Success; output; timed_out}
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Out_of_memory | Stack_overflow | Sys.Break as exn -> raise exn
  | _ -> {command_available = false; output = None; timed_out = false}

let capture_version_output ~env ?timeout_seconds cmd =
  let result = probe_version_command ~env ?timeout_seconds cmd in
  if result.timed_out then
    Error (Printf.sprintf "timeout running %s" (String.concat " " cmd))
  else
    match result.output with
    | Some output -> Ok output
    | None -> Error (Printf.sprintf "no output from %s" (String.concat " " cmd))

let check_available ~env ?(timeout_seconds = default_probe_timeout_seconds) cmd
    =
  (probe_version_command ~env ~timeout_seconds cmd).command_available

(* Standard task execution flow shared by all backends.
   Uses Fun.protect to ensure MCP config cleanup even on exception. *)
let run_task_with ~sw ~env ~spec ~build_command ?parse_cost ?parse_stdout
    ?parse_session_id ?on_stdout ?guardian () =
  match validate_managed_namespace spec.managed_namespace with
  | Error msg -> invalid_managed_namespace_result msg
  | Ok () ->
      let mcp_config_path = setup_mcp_config ~env spec in
      Fun.protect
        ~finally:(fun () ->
          Option.iter (cleanup_mcp_config ~env) mcp_config_path)
        (fun () ->
          let cmd, stdin_content = build_command ~mcp_config_path spec in
          Diagnostics.debug "backend command: %s" (shell_quote_argv cmd) ;
          let timeout_seconds = duration_to_seconds spec.timeout in
          let result =
            run_process
              ~sw
              ~env
              ~cmd
              ~stdin_content:(Some stdin_content)
              ~working_dir:spec.working_dir
              ~timeout_seconds
              ?parse_cost
              ?on_stdout
              ?guardian
              ()
          in
          let files_changed =
            get_git_diff ~sw ~env ~working_dir:spec.working_dir
          in
          let agent_text =
            match parse_stdout with
            | Some f -> ( try f result.stdout with _ -> "")
            | None -> ""
          in
          let session_id =
            match parse_session_id with
            | Some f -> f result.stdout
            | None -> None
          in
          {
            status = result.status;
            files_changed;
            report = None;
            elapsed = result.elapsed;
            cost = result.cost;
            stdout = result.stdout;
            agent_text;
            stderr = result.stderr;
            exit_code = result.exit_code;
            session_id;
          })
