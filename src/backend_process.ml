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

(** Default cap for version / availability probes. Five seconds is long enough
    for cold-start CLIs (large Node binaries on slow disks) but short enough
    that a hung backend cannot freeze registry initialisation for the whole
    host. *)
let default_probe_timeout_seconds = 5.0

type version_probe_result = {
  command_available : bool;
  output : string option;
  timed_out : bool;
}

let probe_version_command ~env
    ?(timeout_seconds = default_probe_timeout_seconds) cmd =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 64 in
  let outcome =
    Eio.Time.with_timeout clock timeout_seconds (fun () ->
        try
          Eio.Process.run
            proc_mgr
            ~stdout:(Eio.Flow.buffer_sink stdout_buf)
            ~stderr:(Eio.Flow.buffer_sink stderr_buf)
            cmd ;
          Ok true
        with Eio.Io _ -> Ok false)
  in
  match outcome with
  | Error `Timeout -> {command_available = false; output = None; timed_out = true}
  | Ok command_available ->
      let out = Buffer.contents stdout_buf in
      if out <> "" then
        {command_available; output = Some out; timed_out = false}
      else
        let err = Buffer.contents stderr_buf in
        {
          command_available;
          output = (if err = "" then None else Some err);
          timed_out = false;
        }

let capture_version_output ~env ?timeout_seconds cmd =
  let result = probe_version_command ~env ?timeout_seconds cmd in
  if result.timed_out then
    Error
      (Printf.sprintf
         "timeout running %s"
         (String.concat " " cmd))
  else
    match result.output with
    | Some output -> Ok output
    | None -> Error (Printf.sprintf "no output from %s" (String.concat " " cmd))

(* Check if a command is available by running it.
   Distinguishes three outcomes: clean exit, process error (missing binary
   or non-zero exit raising Eio.Io), and timeout. *)
let check_available ~env ?(timeout_seconds = default_probe_timeout_seconds) cmd
    =
  (probe_version_command ~env ~timeout_seconds cmd).command_available

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

(* Run a subprocess and capture output with timeout.
   Stderr is captured and returned in process_result for the caller
   to display appropriately (toast in TUI, print in CLI).
   If on_stdout is provided, it's called for each line of stdout as it arrives. *)
let run_process ~sw ~env ~cmd ?(stdin_content = None) ~working_dir
    ~timeout_seconds ?parse_cost ?on_stdout ?guardian () =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let start_time = Eio.Time.now clock in
  (* Create pipes for stdout/stderr *)
  let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw proc_mgr in
  (* Create stdin pipe if we have content to write *)
  let stdin_r, stdin_w =
    match stdin_content with
    | Some _ ->
        let r, w = Eio.Process.pipe ~sw proc_mgr in
        (Some r, Some w)
    | None -> (None, None)
  in
  (* Spawn the process *)
  let proc =
    Eio.Process.spawn
      ~sw
      proc_mgr
      ~cwd:Eio.Path.(fs / working_dir)
      ?stdin:(Option.map (fun r -> (r :> _ Eio.Flow.source)) stdin_r)
      ~stdout:(stdout_w :> _ Eio.Flow.sink)
      ~stderr:(stderr_w :> _ Eio.Flow.sink)
      cmd
  in
  (* Register PID with resource guardian for memory-pressure killing *)
  let pid = Eio.Process.pid proc in
  Option.iter (fun g -> Resource_guardian.register_pid g pid) guardian ;
  (* Close write ends - we only read from stdout/stderr *)
  Eio.Flow.close stdout_w ;
  Eio.Flow.close stderr_w ;
  (* Write stdin content and close *)
  (match (stdin_content, stdin_w) with
  | Some content, Some w ->
      Eio.Flow.copy_string content w ;
      Eio.Flow.close w
  | _ -> ()) ;
  (* Close read end of stdin pipe if we created one *)
  Option.iter Eio.Flow.close stdin_r ;
  (* Read output with timeout *)
  let stdout_buf = Buffer.create 4096 in
  let stderr_buf = Buffer.create 1024 in
  let result = ref None in
  let timeout_result =
    Eio.Time.with_timeout clock timeout_seconds (fun () ->
        (* Read stdout and stderr concurrently *)
        Eio.Fiber.both
          (fun () ->
            let buf =
              Eio.Buf_read.of_flow ~max_size:(128 * 1024 * 1024) stdout_r
            in
            (* If streaming callback provided, read line by line *)
            match on_stdout with
            | Some callback -> (
                try
                  while true do
                    let line = Eio.Buf_read.line buf in
                    Buffer.add_string stdout_buf line ;
                    Buffer.add_char stdout_buf '\n' ;
                    callback line
                  done
                with End_of_file -> ())
            | None ->
                (* No streaming, read all at once *)
                Buffer.add_string stdout_buf (Eio.Buf_read.take_all buf))
          (fun () ->
            (* Capture stderr silently - no live streaming to avoid TUI corruption.
               The full stderr is returned in process_result.stderr for callers
               to display appropriately (toast for TUI, print for CLI). *)
            let buf =
              Eio.Buf_read.of_flow ~max_size:(16 * 1024 * 1024) stderr_r
            in
            try
              while true do
                let line = Eio.Buf_read.line buf in
                Buffer.add_string stderr_buf line ;
                Buffer.add_char stderr_buf '\n'
              done
            with End_of_file -> ()) ;
        (* Wait for process to complete *)
        let status = Eio.Process.await proc in
        result := Some status ;
        Ok ())
  in
  (* Unregister PID from resource guardian — process is done *)
  Option.iter (fun g -> Resource_guardian.unregister_pid g pid) guardian ;
  let elapsed_seconds = Eio.Time.now clock -. start_time in
  let stdout_str = Buffer.contents stdout_buf in
  let stderr_str = Buffer.contents stderr_buf in
  let elapsed = duration_of_seconds elapsed_seconds in
  match timeout_result with
  | Error `Timeout ->
      (* On timeout, try graceful termination first, then force kill. *)
      (try
         Eio.Process.signal proc Sys.sigterm ;
         match
           Eio.Time.with_timeout clock 2.0 (fun () ->
               Ok (Eio.Process.await proc))
         with
         | Ok _ -> ()
         | Error `Timeout ->
             Eio.Process.signal proc Sys.sigkill ;
             ignore (Eio.Process.await proc)
       with _ -> ()) ;
      {
        status = Timeout;
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
