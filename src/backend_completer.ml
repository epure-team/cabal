(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type completion_result = {text : string; backend_session_id : string option}

type completer =
  system_prompt:string ->
  prompt:string ->
  json_schema:Yojson.Safe.t option ->
  resume_session_id:string option ->
  (completion_result, string) result

let complete_with_workspace completer ~workspace ~system_prompt ~prompt
    ~json_schema ~resume_session_id =
  let prepared =
    Virtual_workspace.prepare_completion workspace ~system_prompt ~prompt
  in
  completer
    ~system_prompt:prepared.system_prompt
    ~prompt:prepared.prompt
    ~json_schema
    ~resume_session_id

let make_with_runner ~run ~working_dir ?model ?mcp_servers ?(read_only = false)
    () =
 fun ~system_prompt ~prompt ~json_schema ~resume_session_id ->
  let full_prompt =
    match resume_session_id with
    | Some _ ->
        (* Resuming: CLI already has system prompt in context *)
        prompt
    | None ->
        Printf.sprintf
          "SYSTEM INSTRUCTIONS:\n%s\n\n---\n\nUSER REQUEST:\n%s"
          system_prompt
          prompt
  in
  let spec =
    Backend_types.make_task_spec
      ~prompt:full_prompt
      ~working_dir
      ~expected_outputs:[]
      ?mcp_servers
      ?model
      ?json_schema
      ?resume_session_id
      ~read_only
      ()
  in
  match run spec with
  | Error e -> Error e
  | Ok (result : Backend_types.task_result) -> (
      let with_stderr msg =
        let stderr = String.trim result.stderr in
        if stderr = "" then msg
        else
          let max_len = 2000 in
          let trimmed =
            if String.length stderr > max_len then
              String.sub stderr 0 max_len ^ "..."
            else stderr
          in
          Printf.sprintf "%s\nStderr: %s" msg trimmed
      in
      match result.status with
      | Backend_types.Success ->
          (* Prefer the adapter-normalised agent text; fall back to raw stdout for
             backends or paths that have not yet populated [agent_text]. *)
          let text =
            if String.length result.agent_text > 0 then result.agent_text
            else result.stdout
          in
          Ok {text; backend_session_id = result.session_id}
      | Backend_types.Failed msg -> Error (with_stderr msg)
      | Backend_types.Timeout -> Error "Backend timeout"
      | Backend_types.Cancelled -> Error "Backend cancelled")

let make ~sw ~env ~backend ~working_dir ?model ?mcp_servers () =
  make_with_runner
    ~run:(fun spec -> Json_schema_enforcer.run_task ~sw ~env ~backend spec)
    ~working_dir
    ?model
    ?mcp_servers
    ()

(* Map a built-in backend id to the argv that prints its version.
   Returns [None] for unknown backends — the gate is skipped for those. *)
let version_cmd_for_backend backend_name =
  match Backend_registry.find backend_name with
  | Some descriptor -> Some [descriptor.binary_name; "--version"]
  | None -> None

let run_gate_for_output ~backend_name ~version_output =
  match Backend_registry.find backend_name with
  | None -> Ok ()
  | Some descriptor -> (
      match Backend_version.parse_from_output version_output with
      | Error _ -> Ok ()
      | Ok installed -> Backend_version.check_gate ~descriptor ~installed)

let run_version_gate ~env ~backend_name =
  match version_cmd_for_backend backend_name with
  | None -> Ok ()
  | Some cmd -> (
      match Backend_process.capture_version_output ~env cmd with
      | Error _ -> Ok ()
      | Ok output -> run_gate_for_output ~backend_name ~version_output:output)

let legacy_zero_attachment_limits : Task_preflight.limits =
  {max_attachments = 0; max_file_size_bytes = 0; max_total_size_bytes = 0}

let make_by_name_with_read_only ~read_only ~sw ~env ~backend_name ~working_dir
    ?model ?mcp_servers () =
  if not (Runtime_bootstrap.valid_runtime_id backend_name) then
    Error "backend routing id is structurally invalid"
  else
    Ok
      (make_with_runner
         ~run:(fun spec ->
           match
             Runtime_dispatch.run_task
               ~sw
               ~env
               ~limits:legacy_zero_attachment_limits
               ~backend_id:backend_name
               spec
           with
           | Ok result -> Ok result
           | Error error -> Error (Runtime_dispatch.render_error error))
         ~working_dir
         ?model
         ?mcp_servers
         ~read_only
         ())

let make_by_name ~sw ~env ~backend_name ~working_dir ?model ?mcp_servers () =
  make_by_name_with_read_only
    ~read_only:false
    ~sw
    ~env
    ~backend_name
    ~working_dir
    ?model
    ?mcp_servers
    ()

let check_read_only_routing ?(role_str = "validator") ~backend_name () =
  if not (Backend_registry.supports_read_only backend_name) then
    let alternatives = Backend_registry.read_only_safe_backend_ids () in
    Error
      (Printf.sprintf
         "Cannot route %s to backend '%s': read_only_support=false. Validators \
          require a backend with native read-only execution semantics. \
          Available read-only-safe backends: %s."
         role_str
         backend_name
         (String.concat ", " alternatives))
  else Ok ()

let make_validator_by_name ~sw ~env ~backend_name ~working_dir ?model
    ?mcp_servers () =
  match check_read_only_routing ~backend_name () with
  | Error _ as e -> e
  | Ok () ->
      make_by_name_with_read_only
        ~read_only:true
        ~sw
        ~env
        ~backend_name
        ~working_dir
        ?model
        ?mcp_servers
        ()
