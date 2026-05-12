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
  resume_session_id:string option ->
  (completion_result, string) result

let make ~sw ~env ~backend ~working_dir ?model ?mcp_servers () =
 fun ~system_prompt ~prompt ~resume_session_id ->
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
      ?resume_session_id
      ()
  in
  let result = Agentic_backend.run_task ~sw ~env backend spec in
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
      Ok {text = result.stdout; backend_session_id = result.session_id}
  | Backend_types.Failed msg -> Error (with_stderr msg)
  | Backend_types.Timeout -> Error "Backend timeout"
  | Backend_types.Cancelled -> Error "Backend cancelled"

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

let make_by_name ~sw ~env ~backend_name ~working_dir ?model ?mcp_servers () =
  match Registry.get backend_name with
  | None ->
      let available = Registry.list_ids () in
      let msg =
        Printf.sprintf
          "Backend '%s' not found. Registered: %s"
          backend_name
          (String.concat ", " available)
      in
      Error msg
  | Some backend ->
      if not (Agentic_backend.available ~sw ~env backend) then
        Error
          (Printf.sprintf
             "Backend '%s' is not available (CLI tool not installed?)"
             backend_name)
      else Ok (make ~sw ~env ~backend ~working_dir ?model ?mcp_servers ())

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
      make_by_name ~sw ~env ~backend_name ~working_dir ?model ?mcp_servers ()
