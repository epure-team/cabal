(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type error =
  | Backend_not_registered
  | Runtime_registration_untrusted
  | Preflight_failed of Task_preflight.error
  | Backend_version_unsupported
  | Version_check_failed
  | Backend_unavailable
  | Availability_check_failed
  | Backend_execution_failed
  | Schema_enforcement_failed of string

let render_error = function
  | Backend_not_registered -> "requested backend is not registered at runtime"
  | Runtime_registration_untrusted ->
      "requested backend is raw-registered and not trusted for central dispatch"
  | Preflight_failed error -> Task_preflight.render_error error
  | Backend_version_unsupported ->
      "installed backend version does not satisfy the required stable baseline"
  | Version_check_failed -> "backend version check failed"
  | Backend_unavailable -> "requested backend is not available"
  | Availability_check_failed -> "backend availability check failed"
  | Backend_execution_failed -> "backend execution failed"
  | Schema_enforcement_failed message ->
      "backend schema enforcement failed: "
      ^ Backend_event_redaction.redact_error_message message

let ( let* ) result continuation = Result.bind result continuation

let protect error f =
  try Ok (f ()) with
  | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
  | (Out_of_memory | Stack_overflow | Sys.Break) as fatal -> raise fatal
  | _ -> Error error

let check_version ~env descriptor =
  let command = [descriptor.Backend_registry.binary_name; "--version"] in
  let* probe =
    protect Version_check_failed (fun () ->
        Backend_process.probe_version_command ~env command)
  in
  let* () =
    match probe.Backend_process.output with
    | None -> Ok ()
    | Some output -> (
        match Backend_version.parse_from_output output with
        | Error _ -> Ok ()
        | Ok installed -> (
            match Backend_version.check_gate ~descriptor ~installed with
            | Ok () -> Ok ()
            | Error _ -> Error Backend_version_unsupported))
  in
  Ok probe

let check_availability ~sw ~env ~backend ~origin ~version_probe =
  match origin, version_probe with
  | (Runtime_entry.Handwritten | Runtime_entry.Yaml), Some version_probe ->
      if version_probe.Backend_process.command_available then Ok ()
      else Error Backend_unavailable
  | (Runtime_entry.Handwritten | Runtime_entry.Yaml | Runtime_entry.Custom), _ ->
      let* available =
        protect Availability_check_failed (fun () ->
            Agentic_backend.available ~sw ~env backend)
      in
      if available then Ok () else Error Backend_unavailable

let run_task ~sw ~env ~limits ~backend_id ?on_raw_line spec =
  let* entry =
    match Registry.find_entry backend_id with
    | Some (Registry.Validated entry) -> Ok entry
    | Some (Registry.Raw _) -> Error Runtime_registration_untrusted
    | None -> Error Backend_not_registered
  in
  let backend = entry.Runtime_entry.backend in
  let descriptor = entry.effective_descriptor in
  let* () =
    match Task_preflight.validate_inputs ~limits spec with
    | Ok () -> Ok ()
    | Error error -> Error (Preflight_failed error)
  in
  let* () =
    match Task_preflight.validate_capabilities ~descriptor spec with
    | Ok () -> Ok ()
    | Error error -> Error (Preflight_failed error)
  in
  let* version_probe =
    match entry.version_policy with
    | Runtime_entry.Enforce_baseline ->
        Result.map Option.some (check_version ~env descriptor)
    | Runtime_entry.No_version_gate -> Ok None
  in
  let* () =
    check_availability ~sw ~env ~backend ~origin:entry.origin ~version_probe
  in
  let* execution =
    protect Backend_execution_failed (fun () ->
        Json_schema_enforcer.run_task ~sw ~env ?on_raw_line ~backend spec)
  in
  match execution with
  | Ok result -> Ok result
  | Error message -> Error (Schema_enforcement_failed message)
