(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type error =
  | Backend_not_registered
  | Descriptor_not_registered
  | Runtime_descriptor_invalid of Runtime_bootstrap.validation_error
  | Preflight_failed of Task_preflight.error
  | Backend_unavailable
  | Schema_enforcement_failed of string

let render_error = function
  | Backend_not_registered -> "requested backend is not registered at runtime"
  | Descriptor_not_registered ->
      "requested backend has no registered capability descriptor"
  | Runtime_descriptor_invalid error ->
      Runtime_bootstrap.render_validation_error error
  | Preflight_failed error -> Task_preflight.render_error error
  | Backend_unavailable -> "requested backend is not available"
  | Schema_enforcement_failed message ->
      "backend schema enforcement failed: "
      ^ Backend_event_redaction.redact_error_message message

let ( let* ) result continuation = Result.bind result continuation

let run_task ~sw ~env ~limits ~backend_id ?on_raw_line spec =
  let* backend =
    match Registry.get backend_id with
    | Some backend -> Ok backend
    | None -> Error Backend_not_registered
  in
  let* descriptor =
    match Backend_registry.find backend_id with
    | Some descriptor -> Ok descriptor
    | None -> Error Descriptor_not_registered
  in
  let* () =
    match Runtime_bootstrap.validate_backend ~descriptor ~backend with
    | Ok () -> Ok ()
    | Error error -> Error (Runtime_descriptor_invalid error)
  in
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
  if not (Agentic_backend.available ~sw ~env backend) then
    Error Backend_unavailable
  else
    match Json_schema_enforcer.run_task ~sw ~env ?on_raw_line ~backend spec with
    | Ok result -> Ok result
    | Error message -> Error (Schema_enforcement_failed message)
