(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type implementation_origin = Handwritten | Yaml | Custom

type version_policy = Enforce_baseline | No_version_gate

type validation_error =
  | Invalid_runtime_id
  | Runtime_id_mismatch
  | Invalid_runtime_display_name
  | Invalid_descriptor_display_name
  | Invalid_descriptor_binary_name
  | Invalid_descriptor_baseline_version
  | Descriptor_evidence_invalid of Task_preflight.error
  | Runtime_capabilities_mismatch
  | Session_resume_mismatch
  | Native_json_schema_output_mismatch

type t = {
  backend : Agentic_backend.t;
  effective_descriptor : Backend_registry.descriptor;
  runtime_capabilities : Backend_registry.capabilities;
  origin : implementation_origin;
  version_policy : version_policy;
}

let render_validation_error = function
  | Invalid_runtime_id -> "runtime backend id is structurally invalid"
  | Runtime_id_mismatch -> "runtime backend id does not match its descriptor"
  | Invalid_runtime_display_name ->
      "runtime backend display name is structurally invalid"
  | Invalid_descriptor_display_name ->
      "backend descriptor display name is structurally invalid"
  | Invalid_descriptor_binary_name ->
      "backend descriptor binary name is structurally invalid"
  | Invalid_descriptor_baseline_version ->
      "backend descriptor baseline version must be major.minor.patch"
  | Descriptor_evidence_invalid error -> Task_preflight.render_error error
  | Runtime_capabilities_mismatch ->
      "effective descriptor capabilities differ from the trusted runtime snapshot"
  | Session_resume_mismatch ->
      "runtime and effective session-resume capabilities differ"
  | Native_json_schema_output_mismatch ->
      "runtime and effective native JSON schema capabilities differ"

let nonempty_text value =
  String.trim value <> ""
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x20 && code <> 0x7f)
       value

let valid_runtime_id value =
  nonempty_text value
  && value = String.trim value
  && String.length value <= 128
  && String.for_all
       (function
         | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' -> true
         | _ -> false)
       value

let ( let* ) result continuation = Result.bind result continuation

let create ~backend ~descriptor ~runtime_capabilities ~origin ~version_policy =
  let runtime_id = Agentic_backend.id backend in
  if not (valid_runtime_id runtime_id) then Error Invalid_runtime_id
  else if runtime_id <> descriptor.Backend_registry.id then
    Error Runtime_id_mismatch
  else if not (nonempty_text (Agentic_backend.name backend)) then
    Error Invalid_runtime_display_name
  else if not (nonempty_text descriptor.display_name) then
    Error Invalid_descriptor_display_name
  else if not (nonempty_text descriptor.binary_name) then
    Error Invalid_descriptor_binary_name
  else if
    not (Backend_version.is_valid_version_string descriptor.baseline_version)
  then
    Error Invalid_descriptor_baseline_version
  else
    let* () =
      match Task_preflight.validate_descriptor descriptor with
      | Ok () -> Ok ()
      | Error error -> Error (Descriptor_evidence_invalid error)
    in
    if runtime_capabilities <> descriptor.capabilities then
      Error Runtime_capabilities_mismatch
    else if
      Agentic_backend.supports_session_resume backend
      <> runtime_capabilities.session_resume
    then Error Session_resume_mismatch
    else if
      Agentic_backend.native_json_schema_output backend
      <> runtime_capabilities.native_json_schema_output
    then Error Native_json_schema_output_mismatch
    else
      Ok
        {
          backend;
          effective_descriptor = descriptor;
          runtime_capabilities;
          origin;
          version_policy;
        }
