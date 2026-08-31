(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type profile = Extensible | Hardened_builtins

type option_error =
  | Incomplete_eio_context
  | Probe_context_required
  | Extensible_probe_override_conflict

type validation_error =
  | Invalid_runtime_id
  | Runtime_id_mismatch
  | Invalid_runtime_display_name
  | Invalid_descriptor_display_name
  | Invalid_descriptor_binary_name
  | Invalid_descriptor_baseline_version
  | Descriptor_evidence_invalid of Task_preflight.error
  | Session_resume_mismatch
  | Native_json_schema_output_mismatch

type error =
  | Invalid_options of option_error
  | Hardened_registry_not_empty
  | Embedded_adapter_invalid
  | Hardened_builtin_set_invalid
  | Descriptor_missing
  | Candidate_invalid of validation_error
  | Custom_runtime_id_collision
  | Custom_descriptor_id_collision

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
  | Session_resume_mismatch ->
      "runtime and descriptor session-resume capabilities differ"
  | Native_json_schema_output_mismatch ->
      "runtime and descriptor native JSON schema capabilities differ"

let render_error = function
  | Invalid_options Incomplete_eio_context ->
      "runtime bootstrap requires both the Eio switch and environment together"
  | Invalid_options Probe_context_required ->
      "model probes require an explicit Eio switch and environment"
  | Invalid_options Extensible_probe_override_conflict ->
      "extensible bootstrap cannot disable probes when an Eio context is supplied"
  | Hardened_registry_not_empty ->
      "hardened runtime bootstrap requires an empty runtime registry"
  | Embedded_adapter_invalid ->
      "an approved embedded backend adapter is structurally invalid"
  | Hardened_builtin_set_invalid ->
      "the embedded backend set does not match the approved hardened set"
  | Descriptor_missing -> "a runtime backend has no matching descriptor"
  | Candidate_invalid error -> render_validation_error error
  | Custom_runtime_id_collision ->
      "a runtime backend with this id is already registered"
  | Custom_descriptor_id_collision ->
      "a backend descriptor with this id is already registered"

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

let validate_backend ~descriptor ~backend =
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
    if
      Agentic_backend.supports_session_resume backend
      <> descriptor.capabilities.session_resume
    then Error Session_resume_mismatch
    else if
      Agentic_backend.native_json_schema_output backend
      <> descriptor.capabilities.native_json_schema_output
    then Error Native_json_schema_output_mismatch
    else Ok ()

let approved_ids =
  ["claude-code"; "codex"; "copilot-cli"; "gemini-cli"; "opencode"; "pi"]

let handwritten_backends : Agentic_backend.t list =
  [
    (module Claude_code : Agentic_backend.S);
    (module Gemini_cli : Agentic_backend.S);
    (module Codex_cli : Agentic_backend.S);
    (module Opencode_cli : Agentic_backend.S);
    (module Copilot_cli : Agentic_backend.S);
  ]

let same_approved_set backends =
  let ids =
    List.map Agentic_backend.id backends |> List.sort_uniq String.compare
  in
  List.length ids = List.length backends && ids = approved_ids

let replace_backend candidates replacement =
  let replacement_id = Agentic_backend.id replacement in
  List.map
    (fun candidate ->
      if Agentic_backend.id candidate = replacement_id then replacement
      else candidate)
    candidates

let hardened_candidates () =
  let* embedded =
    match Adapter_loader.embedded_backends () with
    | Ok backends -> Ok backends
    | Error _ -> Error Embedded_adapter_invalid
  in
  if not (same_approved_set embedded) then Error Hardened_builtin_set_invalid
  else
    let final = List.fold_left replace_backend embedded handwritten_backends in
    let rec validate = function
      | [] -> Ok final
      | backend :: rest -> (
          match Backend_registry.find (Agentic_backend.id backend) with
          | None -> Error Descriptor_missing
          | Some descriptor -> (
              match validate_backend ~descriptor ~backend with
              | Ok () -> validate rest
              | Error error -> Error (Candidate_invalid error)))
    in
    validate final

let validate_options ~profile ~sw ~env ~probe_models =
  match sw, env with
  | Some _, None | None, Some _ -> Error (Invalid_options Incomplete_eio_context)
  | None, None ->
      if probe_models = Some true then
        Error (Invalid_options Probe_context_required)
      else Ok ()
  | Some _, Some _ ->
      if profile = Extensible && probe_models = Some false then
        Error (Invalid_options Extensible_probe_override_conflict)
      else Ok ()

let register_extensible ?project_dir ?sw ?env () =
  Adapter_loader.register_all ?project_dir ?sw ?env () ;
  Ok ()

let register_hardened ?sw ?env ~probe_models () =
  if Registry.list_ids () <> [] then Error Hardened_registry_not_empty
  else
    let* candidates = hardened_candidates () in
    List.iter Registry.register candidates ;
    (match probe_models, sw, env with
    | Some true, Some sw, Some env ->
        Adapter_loader.resolve_registered_model_probes ~sw ~env ()
    | _ -> ()) ;
    Ok ()

let register_runtime ?project_dir ?sw ?env ?probe_models ~profile () =
  let* () = validate_options ~profile ~sw ~env ~probe_models in
  match profile with
  | Extensible -> register_extensible ?project_dir ?sw ?env ()
  | Hardened_builtins -> register_hardened ?sw ?env ~probe_models ()

let register_custom ~descriptor ~backend =
  match validate_backend ~descriptor ~backend with
  | Error error -> Error (Candidate_invalid error)
  | Ok () ->
      let id = Agentic_backend.id backend in
      if Option.is_some (Registry.get id) then Error Custom_runtime_id_collision
      else if Option.is_some (Backend_registry.find descriptor.id) then
        Error Custom_descriptor_id_collision
      else
        match Backend_registry.add_descriptor descriptor with
        | Error Backend_registry.Descriptor_id_already_registered ->
            Error Custom_descriptor_id_collision
        | Ok () ->
            Registry.register backend ;
            Ok ()
