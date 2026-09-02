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

type validation_error = Runtime_entry.validation_error =
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

type error =
  | Invalid_options of option_error
  | Hardened_registry_not_empty
  | Embedded_adapter_invalid
  | Hardened_builtin_set_invalid
  | Descriptor_missing
  | Candidate_invalid of validation_error
  | Custom_runtime_id_collision
  | Custom_descriptor_id_collision

let render_validation_error = Runtime_entry.render_validation_error

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

let valid_runtime_id = Runtime_entry.valid_runtime_id

let ( let* ) result continuation = Result.bind result continuation

let validate_backend_capabilities ~runtime_capabilities ~descriptor ~backend =
  Result.map
    (fun _ -> ())
    (Runtime_entry.create
       ~backend
       ~descriptor
       ~runtime_capabilities
       ~origin:Runtime_entry.Custom
       ~version_policy:Runtime_entry.Enforce_baseline)

let validate_backend ~descriptor ~backend =
  validate_backend_capabilities
    ~runtime_capabilities:descriptor.Backend_registry.capabilities
    ~descriptor
    ~backend

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

let no_media : Backend_registry.media_support =
  {media_types = []; evidence = None}

let no_web : Backend_registry.web_support =
  {maximum = Backend_types.Web_disabled; evidence = None}

let native_schema_evidence tested_at_version =
  Some
    {
      Backend_types.tested_at_version;
      json_schema_draft = "2020-12";
      test_method = Backend_types.E2e_test;
    }

let codex_media_evidence : Backend_types.feature_evidence =
  {
    tested_at_version = "0.131.0";
    test_method =
      Backend_types.Manual_probe
        "codex exec --json --skip-git-repo-check --ignore-user-config -s read-only -c 'web_search=\"disabled\"' -i '<workspace-image-1.png>' -i '<workspace-image-2.jpg>' --output-schema '<task-schema-file>' -";
    notes =
      "Authenticated PNG/JPEG upload, native schema composition, and resume probes are recorded in docs/native-json-schema-investigation/codex.md.";
    evidence_url =
      Some
        "https://github.com/openai/codex/blob/rust-v0.131.0/codex-rs/utils/cli/src/shared_options.rs";
  }

let codex_web_evidence : Backend_types.feature_evidence =
  {
    tested_at_version = "0.131.0";
    test_method =
      Backend_types.Manual_probe
        "codex exec --json --skip-git-repo-check --ignore-user-config -s read-only -c 'web_search=\"live\"' -";
    notes =
      "Authenticated live web search returned paired web_search records and fetched an official public page; see docs/native-json-schema-investigation/codex.md.";
    evidence_url =
      Some
        "https://github.com/openai/codex/blob/rust-v0.131.0/codex-rs/core/tests/suite/web_search.rs";
  }

(* This mapping is intentionally independent of [Backend_registry]. It is the
   bootstrap-owned runtime contract for the exact approved implementations.
   Entry construction requires exact equality with the catalog descriptor, so a
   new or changed positive catalog claim cannot become dispatch-trusted without
   an explicit corresponding runtime-contract change here. *)
let approved_runtime_capabilities = function
  | "claude-code" ->
      Some
        Backend_registry.
          {
            structured_output = true;
            streaming_output = true;
            session_resume = true;
            mcp_support = Mcp_config_file;
            read_only_support = true;
            project_config_surface = Config_explicit_flag;
            precedence_confidence = High;
            generated_lsp_config = true;
            file_reading = true;
            media_support = no_media;
            web_support = no_web;
            native_json_schema_output = true;
            native_json_schema_output_evidence =
              native_schema_evidence "2.1.117";
          }
  | "codex" ->
      Some
        Backend_registry.
          {
            structured_output = true;
            streaming_output = false;
            session_resume = true;
            mcp_support = Mcp_config_file;
            read_only_support = true;
            project_config_surface = Config_fixed_path;
            precedence_confidence = Medium;
            generated_lsp_config = false;
            file_reading = false;
            media_support =
              {
                media_types = [Backend_types.Png; Backend_types.Jpeg];
                evidence = Some codex_media_evidence;
              };
            web_support =
              {
                maximum = Backend_types.Web_search_and_fetch;
                evidence = Some codex_web_evidence;
              };
            native_json_schema_output = true;
            native_json_schema_output_evidence =
              native_schema_evidence "0.131.0";
          }
  | "copilot-cli" ->
      Some
        Backend_registry.
          {
            structured_output = false;
            streaming_output = false;
            session_resume = false;
            mcp_support = Mcp_config_file;
            read_only_support = false;
            project_config_surface = Config_fixed_path;
            precedence_confidence = Low;
            generated_lsp_config = true;
            file_reading = false;
            media_support = no_media;
            web_support = no_web;
            native_json_schema_output = false;
            native_json_schema_output_evidence = None;
          }
  | "gemini-cli" ->
      Some
        Backend_registry.
          {
            structured_output = true;
            streaming_output = true;
            session_resume = true;
            mcp_support = Mcp_user_settings;
            read_only_support = false;
            project_config_surface = Config_fixed_path;
            precedence_confidence = Low;
            generated_lsp_config = false;
            file_reading = false;
            media_support = no_media;
            web_support = no_web;
            native_json_schema_output = false;
            native_json_schema_output_evidence = None;
          }
  | "opencode" ->
      Some
        Backend_registry.
          {
            structured_output = true;
            streaming_output = true;
            session_resume = false;
            mcp_support = Mcp_config_file;
            read_only_support = false;
            project_config_surface = Config_fixed_path;
            precedence_confidence = Medium;
            generated_lsp_config = true;
            file_reading = true;
            media_support = no_media;
            web_support = no_web;
            native_json_schema_output = false;
            native_json_schema_output_evidence = None;
          }
  | "pi" ->
      Some
        Backend_registry.
          {
            structured_output = true;
            streaming_output = true;
            session_resume = false;
            mcp_support = Mcp_none;
            read_only_support = false;
            project_config_surface = Config_none;
            precedence_confidence = High;
            generated_lsp_config = false;
            file_reading = true;
            media_support = no_media;
            web_support = no_web;
            native_json_schema_output = false;
            native_json_schema_output_evidence = None;
          }
  | _ -> None

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
      | [] -> Ok []
      | backend :: rest ->
          let id = Agentic_backend.id backend in
          let origin =
            if id = "pi" then Runtime_entry.Yaml else Runtime_entry.Handwritten
          in
          (match
             Backend_registry.find id, approved_runtime_capabilities id
           with
          | None, _ | _, None -> Error Descriptor_missing
          | Some descriptor, Some runtime_capabilities -> (
              match
                Runtime_entry.create
                  ~backend
                  ~descriptor
                  ~runtime_capabilities
                  ~origin
                  ~version_policy:Runtime_entry.Enforce_baseline
              with
              | Ok entry ->
                  let* rest = validate rest in
                  Ok (entry :: rest)
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
    Registry.replace_all_validated candidates ;
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
  match
    Runtime_entry.create
      ~backend
      ~descriptor
      ~runtime_capabilities:descriptor.Backend_registry.capabilities
      ~origin:Runtime_entry.Custom
      ~version_policy:Runtime_entry.Enforce_baseline
  with
  | Error error -> Error (Candidate_invalid error)
  | Ok entry ->
      let id = Agentic_backend.id backend in
      if Option.is_some (Registry.get id) then Error Custom_runtime_id_collision
      else if Option.is_some (Backend_registry.find descriptor.id) then
        Error Custom_descriptor_id_collision
      else
        match Backend_registry.add_descriptor descriptor with
        | Error Backend_registry.Descriptor_id_already_registered ->
            Error Custom_descriptor_id_collision
        | Ok () ->
            (* [entry] is already an immutable validated token. Registry commit
               is a no-fail whole-entry replacement in the documented
               single-domain startup model, so catalog insertion cannot expose
               a usable half-pair. *)
            Registry.register_validated entry ;
            Ok ()
