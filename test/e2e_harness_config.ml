(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Shared configuration for Cabal's manual E2E test harnesses. *)

let managed_namespace : Cabal.Backend_types.managed_namespace =
  {
    id = "cabal-tests";
    display_name = "Cabal tests";
    config_dir = ".cabal-tests/backend-config";
  }

let all_backend_ids = ["claude-code"; "codex"; "opencode"; "copilot-cli"]

let default_model_for_backend = function
  | "claude-code" -> Some "haiku"
  | "codex" -> None
  | "copilot-cli" -> Some "claude-haiku-4.5"
  | "opencode" -> Some "openai/gpt-5.4-mini"
  | "gemini-cli" -> Some "gemini-3-flash-preview"
  | _ -> None

let known_model_env_vars =
  [
    ("claude-code", "CABAL_E2E_MODEL_CLAUDE_CODE");
    ("codex", "CABAL_E2E_MODEL_CODEX");
    ("copilot-cli", "CABAL_E2E_MODEL_COPILOT_CLI");
    ("opencode", "CABAL_E2E_MODEL_OPENCODE");
    ("gemini-cli", "CABAL_E2E_MODEL_GEMINI_CLI");
  ]

let sanitize_env_fragment id =
  String.map
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> Char.uppercase_ascii c
      | _ -> '_')
    id

let model_env_var_for_backend backend_id =
  match List.assoc_opt backend_id known_model_env_vars with
  | Some env_var -> env_var
  | None -> "CABAL_E2E_MODEL_" ^ sanitize_env_fragment backend_id

let non_empty_env ?(getenv = Sys.getenv_opt) name =
  match getenv name with
  | Some value when String.trim value <> "" -> Some (String.trim value)
  | _ -> None

let model_for_backend ?(getenv = Sys.getenv_opt) backend_id =
  let env_var = model_env_var_for_backend backend_id in
  match non_empty_env ~getenv env_var with
  | Some override -> Some override
  | None -> default_model_for_backend backend_id

let model_label = function
  | Some model -> model
  | None -> "<backend CLI default>"

let split_backend_filter raw =
  raw |> String.split_on_char ',' |> List.map String.trim
  |> List.filter (fun value -> value <> "")

let selected_backend_ids ?(getenv = Sys.getenv_opt) ~all_backend_ids () =
  match non_empty_env ~getenv "CABAL_E2E_BACKEND" with
  | Some raw -> split_backend_filter raw
  | None -> all_backend_ids

let positive_media_descriptor (d : Cabal.Backend_registry.descriptor) =
  d.capabilities.media_support.media_types <> []

let native_schema_draft = "2020-12"

let valid_native_schema_descriptor (d : Cabal.Backend_registry.descriptor) =
  d.capabilities.native_json_schema_output
  &&
  match d.capabilities.native_json_schema_output_evidence with
  | Some evidence
    when String.equal evidence.Cabal.Backend_types.json_schema_draft
           native_schema_draft ->
      Result.is_ok (Cabal.Task_preflight.validate_descriptor d)
  | Some _ | None -> false

let positive_web_descriptor (d : Cabal.Backend_registry.descriptor) =
  d.capabilities.web_support.maximum <> Cabal.Backend_types.Web_disabled

let media_schema_descriptors ~descriptors () =
  List.filter
    (fun descriptor ->
      positive_media_descriptor descriptor
      && valid_native_schema_descriptor descriptor)
    descriptors

let media_descriptors ~descriptors () =
  List.filter
    (fun descriptor ->
      positive_media_descriptor descriptor
      && Result.is_ok (Cabal.Task_preflight.validate_descriptor descriptor))
    descriptors

let web_descriptors ~descriptors () =
  List.filter positive_web_descriptor descriptors

let select_descriptors ?(getenv = Sys.getenv_opt) descriptors =
  let selected_ids =
    selected_backend_ids ~getenv
      ~all_backend_ids:
        (List.map
           (fun (d : Cabal.Backend_registry.descriptor) -> d.id)
           descriptors)
      ()
  in
  List.filter
    (fun (d : Cabal.Backend_registry.descriptor) -> List.mem d.id selected_ids)
    descriptors

let selected_media_schema_descriptors ?(getenv = Sys.getenv_opt) ~descriptors ()
    =
  media_schema_descriptors ~descriptors () |> select_descriptors ~getenv

let selected_media_descriptors ?(getenv = Sys.getenv_opt) ~descriptors () =
  media_descriptors ~descriptors () |> select_descriptors ~getenv

let selected_web_descriptors ?(getenv = Sys.getenv_opt) ~descriptors () =
  web_descriptors ~descriptors () |> select_descriptors ~getenv

let runtime_binding_matches_descriptor
    (descriptor : Cabal.Backend_registry.descriptor)
    (entry : Cabal.Runtime_entry.t) =
  entry.effective_descriptor = descriptor
  && entry.runtime_capabilities = descriptor.capabilities
  && Cabal.Agentic_backend.native_json_schema_output entry.backend
     = descriptor.capabilities.native_json_schema_output

type executable_lookup =
  | Executable_present
  | Executable_absent
  | Executable_lookup_failed

let lookup_error_is_absent = function
  | Unix.ENOENT | Unix.ENOTDIR -> true
  | _ -> false

type candidate_lookup = Candidate_present | Candidate_absent | Candidate_failed

let inspect_executable_candidate candidate =
  match
    try `Found (Unix.lstat candidate)
    with
    | Unix.Unix_error (error, _, _) -> `Lookup_error error
  with
  | `Lookup_error error ->
      if lookup_error_is_absent error then Candidate_absent else Candidate_failed
  | `Found _ -> (
      match
        try `Found (Unix.stat candidate)
        with
        | Unix.Unix_error (error, _, _) -> `Lookup_error error
      with
      | `Lookup_error _ -> Candidate_failed
      | `Found stats when stats.Unix.st_kind <> Unix.S_REG -> Candidate_failed
      | `Found _ -> (
          try
            Unix.access candidate [Unix.X_OK] ;
            Candidate_present
          with Unix.Unix_error _ -> Candidate_failed))

let lookup_executable ?(getenv = Sys.getenv_opt) binary_name =
  let candidates =
    if binary_name = "" then Error ()
    else if Filename.is_relative binary_name && not (String.contains binary_name '/')
    then
      match getenv "PATH" with
      | None -> Error ()
      | Some path ->
          Ok
            (String.split_on_char ':' path
            |> List.map (fun directory ->
                   Filename.concat
                     (if directory = "" then "." else directory)
                     binary_name))
    else Ok [binary_name]
  in
  match candidates with
  | Error () -> Executable_lookup_failed
  | Ok candidates ->
      let rec inspect = function
        | [] -> Executable_absent
        | candidate :: rest -> (
            match inspect_executable_candidate candidate with
            | Candidate_present -> Executable_present
            | Candidate_absent -> inspect rest
            | Candidate_failed -> Executable_lookup_failed)
      in
      inspect candidates

type version_probe_result =
  | Version_supported of Cabal.Backend_version.semver
  | Version_probe_failed
  | Version_output_malformed
  | Version_gate_rejected

let probe_version ~capture (descriptor : Cabal.Backend_registry.descriptor) =
  match capture [descriptor.binary_name; "--version"] with
  | Error _ -> Version_probe_failed
  | Ok output -> (
      match Cabal.Backend_version.parse_from_output output with
      | Error _ -> Version_output_malformed
      | Ok installed -> (
          match Cabal.Backend_version.check_gate ~descriptor ~installed with
          | Ok () -> Version_supported installed
          | Error _ -> Version_gate_rejected))
