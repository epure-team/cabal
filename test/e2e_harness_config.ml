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

let schema_compatible_descriptor (d : Cabal.Backend_registry.descriptor) =
  d.capabilities.structured_output

let positive_web_descriptor (d : Cabal.Backend_registry.descriptor) =
  d.capabilities.web_support.maximum <> Cabal.Backend_types.Web_disabled

let media_schema_descriptors ~descriptors () =
  List.filter
    (fun descriptor ->
      positive_media_descriptor descriptor
      && schema_compatible_descriptor descriptor)
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

let selected_web_descriptors ?(getenv = Sys.getenv_opt) ~descriptors () =
  web_descriptors ~descriptors () |> select_descriptors ~getenv
