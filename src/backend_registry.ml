(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type mcp_support =
  | Mcp_none
  | Mcp_config_file
  | Mcp_config_flag
  | Mcp_user_settings

type project_config_surface =
  | Config_none
  | Config_explicit_flag
  | Config_fixed_path
  | Config_env_var

type precedence_confidence = High | Medium | Low

type media_support = {
  media_types : Backend_types.media_type list;
  evidence : Backend_types.feature_evidence option;
}

type web_support = {
  maximum : Backend_types.web_access;
  evidence : Backend_types.feature_evidence option;
}

type capabilities = {
  structured_output : bool;
  streaming_output : bool;
  session_resume : bool;
  mcp_support : mcp_support;
  read_only_support : bool;
  project_config_surface : project_config_surface;
  precedence_confidence : precedence_confidence;
  generated_lsp_config : bool;
  file_reading : bool;
  media_support : media_support;
  web_support : web_support;
  native_json_schema_output : bool;
  native_json_schema_output_evidence : Backend_types.capability_evidence option;
}

type descriptor = {
  id : string;
  display_name : string;
  binary_name : string;
  baseline_version : string;
  capabilities : capabilities;
}

type unsupported_capability_note = {
  backend_id : string;
  feature : string;
  evidence_url : string;
  note : string;
}

type descriptor_registration_error = Descriptor_id_already_registered

type yaml_descriptor_registration_error =
  | Immutable_builtin_descriptor
  | Descriptor_not_owned_by_yaml_loader

let builtin_backends =
  [
    {
      id = "claude-code";
      display_name = "Claude Code";
      binary_name = "claude";
      baseline_version = "2.1.117";
      capabilities =
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
          media_support = {media_types = []; evidence = None};
          web_support =
            {maximum = Backend_types.Web_disabled; evidence = None};
          native_json_schema_output = true;
          native_json_schema_output_evidence =
            Some
              {
                Backend_types.tested_at_version = "2.1.117";
                json_schema_draft = "2020-12";
                test_method = Backend_types.E2e_test;
              };
        };
    };
    {
      id = "codex";
      display_name = "Codex CLI";
      binary_name = "codex";
      baseline_version = "0.122.0";
      capabilities =
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
          media_support = {media_types = []; evidence = None};
          web_support =
            {maximum = Backend_types.Web_disabled; evidence = None};
          native_json_schema_output = true;
          native_json_schema_output_evidence =
            Some
              {
                Backend_types.tested_at_version = "0.131.0";
                json_schema_draft = "2020-12";
                test_method = Backend_types.E2e_test;
              };
        };
    };
    {
      id = "opencode";
      display_name = "OpenCode";
      binary_name = "opencode";
      baseline_version = "1.14.20";
      capabilities =
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
          media_support = {media_types = []; evidence = None};
          web_support =
            {maximum = Backend_types.Web_disabled; evidence = None};
          native_json_schema_output = false;
          native_json_schema_output_evidence = None;
        };
    };
    {
      id = "pi";
      display_name = "Pi Coding Agent";
      binary_name = "pi";
      baseline_version = "0.84.3";
      capabilities =
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
          media_support = {media_types = []; evidence = None};
          web_support =
            {maximum = Backend_types.Web_disabled; evidence = None};
          native_json_schema_output = false;
          native_json_schema_output_evidence = None;
        };
    };
    {
      id = "gemini-cli";
      display_name = "Gemini CLI";
      binary_name = "gemini";
      baseline_version = "0.38.2";
      capabilities =
        {
          structured_output = true;
          streaming_output = true;
          (* Gemini 0.38.2 supports session resume via --resume <sid>;
             the implementation in gemini_cli.ml wires this correctly. *)
          session_resume = true;
          (* Gemini reads MCP config from settings.json files.  Workspace
             .gemini/settings.json can override user settings, but the
             mcp_config_path arg in build_command is intentionally unused
             because there is no per-invocation MCP file injection flag. *)
          mcp_support = Mcp_user_settings;
          read_only_support = false;
          project_config_surface = Config_fixed_path;
          precedence_confidence = Low;
          generated_lsp_config = false;
          file_reading = false;
          media_support = {media_types = []; evidence = None};
          web_support =
            {maximum = Backend_types.Web_disabled; evidence = None};
          native_json_schema_output = false;
          native_json_schema_output_evidence = None;
        };
    };
    {
      id = "copilot-cli";
      display_name = "Copilot CLI";
      binary_name = "copilot";
      baseline_version = "1.0.34";
      capabilities =
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
          media_support = {media_types = []; evidence = None};
          web_support =
            {maximum = Backend_types.Web_disabled; evidence = None};
          native_json_schema_output = false;
          native_json_schema_output_evidence = None;
        };
    };
  ]

let unsupported_capability_notes_data =
  [
    {
      backend_id = "codex";
      feature = "streaming_output";
      evidence_url = "https://developers.openai.com/codex/cli/reference";
      note =
        "Codex CLI reference documents exec JSON output, but no stable \
         stream-json/NDJSON streaming output mode was found in the official \
         CLI surface.";
    };
    {
      backend_id = "codex";
      feature = "generated_lsp_config";
      evidence_url = "https://developers.openai.com/codex/config-reference";
      note =
        "Codex config reference lists project config keys including \
         mcp_servers.<id>.*, and official config/advanced docs were searched \
         for lsp or language-server entries; none are documented.";
    };
    {
      backend_id = "codex";
      feature = "file_reading";
      evidence_url = "https://developers.openai.com/codex/cli/features";
      note =
        "Codex CLI feature docs cover prompt and file attachment behavior, but \
         do not document an arbitrary prompt file-reference ingestion surface \
         equivalent to Claude/OpenCode file reading.";
    };
    {
      backend_id = "opencode";
      feature = "session_resume";
      evidence_url = "https://opencode.ai/docs/cli/";
      note =
        "OpenCode CLI docs document run mode and session continuation flags, \
         but no stable non-interactive task_spec.resume_session_id contract is \
         wired for opencode run at the baseline.";
    };
    {
      backend_id = "opencode";
      feature = "read_only_support";
      evidence_url = "https://opencode.ai/docs/cli/";
      note =
        "OpenCode CLI docs document permission-related flags, but not a native \
         all-read/no-write sandbox mode equivalent to Codex read-only or \
         Claude permission denial for validators.";
    };
    {
      backend_id = "gemini-cli";
      feature = "read_only_support";
      evidence_url = "https://www.geminicli.com/docs/reference/configuration";
      note =
        "Gemini configuration docs were searched for read-only, sandbox, and \
         write-denial controls; no native read-only validator mode is \
         documented.";
    };
    {
      backend_id = "gemini-cli";
      feature = "generated_lsp_config";
      evidence_url = "https://www.geminicli.com/docs/reference/configuration";
      note =
        "Gemini official configuration reference was searched for lsp and \
         language-server settings; none are documented, so Épure does not \
         generate backend-native LSP config.";
    };
    {
      backend_id = "gemini-cli";
      feature = "file_reading";
      evidence_url = "https://www.geminicli.com/docs/cli/headless/";
      note =
        "Gemini headless docs document prompt and JSON/stream-json output \
         modes, but no arbitrary file-reference ingestion surface is \
         documented for Épure prompt file refs.";
    };
    {
      backend_id = "copilot-cli";
      feature = "structured_output";
      evidence_url =
        "https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically";
      note =
        "Copilot programmatic CLI docs document prompt forms and silent mode, \
         but no JSON or schema-structured output mode is documented.";
    };
    {
      backend_id = "copilot-cli";
      feature = "streaming_output";
      evidence_url =
        "https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically";
      note =
        "Copilot programmatic CLI docs document non-interactive/silent \
         execution, but no streaming JSON or event stream output mode is \
         documented.";
    };
    {
      backend_id = "copilot-cli";
      feature = "session_resume";
      evidence_url =
        "https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically";
      note =
        "Copilot programmatic CLI docs document one-shot prompt invocation and \
         automation flags; no session resume flag or identifier contract is \
         documented.";
    };
    {
      backend_id = "copilot-cli";
      feature = "read_only_support";
      evidence_url =
        "https://docs.github.com/en/copilot/concepts/agents/copilot-cli/about-copilot-cli";
      note =
        "Copilot overview/security docs describe approval and tool access \
         behavior, but no native read-only sandbox mode for validator \
         invocations is documented.";
    };
    {
      backend_id = "copilot-cli";
      feature = "file_reading";
      evidence_url =
        "https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically";
      note =
        "Copilot programmatic docs document prompt forms and flags, but no \
         arbitrary prompt file-reference ingestion surface is documented for \
         Épure file refs.";
    };
  ]

type descriptor_owner = Compatibility | Host_custom | Yaml_loader

type extra_descriptor = {descriptor : descriptor; owner : descriptor_owner}

(* Extra descriptors registered at runtime. Used by tests, explicit host
   registration, and the extensible YAML loader. Not included in [all ()] or
   [read_only_safe_backend_ids ()], which remain builtin-only. *)
let extra_descriptors : extra_descriptor list ref = ref []

let find_builtin id = List.find_opt (fun d -> d.id = id) builtin_backends

let find_extra id =
  List.find_opt (fun entry -> entry.descriptor.id = id) !extra_descriptors

let register_descriptor d =
  if Option.is_none (find_builtin d.id) && Option.is_none (find_extra d.id) then
    extra_descriptors :=
      {descriptor = d; owner = Compatibility} :: !extra_descriptors

let all () = builtin_backends

let unsupported_capability_notes () = unsupported_capability_notes_data

let find id =
  match find_builtin id with
  | Some _ as r -> r
  | None -> Option.map (fun entry -> entry.descriptor) (find_extra id)

let add_descriptor d =
  match find d.id with
  | Some _ -> Error Descriptor_id_already_registered
  | None ->
      extra_descriptors :=
        {descriptor = d; owner = Host_custom} :: !extra_descriptors ;
      Ok ()

let upsert_yaml_descriptor descriptor =
  if Option.is_some (find_builtin descriptor.id) then
    Error Immutable_builtin_descriptor
  else
    match find_extra descriptor.id with
    | Some {owner = (Compatibility | Host_custom); _} ->
        Error Descriptor_not_owned_by_yaml_loader
    | Some {owner = Yaml_loader; _} ->
        extra_descriptors :=
          List.map
            (fun entry ->
              if entry.descriptor.id = descriptor.id then
                {descriptor; owner = Yaml_loader}
              else entry)
            !extra_descriptors ;
        Ok ()
    | None ->
        extra_descriptors :=
          {descriptor; owner = Yaml_loader} :: !extra_descriptors ;
        Ok ()

let supports_file_reading backend_id =
  match find backend_id with
  | Some d -> d.capabilities.file_reading
  | None -> false

let supports_generated_lsp_config backend_id =
  match find backend_id with
  | Some d -> d.capabilities.generated_lsp_config
  | None -> false

let supports_read_only backend_id =
  match find backend_id with
  | Some d -> d.capabilities.read_only_support
  | None -> false

let read_only_safe_backend_ids () =
  List.filter_map
    (fun d -> if d.capabilities.read_only_support then Some d.id else None)
    builtin_backends

(** [get_capability_evidence backend_id] returns the native JSON schema
    capability evidence for the descriptor, if present. Returns [None]
    when the backend is unknown or has no recorded evidence. *)
let get_capability_evidence backend_id =
  match find backend_id with
  | Some d -> d.capabilities.native_json_schema_output_evidence
  | None -> None

(** [capability_evidence_table ()] returns [(backend_id, evidence)] pairs for
    every built-in backend that carries a recorded native-schema capability
    evidence record. Backends without evidence are omitted. *)
let capability_evidence_table () =
  List.filter_map
    (fun d ->
      match d.capabilities.native_json_schema_output_evidence with
      | Some ev -> Some (d.id, ev)
      | None -> None)
    builtin_backends
