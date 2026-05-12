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

(* Extra descriptors registered at runtime. Used by tests to wire custom
   backends (e.g. "test-backend") with explicit capability declarations so
   they pass the read-only gate. Not included in all() or
   read_only_safe_backend_ids() — those remain builtin-only. *)
let extra_descriptors : descriptor list ref = ref []

let register_descriptor d =
  if not (List.exists (fun e -> e.id = d.id) !extra_descriptors) then
    extra_descriptors := d :: !extra_descriptors

let all () = builtin_backends

let unsupported_capability_notes () = unsupported_capability_notes_data

let find id =
  match List.find_opt (fun d -> d.id = id) builtin_backends with
  | Some _ as r -> r
  | None -> List.find_opt (fun d -> d.id = id) !extra_descriptors

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
