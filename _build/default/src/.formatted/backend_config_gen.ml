(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

include Backend_config_writer

let provider_for = function
  | "claude-code" -> Some Claude_code.project_config_artifacts
  | "codex" -> Some Codex_cli.project_config_artifacts
  | "opencode" -> Some Opencode_cli.project_config_artifacts
  | "gemini-cli" -> Some Gemini_cli.project_config_artifacts
  | "copilot-cli" -> Some Copilot_cli.project_config_artifacts
  | _ -> None

let generated_lsp_servers ~backend_id lsp_servers =
  if Backend_registry.supports_generated_lsp_config backend_id then lsp_servers
  else []

let generate_all_with_options
    ?(managed_namespace = Backend_types.default_managed_namespace)
    ?(lsp_servers = []) ~mcp_servers ~backend_id () =
  match provider_for backend_id with
  | None -> []
  | Some provider ->
      provider
        ~managed_namespace
        ~mcp_servers
        ~lsp_servers:(generated_lsp_servers ~backend_id lsp_servers)

let generate_all ~mcp_servers ~backend_id =
  generate_all_with_options ~mcp_servers ~backend_id ()

let generate ~backend_id =
  match generate_all ~mcp_servers:[] ~backend_id with
  | artifact :: _ -> Some artifact
  | [] -> None

let generate_with_lsp_defs
    ?(managed_namespace = Backend_types.default_managed_namespace) ~mcp_servers
    ~backend_id ~lsp_servers () =
  match provider_for backend_id with
  | None -> None
  | Some provider -> (
      match
        provider
          ~managed_namespace
          ~mcp_servers
          ~lsp_servers:(generated_lsp_servers ~backend_id lsp_servers)
      with
      | artifact :: _ -> Some artifact
      | [] -> None)

let setup_project_config_with_options
    ?(managed_namespace = Backend_types.default_managed_namespace)
    ?(lsp_servers = []) ~mcp_servers ~backend_id ~project_dir ~force () =
  match Backend_types.validate_managed_namespace managed_namespace with
  | Error msg ->
      {
        project_config_path = None;
        write_outcome = Some (Invalid_managed_namespace msg);
        write_outcomes = [];
      }
  | Ok () -> (
      match provider_for backend_id with
      | None ->
          {
            project_config_path = None;
            write_outcome = None;
            write_outcomes = [];
          }
      | Some provider ->
          setup_artifacts
            ~project_dir
            ~force
            (provider
               ~managed_namespace
               ~mcp_servers
               ~lsp_servers:(generated_lsp_servers ~backend_id lsp_servers)))

let setup_project_config ~mcp_servers ~backend_id ~project_dir ~force =
  setup_project_config_with_options
    ~mcp_servers
    ~backend_id
    ~project_dir
    ~force
    ()
