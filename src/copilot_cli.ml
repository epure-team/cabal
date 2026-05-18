(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Backend_types

let id = "copilot-cli"

let name = "GitHub Copilot"

(* GitHub Copilot CLI is a meta-CLI that proxies to several upstream model
   providers; we expose the canonical pair selectable via its --model flag. *)
let models =
  [
    "claude-opus-4-7";
    "claude-sonnet-4-6";
    "claude-haiku-4-5-20251001";
    "gpt-4o";
    "gpt-4o-mini";
    "gpt-5";
  ]

(* `copilot` only accepts `--model <id>` and ships a `providers` subcommand
   that requires interactive input (no machine-parseable listing).  Stay on
   the static list until upstream adds a non-interactive enumeration. *)
let models_probe = None

let available ~sw:_ ~env =
  Backend_process.check_available ~env ["copilot"; "--version"]

let supports_session_resume = false

let is_resume_failure (_result : task_result) = false

let read_project_file ~env ~project_dir rel_path =
  let path = Filename.concat project_dir rel_path in
  try Ok (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path))
  with e ->
    Error (Printf.sprintf "could not read %s: %s" path (Printexc.to_string e))

(* json_string_map_{to,of}_yojson live in Backend_json_helpers; re-export
   the type alias so [@@deriving yojson] resolves the field codec by name. *)
type json_string_map = Backend_json_helpers.json_string_map

let json_string_map_to_yojson = Backend_json_helpers.json_string_map_to_yojson

let json_string_map_of_yojson = Backend_json_helpers.json_string_map_of_yojson

type mcp_server_settings = {
  type_ : string; [@key "type"]
  command : string;
  args : string list;
  env : json_string_map;
  tools : string list;
}
[@@deriving yojson]

type mcp_server_map = (string * mcp_server_settings) list

let mcp_server_map_to_yojson servers =
  `Assoc
    (List.map
       (fun (name, server) -> (name, mcp_server_settings_to_yojson server))
       servers)

let mcp_server_map_of_yojson = function
  | `Assoc fields ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (name, json) :: rest -> (
            match mcp_server_settings_of_yojson json with
            | Ok server -> loop ((name, server) :: acc) rest
            | Error msg -> Error (Printf.sprintf "%s: %s" name msg))
      in
      loop [] fields
  | _ -> Error "expected JSON object"

type project_mcp_json = {mcpServers : mcp_server_map} [@@deriving yojson]

type copilot_lsp_server_settings = {
  command : string;
  args : string list;
  file_extensions : json_string_map; [@key "fileExtensions"]
}
[@@deriving yojson]

type copilot_lsp_server_map = (string * copilot_lsp_server_settings) list

let copilot_lsp_server_map_to_yojson servers =
  `Assoc
    (List.map
       (fun (name, server) ->
         (name, copilot_lsp_server_settings_to_yojson server))
       servers)

let copilot_lsp_server_map_of_yojson = function
  | `Assoc fields ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (name, json) :: rest -> (
            match copilot_lsp_server_settings_of_yojson json with
            | Ok server -> loop ((name, server) :: acc) rest
            | Error msg -> Error (Printf.sprintf "%s: %s" name msg))
      in
      loop [] fields
  | _ -> Error "expected JSON object"

type project_lsp_json = {lspServers : copilot_lsp_server_map}
[@@deriving yojson]

type repository_settings_json = {
  enabled : bool option; [@default None] [@yojson.option]
}
[@@deriving yojson]

let repository_settings_json_content () =
  let json =
    match repository_settings_json_to_yojson {enabled = None} with
    | `Assoc fields ->
        `Assoc (List.filter (fun (_name, value) -> value <> `Null) fields)
    | other -> other
  in
  json |> Yojson.Safe.pretty_to_string |> fun s -> s ^ "\n"

let env_reference_value ~name value =
  if String.length value > 0 && value.[0] = '$' then value
  else if name = "" then value
  else "$" ^ name

let persistent_env_references env =
  List.map (fun (name, value) -> (name, env_reference_value ~name value)) env

let mcp_server_entry (cfg : Backend_types.mcp_server_config) =
  ( cfg.name,
    {
      type_ = "local";
      command = cfg.command;
      args = cfg.args;
      env = persistent_env_references cfg.env;
      tools = ["*"];
    } )

let mcp_json_content mcp_servers =
  project_mcp_json_to_yojson
    {mcpServers = List.map mcp_server_entry mcp_servers}
  |> Yojson.Safe.pretty_to_string
  |> fun s -> s ^ "\n"

let lsp_server_entry (cfg : Backend_types.lsp_server_config) =
  ( cfg.name,
    {
      command = cfg.command;
      args = cfg.args;
      file_extensions =
        List.map
          (fun assoc -> (assoc.Backend_types.extension, assoc.language_id))
          cfg.file_associations;
    } )

let lsp_json_content lsp_servers =
  project_lsp_json_to_yojson
    {lspServers = List.map lsp_server_entry lsp_servers}
  |> Yojson.Safe.pretty_to_string
  |> fun s -> s ^ "\n"

let copilot_instructions_body =
  "# Copilot CLI Project Configuration\n\n\
   This file is managed by the host application via Cabal. Custom\n\
   instructions live at `.github/copilot-instructions.md`, repository\n\
   settings live at `.github/copilot/settings.json`, and project MCP\n\
   config lives at `.github/mcp.json` for Copilot CLI 1.0.34 (stable\n\
   channel).\n\n\
   ## Project Context\n\n\
   Configured by the host application for this project.\n\n\
   ## LSP Configuration\n\n\
   Copilot CLI uses Language Server Protocol (LSP) for enhanced code\n\
   analysis. The host writes project LSP server definitions to\n\
   `.github/lsp.json` for detected languages, following GitHub's documented\n\
   project LSP config shape. Tooling readiness is tracked by the host's\n\
   own project hook layer.\n\n\
   ## MCP Servers\n\n\
   MCP servers are not activated by default. Approved registry-backed\n\
   entries are written to `.github/mcp.json` as local project MCP server\n\
   definitions with explicit command, args, env, and tool allow-list\n\
   fields.\n\
   <!-- mcp: disabled by default — no approved entries activated -->\n\n\
   ## Stable Limitations (Copilot CLI 1.0.34)\n\n\
   The following features are not available in the stable channel and\n\
   are intentionally not configured for this backend:\n\
   - streaming_output: not supported in stable 1.0.34\n\
   - structured_output: not supported in stable 1.0.34\n\
   - session_resume: not supported in stable 1.0.34\n\
   - read_only_support: not available via stable CLI flags\n\
   - file_reading: not supported in stable 1.0.34\n\
   <!-- stable-limitations: documented per backend parity policy -->\n"

let project_config_artifacts ~managed_namespace ~mcp_servers ~lsp_servers =
  [
    {
      Backend_config_writer.backend_id = id;
      ownership = Backend_config_writer.Backend_project;
      managed_namespace;
      project_relative_path = ".github/copilot-instructions.md";
      content =
        Backend_config_writer.with_managed_header
          ~managed_namespace
          Backend_config_writer.Html
          ~backend_id:id
          copilot_instructions_body;
    };
    {
      Backend_config_writer.backend_id = id;
      ownership = Backend_config_writer.Backend_project;
      managed_namespace;
      project_relative_path = ".github/copilot/settings.json";
      content = repository_settings_json_content ();
    };
    {
      Backend_config_writer.backend_id = id;
      ownership = Backend_config_writer.Backend_project;
      managed_namespace;
      project_relative_path = ".github/lsp.json";
      content = lsp_json_content lsp_servers;
    };
    {
      Backend_config_writer.backend_id = id;
      ownership = Backend_config_writer.Backend_project;
      managed_namespace;
      project_relative_path = ".github/mcp.json";
      content = mcp_json_content mcp_servers;
    };
  ]

(* Extract the agent response text from Copilot's stdout.
   Copilot does not support a structured (JSON) output mode; its stdout is
   already the assistant's plain-text reply.  We therefore return the captured
   stdout verbatim (stripped of trailing whitespace) so hosts get the same
   shape they get from JSON-emitting backends via [agent_text].

   TODO: revisit if [copilot] gains a structured output mode that requires
   parsing event envelopes. *)
let parse_stdout_text stdout =
  let trim_trailing s =
    let len = String.length s in
    let rec last i =
      if i < 0 then -1
      else match s.[i] with ' ' | '\t' | '\n' | '\r' -> last (i - 1) | _ -> i
    in
    let stop = last (len - 1) in
    if stop = len - 1 then s else String.sub s 0 (stop + 1)
  in
  trim_trailing stdout

(* Build the copilot command for non-interactive execution.
   Copilot uses -p <prompt> for prompt, --yolo for auto-approval,
   -s for silent (no stats), and --no-ask-user for full autonomy.
   Copilot does not support JSON output; we capture plain text. *)
let build_command ~mcp_config_path:_ (spec : task_spec) =
  let full_prompt =
    if String.length spec.instructions > 0 then
      Printf.sprintf
        "%s\n\n---\nProject Instructions:\n%s"
        spec.prompt
        spec.instructions
    else spec.prompt
  in
  let base = ["copilot"; "-p"; full_prompt] in
  let model_args =
    match spec.model with Some m -> ["--model"; m] | None -> []
  in
  let flags = ["--yolo"; "-s"; "--no-ask-user"] in
  (* Copilot discovers project MCP config from [.github/mcp.json].  The shared
     runner still offers a transient [mcp_config_path] hook for flag-based
     backends, but this adapter intentionally ignores it after [run_task]
     writes the project file. *)
  (base @ model_args @ flags, "")

let setup_outcome_for_path setup path =
  List.find_opt
    (fun outcome ->
      outcome.Backend_config_writer.artifact.project_relative_path = path)
    setup.Backend_config_writer.write_outcomes

let outcome_applied = function
  | Some outcome ->
      Backend_config_writer.write_result_was_applied
        outcome.Backend_config_writer.result
  | None -> false

let setup_outcome_reason = function
  | None -> "no setup outcome was recorded"
  | Some
      {
        Backend_config_writer.result =
          Backend_config_writer.Skipped_user_content _;
        _;
      } ->
      "user-authored file was skipped"
  | Some
      {
        Backend_config_writer.result =
          Backend_config_writer.Refused_hash_mismatch _;
        _;
      } ->
      "hash mismatch"
  | Some
      {Backend_config_writer.result = Backend_config_writer.Already_current; _}
    ->
      "already current"
  | Some {Backend_config_writer.result = Backend_config_writer.Written _; _} ->
      "written"
  | Some
      {
        Backend_config_writer.result =
          Backend_config_writer.Backed_up_and_written _;
        _;
      } ->
      "backed up and written"
  | Some
      {
        Backend_config_writer.result =
          Backend_config_writer.Invalid_managed_namespace _;
        _;
      } ->
      "managed namespace was invalid"

let first_non_valid results =
  match
    List.find_map
      (function
        | Agentic_backend.Config_invalid _ as result -> Some result
        | Agentic_backend.Config_valid
        | Agentic_backend.Config_check_unsupported _ ->
            None)
      results
  with
  | Some result -> result
  | None -> (
      match
        List.find_map
          (function
            | Agentic_backend.Config_check_unsupported _ as result ->
                Some result
            | Agentic_backend.Config_valid | Agentic_backend.Config_invalid _ ->
                None)
          results
      with
      | Some result -> result
      | None -> Agentic_backend.Config_valid)

let validate_strict_json_file ~env ~project_dir ~setup_result ~path ~label
    ~of_yojson =
  let outcome = setup_outcome_for_path setup_result path in
  if not (outcome_applied outcome) then
    Agentic_backend.Config_check_unsupported
      (Printf.sprintf
         "Copilot %s was not applied (%s); refusing to validate user-authored \
          %s"
         label
         (setup_outcome_reason outcome)
         path)
  else
    match read_project_file ~env ~project_dir path with
    | Error msg -> Agentic_backend.Config_invalid msg
    | Ok content -> (
        try
          let json = Yojson.Safe.from_string content in
          match of_yojson json with
          | Ok _ -> Agentic_backend.Config_valid
          | Error msg ->
              Agentic_backend.Config_invalid
                (Printf.sprintf "%s schema check failed: %s" label msg)
        with e ->
          Agentic_backend.Config_invalid
            (Printf.sprintf
               "%s is not strict JSON: %s"
               label
               (Printexc.to_string e)))

let check_project_config ~sw:_ ~env ~project_dir ~setup_result =
  let instructions_path = ".github/copilot-instructions.md" in
  let instructions_outcome =
    setup_outcome_for_path setup_result instructions_path
  in
  let instructions_check =
    if not (outcome_applied instructions_outcome) then
      Agentic_backend.Config_check_unsupported
        (Printf.sprintf
           "Copilot instructions were not applied (%s); refusing to validate \
            user-authored %s"
           (setup_outcome_reason instructions_outcome)
           instructions_path)
    else
      match read_project_file ~env ~project_dir instructions_path with
      | Error msg -> Agentic_backend.Config_invalid msg
      | Ok content ->
          let managed_namespace =
            match instructions_outcome with
            | Some outcome ->
                outcome.Backend_config_writer.artifact.managed_namespace
            | None -> Backend_types.default_managed_namespace
          in
          if Backend_config_writer.is_managed_content ~managed_namespace content
          then Agentic_backend.Config_valid
          else
            Agentic_backend.Config_invalid
              "Copilot instructions file lacks the Épure managed marker"
  in
  let command_check =
    let spec =
      Backend_types.make_task_spec
        ~prompt:"config validation"
        ~working_dir:project_dir
        ()
    in
    let cmd, _stdin = build_command ~mcp_config_path:None spec in
    if List.mem "--no-custom-instructions" cmd then
      Agentic_backend.Config_invalid
        "Copilot command disables the generated .github/copilot-instructions.md"
    else Agentic_backend.Config_valid
  in
  first_non_valid
    [
      instructions_check;
      command_check;
      validate_strict_json_file
        ~env
        ~project_dir
        ~setup_result
        ~path:".github/copilot/settings.json"
        ~label:"Copilot repository settings"
        ~of_yojson:repository_settings_json_of_yojson;
      validate_strict_json_file
        ~env
        ~project_dir
        ~setup_result
        ~path:".github/lsp.json"
        ~label:"Copilot LSP config"
        ~of_yojson:project_lsp_json_of_yojson;
      validate_strict_json_file
        ~env
        ~project_dir
        ~setup_result
        ~path:".github/mcp.json"
        ~label:"Copilot MCP config"
        ~of_yojson:project_mcp_json_of_yojson;
    ]

let mcp_project_config_error_if_needed setup mcp_servers =
  match mcp_servers with
  | [] -> None
  | _ -> (
      let path = ".github/mcp.json" in
      match setup_outcome_for_path setup path with
      | Some outcome
        when Backend_config_writer.write_result_was_applied
               outcome.Backend_config_writer.result ->
          None
      | outcome ->
          Some
            (Printf.sprintf
               "Copilot MCP servers were requested, but %s was not applied \
                (%s). Refusing to run without the requested MCP config."
               path
               (setup_outcome_reason outcome)))

let preserves_existing_lsp_artifact ~project_dir lsp_servers artifact =
  lsp_servers = []
  && artifact.Backend_config_writer.project_relative_path = ".github/lsp.json"
  && Sys.file_exists (Filename.concat project_dir ".github/lsp.json")

let runtime_project_config_artifacts ~project_dir ~managed_namespace
    ~mcp_servers ~lsp_servers =
  project_config_artifacts ~managed_namespace ~mcp_servers ~lsp_servers
  |> List.filter (fun artifact ->
      not (preserves_existing_lsp_artifact ~project_dir lsp_servers artifact))

let run_task ~sw ~env ?on_raw_line:_ spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None -> (
      (* AC3/AC4 story #479: Copilot uses fixed project paths under .github/.
         Write instructions, repository settings, and project MCP config now so
         they are in place before invocation; pass the primary write outcome so the
         precedence warning accurately reflects whether the instruction file was
         applied. *)
      let setup =
        Backend_config_writer.setup_artifacts
          ~project_dir:spec.working_dir
          ~force:false
          (runtime_project_config_artifacts
             ~project_dir:spec.working_dir
             ~managed_namespace:spec.managed_namespace
             ~mcp_servers:spec.mcp_servers
             ~lsp_servers:spec.lsp_servers)
      in
      (match
         Backend_config_writer.precedence_warning_for
           ~backend_id:id
           ~write_outcome:setup.Backend_config_writer.write_outcome
       with
      | None -> ()
      | Some msg -> Diagnostics.user_warning "%s" msg) ;
      match mcp_project_config_error_if_needed setup spec.mcp_servers with
      | Some msg -> make_task_result ~status:(Failed msg) ()
      | None ->
          (* Copilot has no JSON output, so no cost parsing.  MCP has already been
             serialized into [.github/mcp.json], so clear [mcp_servers] before entering
             the shared runner to avoid creating an unused transient MCP file. *)
          let runtime_spec = {spec with mcp_servers = []} in
          Backend_process.run_task_with
            ~sw
            ~env
            ~spec:runtime_spec
            ~build_command
            ~parse_stdout:parse_stdout_text
            ())
