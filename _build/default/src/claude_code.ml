(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Backend_types

let id = "claude-code"

let name = "Claude Code"

(* Static fallback — used when the Anthropic Models API is unreachable or
   ANTHROPIC_API_KEY is absent.  Keep in sync with
   https://docs.anthropic.com/en/docs/about-claude/models/overview *)
let models =
  [
    "claude-opus-4-7";
    "claude-sonnet-4-6";
    "claude-haiku-4-5-20251001";
    "claude-opus-4-6";
    "claude-sonnet-4-5-20250929";
  ]

(* Parse the model id strings out of a GET /v1/models JSON response.
   Shape: {"data": [{"id": "claude-opus-4-7", ...}, ...], ...} *)
let parse_anthropic_models_json json_str =
  try
    match Yojson.Safe.from_string json_str with
    | `Assoc fields -> (
        match List.assoc_opt "data" fields with
        | Some (`List items) ->
            List.filter_map
              (function
                | `Assoc fs -> (
                    match List.assoc_opt "id" fs with
                    | Some (`String id) -> Some id
                    | _ -> None)
                | _ -> None)
              items
        | _ -> [])
    | _ -> []
  with _ -> []

(* Live probe via the Anthropic Models REST API.
   Requires ANTHROPIC_API_KEY in the environment and curl on PATH.
   Falls back gracefully to [models] when either is missing. *)
let models_probe =
  Some
    (fun ~sw:_ ~env ->
      match Sys.getenv_opt "ANTHROPIC_API_KEY" with
      | None -> Error "ANTHROPIC_API_KEY not set"
      | Some api_key -> (
          match
            Backend_process.capture_version_output
              ~env
              ~timeout_seconds:10.0
              [
                "curl";
                "-sf";
                "-H";
                "x-api-key: " ^ api_key;
                "-H";
                "anthropic-version: 2023-06-01";
                "https://api.anthropic.com/v1/models?limit=100";
              ]
          with
          | Error msg -> Error msg
          | Ok json_str -> (
              match parse_anthropic_models_json json_str with
              | [] ->
                  Error "Anthropic models API returned no parseable model IDs"
              | ms -> Ok ms)))

(* Check if claude CLI is available by running `claude --version` *)
let available ~sw:_ ~env =
  Backend_process.check_available ~env ["claude"; "--version"]

let supports_session_resume = true

(* Claude Code CLI v2.1.117+ supports native JSON Schema-constrained output
   via --json-schema <inline-schema-json>.  The CLI enforces the schema at
   invocation time using JSON Schema draft 2020-12, returning a non-zero exit
   code when the schema contains unsupported keywords.  Callers using the
   native path must supply a draft-2020-12-compatible schema.
   Evidence: see backend_registry.ml for the capability_evidence record. *)
let native_json_schema_output = true

type permissions_json = {allow : string list; deny : string list}
[@@deriving yojson]

type lsp_server_json = {command : string; args : string list}
[@@deriving yojson]

type settings_json = {
  permissions : permissions_json;
  lsp : lsp_server_json list option; [@yojson.option]
}
[@@deriving yojson]

let settings_body lsp_servers =
  let lsp =
    match lsp_servers with
    | [] -> None
    | servers ->
        Some
          (List.map
             (fun (cfg : Backend_types.lsp_server_config) ->
               {command = cfg.command; args = cfg.args})
             servers)
  in
  let json =
    match
      settings_json_to_yojson {permissions = {allow = []; deny = []}; lsp}
    with
    | `Assoc fields ->
        `Assoc (List.filter (fun (_name, value) -> value <> `Null) fields)
    | other -> other
  in
  json |> Yojson.Safe.pretty_to_string |> fun s -> s ^ "\n"

let project_config_artifacts ~managed_namespace ~mcp_servers:_ ~lsp_servers =
  let content =
    Backend_config_writer.with_epure_header
      ~managed_namespace
      Backend_config_writer.Slash
      (settings_body lsp_servers)
  in
  [
    {
      Backend_config_writer.backend_id = id;
      ownership = Backend_config_writer.Epure_owned;
      managed_namespace;
      project_relative_path =
        Filename.concat managed_namespace.config_dir "claude-code/settings.json";
      content;
    };
  ]

let contains_substring haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    i + nlen <= hlen && (String.sub haystack i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let is_resume_failure_text text =
  let lower = String.lowercase_ascii text in
  (contains_substring lower "resume" || contains_substring lower "resum")
  && (contains_substring lower "fail"
     || contains_substring lower "invalid"
     || contains_substring lower "not found"
     || contains_substring lower "unknown"
     || contains_substring lower "missing"
     || contains_substring lower "expired")

let is_resume_failure (result : task_result) =
  match result.status with
  | Failed msg ->
      is_resume_failure_text msg
      || is_resume_failure_text result.stdout
      || is_resume_failure_text result.stderr
  | Success | Timeout | Cancelled -> false

let status_text = function
  | Success -> "success"
  | Failed msg -> msg
  | Timeout -> "timed out"
  | Cancelled -> "cancelled"

let invalid_process_result cmd (result : Backend_process.process_result) =
  Agentic_backend.Config_invalid
    (Printf.sprintf
       "native config validation failed: %s\n\
        status: %s\n\
        stdout:\n\
        %s\n\
        stderr:\n\
        %s"
       (String.concat " " cmd)
       (status_text result.status)
       result.stdout
       result.stderr)

let check_project_config ~sw ~env ~project_dir ~setup_result =
  match setup_result.Backend_config_writer.project_config_path with
  | None ->
      Agentic_backend.Config_check_unsupported
        "Claude Code settings were not applied; no generated --settings path \
         to validate"
  | Some settings_path -> (
      if not (Sys.file_exists settings_path) then
        Agentic_backend.Config_invalid
          (Printf.sprintf
             "generated Claude Code settings file does not exist: %s"
             settings_path)
      else if not (available ~sw ~env) then
        Agentic_backend.Config_check_unsupported
          "Claude Code CLI is not available on PATH; cannot run native \
           --init-only validation"
      else
        let cmd = ["claude"; "--settings"; settings_path; "--init-only"] in
        try
          let result =
            Backend_process.run_process
              ~sw
              ~env
              ~cmd
              ~working_dir:project_dir
              ~timeout_seconds:10.0
              ()
          in
          match result.status with
          | Success -> Agentic_backend.Config_valid
          | Failed _ | Timeout | Cancelled -> invalid_process_result cmd result
        with e ->
          Agentic_backend.Config_check_unsupported
            (Printf.sprintf
               "Claude Code native config validation could not run: %s"
               (Printexc.to_string e)))

(* Write MCP server configuration to a JSON file *)
let write_mcp_config = Backend_process.write_mcp_config

(* Parse Claude Code's JSON output format *)
let parse_json_output json =
  let open Yojson.Safe.Util in
  (* Claude Code JSON output contains a "result" field with the text response,
     or a native-schema "structured_output" value, and optionally "usage" with
     token counts. *)
  let result_text =
    match json |> member "structured_output" with
    | `Null -> (
        try json |> member "result" |> to_string
        with Type_error _ -> (
          (* Fallback: try to get raw text if not in expected format *)
          try Yojson.Safe.to_string json with _ -> ""))
    | structured -> Yojson.Safe.to_string structured
  in
  let cost =
    try
      let usage = json |> member "usage" in
      if usage = `Null then None
      else
        let input_tokens =
          try Some (usage |> member "input_tokens" |> to_int) with _ -> None
        in
        let output_tokens =
          try Some (usage |> member "output_tokens" |> to_int) with _ -> None
        in
        let cache_creation =
          try Some (usage |> member "cache_creation_input_tokens" |> to_int)
          with _ -> None
        in
        let cache_read =
          try Some (usage |> member "cache_read_input_tokens" |> to_int)
          with _ -> None
        in
        Some
          {
            tokens_input = input_tokens;
            tokens_output = output_tokens;
            cost_usd = None;
            cache_creation_input_tokens = cache_creation;
            cache_read_input_tokens = cache_read;
          }
    with _ -> None
  in
  (result_text, cost)

(* Extract session_id from Claude Code JSON stdout.
   Handles both single JSON (--output-format json) and JSONL
   (--output-format stream-json) by scanning each line. The session_id
   typically appears in the first "init" event. *)
let parse_session_id_from_stdout stdout =
  let try_line line =
    try
      let json = Yojson.Safe.from_string line in
      Yojson.Safe.Util.(json |> member "session_id" |> to_string_option)
    with _ -> None
  in
  let lines = String.split_on_char '\n' stdout in
  let rec find = function
    | [] -> None
    | line :: rest -> (
        match try_line line with Some _ as sid -> sid | None -> find rest)
  in
  find lines

(* Get list of changed files via git diff *)
let get_git_diff = Backend_process.get_git_diff

(* Get full diff content *)
let get_git_diff_content = Backend_process.get_git_diff_content

(* Parse cost from stdout string *)
let parse_cost_from_stdout stdout =
  try
    let json = Yojson.Safe.from_string stdout in
    let _, cost = parse_json_output json in
    cost
  with _ -> None

(* Build the claude command with all required arguments.
   Returns (command, prompt) where prompt should be passed via stdin.
   This avoids "Argument list too long" errors for large prompts.
   [project_config_path]: when [Some path], pass [--settings path] so
   the Épure-owned settings.json is injected explicitly at invocation
   (AC5 of story #478). *)
let build_command ?(streaming = false) ?(project_config_path = None)
    ~mcp_config_path (spec : task_spec) =
  let output_format = if streaming then "stream-json" else "json" in
  let resume_args =
    match spec.resume_session_id with
    | Some sid -> ["--resume"; sid]
    | None -> []
  in
  (* Read-only agents (validators) cannot compile, patch, or run tests.
     They may still read files and search code.  Use --disallowedTools to
     block write/execute tools while keeping read tools available.

     Note: --disallowedTools must NOT be combined with --allowedTools — the
     combination causes silent empty output in claude 2.1.70+.  Read-only
     mode therefore uses only --disallowedTools; builder mode uses only
     --allowedTools. *)
  let tool_args =
    if spec.read_only then
      [
        "--dangerously-skip-permissions";
        "--disallowedTools";
        "Bash,Edit,Write,NotebookEdit";
      ]
    else
      [
        "--dangerously-skip-permissions";
        "--allowedTools";
        "WebSearch,WebFetch,Read,Glob,Grep,Bash,Edit,Write,Task";
      ]
  in
  let base =
    ["claude"; "--print"; "--output-format"; output_format]
    @ (if streaming then ["--verbose"] else [])
    @ resume_args @ tool_args
    @ [
        (* Skip project CLAUDE.md/AGENTS.md - only load user settings *)
        "--setting-sources";
        "user";
        (* Read prompt from stdin *)
        "-p";
        "-";
      ]
  in
  let mcp_args =
    match mcp_config_path with
    | Some path -> ["--mcp-config"; path]
    | None -> []
  in
  (* Épure-owned project settings.json passed via --settings (AC5 story #478).
     The flag injects permissions/settings without relying on user-global
     discovery; placed after --setting-sources so it can complement it. *)
  let config_args =
    match project_config_path with
    | Some path -> ["--settings"; path]
    | None -> []
  in
  let model_args =
    match spec.model with Some m -> ["--model"; m] | None -> []
  in
  let max_turns_args =
    match spec.max_turns with
    | Some n -> ["--max-turns"; string_of_int n]
    | None -> []
  in
  (* Native JSON Schema constraint — passed when spec.json_schema is set.
     The CLI validates the schema at invocation; unsupported keywords cause a
     non-zero exit which the enforcer surfaces as native rejection (D-5). *)
  let schema_args =
    match spec.json_schema with
    | Some s -> ["--json-schema"; Yojson.Safe.to_string ~std:true s]
    | None -> []
  in
  (* Combine prompt and instructions into a single task description *)
  let full_prompt =
    if String.length spec.instructions > 0 then
      Printf.sprintf
        "%s\n\n---\nProject Instructions:\n%s"
        spec.prompt
        spec.instructions
    else spec.prompt
  in
  (* Return command args and prompt separately - prompt goes via stdin *)
  ( base @ mcp_args @ config_args @ model_args @ max_turns_args @ schema_args,
    full_prompt )

(** Parse a stream-json event line and extract displayable content. Returns Some
    text if there's something to display, None otherwise. *)
let parse_stream_event line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let event_type = json |> member "type" |> to_string_option in
    match event_type with
    | Some "assistant" -> (
        (* Assistant message with content blocks *)
        let message = json |> member "message" in
        let content = message |> member "content" |> to_list in
        let texts =
          List.filter_map
            (fun block ->
              let block_type = block |> member "type" |> to_string_option in
              match block_type with
              | Some "text" -> block |> member "text" |> to_string_option
              | Some "tool_use" ->
                  let name =
                    block |> member "name" |> to_string_option
                    |> Option.value ~default:"tool"
                  in
                  (* Extract file path from input for common tools *)
                  let input = block |> member "input" in
                  let arg =
                    match name with
                    | "Edit" | "Write" | "Read" ->
                        input |> member "file_path" |> to_string_option
                    | "Bash" ->
                        input |> member "command" |> to_string_option
                        |> Option.map (fun c ->
                            if String.length c > 60 then
                              String.sub c 0 57 ^ "..."
                            else c)
                    | "Glob" -> input |> member "pattern" |> to_string_option
                    | "Grep" -> input |> member "pattern" |> to_string_option
                    | _ -> None
                  in
                  let suffix =
                    match arg with Some a -> " " ^ a | None -> ""
                  in
                  Some (Printf.sprintf "\xe2\x86\x92 %s%s" name suffix)
              | _ -> None)
            content
        in
        match texts with [] -> None | _ -> Some (String.concat "\n" texts))
    | Some "user" -> (
        (* User message — keep text blocks, skip tool_result blocks *)
        let message = json |> member "message" in
        let content = message |> member "content" |> to_list in
        let texts =
          List.filter_map
            (fun block ->
              let block_type = block |> member "type" |> to_string_option in
              match block_type with
              | Some "text" -> block |> member "text" |> to_string_option
              | _ -> None)
            content
        in
        match texts with [] -> None | _ -> Some (String.concat "\n" texts))
    | Some "result" ->
        (* Final result *)
        let subtype = json |> member "subtype" |> to_string_option in
        let is_error =
          json |> member "is_error" |> to_bool_option
          |> Option.value ~default:false
        in
        let status = if is_error then "error" else "success" in
        Some
          (Printf.sprintf
             "[Build %s: %s]"
             status
             (Option.value subtype ~default:"done"))
    | Some "system" -> (
        let subtype = json |> member "subtype" |> to_string_option in
        match subtype with
        | Some "init" ->
            (* Init event: extract session ID, skip raw JSON dump *)
            let sid =
              json |> member "session_id" |> to_string_option
              |> Option.value ~default:"unknown"
            in
            let short_id =
              if String.length sid > 12 then String.sub sid 0 12 ^ "..."
              else sid
            in
            Some (Printf.sprintf "[Session: %s]" short_id)
        | _ ->
            (* Other system messages *)
            let message = json |> member "message" in
            let content = message |> member "content" |> to_string_option in
            Option.map (fun c -> Printf.sprintf "[System: %s]" c) content)
    | _ -> None
  with _ -> None

(* Extract response text from Claude Code JSON stdout *)
let parse_stdout_text stdout =
  try
    let json = Yojson.Safe.from_string stdout in
    let text, _ = parse_json_output json in
    text
  with _ -> stdout

let run_task ~sw ~env ?on_raw_line:_ spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None ->
      (* Generate and write Épure-owned project settings.json; pass the path
     explicitly via --settings so project config takes precedence over any
     user-global discovery (AC5 story #478). *)
      let project_config_path =
        let setup =
          Backend_config_writer.setup_artifacts
            ~project_dir:spec.working_dir
            ~force:false
            (project_config_artifacts
               ~managed_namespace:spec.managed_namespace
               ~mcp_servers:[]
               ~lsp_servers:spec.lsp_servers)
        in
        setup.project_config_path
      in
      let build_cmd ~mcp_config_path s =
        build_command ~project_config_path ~mcp_config_path s
      in
      Backend_process.run_task_with
        ~sw
        ~env
        ~spec
        ~build_command:build_cmd
        ~parse_cost:parse_cost_from_stdout
        ~parse_stdout:parse_stdout_text
        ~parse_session_id:parse_session_id_from_stdout
        ()

let run_task_streaming ~sw ~env ~on_stdout ?on_raw_line spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None ->
      (* Generate and write Épure-owned project settings.json (AC5 story #478). *)
      let project_config_path =
        let setup =
          Backend_config_writer.setup_artifacts
            ~project_dir:spec.working_dir
            ~force:false
            (project_config_artifacts
               ~managed_namespace:spec.managed_namespace
               ~mcp_servers:[]
               ~lsp_servers:spec.lsp_servers)
        in
        setup.project_config_path
      in
      (* Wrap the callback to parse stream-json events and only show relevant
         content.  Each raw line is forwarded to [on_raw_line] before parsing so
         callers can capture the full backend-native event stream (Story #466). *)
      let on_stdout_parsed line =
        Option.iter (fun cb -> cb line) on_raw_line ;
        match parse_stream_event line with
        | Some text -> on_stdout text
        | None -> ()
      in
      let build_cmd ~mcp_config_path s =
        build_command ~streaming:true ~project_config_path ~mcp_config_path s
      in
      Backend_process.run_task_with
        ~sw
        ~env
        ~spec
        ~build_command:build_cmd
        ~parse_cost:parse_cost_from_stdout
        ~parse_stdout:parse_stdout_text
        ~parse_session_id:parse_session_id_from_stdout
        ~on_stdout:on_stdout_parsed
        ()
