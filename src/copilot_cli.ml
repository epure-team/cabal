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

let native_json_schema_output = false

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
  {|# Copilot CLI Project Configuration

This file is managed by the host application via Cabal. Custom instructions
live at `.github/copilot-instructions.md`, repository settings live at
`.github/copilot/settings.json`, and project LSP config lives at
`.github/lsp.json` for Copilot CLI 1.0.54 (stable channel).

## Project Context

    Configured by the host application for this project.

## Runtime Status

Authenticated Copilot CLI 1.0.54 attachment behavior was observed during the
bounded investigation. No positive media evidence is recorded because
complete MCP discovery isolation is unproven. The runtime is quarantined, and
Cabal starts no Copilot task process.

## LSP Configuration

Copilot CLI uses Language Server Protocol (LSP) for enhanced code analysis. The
host writes project LSP server definitions to `.github/lsp.json` for detected
languages, following GitHub's documented project LSP config shape. Tooling
readiness is tracked by the host's own project hook layer.

## MCP Servers

MCP servers are unsupported by the quarantined task transport. Cabal does not
write repository MCP configuration for Copilot, and the runtime quarantine
applies even when a task requests no MCP server.

## Stable Limitations (Copilot CLI 1.0.54)

Cabal retains a tested candidate JSONL validator for future investigation, but
does not execute it while the runtime is quarantined. The following capabilities
are intentionally not supported:
- structured_output: disabled while quarantined; JSONL observations are dormant
- media_support: disabled; no positive evidence is recorded
- streaming_output: disabled; whole-stream verification is required
- session_resume: unsupported by the hardened adapter
- read_only_support: task requests fail closed
- file_reading: arbitrary prompt path references are unsupported
- web_support: Web_disabled
- native_json_schema_output: unsupported
<!-- stable-limitations: documented per backend parity policy -->
|}

let project_config_artifacts ~managed_namespace ~mcp_servers:_ ~lsp_servers =
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
  ]

type backend_invocation = {
  argv : string list;
  stdin : string option;
  redacted_argv : string list;
}

type verified_terminal = {
  text : string;
  session_id : string option;
  cost : cost option;
}

type verified_stream = {
  terminal : verified_terminal;
  events : Task_event.payload list;
  callback_lines : string list;
}

let invocation_error message = Error ("Copilot invocation rejected: " ^ message)

let path_is_safe path =
  path <> ""
  && not (Filename.is_relative path)
  && not (String.exists (fun character -> character = '\000' || character = '\n' || character = '\r') path)

let lowercase_suffix path suffix =
  String.ends_with ~suffix (String.lowercase_ascii path)

let path_matches_media_type path = function
  | Png -> lowercase_suffix path ".png"
  | Jpeg -> lowercase_suffix path ".jpg" || lowercase_suffix path ".jpeg"

let validate_attachment_paths attachment_paths attachments =
  if List.length attachment_paths <> List.length attachments then
    invocation_error "the sealed attachment set does not match the task"
  else if
    not
      (List.for_all2
         (fun path attachment ->
           path_is_safe path && path_matches_media_type path attachment.media_type)
         attachment_paths attachments)
  then invocation_error "the sealed attachment set is invalid"
  else Ok ()

let validate_transport_request ~attachment_delivery ~attachment_paths
    (spec : task_spec) =
  let ( let* ) = Result.bind in
  let* () =
    match spec.web_access with
    | Web_disabled -> Ok ()
    | Web_search | Web_search_and_fetch ->
        invocation_error "positive web access is unsupported"
  in
  let* () =
    if spec.read_only then invocation_error "read-only execution is unsupported"
    else Ok ()
  in
  let* () =
    match spec.resume_session_id with
    | None -> Ok ()
    | Some _ -> invocation_error "session resume is unsupported"
  in
  let* () =
    match spec.mcp_servers with
    | [] -> Ok ()
    | _ -> invocation_error "MCP servers are unsupported by the hardened tool set"
  in
  match attachment_delivery with
  | Reuse_session_attachments ->
      invocation_error "session attachment reuse is unsupported"
  | Upload_attachments ->
      validate_attachment_paths attachment_paths spec.attachments

let full_prompt spec =
  if String.length spec.instructions > 0 then
    Printf.sprintf "%s\n\n---\nProject Instructions:\n%s" spec.prompt
      spec.instructions
  else spec.prompt

let attachment_args paths =
  List.concat_map (fun path -> ["--attachment"; path]) paths

let redacted_attachment_args paths =
  List.mapi
    (fun index _ ->
      ["--attachment"; Printf.sprintf "<attachment-%d>" (index + 1)])
    paths
  |> List.concat

let attachment_directories paths =
  paths |> List.map Filename.dirname |> List.sort_uniq String.compare

let add_directory_args paths =
  attachment_directories paths
  |> List.concat_map (fun path -> ["--add-dir"; path])

let redacted_add_directory_args paths =
  attachment_directories paths
  |> List.mapi (fun index _ ->
         ["--add-dir"; Printf.sprintf "<attachment-directory-%d>" (index + 1)])
  |> List.concat

let fixed_cli_args =
  [
    "copilot";
    "--prefer-version";
    "1.0.54";
    "--no-auto-update";
    "--no-remote";
    "--no-experimental";
    "--no-ask-user";
    "--disable-builtin-mcps";
    "--output-format";
    "json";
    "--stream";
    "off";
    "--available-tools=view,grep,glob";
    "--allow-all-tools";
    "--deny-tool=shell";
    "--deny-tool=write";
    "--deny-tool=url";
    "--deny-tool=memory";
    "--disallow-temp-dir";
  ]

let build_invocation ?(attachment_paths = [])
    ?(attachment_delivery = Upload_attachments) ~config_home ~mcp_config_path:_
    (spec : task_spec) =
  let ( let* ) = Result.bind in
  let* () =
    if path_is_safe config_home then Ok ()
    else invocation_error "the isolated config directory is invalid"
  in
  let* () =
    validate_transport_request ~attachment_delivery ~attachment_paths spec
  in
  let model_args, redacted_model_args =
    match spec.model with
    | Some model -> (["--model"; model], ["--model"; "<model>"])
    | None -> ([], [])
  in
  let environment =
    [
      "env";
      "-u";
      "COPILOT_ALLOW_ALL";
      "-u";
      "COPILOT_ALLOW_ALL_PATHS";
      "-u";
      "COPILOT_ALLOW_ALL_URLS";
      "COPILOT_HOME=" ^ config_home;
      "NO_COLOR=1";
      "COPILOT_DISABLE_TERMINAL_TITLE=1";
    ]
  in
  let redacted_environment =
    [
      "env";
      "-u";
      "COPILOT_ALLOW_ALL";
      "-u";
      "COPILOT_ALLOW_ALL_PATHS";
      "-u";
      "COPILOT_ALLOW_ALL_URLS";
      "COPILOT_HOME=<isolated-config>";
      "NO_COLOR=1";
      "COPILOT_DISABLE_TERMINAL_TITLE=1";
    ]
  in
  Ok
    {
      argv =
        environment @ fixed_cli_args @ add_directory_args attachment_paths
        @ model_args @ attachment_args attachment_paths @ ["-p"; full_prompt spec];
      stdin = None;
      redacted_argv =
        redacted_environment @ fixed_cli_args
        @ redacted_add_directory_args attachment_paths @ redacted_model_args
        @ redacted_attachment_args attachment_paths @ ["-p"; "<prompt>"];
    }

let build_command ~mcp_config_path (spec : task_spec) =
  match
    build_invocation ~config_home:"/isolated-config" ~mcp_config_path spec
  with
  | Ok invocation -> (invocation.argv, "")
  | Error message -> invalid_arg message

type protocol_state = {
  seen_event_ids : string list;
  seen_user_message : bool;
  active_turn : string option;
  active_interaction : string option;
  turns_finished : int;
  requested_tools : (string * (string * string)) list;
  started_tools : (string * string) list;
  completed_tools : (string * string) list;
  final_text : string option;
  output_tokens : int;
  terminal : verified_terminal option;
  events_rev : Task_event.payload list;
  callback_lines_rev : string list;
}

let initial_protocol_state =
  {
    seen_event_ids = [];
    seen_user_message = false;
    active_turn = None;
    active_interaction = None;
    turns_finished = 0;
    requested_tools = [];
    started_tools = [];
    completed_tools = [];
    final_text = None;
    output_tokens = 0;
    terminal = None;
    events_rev = [];
    callback_lines_rev = [];
  }

let protocol_error category =
  Error ("Copilot protocol rejected: " ^ category)

let contains_substring value fragment =
  let value_length = String.length value in
  let fragment_length = String.length fragment in
  let rec loop offset =
    offset + fragment_length <= value_length
    &&
    (String.sub value offset fragment_length = fragment || loop (offset + 1))
  in
  fragment_length = 0 || loop 0

let member name fields = List.assoc_opt name fields

let json_string = function `String value -> Some value | _ -> None

let json_number = function
  | `Int value -> Some (float_of_int value)
  | `Float value -> Some value
  | _ -> None

let nonnegative_integer = function
  | `Int value when value >= 0 -> Some value
  | _ -> None

let safe_identifier value =
  let valid_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | ':' -> true
    | _ -> false
  in
  let length = String.length value in
  length > 0 && length <= 128 && String.for_all valid_character value

let canonical_uuid value =
  let hex = function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false
  in
  let rec characters_valid index =
    if index = String.length value then true
    else
      let valid =
        if List.mem index [8; 13; 18; 23] then value.[index] = '-'
        else hex value.[index]
      in
      valid && characters_valid (index + 1)
  in
  String.length value = 36 && characters_valid 0

let string_field name fields =
  Option.bind (member name fields) json_string

let object_field name fields =
  match member name fields with Some (`Assoc value) -> Some value | _ -> None

let list_field name fields =
  match member name fields with Some (`List value) -> Some value | _ -> None

let field_names fields = fields |> List.map fst |> List.sort String.compare

let exact_fields expected fields =
  field_names fields = List.sort String.compare expected

let validate_event_envelope state fields =
  let ( let* ) = Result.bind in
  let* () =
    if exact_fields ["type"; "id"; "parentId"; "timestamp"; "data"] fields
    then Ok ()
    else protocol_error "invalid event envelope"
  in
  let* identifier =
    match string_field "id" fields with
    | Some value when safe_identifier value -> Ok value
    | _ -> protocol_error "invalid event identifier"
  in
  let* () =
    if List.mem identifier state.seen_event_ids then
      protocol_error "duplicate event identifier"
    else Ok ()
  in
  let* _parent_id =
    match string_field "parentId" fields with
    | Some value -> Ok value
    | _ -> protocol_error "invalid parent event identifier"
  in
  let* _timestamp =
    match string_field "timestamp" fields with
    | Some value -> Ok value
    | _ -> protocol_error "invalid event timestamp"
  in
  let* data =
    match object_field "data" fields with
    | Some value -> Ok value
    | None -> protocol_error "invalid event data"
  in
  Ok
    ( {
        state with
        seen_event_ids = identifier :: state.seen_event_ids;
      },
      data )

let allowed_tool_name = function
  | "view" | "grep" | "glob" -> true
  | _ -> false

let add_tool_requests state ~turn_id requests =
  let ( let* ) = Result.bind in
  List.fold_left
    (fun result request ->
      let* state = result in
      match request with
      | `Assoc request_fields -> (
          match
            ( string_field "toolCallId" request_fields,
              string_field "name" request_fields,
              object_field "arguments" request_fields )
          with
          | Some identifier, Some name, Some _arguments
            when safe_identifier identifier && safe_identifier name
                 && allowed_tool_name name
                 && exact_fields
                      ["toolCallId"; "name"; "arguments"]
                      request_fields
                 && not (List.mem_assoc identifier state.requested_tools) ->
              Ok
                {
                  state with
                  requested_tools =
                    state.requested_tools @ [(identifier, (name, turn_id))];
                }
          | _ -> protocol_error "invalid tool request")
      | _ -> protocol_error "invalid tool request")
    (Ok state) requests

let validate_terminal_usage fields =
  let ( let* ) = Result.bind in
  match object_field "usage" fields with
  | Some usage_fields
    when exact_fields
           [
             "codeChanges";
             "premiumRequests";
             "sessionDurationMs";
             "totalApiDurationMs";
           ]
           usage_fields ->
      let* code_changes =
        match object_field "codeChanges" usage_fields with
        | Some fields
          when exact_fields ["filesModified"; "linesAdded"; "linesRemoved"] fields ->
            Ok fields
        | _ -> protocol_error "invalid terminal usage"
      in
      let files_unchanged =
        match list_field "filesModified" code_changes with
        | Some [] -> true
        | Some _ | None -> false
      in
      let integer_valid name fields =
        Option.bind (member name fields) nonnegative_integer |> Option.is_some
      in
      let number_valid name fields =
        match Option.bind (member name fields) json_number with
        | Some value -> Float.is_finite value && value >= 0.0
        | None -> false
      in
      if
        files_unchanged
        && member "linesAdded" code_changes = Some (`Int 0)
        && member "linesRemoved" code_changes = Some (`Int 0)
        && number_valid "premiumRequests" usage_fields
        && integer_valid "sessionDurationMs" usage_fields
        && integer_valid "totalApiDurationMs" usage_fields
      then Ok ()
      else protocol_error "invalid terminal usage"
  | _ -> protocol_error "invalid terminal usage"

let validate_ignored_record state record_type data =
  let required name predicate =
    match member name data with Some value -> predicate value | None -> false
  in
  let string_value = function `String _ -> true | _ -> false in
  let list_value = function `List _ -> true | _ -> false in
  let valid =
    match record_type with
    | "session.mcp_servers_loaded" ->
        not state.seen_user_message
        && exact_fields ["servers"] data
        && member "servers" data = Some (`List [])
    | "session.skills_loaded" ->
        not state.seen_user_message && exact_fields ["skills"] data
        && required "skills" list_value
    | "session.info" ->
        not state.seen_user_message
        && exact_fields ["infoType"; "message"] data
        && required "infoType" string_value
        && required "message" string_value
    | "session.tools_updated" ->
        not state.seen_user_message && exact_fields ["model"] data
        && required "model" string_value
    | "assistant.reasoning" ->
        Option.is_some state.active_turn
        && exact_fields ["content"; "reasoningId"] data
        && required "content" string_value
        && required "reasoningId" string_value
    | _ -> false
  in
  if valid then Ok state else protocol_error "invalid auxiliary event"

let add_output_tokens left right =
  if max_int - left < right then protocol_error "invalid assistant output tokens"
  else Ok (left + right)

let terminal_cost output_tokens =
  {
    tokens_input = None;
    tokens_output = Some output_tokens;
    cost_usd = None;
    cache_creation_input_tokens = None;
    cache_read_input_tokens = None;
  }

let sanitized_callback_lines ~text ~session_id ~output_tokens =
  let assistant =
    `Assoc
      [
        ("type", `String "assistant.message");
        ("data", `Assoc [("content", `String text)]);
      ]
  in
  let result =
    `Assoc
      [
        ("type", `String "result");
        ("exitCode", `Int 0);
        ("sessionId", `String session_id);
        ("usage", `Assoc [("outputTokens", `Int output_tokens)]);
      ]
  in
  List.map (Yojson.Safe.to_string ~std:true) [assistant; result]

let parse_result state _raw_line fields =
  let ( let* ) = Result.bind in
  let* () =
    if
      exact_fields ["type"; "timestamp"; "exitCode"; "sessionId"; "usage"]
        fields
    then Ok ()
    else protocol_error "terminal result fields changed"
  in
  let* _timestamp =
    match string_field "timestamp" fields with
    | Some value -> Ok value
    | _ -> protocol_error "invalid terminal timestamp"
  in
  let* exit_code =
    match member "exitCode" fields with
    | Some (`Int value) -> Ok value
    | None -> protocol_error "invalid terminal exit code"
    | Some _ -> protocol_error "invalid terminal exit code"
  in
  let* () =
    if exit_code = 0 then Ok ()
    else protocol_error "terminal result reports failure"
  in
  let* session_id =
    match string_field "sessionId" fields with
    | Some value when canonical_uuid value -> Ok value
    | _ -> protocol_error "invalid terminal session"
  in
  let* () = validate_terminal_usage fields in
  let* text =
    match state.final_text with
    | Some value when String.trim value <> "" -> Ok value
    | _ -> protocol_error "missing final assistant text"
  in
  let* () =
    if Option.is_some state.active_turn then
      protocol_error "terminal result has an active turn"
    else if state.turns_finished = 0 then
      protocol_error "terminal result has no finished turn"
    else if
      List.length state.requested_tools <> List.length state.started_tools
      || List.length state.started_tools <> List.length state.completed_tools
    then protocol_error "terminal result has incomplete tools"
    else Ok ()
  in
  let cost = terminal_cost state.output_tokens in
  let final_events =
    [
      Task_event.Agent_text_delta text;
      Task_event.Session_id session_id;
      Task_event.Token_usage cost;
    ]
  in
  Ok
    {
      state with
      terminal = Some {text; session_id = Some session_id; cost = Some cost};
      events_rev = List.rev_append final_events state.events_rev;
      callback_lines_rev =
        List.rev
          (sanitized_callback_lines ~text ~session_id
             ~output_tokens:state.output_tokens);
    }

let parse_event state _raw_line fields record_type =
  let ( let* ) = Result.bind in
  let* state, data = validate_event_envelope state fields in
  match record_type with
  | ( "session.mcp_servers_loaded" | "session.skills_loaded" | "session.info"
    | "session.tools_updated" | "assistant.reasoning" ) as record_type ->
      validate_ignored_record state record_type data
  | "user.message" ->
      let valid =
        not state.seen_user_message && Option.is_none state.active_turn
        && state.turns_finished = 0
        && exact_fields ["attachments"; "content"; "interactionId"] data
        && Option.is_some (list_field "attachments" data)
        && Option.is_some (string_field "content" data)
        &&
        match string_field "interactionId" data with
        | Some value -> safe_identifier value
        | None -> false
      in
      if valid then Ok {state with seen_user_message = true}
      else protocol_error "invalid user message order"
  | "assistant.turn_start" ->
      let turn_id = string_field "turnId" data in
      let interaction_id = string_field "interactionId" data in
      begin
        match (turn_id, interaction_id) with
        | Some turn_id, Some interaction_id
          when state.seen_user_message && Option.is_none state.active_turn
               && safe_identifier turn_id && safe_identifier interaction_id
               && exact_fields ["interactionId"; "turnId"] data ->
             Ok
               {
                 state with
                 active_turn = Some turn_id;
                 active_interaction = Some interaction_id;
                 final_text = None;
                 callback_lines_rev = [];
               }
        | _ -> protocol_error "invalid assistant turn start"
      end
  | "assistant.message" ->
      let turn_id = string_field "turnId" data in
      let interaction_id = string_field "interactionId" data in
      let message_id = string_field "messageId" data in
      let content = string_field "content" data in
      let output_tokens =
        Option.bind (member "outputTokens" data) nonnegative_integer
      in
      let requests = list_field "toolRequests" data in
      begin
        match (turn_id, interaction_id, message_id, content, output_tokens, requests) with
        | ( Some turn_id,
            Some interaction_id,
            Some message_id,
            Some content,
            Some output_tokens,
            Some requests )
          when state.active_turn = Some turn_id
               && state.active_interaction = Some interaction_id
               && safe_identifier message_id
               && exact_fields
                    [
                      "content";
                      "interactionId";
                      "messageId";
                      "outputTokens";
                      "toolRequests";
                      "turnId";
                    ]
                    data ->
            let* state = add_tool_requests state ~turn_id requests in
            let* output_tokens =
              add_output_tokens state.output_tokens output_tokens
            in
            Ok
              {
                state with
                final_text =
                  (if requests = [] && String.trim content <> "" then Some content
                   else None);
                output_tokens;
                callback_lines_rev = [];
              }
        | _ -> protocol_error "invalid assistant message"
      end
  | "tool.execution_start" ->
      let turn_id = string_field "turnId" data in
      let identifier = string_field "toolCallId" data in
      let name = string_field "toolName" data in
      begin
        match (turn_id, identifier, name) with
        | Some turn_id, Some identifier, Some name
          when state.active_turn = Some turn_id
               && allowed_tool_name name
               && exact_fields ["turnId"; "toolCallId"; "toolName"] data
               && List.assoc_opt identifier state.requested_tools
                  = Some (name, turn_id)
               && not (List.mem (identifier, turn_id) state.started_tools) ->
            Ok
              {
                state with
                started_tools = (identifier, turn_id) :: state.started_tools;
                events_rev =
                  Task_event.Tool_started {name; id = Some identifier}
                  :: state.events_rev;
              }
        | _ -> protocol_error "invalid tool execution start"
      end
  | "tool.execution_complete" ->
      let turn_id = string_field "turnId" data in
      let identifier = string_field "toolCallId" data in
      let succeeded = member "success" data = Some (`Bool true) in
      begin
        match (turn_id, identifier) with
        | Some turn_id, Some identifier
          when state.active_turn = Some turn_id && succeeded
               && (exact_fields ["turnId"; "toolCallId"; "success"] data
                  || exact_fields
                       ["turnId"; "toolCallId"; "toolName"; "success"]
                       data)
               && List.mem (identifier, turn_id) state.started_tools
               && not (List.mem (identifier, turn_id) state.completed_tools) -> (
            match List.assoc_opt identifier state.requested_tools with
            | Some (name, request_turn)
              when request_turn = turn_id && allowed_tool_name name
                   &&
                    (match member "toolName" data with
                    | None -> true
                    | Some (`String completed_name) ->
                        completed_name = name && allowed_tool_name completed_name
                    | Some _ -> false) ->
                Ok
                  {
                    state with
                    completed_tools =
                      (identifier, turn_id) :: state.completed_tools;
                    events_rev =
                      Task_event.Tool_finished
                        {name = Some name; id = Some identifier}
                      :: state.events_rev;
                  }
            | Some _ | None -> protocol_error "invalid tool execution completion")
        | _ -> protocol_error "invalid tool execution completion"
      end
  | "assistant.turn_end" ->
      begin
        match string_field "turnId" data with
        | Some turn_id
          when state.active_turn = Some turn_id
               && exact_fields ["turnId"] data
               && List.for_all
                    (fun (identifier, (_name, request_turn)) ->
                      request_turn <> turn_id
                      || List.mem (identifier, turn_id) state.completed_tools)
                    state.requested_tools ->
            Ok
              {
                state with
                active_turn = None;
                active_interaction = None;
                turns_finished = state.turns_finished + 1;
              }
        | Some turn_id when state.active_turn = Some turn_id ->
            protocol_error "turn end has outstanding tools"
        | _ -> protocol_error "invalid assistant turn end"
      end
  | _ -> protocol_error "unknown record type"

let parse_record state raw_line json =
  let ( let* ) = Result.bind in
  match json with
  | `Assoc fields ->
      let* record_type =
        match string_field "type" fields with
        | Some value when safe_identifier value -> Ok value
        | _ -> protocol_error "missing record type"
      in
      if Option.is_some state.terminal then
        protocol_error "record follows terminal result"
      else if String.equal record_type "result" then parse_result state raw_line fields
      else if
        contains_substring record_type "result"
        || contains_substring record_type "error"
      then protocol_error "backend error record"
      else parse_event state raw_line fields record_type
  | _ -> protocol_error "record is not an object"

let nonempty_lines stdout =
  stdout |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")

let verify_stream_stdout stdout =
  let ( let* ) = Result.bind in
  let* state =
    List.fold_left
      (fun result raw_line ->
        let* state = result in
        let* json =
          try Ok (Yojson.Safe.from_string raw_line)
          with Yojson.Json_error _ -> protocol_error "malformed JSONL"
        in
        parse_record state raw_line json)
      (Ok initial_protocol_state) (nonempty_lines stdout)
  in
  match state.terminal with
  | Some terminal ->
      Ok
        {
          terminal;
          events = List.rev state.events_rev;
          callback_lines = List.rev state.callback_lines_rev;
        }
  | None -> protocol_error "missing terminal result"

let verify_terminal_stdout stdout =
  Result.map
    (fun (verified : verified_stream) -> verified.terminal)
    (verify_stream_stdout stdout)

let normalized_events_of_stdout stdout =
  Result.map (fun verified -> verified.events) (verify_stream_stdout stdout)

let reconstructed_callback_lines_of_stdout stdout =
  Result.map (fun verified -> verified.callback_lines) (verify_stream_stdout stdout)

let parse_stdout_text stdout =
  match verify_terminal_stdout stdout with
  | Ok terminal -> terminal.text
  | Error _ -> ""

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
  | Some
      {Backend_config_writer.result = Backend_config_writer.Unsafe_project_path _; _}
    ->
      "project path was unsafe"

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
    ]

let preserves_existing_lsp_artifact ~project_dir lsp_servers artifact =
  lsp_servers = []
  && artifact.Backend_config_writer.project_relative_path = ".github/lsp.json"
  &&
  match
    Backend_config_writer.Private.inspect_project_file ~project_dir
      ~relative_path:".github/lsp.json"
  with
  | Backend_config_writer.Private.File _ -> true
  | Backend_config_writer.Private.Missing | Backend_config_writer.Private.Unsafe ->
      false

let runtime_project_config_artifacts ~project_dir ~managed_namespace
    ~mcp_servers ~lsp_servers =
  project_config_artifacts ~managed_namespace ~mcp_servers ~lsp_servers
  |> List.filter (fun artifact ->
      not (preserves_existing_lsp_artifact ~project_dir lsp_servers artifact))

let validate_complete_mcp_isolation () =
  (* 1.0.54 can discover user, workspace, installed-plugin, built-in, and
     account-controlled ODR MCP servers. [--disable-builtin-mcps] covers only
     one source, [COPILOT_HOME] isolates only local user/plugin state, and no
     flag disables all remaining sources. An empty workspace file therefore
     cannot prove that the effective MCP set is empty before process start. *)
  invocation_error "Copilot CLI 1.0.54 cannot disable all MCP discovery"

let setup_has_unsafe_path setup =
  List.exists
    (fun outcome ->
      match outcome.Backend_config_writer.result with
      | Backend_config_writer.Unsafe_project_path _ -> true
      | _ -> false)
    setup.Backend_config_writer.write_outcomes

let failed_result message =
  make_task_result ~status:(Failed message) ~stderr:message ~exit_code:1 ()

type transport_inputs = {
  attachment_delivery : Backend_types.attachment_delivery;
  attachment_paths : string list;
}

let validate_supported_request (spec : task_spec) =
  let ( let* ) = Result.bind in
  let* () =
    match spec.web_access with
    | Web_disabled -> Ok ()
    | Web_search | Web_search_and_fetch ->
        invocation_error "positive web access is unsupported"
  in
  let* () =
    if spec.read_only then invocation_error "read-only execution is unsupported"
    else Ok ()
  in
  let* () =
    match spec.resume_session_id with
    | None -> Ok ()
    | Some _ -> invocation_error "session resume is unsupported"
  in
  let* () =
    match spec.mcp_servers with
    | [] -> Ok ()
    | _ -> invocation_error "MCP servers are unsupported by the hardened tool set"
  in
  if
    List.for_all
      (fun attachment ->
        match attachment.media_type with Png | Jpeg -> true)
      spec.attachments
  then Ok ()
  else invocation_error "an attachment media type is unsupported"

let requested_attachment_delivery context spec =
  match Task_execution_context.requested_delivery context with
  | None -> Ok Backend_types.Upload_attachments
  | Some delivery
    when delivery.attachment_references = spec.Backend_types.attachments
         && delivery.web_access_policy = spec.web_access ->
      Ok delivery.attachment_delivery
  | Some _ ->
      invocation_error "the execution delivery context does not match the task"

let requested_transport_inputs ?context spec =
  if spec.Backend_types.attachments = [] then
    let ( let* ) = Result.bind in
    let* attachment_delivery =
      match context with
      | None -> Ok Backend_types.Upload_attachments
      | Some context -> requested_attachment_delivery context spec
    in
    Ok {attachment_delivery; attachment_paths = []}
  else
    match context with
    | None ->
        invocation_error "central prepared transport authorization is required"
    | Some context -> (
        match
          Task_execution_context.sealed_attachment_delivery context ~backend_id:id
            ~attachment_references:spec.attachments
            ~web_access_policy:spec.web_access
        with
        | Error _ ->
            invocation_error "central prepared transport authorization failed"
        | Ok sealed ->
            Ok
              {
                attachment_delivery = sealed.attachment_delivery;
                attachment_paths = sealed.attachment_paths;
              })

external secure_remove_tree : string -> bool = "cabal_secure_remove_tree"

let cleanup_retry_limit = 3
let cleanup_failure_message = "Copilot config isolation cleanup failed"

let cleanup_isolated_home ?(on_cleanup_attempt = fun () -> ()) directory =
  Eio.Cancel.protect (fun () ->
      let rec loop remaining =
        let removed =
          try
            on_cleanup_attempt () ;
            secure_remove_tree directory
          with _ -> false
        in
        if removed then true
        else if remaining > 1 then (
          Eio.Fiber.yield () ;
          loop (remaining - 1))
        else false
      in
      loop cleanup_retry_limit)

let with_isolated_config_home ?on_cleanup_attempt f =
  match
    try Ok (Filename.temp_dir ~perms:0o700 "cabal-copilot-config-" "")
    with Sys_error _ | Unix.Unix_error _ -> Error ()
  with
  | Error () -> failed_result "Copilot config isolation could not be created"
  | Ok directory ->
      let outcome =
        try `Result (f directory) with
        | (Out_of_memory | Stack_overflow | Sys.Break) as fatal -> `Fatal fatal
        | error -> `Exception error
      in
      let removed = cleanup_isolated_home ?on_cleanup_attempt directory in
      if not removed then Diagnostics.warn "%s" cleanup_failure_message ;
      match outcome with
      | `Fatal fatal -> raise fatal
      | `Exception error -> raise error
      | `Result result ->
          if removed then result else failed_result cleanup_failure_message

let scrub_process_result result =
  {
    status = result.Backend_process.status;
    files_changed = [];
    report = None;
    elapsed = result.elapsed;
    cost = None;
    stdout = "";
    agent_text = "";
    stderr = "";
    exit_code = result.exit_code;
    session_id = None;
  }

let run_invocation ~sw ~env ~spec ?context ?on_raw_line transport =
  match validate_complete_mcp_isolation () with
  | Error message -> failed_result message
  | Ok () ->
      with_isolated_config_home (fun config_home ->
        match
          build_invocation
            ~attachment_paths:transport.attachment_paths
            ~attachment_delivery:transport.attachment_delivery ~config_home
            ~mcp_config_path:None spec
        with
        | Error message -> failed_result message
        | Ok invocation ->
            Diagnostics.debug "backend command: %s"
              (String.concat " " invocation.redacted_argv) ;
            Option.iter Task_execution_context.claim_structured_text context ;
            let result =
              Backend_process.run_process
                ~sw ~env ~cmd:invocation.argv ~stdin_content:invocation.stdin
                ~working_dir:spec.working_dir
                ~timeout_seconds:(duration_to_seconds spec.timeout)
                ?context ()
            in
            begin
              match result.status with
              | Failed _ | Timeout | Cancelled -> scrub_process_result result
              | Success -> (
                  match verify_stream_stdout result.stdout with
                  | Error message ->
                      {
                        (scrub_process_result result) with
                        status = Failed message;
                        stderr = message;
                      }
                  | Ok verified ->
                      Option.iter
                        (fun callback ->
                          List.iter callback verified.callback_lines)
                        on_raw_line ;
                      Option.iter
                        (fun context ->
                          List.iter
                            (Task_execution_context.emit context)
                            verified.events ;
                          Task_execution_context.mark_final_public_text context)
                        context ;
                      {
                        status = Success;
                        files_changed =
                          Backend_process.get_git_diff ~sw ~env
                            ~working_dir:spec.working_dir;
                        report = None;
                        elapsed = result.elapsed;
                        cost = verified.terminal.cost;
                        stdout = "";
                        agent_text = verified.terminal.text;
                        stderr = "";
                        exit_code = result.exit_code;
                        session_id = verified.terminal.session_id;
                      })
            end)

let run_task ~sw ~env ?context ?on_raw_line spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None ->
      (match validate_supported_request spec with
      | Error message -> failed_result message
      | Ok () ->
          (match requested_transport_inputs ?context spec with
          | Error message -> failed_result message
          | Ok transport ->
              match validate_complete_mcp_isolation () with
              | Error message -> failed_result message
              | Ok () ->
                  match
                    build_invocation
                      ~attachment_paths:transport.attachment_paths
                      ~attachment_delivery:transport.attachment_delivery
                      ~config_home:"/isolated-config" ~mcp_config_path:None spec
                  with
                  | Error message -> failed_result message
                  | Ok _ ->
                      let setup =
                        Backend_config_writer.setup_artifacts
                          ~project_dir:spec.working_dir ~force:false
                          (runtime_project_config_artifacts
                             ~project_dir:spec.working_dir
                             ~managed_namespace:spec.managed_namespace
                             ~mcp_servers:[] ~lsp_servers:spec.lsp_servers)
                      in
                      (match
                         Backend_config_writer.precedence_warning_for
                           ~backend_id:id
                           ~write_outcome:
                             setup.Backend_config_writer.write_outcome
                       with
                      | None -> ()
                      | Some message -> Diagnostics.user_warning "%s" message) ;
                      if setup_has_unsafe_path setup then
                        failed_result
                          "Copilot project configuration path is unsafe"
                      else
                        run_invocation ~sw ~env ~spec ?context ?on_raw_line
                          transport))

module Private = struct
  type nonrec backend_invocation = backend_invocation = {
    argv : string list;
    stdin : string option;
    redacted_argv : string list;
  }

  type nonrec verified_terminal = verified_terminal = {
    text : string;
    session_id : string option;
    cost : cost option;
  }

  let build_invocation = build_invocation
  let build_command = build_command
  let verify_terminal_stdout = verify_terminal_stdout
  let normalized_events_of_stdout = normalized_events_of_stdout
  let reconstructed_callback_lines_of_stdout =
    reconstructed_callback_lines_of_stdout
  let parse_stdout_text = parse_stdout_text
  let cleanup_retry_limit = cleanup_retry_limit

  let with_isolated_config_home_for_test ?on_cleanup_attempt f =
    with_isolated_config_home ?on_cleanup_attempt f
end
