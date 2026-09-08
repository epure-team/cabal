(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Backend_types

let id = "codex"
let name = "OpenAI Codex"

(* Static fallback — used when OPENAI_API_KEY is absent or the probe fails. *)
let models =
  [ "gpt-5"; "gpt-4o"; "gpt-4o-mini"; "o3"; "o3-mini"; "o1"; "o1-mini" ]

(* Keep only chat / reasoning models from the OpenAI /v1/models response;
   skip embeddings, tts, whisper, dall-e, etc. *)
let is_chat_model id =
  let starts s prefix =
    String.length s >= String.length prefix
    && String.sub s 0 (String.length prefix) = prefix
  in
  starts id "gpt-" || starts id "o1" || starts id "o2" || starts id "o3"
  || starts id "o4" || starts id "o5"

let parse_openai_models_json json_str =
  try
    match Yojson.Safe.from_string json_str with
    | `Assoc fields -> (
        match List.assoc_opt "data" fields with
        | Some (`List items) ->
            List.filter_map
              (function
                | `Assoc fs -> (
                    match List.assoc_opt "id" fs with
                    | Some (`String id) when is_chat_model id -> Some id
                    | _ -> None)
                | _ -> None)
              items
        | _ -> [])
    | _ -> []
  with _ -> []

(* Live probe via OpenAI Models REST API.
   Requires OPENAI_API_KEY in the environment and curl on PATH. *)
let models_probe =
  Some
    (fun ~sw:_ ~env ->
      match Sys.getenv_opt "OPENAI_API_KEY" with
      | None -> Error "OPENAI_API_KEY not set"
      | Some api_key -> (
          match
            Backend_process.capture_version_output ~env ~timeout_seconds:10.0
              [
                "curl";
                "-sf";
                "-H";
                "Authorization: Bearer " ^ api_key;
                "https://api.openai.com/v1/models";
              ]
          with
          | Error msg -> Error msg
          | Ok json_str -> (
              match parse_openai_models_json json_str with
              | [] ->
                  Error "OpenAI models API returned no parseable chat model IDs"
              | ms -> Ok ms)))

let available ~sw:_ ~env =
  Backend_process.check_available ~env [ "codex"; "--version" ]

let supports_session_resume = true
let native_json_schema_output = true

let config_body =
  "# MCP servers are disabled by default — activate only approved\n\
   # registry-backed entries.  Codex uses top-level [mcp_servers.<name>]\n\
   # tables; keep this example commented until a server is approved.\n\
   # [mcp_servers.example]\n\
   # command = \"example-server\"\n\
   # args = []\n"

let toml_basic_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\r' -> Buffer.add_string buf "\\r"
      | c ->
          let code = Char.code c in
          if code < 0x20 then
            Buffer.add_string buf (Printf.sprintf "\\u%04X" code)
          else Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let is_bare_key s =
  let len = String.length s in
  len > 0
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true | _ -> false)
       s

let toml_key s = if is_bare_key s then s else toml_basic_string s

let toml_string_array values =
  values |> List.map toml_basic_string |> String.concat ", "
  |> Printf.sprintf "[%s]"

let env_reference_value ~name value =
  if String.length value > 0 && value.[0] = '$' then value
  else if name = "" then ""
  else "$" ^ name

let persistent_env_references env =
  env
  |> List.filter (fun (name, _) -> name <> "")
  |> List.map (fun (name, value) -> (name, env_reference_value ~name value))

let toml_env_entries env =
  env
  |> List.map (fun (key, value) ->
      Printf.sprintf "%s = %s" (toml_key key) (toml_basic_string value))
  |> String.concat "\n"

let mcp_server_toml (cfg : mcp_server_config) =
  let server_key = toml_key cfg.name in
  let base =
    Printf.sprintf "[mcp_servers.%s]\ncommand = %s\nargs = %s\n" server_key
      (toml_basic_string cfg.command)
      (toml_string_array cfg.args)
  in
  match persistent_env_references cfg.env with
  | [] -> base
  | env ->
      Printf.sprintf "%s\n[mcp_servers.%s.env]\n%s\n" base server_key
        (toml_env_entries env)

let config_body_for_mcp_servers = function
  | [] -> config_body
  | servers ->
      config_body ^ "\n" ^ String.concat "\n" (List.map mcp_server_toml servers)

let project_config_artifacts ~managed_namespace ~mcp_servers ~lsp_servers:_ =
  [
    {
      Backend_config_writer.backend_id = id;
      ownership = Backend_config_writer.Backend_project;
      managed_namespace;
      project_relative_path = ".codex/config.toml";
      content =
        Backend_config_writer.with_managed_header ~managed_namespace
          Backend_config_writer.Hash ~backend_id:id
          (config_body_for_mcp_servers mcp_servers);
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

let config_applied setup_result =
  match setup_result.Backend_config_writer.write_outcome with
  | Some result -> Backend_config_writer.write_result_was_applied result
  | None -> false

let write_outcome_reason = function
  | None -> "no setup outcome was recorded"
  | Some (Backend_config_writer.Skipped_user_content _) ->
      "user-authored .codex/config.toml was skipped"
  | Some (Backend_config_writer.Refused_hash_mismatch _) ->
      ".codex/config.toml was refused because of a hash mismatch"
  | Some Backend_config_writer.Already_current ->
      ".codex/config.toml is current"
  | Some (Backend_config_writer.Written _) -> ".codex/config.toml was written"
  | Some (Backend_config_writer.Backed_up_and_written _) ->
      ".codex/config.toml was backed up and written"
  | Some (Backend_config_writer.Invalid_managed_namespace _) ->
      "managed namespace was invalid"
  | Some (Backend_config_writer.Unsafe_project_path _) ->
      "project path was unsafe"

let mcp_config_error_if_needed setup mcp_servers =
  match mcp_servers with
  | [] -> None
  | _ when config_applied setup -> None
  | _ ->
      Some
        (Printf.sprintf
           "Codex MCP servers were requested, but .codex/config.toml was not \
            updated (%s). Refusing to run without the requested MCP config."
           (write_outcome_reason setup.Backend_config_writer.write_outcome))

let read_project_file ~env ~project_dir rel_path =
  let path = Filename.concat project_dir rel_path in
  try Ok (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path))
  with e ->
    Error (Printf.sprintf "could not read %s: %s" path (Printexc.to_string e))

let active_mcp_table_line line =
  let trimmed = String.trim line in
  trimmed <> ""
  && trimmed.[0] <> '#'
  && trimmed.[0] = '['
  && contains_substring trimmed "mcp"

let active_mcp_table_has_valid_shape line =
  let trimmed = String.trim line in
  String.starts_with ~prefix:"[mcp_servers." trimmed

let check_project_config ~sw:_ ~env ~project_dir ~setup_result =
  if not (config_applied setup_result) then
    Agentic_backend.Config_check_unsupported
      "Codex config was not applied; refusing to validate user-authored \
       .codex/config.toml"
  else
    match read_project_file ~env ~project_dir ".codex/config.toml" with
    | Error msg -> Agentic_backend.Config_invalid msg
    | Ok content -> (
        let lines = String.split_on_char '\n' content in
        let findings = ref [] in
        let add finding = findings := finding :: !findings in
        if contains_substring content "[model]" then
          add "stale [model] table is present";
        if contains_substring content "model_provider" then
          add "stale model_provider key is present";
        if contains_substring content "provider =" then
          add "stale provider assignment is present";
        if not (contains_substring content "# [mcp_servers.example]") then
          add "commented [mcp_servers.example] template is missing";
        if
          List.exists
            (fun line ->
              active_mcp_table_line line
              && not (active_mcp_table_has_valid_shape line))
            lines
        then add "active MCP tables must use [mcp_servers.<name>] shape";
        match !findings with
        | [] -> Agentic_backend.Config_valid
        | findings ->
            Agentic_backend.Config_invalid
              ("Codex generated TOML shape check failed: "
              ^ String.concat "; " (List.rev findings)))

let safe_protocol_identifier value =
  let safe_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  if
    value <> ""
    && String.length value <= 128
    && String.for_all safe_character value
  then Some value
  else None

let canonical_codex_thread_id value =
  let lowercase_hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
  let rec valid_from index =
    if index = 36 then true
    else
      let valid_character =
        match index with
        | 8 | 13 | 18 | 23 -> value.[index] = '-'
        | _ -> lowercase_hex value.[index]
      in
      valid_character && valid_from (index + 1)
  in
  if String.length value = 36 && valid_from 0 then Some value else None

let nonnegative_token_member name json =
  match Yojson.Safe.Util.member name json with
  | `Int value when value >= 0 -> Some value
  | _ -> None

let token_usage_of_json usage =
  match usage with
  | `Assoc _ ->
      let tokens_input = nonnegative_token_member "input_tokens" usage in
      let tokens_output = nonnegative_token_member "output_tokens" usage in
      let cache_read_input_tokens =
        nonnegative_token_member "cached_input_tokens" usage
      in
      if
        Option.is_none tokens_input
        && Option.is_none tokens_output
        && Option.is_none cache_read_input_tokens
      then None
      else
        Some
          {
            Backend_types.tokens_input;
            tokens_output;
            cost_usd = None;
            cache_creation_input_tokens = None;
            cache_read_input_tokens;
          }
  | _ -> None

let normalized_events_of_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let events = ref [] in
    let add event = events := event :: !events in
    let event_type = json |> member "type" |> to_string_option in
    let item = json |> member "item" in
    let item_type =
      match item with
      | `Assoc _ -> item |> member "type" |> to_string_option
      | _ -> None
    in
    (match (event_type, item_type) with
    | Some "thread.started", _ ->
        Option.bind
          (json |> member "thread_id" |> to_string_option)
          canonical_codex_thread_id
        |> Option.iter (fun id -> add (Task_event.Session_id id))
    | Some "item.completed", Some "agent_message" ->
        Option.iter
          (fun text -> add (Task_event.Agent_text_delta text))
          (item |> member "text" |> to_string_option)
    | ( (Some "item.started" | Some "item.completed"),
        Some
          (("command_execution" | "file_change" | "mcp_tool_call" | "web_search")
           as item_kind) ) ->
        let id =
          Option.bind
            (item |> member "id" |> to_string_option)
            safe_protocol_identifier
        in
        let name = item_kind in
        if event_type = Some "item.started" then
          add (Task_event.Tool_started { id; name })
        else add (Task_event.Tool_finished { id; name = Some name })
    | _ -> ());
    let usage = json |> member "usage" in
    if event_type = Some "turn.completed" then
      Option.iter
        (fun usage -> add (Task_event.Token_usage usage))
        (token_usage_of_json usage);
    List.rev !events
  with _ -> []

(* Parse only public, versioned Codex JSONL protocol records. Malformed lines,
   reasoning/error records, and raw stdout are never promoted to agent text. *)
let parse_jsonl_output stdout =
  let last_text = ref "" in
  let total_input = ref None in
  let total_output = ref None in
  let total_cache_read_input = ref None in
  let saturating_add left right =
    if right > Stdlib.max_int - left then Stdlib.max_int else left + right
  in
  let add_token_value total = function
    | None -> ()
    | Some value ->
        total :=
          Some
            (match !total with
            | None -> value
            | Some accumulated -> saturating_add accumulated value)
  in
  String.split_on_char '\n' stdout
  |> List.iter (fun line ->
      normalized_events_of_line line
      |> List.iter (function
        | Task_event.Agent_text_delta text when text <> "" -> last_text := text
        | Token_usage usage ->
            add_token_value total_input usage.tokens_input;
            add_token_value total_output usage.tokens_output;
            add_token_value total_cache_read_input usage.cache_read_input_tokens
        | _ -> ()));
  let cost =
    if
      Option.is_some !total_input
      || Option.is_some !total_output
      || Option.is_some !total_cache_read_input
    then
      Some
        {
          tokens_input = !total_input;
          tokens_output = !total_output;
          cost_usd = None;
          cache_creation_input_tokens = None;
          cache_read_input_tokens = !total_cache_read_input;
        }
    else None
  in
  (!last_text, cost)

let parse_cost_from_stdout stdout =
  let _, cost = parse_jsonl_output stdout in
  cost

let remove_file_noerr path = try Sys.remove path with _ -> ()

let create_output_schema_file schema =
  let path = Filename.temp_file "cabal_schema_" ".json" in
  try
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel (Yojson.Safe.to_string ~std:true schema));
    path
  with error ->
    remove_file_noerr path;
    raise error

let with_output_schema_file schema f =
  let path = create_output_schema_file schema in
  Fun.protect ~finally:(fun () -> remove_file_noerr path) (fun () -> f path)

type backend_invocation = {
  argv : string list;
  stdin : string option;
  redacted_argv : string list;
}

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as e -> e

let invocation_error message = Error ("Codex invocation rejected: " ^ message)

let staged_path_matches_media_type attachment path =
  path <> ""
  && (not (Filename.is_relative path))
  && (not (String.contains path '\000'))
  &&
  match attachment.Backend_types.media_type with
  | Backend_types.Png -> String.ends_with ~suffix:".png" path
  | Backend_types.Jpeg -> String.ends_with ~suffix:".jpg" path

let validate_resume_session_id = function
  | None -> Ok ()
  | Some session_id -> (
      match canonical_codex_thread_id session_id with
      | Some _ -> Ok ()
      | None -> invocation_error "the resume session id is invalid")

let validate_attachment_delivery attachment_delivery spec =
  match (attachment_delivery, spec.resume_session_id) with
  | Reuse_session_attachments, None ->
      invocation_error "session attachment reuse requires a resumed session"
  | (Upload_attachments | Reuse_session_attachments), (None | Some _) -> Ok ()

let validate_staged_attachment_paths ~attachment_delivery ~attachment_paths spec
    =
  match attachment_delivery with
  | Reuse_session_attachments ->
      if attachment_paths = [] then Ok ()
      else
        invocation_error "session attachment reuse must not carry image paths"
  | Upload_attachments ->
      if
        List.length attachment_paths
        <> List.length spec.Backend_types.attachments
        || not
             (List.for_all2 staged_path_matches_media_type spec.attachments
                attachment_paths)
      then invocation_error "the sealed attachment set does not match the task"
      else Ok ()

let validate_transport_request ~attachment_delivery ~attachment_paths spec =
  let* () = validate_resume_session_id spec.resume_session_id in
  let* () = validate_attachment_delivery attachment_delivery spec in
  validate_staged_attachment_paths ~attachment_delivery ~attachment_paths spec

let web_search_mode = function
  | Web_disabled -> "disabled"
  | Web_search -> "cached"
  | Web_search_and_fetch -> "live"

let image_args paths = List.concat_map (fun path -> [ "-i"; path ]) paths

let redacted_image_args paths =
  List.mapi
    (fun index _ -> [ "-i"; Printf.sprintf "<attachment-%d>" (index + 1) ])
    paths
  |> List.concat

let build_invocation ?schema_path ?(attachment_paths = [])
    ?(attachment_delivery = Upload_attachments) ~mcp_config_path:_
    (spec : task_spec) =
  let* () =
    validate_transport_request ~attachment_delivery ~attachment_paths spec
  in
  let* () =
    match (spec.json_schema, schema_path) with
    | None, None | Some _, Some _ -> Ok ()
    | Some _, None -> invocation_error "a schema file path is required"
    | None, Some _ ->
        invocation_error "an unexpected schema file path was supplied"
  in
  let model_args, redacted_model_args =
    match spec.model with
    | Some model -> ([ "-m"; model ], [ "-m"; "<model>" ])
    | None -> ([], [])
  in
  let sandbox_args =
    if spec.read_only then [ "-s"; "read-only" ] else [ "--full-auto" ]
  in
  let schema_args, redacted_schema_args =
    match schema_path with
    | Some path ->
        ([ "--output-schema"; path ], [ "--output-schema"; "<schema>" ])
    | None -> ([], [])
  in
  let shared_options =
    [
      "--json";
      "--skip-git-repo-check";
      "--ignore-user-config";
      "-c";
      Printf.sprintf "web_search=\"%s\"" (web_search_mode spec.web_access);
    ]
  in
  let images = image_args attachment_paths in
  let redacted_images = redacted_image_args attachment_paths in
  let command, redacted_command =
    match spec.resume_session_id with
    | None ->
        ( [ "codex"; "exec" ] @ shared_options @ sandbox_args @ model_args
          @ schema_args @ images @ [ "-" ],
          [ "codex"; "exec" ] @ shared_options @ sandbox_args
          @ redacted_model_args @ redacted_schema_args @ redacted_images
          @ [ "-" ] )
    | Some session_id ->
        ( [ "codex"; "exec" ] @ schema_args @ sandbox_args
          @ [ "resume"; session_id ] @ shared_options @ model_args @ images
          @ [ "-" ],
          [ "codex"; "exec" ] @ redacted_schema_args @ sandbox_args
          @ [ "resume"; "<session-id>" ]
          @ shared_options @ redacted_model_args @ redacted_images @ [ "-" ] )
  in
  let full_prompt =
    if String.length spec.instructions > 0 then
      Printf.sprintf "%s\n\n---\nProject Instructions:\n%s" spec.prompt
        spec.instructions
    else spec.prompt
  in
  Ok
    {
      argv = command;
      stdin = Some full_prompt;
      redacted_argv = redacted_command;
    }

let build_command_with_schema_path ?schema_path ?attachment_paths
    ?attachment_delivery ~mcp_config_path spec =
  match
    build_invocation ?schema_path ?attachment_paths ?attachment_delivery
      ~mcp_config_path spec
  with
  | Ok invocation ->
      ( invocation.argv,
        match invocation.stdin with Some stdin -> stdin | None -> "" )
  | Error message -> invalid_arg message

let build_command ~mcp_config_path (spec : task_spec) =
  match
    validate_transport_request ~attachment_delivery:Upload_attachments
      ~attachment_paths:[] spec
  with
  | Error message -> invalid_arg message
  | Ok () -> (
      match spec.json_schema with
      | None -> build_command_with_schema_path ~mcp_config_path spec
      | Some schema ->
          let schema_path = create_output_schema_file schema in
          build_command_with_schema_path ~schema_path ~mcp_config_path spec)

(* Extract response text from Codex JSONL stdout *)
let parse_stdout_text stdout =
  let text, _ = parse_jsonl_output stdout in
  if text = "" then stdout else text

let normalized_events_of_stdout stdout =
  String.split_on_char '\n' stdout |> List.concat_map normalized_events_of_line

let parse_public_stdout_text stdout =
  normalized_events_of_stdout stdout
  |> List.fold_left
       (fun last -> function
         | Task_event.Agent_text_delta text -> text | _ -> last)
       ""

let parse_public_session_id stdout =
  normalized_events_of_stdout stdout
  |> List.find_map (function Task_event.Session_id id -> Some id | _ -> None)

let failed_result message =
  make_task_result ~status:(Failed message) ~stderr:message ~exit_code:1 ()

type transport_inputs = {
  attachment_delivery : Backend_types.attachment_delivery;
  attachment_paths : string list;
}

let sensitive_transport_requested spec =
  spec.Backend_types.attachments <> []
  || spec.web_access <> Backend_types.Web_disabled

let requested_attachment_delivery context spec =
  match Task_execution_context.requested_delivery context with
  | None -> Ok Upload_attachments
  | Some delivery
    when delivery.attachment_references = spec.Backend_types.attachments
         && delivery.web_access_policy = spec.web_access ->
      Ok delivery.attachment_delivery
  | Some _ ->
      invocation_error "the execution delivery context does not match the task"

let requested_transport_inputs ?context spec =
  if not (sensitive_transport_requested spec) then
    let* attachment_delivery =
      match context with
      | None -> Ok Upload_attachments
      | Some context -> requested_attachment_delivery context spec
    in
    Ok { attachment_delivery; attachment_paths = [] }
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
        | Error message -> invocation_error message
        | Ok sealed ->
            Ok
              {
                attachment_delivery = sealed.attachment_delivery;
                attachment_paths = sealed.attachment_paths;
              })

let run_invocation ~sw ~env ~spec ?context ?on_raw_line invocation =
  Diagnostics.debug "backend command: %s"
    (String.concat " " invocation.redacted_argv);
  let on_stdout line =
    Option.iter (fun callback -> callback line) on_raw_line;
    Option.iter
      (fun context ->
        List.iter
          (Task_execution_context.emit context)
          (normalized_events_of_line line))
      context
  in
  Option.iter Task_execution_context.claim_structured_text context;
  let result =
    Backend_process.run_process ~sw ~env ~cmd:invocation.argv
      ~stdin_content:invocation.stdin ~working_dir:spec.working_dir
      ~timeout_seconds:(duration_to_seconds spec.timeout)
      ?context ~parse_cost:parse_cost_from_stdout ~on_stdout ()
  in
  let task_result =
    {
      status = result.status;
      files_changed =
        Backend_process.get_git_diff ~sw ~env ~working_dir:spec.working_dir;
      report = None;
      elapsed = result.elapsed;
      cost = result.cost;
      stdout = result.stdout;
      agent_text = parse_public_stdout_text result.stdout;
      stderr = result.stderr;
      exit_code = result.exit_code;
      session_id = parse_public_session_id result.stdout;
    }
  in
  Option.iter Task_execution_context.mark_final_public_text context;
  task_result

let run_task ~sw ~env ?context ?on_raw_line spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None -> (
      match requested_transport_inputs ?context spec with
      | Error message -> failed_result message
      | Ok transport -> (
          match
            validate_transport_request
              ~attachment_delivery:transport.attachment_delivery
              ~attachment_paths:transport.attachment_paths spec
          with
          | Error message -> failed_result message
          | Ok () -> (
              (* Write project config to .codex/config.toml if absent or
                 managed. Codex discovers this fixed path automatically. *)
              let setup =
                Backend_config_writer.setup_artifacts
                  ~project_dir:spec.working_dir ~force:false
                  (project_config_artifacts
                     ~managed_namespace:spec.managed_namespace
                     ~mcp_servers:spec.mcp_servers ~lsp_servers:spec.lsp_servers)
              in
              (* AC2/AC4 story #479: Codex has Medium precedence confidence.
                 Warn according to whether the config was applied. *)
              (match
                 Backend_config_writer.precedence_warning_for ~backend_id:id
                   ~write_outcome:setup.Backend_config_writer.write_outcome
               with
              | None -> ()
              | Some msg -> Diagnostics.user_warning "%s" msg);
              match mcp_config_error_if_needed setup spec.mcp_servers with
              | Some msg -> make_task_result ~status:(Failed msg) ()
              | None -> (
                  let runtime_spec = { spec with mcp_servers = [] } in
                  let run ?schema_path () =
                    match
                      build_invocation ?schema_path
                        ~attachment_paths:transport.attachment_paths
                        ~attachment_delivery:transport.attachment_delivery
                        ~mcp_config_path:None runtime_spec
                    with
                    | Error message -> failed_result message
                    | Ok invocation ->
                        run_invocation ~sw ~env ~spec:runtime_spec ?context
                          ?on_raw_line invocation
                  in
                  match runtime_spec.json_schema with
                  | None -> run ()
                  | Some schema ->
                      with_output_schema_file schema (fun schema_path ->
                          run ~schema_path ())))))
