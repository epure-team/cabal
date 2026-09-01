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
  ["gpt-5"; "gpt-4o"; "gpt-4o-mini"; "o3"; "o3-mini"; "o1"; "o1-mini"]

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
            Backend_process.capture_version_output
              ~env
              ~timeout_seconds:10.0
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
  Backend_process.check_available ~env ["codex"; "--version"]

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
  Buffer.add_char buf '"' ;
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
    s ;
  Buffer.add_char buf '"' ;
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
    Printf.sprintf
      "[mcp_servers.%s]\ncommand = %s\nargs = %s\n"
      server_key
      (toml_basic_string cfg.command)
      (toml_string_array cfg.args)
  in
  match persistent_env_references cfg.env with
  | [] -> base
  | env ->
      Printf.sprintf
        "%s\n[mcp_servers.%s.env]\n%s\n"
        base
        server_key
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
        Backend_config_writer.with_managed_header
          ~managed_namespace
          Backend_config_writer.Hash
          ~backend_id:id
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
          add "stale [model] table is present" ;
        if contains_substring content "model_provider" then
          add "stale model_provider key is present" ;
        if contains_substring content "provider =" then
          add "stale provider assignment is present" ;
        if not (contains_substring content "# [mcp_servers.example]") then
          add "commented [mcp_servers.example] template is missing" ;
        if
          List.exists
            (fun line ->
              active_mcp_table_line line
              && not (active_mcp_table_has_valid_shape line))
            lines
        then add "active MCP tables must use [mcp_servers.<name>] shape" ;
        match !findings with
        | [] -> Agentic_backend.Config_valid
        | findings ->
            Agentic_backend.Config_invalid
              ("Codex generated TOML shape check failed: "
              ^ String.concat "; " (List.rev findings)))

(* Parse Codex JSONL output.
   Each line is a JSON event. Codex uses these event types:
   - {type: "item.completed", item: {type: "agent_message", text: "..."}}
     for assistant responses
   - {type: "turn.completed", usage: {input_tokens: N, output_tokens: N}}
     for token usage *)
let parse_jsonl_output stdout =
  let lines =
    stdout |> String.split_on_char '\n'
    |> List.filter (fun s -> String.length (String.trim s) > 0)
  in
  let last_text = ref "" in
  let total_input = ref 0 in
  let total_output = ref 0 in
  let has_usage = ref false in
  List.iter
    (fun line ->
      try
        let json = Yojson.Safe.from_string line in
        let open Yojson.Safe.Util in
        let event_type = json |> member "type" |> to_string_option in
        (* Codex item.completed events with agent_message carry the response *)
        (try
           let item = json |> member "item" in
           if item <> `Null then
             let item_type =
               try item |> member "type" |> to_string with _ -> ""
             in
             if item_type = "agent_message" then
               let text =
                 try item |> member "text" |> to_string with _ -> ""
               in
               if String.length text > 0 then last_text := text
         with _ -> ()) ;
        (* Codex turn.completed events carry usage at top level *)
         try
           let usage = json |> member "usage" in
           if event_type = Some "turn.completed" && usage <> `Null then (
            has_usage := true ;
            (try
               total_input :=
                 !total_input + (usage |> member "input_tokens" |> to_int)
             with _ -> ()) ;
            try
              total_output :=
                !total_output + (usage |> member "output_tokens" |> to_int)
            with _ -> ())
        with _ -> ()
      with _ -> ())
    lines ;
  (* If no structured message found, use the raw stdout *)
  let text = if String.length !last_text > 0 then !last_text else stdout in
  let cost =
    if !has_usage then
      Some
        {
          tokens_input = (if !total_input > 0 then Some !total_input else None);
          tokens_output =
            (if !total_output > 0 then Some !total_output else None);
          cost_usd = None;
          cache_creation_input_tokens = None;
          cache_read_input_tokens = None;
        }
    else None
  in
  (text, cost)

let parse_cost_from_stdout stdout =
  let _, cost = parse_jsonl_output stdout in
  cost

let normalized_events_of_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let events = ref [] in
    let add event = events := event :: !events in
    let event_type = json |> member "type" |> to_string_option in
    let item = json |> member "item" in
    let item_type = item |> member "type" |> to_string_option in
    (match event_type, item_type with
    | Some "thread.started", _ ->
        let session_id =
          match json |> member "thread_id" |> to_string_option with
          | Some id -> Some id
          | None -> json |> member "session_id" |> to_string_option
        in
        Option.iter (fun id -> add (Task_event.Session_id id)) session_id
    | Some "item.completed", Some "agent_message" ->
        Option.iter
          (fun text -> add (Task_event.Agent_text_delta text))
          (item |> member "text" |> to_string_option)
    | (Some "item.started" | Some "item.completed"),
      Some
        (( "command_execution" | "file_change" | "mcp_tool_call" | "web_search" )
        as item_kind) ->
        let id = item |> member "id" |> to_string_option in
        let name = item_kind in
        if event_type = Some "item.started" then
          add (Task_event.Tool_started {id; name})
        else add (Task_event.Tool_finished {id; name = Some name})
    | _ -> ()) ;
    let usage = json |> member "usage" in
    if event_type = Some "turn.completed" && usage <> `Null then begin
      add
        (Task_event.Token_usage
           {
             Backend_types.tokens_input =
               usage |> member "input_tokens" |> to_int_option;
             tokens_output = usage |> member "output_tokens" |> to_int_option;
             cost_usd = None;
             cache_creation_input_tokens = None;
             cache_read_input_tokens = None;
           })
    end ;
    List.rev !events
  with _ -> []

let remove_file_noerr path = try Sys.remove path with _ -> ()

let create_output_schema_file schema =
  let path = Filename.temp_file "cabal_schema_" ".json" in
  try
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel (Yojson.Safe.to_string ~std:true schema)) ;
    path
  with error ->
    remove_file_noerr path ;
    raise error

let with_output_schema_file schema f =
  let path = create_output_schema_file schema in
  Fun.protect ~finally:(fun () -> remove_file_noerr path) (fun () -> f path)

(* Build the codex exec command *)
let build_command_with_schema_path ?schema_path ~mcp_config_path:_
    (spec : task_spec) =
  (* Codex reads prompt from stdin with - argument *)
  let model_flags = match spec.model with Some m -> ["-m"; m] | None -> [] in
  (* Read-only agents (validators) get --sandbox read-only so they cannot
     execute shell commands or write files.  Builders keep --full-auto
     (workspace-write sandbox). *)
  let sandbox_flags =
    if spec.read_only then ["-s"; "read-only"] else ["--full-auto"]
  in
  (* Native JSON Schema constraint — passed via a task-scoped temp file.
     Codex reads the schema path as a PathBuf (codex-rs/exec/src/cli.rs).
     Feature ships since codex @openai/codex@0.41.0 / rust-v0.41.0 (PR #4079). *)
  let schema_args =
    match schema_path with Some path -> ["--output-schema"; path] | None -> []
  in
  let base =
    match spec.resume_session_id with
    | Some sid ->
        (["codex"; "exec"; "resume"; sid; "--json"; "--skip-git-repo-check"]
        @ sandbox_flags)
        @ model_flags @ schema_args @ ["-"]
    | None ->
        (["codex"; "exec"; "--json"; "--skip-git-repo-check"] @ sandbox_flags)
        @ model_flags @ schema_args @ ["-"]
  in
  let full_prompt =
    if String.length spec.instructions > 0 then
      Printf.sprintf
        "%s\n\n---\nProject Instructions:\n%s"
        spec.prompt
        spec.instructions
    else spec.prompt
  in
  (* Return command and stdin content separately to avoid arg list too long *)
  (base, full_prompt)

let build_command ~mcp_config_path (spec : task_spec) =
  match spec.json_schema with
  | None -> build_command_with_schema_path ~mcp_config_path spec
  | Some schema ->
      let schema_path = create_output_schema_file schema in
      build_command_with_schema_path ~schema_path ~mcp_config_path spec

(* Extract response text from Codex JSONL stdout *)
let parse_stdout_text stdout =
  let text, _ = parse_jsonl_output stdout in
  text

let normalized_events_of_stdout stdout =
  String.split_on_char '\n' stdout |> List.concat_map normalized_events_of_line

let parse_public_stdout_text stdout =
  normalized_events_of_stdout stdout
  |> List.fold_left
       (fun last -> function
         | Task_event.Agent_text_delta text -> text
         | _ -> last)
       ""

let parse_public_session_id stdout =
  normalized_events_of_stdout stdout
  |> List.find_map (function Task_event.Session_id id -> Some id | _ -> None)

let run_task ~sw ~env ?context ?on_raw_line spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None -> (
      (* Write project config to .codex/config.toml if absent or managed.
         Codex discovers this fixed path automatically (Config_fixed_path). *)
      let setup =
        Backend_config_writer.setup_artifacts
          ~project_dir:spec.working_dir
          ~force:false
          (project_config_artifacts
             ~managed_namespace:spec.managed_namespace
             ~mcp_servers:spec.mcp_servers
             ~lsp_servers:spec.lsp_servers)
      in
      (* AC2/AC4 story #479: Codex has Medium precedence confidence.  Emit
         non-fatal warning conditioned on whether the config was applied. *)
      (match
         Backend_config_writer.precedence_warning_for
           ~backend_id:id
           ~write_outcome:setup.Backend_config_writer.write_outcome
       with
      | None -> ()
      | Some msg -> Diagnostics.user_warning "%s" msg) ;
      match mcp_config_error_if_needed setup spec.mcp_servers with
      | Some msg -> make_task_result ~status:(Failed msg) ()
      | None ->
          let runtime_spec = {spec with mcp_servers = []} in
          let on_stdout line =
            Option.iter (fun callback -> callback line) on_raw_line ;
            Option.iter
              (fun context ->
                List.iter
                  (Task_execution_context.emit context)
                  (normalized_events_of_line line))
              context
          in
          Option.iter Task_execution_context.claim_structured_text context ;
          let run build_command =
            let result =
              Backend_process.run_task_with
                ~sw
                ~env
                ~spec:runtime_spec
                ~build_command
                ?context
                ~parse_cost:parse_cost_from_stdout
                ~parse_stdout:parse_public_stdout_text
                ~parse_session_id:parse_public_session_id
                ~on_stdout
                ()
            in
            Option.iter Task_execution_context.mark_final_public_text context ;
            result
          in
          match runtime_spec.json_schema with
          | None -> run build_command
          | Some schema ->
              with_output_schema_file schema (fun schema_path ->
                  run (build_command_with_schema_path ~schema_path)))
