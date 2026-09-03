(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Backend_types

let id = "gemini-cli"

let name = "Gemini CLI"

(* Static fallback — used when GOOGLE_API_KEY / GEMINI_API_KEY is absent. *)
let models = ["gemini-2.5-pro"; "gemini-2.5-flash"; "gemini-2.0-flash"]

(* Strip the "models/" prefix that the REST API returns but the CLI does not use. *)
let strip_models_prefix s =
  let prefix = "models/" in
  let plen = String.length prefix in
  if String.length s >= plen && String.sub s 0 plen = prefix then
    String.sub s plen (String.length s - plen)
  else s

let parse_google_models_json json_str =
  try
    match Yojson.Safe.from_string json_str with
    | `Assoc fields -> (
        match List.assoc_opt "models" fields with
        | Some (`List items) ->
            List.filter_map
              (function
                | `Assoc fs -> (
                    match List.assoc_opt "name" fs with
                    | Some (`String raw_id) ->
                        let id = strip_models_prefix raw_id in
                        if String.length id > 0 then Some id else None
                    | _ -> None)
                | _ -> None)
              items
        | _ -> [])
    | _ -> []
  with _ -> []

(* Live probe via Google AI Models REST API (v1beta).
   Checks GOOGLE_API_KEY first, then GEMINI_API_KEY. *)
let models_probe =
  Some
    (fun ~sw:_ ~env ->
      let api_key =
        match Sys.getenv_opt "GOOGLE_API_KEY" with
        | Some k -> Some k
        | None -> Sys.getenv_opt "GEMINI_API_KEY"
      in
      match api_key with
      | None -> Error "Neither GOOGLE_API_KEY nor GEMINI_API_KEY is set"
      | Some key -> (
          match
            Backend_process.capture_version_output
              ~env
              ~timeout_seconds:10.0
              [
                "curl";
                "-sf";
                "https://generativelanguage.googleapis.com/v1beta/models?key="
                ^ key;
              ]
          with
          | Error msg -> Error msg
          | Ok json_str -> (
              match parse_google_models_json json_str with
              | [] -> Error "Google models API returned no parseable model IDs"
              | ms -> Ok ms)))

let available ~sw:_ ~env =
  Backend_process.check_available ~env ["gemini"; "--version"]

let supports_session_resume = true

let native_json_schema_output = false

(* Shared json_string_map encoding lives in Backend_json_helpers. *)
type json_string_map = Backend_json_helpers.json_string_map

let json_string_map_to_yojson = Backend_json_helpers.json_string_map_to_yojson

let json_string_map_of_yojson = Backend_json_helpers.json_string_map_of_yojson

type mcp_server_settings = {
  command : string;
  args : string list;
  env : json_string_map;
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

type settings_json = {mcpServers : mcp_server_map} [@@deriving yojson]

let env_reference_value ~name value =
  if String.length value > 0 && value.[0] = '$' then value
  else if name = "" then value
  else "$" ^ name

let persistent_env_references env =
  List.map (fun (name, value) -> (name, env_reference_value ~name value)) env

let mcp_server_entry (cfg : Backend_types.mcp_server_config) =
  ( cfg.name,
    {
      command = cfg.command;
      args = cfg.args;
      env = persistent_env_references cfg.env;
    } )

let settings_json_content mcp_servers =
  settings_json_to_yojson {mcpServers = List.map mcp_server_entry mcp_servers}
  |> Yojson.Safe.pretty_to_string
  |> fun s -> s ^ "\n"

let project_config_artifacts ~managed_namespace ~mcp_servers ~lsp_servers:_ =
  [
    {
      Backend_config_writer.backend_id = id;
      ownership = Backend_config_writer.Backend_project;
      managed_namespace;
      project_relative_path = ".gemini/settings.json";
      content = settings_json_content mcp_servers;
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

let read_project_file ~env ~project_dir rel_path =
  let path = Filename.concat project_dir rel_path in
  try Ok (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path))
  with e ->
    Error (Printf.sprintf "could not read %s: %s" path (Printexc.to_string e))

let check_project_config ~sw:_ ~env ~project_dir ~setup_result =
  if not (config_applied setup_result) then
    Agentic_backend.Config_check_unsupported
      "Gemini settings were not applied; refusing to validate user-authored \
       .gemini/settings.json"
  else
    match read_project_file ~env ~project_dir ".gemini/settings.json" with
    | Error msg -> Agentic_backend.Config_invalid msg
    | Ok content -> (
        try
          let json = Yojson.Safe.from_string content in
          match settings_json_of_yojson json with
          | Ok _ -> Agentic_backend.Config_valid
          | Error msg ->
              Agentic_backend.Config_invalid
                ("Gemini settings JSON schema check failed: " ^ msg)
        with e ->
          Agentic_backend.Config_invalid
            (Printf.sprintf
               "Gemini settings are not strict JSON: %s"
               (Printexc.to_string e)))

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

let canonical_uuid value =
  let lowercase_hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
  let rec valid_from index =
    if index = 36 then true
    else
      let valid =
        match index with
        | 8 | 13 | 18 | 23 -> value.[index] = '-'
        | _ -> lowercase_hex value.[index]
      in
      valid && valid_from (index + 1)
  in
  String.length value = 36 && valid_from 0

let valid_gemini_session_id value =
  canonical_uuid value

let nonnegative_int_member name json =
  match Yojson.Safe.Util.member name json with
  | `Int value when value >= 0 -> Some value
  | _ -> None

let cost_of_usage usage =
  match usage with
  | `Assoc _ ->
      let first_nonnegative first second =
        match nonnegative_int_member first usage with
        | Some _ as value -> value
        | None -> nonnegative_int_member second usage
      in
      let tokens_input =
        first_nonnegative "input_tokens" "promptTokenCount"
      in
      let tokens_output =
        first_nonnegative "output_tokens" "candidatesTokenCount"
      in
      let cache_read_input_tokens =
        first_nonnegative "cached" "cachedContentTokenCount"
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

(* [--output-format json] has one documented public response envelope. Unknown
   fields and API-internal usage shapes are not promoted. *)
let parse_gemini_json_output json =
  let open Yojson.Safe.Util in
  let response = json |> member "response" |> to_string_option in
  let cost = cost_of_usage (json |> member "stats") in
  (Option.value ~default:"" response, cost)

let normalized_events_of_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let event_type = json |> member "type" |> to_string_option in
    match event_type with
    | Some "init" -> (
        match json |> member "session_id" |> to_string_option with
        | Some id when valid_gemini_session_id id ->
            [Task_event.Session_id id]
        | Some _ | None -> [])
    | Some "message"
      when json |> member "role" |> to_string_option = Some "assistant" -> (
        match json |> member "content" |> to_string_option with
        | Some text -> [Task_event.Agent_text_delta text]
        | None -> [])
    | Some "tool_use" -> (
        match
          ( json |> member "tool_name" |> to_string_option,
            json |> member "tool_id" |> to_string_option )
        with
        | Some name, Some id -> (
            match (safe_protocol_identifier name, safe_protocol_identifier id) with
            | Some name, Some id ->
                [Task_event.Tool_started {id = Some id; name}]
            | (Some _ | None), (Some _ | None) -> [])
        | (Some _ | None), (Some _ | None) -> [])
    | Some "tool_result" -> (
        match
          ( json |> member "status" |> to_string_option,
            json |> member "tool_id" |> to_string_option )
        with
        | (Some "success" | Some "error"), Some id -> (
            match safe_protocol_identifier id with
            | Some id -> [Task_event.Tool_finished {id = Some id; name = None}]
            | None -> [])
        | (Some _ | None), (Some _ | None) -> [])
    | Some "result"
      when json |> member "status" |> to_string_option = Some "success" ->
        Option.to_list
          (Option.map
             (fun cost -> Task_event.Token_usage cost)
             (cost_of_usage (json |> member "stats")))
    | Some _ | None -> []
  with _ -> []

let normalized_events_of_stdout stdout =
  String.split_on_char '\n' stdout |> List.concat_map normalized_events_of_line

let parse_public_stdout_text stdout =
  normalized_events_of_stdout stdout
  |> List.filter_map (function
       | Task_event.Agent_text_delta text -> Some text
       | _ -> None)
  |> String.concat ""

let parse_public_session_id stdout =
  normalized_events_of_stdout stdout
  |> List.find_map (function Task_event.Session_id id -> Some id | _ -> None)

let parse_public_cost stdout =
  normalized_events_of_stdout stdout
  |> List.fold_left
       (fun latest -> function Task_event.Token_usage cost -> Some cost | _ -> latest)
       None

(* Compatibility parser for historical callers of this helper. Runtime output
   uses the strict [parse_public_*] projections above. The legacy response and
   content records are never converted into normalized task events. *)
let parse_gemini_stream_json stdout =
  let legacy_chunks = ref [] in
  let legacy_response = ref None in
  let legacy_cost = ref None in
  String.split_on_char '\n' stdout
  |> List.iter (fun line ->
       try
         let json = Yojson.Safe.from_string line in
         let open Yojson.Safe.Util in
         (match json |> member "response" |> to_string_option with
         | Some response -> legacy_response := Some response
         | None -> (
             match json |> member "type" |> to_string_option with
             | Some "content" ->
                 Option.iter
                   (fun text -> legacy_chunks := text :: !legacy_chunks)
                   (json |> member "text" |> to_string_option)
             | Some _ | None -> ())) ;
         let usage = json |> member "usageMetadata" in
         if usage <> `Null then legacy_cost := cost_of_usage usage
       with _ -> ()) ;
  let public_text = parse_public_stdout_text stdout in
  let text =
    if public_text <> "" then public_text
    else
      match !legacy_response with
      | Some response -> response
      | None -> String.concat "" (List.rev !legacy_chunks)
  in
  let cost =
    match parse_public_cost stdout with Some _ as cost -> cost | None -> !legacy_cost
  in
  (text, cost, parse_public_session_id stdout)

type backend_invocation = {
  argv : string list;
  stdin : string option;
  redacted_argv : string list;
}

type transport_inputs = {
  attachment_delivery : Backend_types.attachment_delivery;
  attachment_paths : string list;
}

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let invocation_error message = Error ("Gemini invocation rejected: " ^ message)

let validate_resume_session_id = function
  | None -> Ok ()
  | Some id when valid_gemini_session_id id -> Ok ()
  | Some _ -> invocation_error "the resume session id is invalid"

let validate_attachment_delivery attachment_delivery spec =
  match (attachment_delivery, spec.resume_session_id) with
  | Reuse_session_attachments, None ->
      invocation_error "session attachment reuse requires a resumed session"
  | (Upload_attachments | Reuse_session_attachments), (None | Some _) -> Ok ()

let validate_media_request ~attachment_paths spec =
  if spec.Backend_types.attachments = [] && attachment_paths = [] then Ok ()
  else invocation_error "media attachments are unsupported at baseline 0.38.2"

let validate_web_access = function
  | Web_disabled -> Ok ()
  | Web_search | Web_search_and_fetch ->
      invocation_error "positive web access is unsupported by this transport"

let validate_transport_request ~attachment_delivery ~attachment_paths spec =
  let* () = validate_resume_session_id spec.resume_session_id in
  let* () = validate_attachment_delivery attachment_delivery spec in
  let* () = validate_media_request ~attachment_paths spec in
  validate_web_access spec.web_access

let validate_policy_path path =
  if
    path <> ""
    && not (Filename.is_relative path)
    && not (String.contains path '\000')
    && not (String.contains path '\n')
    && not (String.contains path '\r')
    && not (String.contains path ',')
  then Ok ()
  else invocation_error "the task web policy path is invalid"

let escape_literal_ats text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun character ->
      if character = '@' then Buffer.add_char buffer '\\' ;
      Buffer.add_char buffer character)
    text ;
  Buffer.contents buffer

let encode_json_ats text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun character ->
      if character = '@' then Buffer.add_string buffer "\\u0040"
      else Buffer.add_char buffer character)
    text;
  Buffer.contents buffer

let find_substring_from text ~start needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then None
    else if String.sub text index needle_length = needle then Some index
    else loop (index + 1)
  in
  if start < 0 || start > text_length then None else loop start

let replace_all text ~needle ~replacement =
  let buffer = Buffer.create (String.length text) in
  let rec loop start =
    match find_substring_from text ~start needle with
    | None -> Buffer.add_substring buffer text start (String.length text - start)
    | Some index ->
        Buffer.add_substring buffer text start (index - start);
        Buffer.add_string buffer replacement;
        loop (index + String.length needle)
  in
  loop 0;
  Buffer.contents buffer

let encode_known_schema_ats schema text =
  let schema_json = Yojson.Safe.to_string ~std:true schema in
  let encoded = encode_json_ats schema_json in
  replace_all text ~needle:schema_json ~replacement:encoded

let encode_json_candidate schema_json =
  let encoded = encode_json_ats schema_json in
  try
    let original = Yojson.Safe.from_string schema_json in
    if Yojson.Safe.from_string encoded = original then Some encoded else None
  with Yojson.Json_error _ -> None

(* Non-native retries append one compact JSON schema between these pinned
   enforcer markers. At-signs inside that JSON must use JSON's Unicode escape,
   not Gemini's backslash-at path escape, which would make the schema invalid.
   Scan every delimiter-bounded candidate rather than selecting a last header:
   validation errors are model-influenced and may contain the header verbatim.
   Only candidates that parse as JSON and reparse identically after encoding are
   transformed, so the real schema is handled independently of later text. *)
let encode_retry_schema_ats text =
  let header = "## Required output schema\n\n" in
  let compliance =
    "\n\nYour previous response did not conform to the required JSON schema.\n"
  in
  let rec candidates search_start selected =
    match find_substring_from text ~start:search_start header with
    | None -> List.rev selected
    | Some header_index ->
        let schema_start = header_index + String.length header in
        (match find_substring_from text ~start:schema_start compliance with
        | None -> List.rev selected
        | Some schema_end ->
            let schema_json =
              String.sub text schema_start (schema_end - schema_start)
            in
            (match encode_json_candidate schema_json with
            | Some encoded ->
                candidates schema_end ((schema_start, schema_end, encoded) :: selected)
            | None -> candidates (header_index + 1) selected))
  in
  let spans = candidates 0 [] in
  let buffer = Buffer.create (String.length text) in
  let rec render cursor = function
    | [] -> Buffer.add_substring buffer text cursor (String.length text - cursor)
    | (schema_start, schema_end, encoded) :: rest ->
        Buffer.add_substring buffer text cursor (schema_start - cursor);
        Buffer.add_string buffer encoded;
        render schema_end rest
  in
  render 0 spans;
  Buffer.contents buffer

let neutralize_ats ?schema text =
  let text =
    match schema with None -> text | Some schema -> encode_known_schema_ats schema text
  in
  text |> encode_retry_schema_ats |> escape_literal_ats

let full_prompt spec =
  let prompt = neutralize_ats ?schema:spec.json_schema spec.prompt in
  let instructions = neutralize_ats ?schema:spec.json_schema spec.instructions in
  if instructions = "" then prompt
  else Printf.sprintf "%s\n\n---\nProject Instructions:\n%s" prompt instructions

let build_invocation ?(attachment_paths = [])
    ?(attachment_delivery = Upload_attachments) ~web_policy_path
    ~mcp_config_path:_ (spec : task_spec) =
  let* () = validate_policy_path web_policy_path in
  let* () =
    validate_transport_request ~attachment_delivery ~attachment_paths spec
  in
  let policy_args =
    ["--policy"; web_policy_path; "--admin-policy"; web_policy_path]
  in
  let redacted_policy_args =
    ["--policy"; "<web-policy>"; "--admin-policy"; "<web-policy>"]
  in
  let model_args, redacted_model_args =
    match spec.model with
    | Some model -> (["-m"; model], ["-m"; "<model>"])
    | None -> ([], [])
  in
  let resume_args, redacted_resume_args =
    match spec.resume_session_id with
    | Some id -> (["--resume"; id], ["--resume"; "<session-id>"])
    | None -> ([], [])
  in
  let fixed = ["gemini"; "--output-format"; "stream-json"; "-y"] in
  Ok
    {
      argv =
        fixed @ policy_args @ model_args @ resume_args @ ["-p"; ""];
      stdin = Some (full_prompt spec);
      redacted_argv =
        fixed @ redacted_policy_args @ redacted_model_args
        @ redacted_resume_args @ ["-p"; ""];
    }

let web_disabled_policy =
  "[[rule]]\n\
   toolName = [\"google_web_search\", \"web_fetch\"]\n\
   decision = \"deny\"\n\
   priority = 999\n"

let warn_policy_cleanup_failure () =
  try Diagnostics.warn "Gemini task web policy cleanup failed"
  with _ -> ()

let remove_file_noerr path =
  try Sys.remove path
  with Sys_error _ ->
    if Sys.file_exists path then warn_policy_cleanup_failure ()

let create_web_disabled_policy_file () =
  let path = Filename.temp_file "cabal_gemini_web_" ".toml" in
  try
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel web_disabled_policy) ;
    Unix.chmod path 0o600 ;
    path
  with error ->
    remove_file_noerr path ;
    raise error

let with_web_disabled_policy_file f =
  let path = create_web_disabled_policy_file () in
  Fun.protect ~finally:(fun () -> remove_file_noerr path) (fun () -> f path)

let standard_admin_policy_conflict ~directory =
  try
    Sys.readdir directory
    |> Array.exists (fun name -> String.ends_with ~suffix:".toml" name)
  with Sys_error _ -> Sys.file_exists directory

let standard_admin_policy_directories () =
  if Sys.win32 then ["C:\\ProgramData\\gemini-cli\\policies"]
  else
    [
      "/etc/gemini-cli/policies";
      "/Library/Application Support/GeminiCli/policies";
    ]

let standard_admin_policy_blocks_override () =
  List.exists
    (fun directory -> standard_admin_policy_conflict ~directory)
    (standard_admin_policy_directories ())

(* Compatibility command constructor. Runtime execution uses [build_invocation]
   with a scoped deny policy. *)
let build_command ~mcp_config_path:_ (spec : task_spec) =
  match
    validate_transport_request ~attachment_delivery:Upload_attachments
      ~attachment_paths:[] spec
  with
  | Error message -> invalid_arg message
  | Ok () ->
      let model_args =
        match spec.model with Some model -> ["-m"; model] | None -> []
      in
      let resume_args =
        match spec.resume_session_id with
        | Some id -> ["--resume"; id]
        | None -> []
      in
      ( ["gemini"; "--output-format"; "stream-json"; "-y"] @ model_args
        @ resume_args @ ["-p"; ""],
        full_prompt spec )

let parse_stdout_text stdout =
  let text, _, _ = parse_gemini_stream_json stdout in
  if text = "" then stdout else text

let setup_outcome_for_path setup path =
  List.find_opt
    (fun outcome ->
      outcome.Backend_config_writer.artifact.project_relative_path = path)
    setup.Backend_config_writer.write_outcomes

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

let mcp_settings_error_if_needed setup mcp_servers =
  match mcp_servers with
  | [] -> None
  | _ -> (
      let path = ".gemini/settings.json" in
      match setup_outcome_for_path setup path with
      | Some outcome
        when Backend_config_writer.write_result_was_applied
               outcome.Backend_config_writer.result ->
          None
      | outcome ->
          Some
             (Printf.sprintf
                "Gemini MCP servers were requested, but %s was not applied \
                 (%s). Refusing to run without the requested MCP config."
                path
                (setup_outcome_reason outcome)))

let failed_result message =
  make_task_result ~status:(Failed message) ~stderr:message ~exit_code:1 ()

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
        | Error message -> invocation_error message
        | Ok sealed ->
            Ok
              {
                attachment_delivery = sealed.attachment_delivery;
                attachment_paths = sealed.attachment_paths;
              })

let run_invocation ~sw ~env ~spec ?context ?on_raw_line invocation =
  Diagnostics.debug "backend command: %s"
    (String.concat " " invocation.redacted_argv) ;
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
  let result =
    Backend_process.run_process ~sw ~env ~cmd:invocation.argv
      ~stdin_content:invocation.stdin ~working_dir:spec.working_dir
      ~timeout_seconds:(duration_to_seconds spec.timeout) ?context
      ~parse_cost:parse_public_cost ~on_stdout ()
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
  Option.iter Task_execution_context.mark_final_public_text context ;
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
          | Ok () ->
              if standard_admin_policy_blocks_override () then
                failed_result
                  "Gemini invocation rejected: Web_disabled cannot be enforced \
                   in the presence of a standard administrator policy"
              else
              (* Gemini discovers this fixed workspace settings path. GEMINI.md
                 remains human/project context and is not generated here. *)
              let setup =
                Backend_config_writer.setup_artifacts
                  ~project_dir:spec.working_dir ~force:false
                  (project_config_artifacts
                     ~managed_namespace:spec.managed_namespace
                     ~mcp_servers:spec.mcp_servers
                     ~lsp_servers:spec.lsp_servers)
              in
              (match
                 Backend_config_writer.precedence_warning_for ~backend_id:id
                   ~write_outcome:setup.Backend_config_writer.write_outcome
               with
              | None -> ()
              | Some msg -> Diagnostics.user_warning "%s" msg) ;
              match mcp_settings_error_if_needed setup spec.mcp_servers with
              | Some msg -> make_task_result ~status:(Failed msg) ()
              | None ->
                  let runtime_spec = {spec with mcp_servers = []} in
                  (try
                     with_web_disabled_policy_file (fun web_policy_path ->
                         match
                           build_invocation
                             ~attachment_paths:transport.attachment_paths
                             ~attachment_delivery:transport.attachment_delivery
                             ~web_policy_path ~mcp_config_path:None runtime_spec
                         with
                         | Error message -> failed_result message
                         | Ok invocation ->
                             run_invocation ~sw ~env ~spec:runtime_spec ?context
                               ?on_raw_line invocation)
                   with
                  | Eio.Cancel.Cancelled _ as error -> raise error
                  | Out_of_memory | Stack_overflow | Sys.Break as error ->
                      raise error
                  | _ ->
                      failed_result
                        "Gemini invocation rejected: the web policy could not \
                         be prepared")))
