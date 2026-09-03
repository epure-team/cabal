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

let status_label = function
  | Success -> "success"
  | Failed _ -> "failed"
  | Timeout -> "timed out"
  | Cancelled -> "cancelled"

let invalid_process_result _cmd (result : Backend_process.process_result) =
  Agentic_backend.Config_invalid
    (Printf.sprintf
       "native config validation failed: claude --settings <settings> \
        --init-only\nstatus: %s\nbackend output omitted"
       (status_label result.status))

let check_project_config ~sw ~env ~project_dir ~setup_result =
  match setup_result.Backend_config_writer.project_config_path with
  | None ->
      Agentic_backend.Config_check_unsupported
        "Claude Code settings were not applied; no generated --settings path \
         to validate"
  | Some settings_path -> (
      if not (Sys.file_exists settings_path) then
        Agentic_backend.Config_invalid
          "generated Claude Code settings file does not exist: <settings>"
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
        with _ ->
          Agentic_backend.Config_check_unsupported
            "Claude Code native config validation could not run")

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

let canonical_session_id value =
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

(* Extract session_id from Claude Code JSON stdout.
   Handles both single JSON (--output-format json) and JSONL
   (--output-format stream-json) by scanning each line. The session_id
   typically appears in the first "init" event. *)
let parse_session_id_from_stdout stdout =
  let try_line line =
    try
      let json = Yojson.Safe.from_string line in
      let open Yojson.Safe.Util in
      let is_public_record =
        match json |> member "type" |> to_string_option with
        | Some "system" ->
            json |> member "subtype" |> to_string_option = Some "init"
        | Some "result" ->
            json |> member "subtype" |> to_string_option = Some "success"
            && json |> member "is_error" |> to_bool_option = Some false
        | Some _ | None -> false
      in
      if is_public_record then
        Option.bind
          (json |> member "session_id" |> to_string_option)
          canonical_session_id
      else None
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

(* Build the claude command with all required arguments.
   Returns (command, prompt) where prompt should be passed via stdin.
   This avoids "Argument list too long" errors for large prompts.
   [project_config_path]: when [Some path], pass [--settings path] so
   the Épure-owned settings.json is injected explicitly at invocation
   (AC5 of story #478). *)
(* The Claude Code CLI validates [--json-schema] against its own registry of
   meta-schemas before any request is made, and it cannot resolve the
   2020-12 meta-schema URI: a schema carrying
   ["$schema": "https://json-schema.org/draft/2020-12/schema"] is refused with
   "no schema with key or ref ...", exit 1, before the API is reached.

   Every schema Epure emits carries that key (25 agent modules; the value is
   fixed by AGENTS.md decision D-2), so stripping it here — at the one boundary
   that talks to this CLI — fixes them all without touching the agents. The
   draft a schema is validated against is a concern of
   [Json_schema_validator] on our side; the CLI supplies its own. Only the
   top-level key is removed: Epure never declares a per-subschema draft, and
   removing a nested one could change that subschema's meaning.

   See epure issue #283. *)
let strip_meta_schema (schema : Yojson.Safe.t) : Yojson.Safe.t =
  match schema with
  | `Assoc fields -> `Assoc (List.filter (fun (k, _) -> k <> "$schema") fields)
  | other -> other

type backend_invocation = {
  argv : string list;
  stdin : string;
  redacted_argv : string list;
  redacted_stdin : string;
}

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let invocation_error message =
  Error ("Claude Code invocation rejected: " ^ message)

let canonical_resume_session_id value =
  Option.is_some (canonical_session_id value)

let validate_resume_session_id = function
  | None -> Ok ()
  | Some id when canonical_resume_session_id id -> Ok ()
  | Some _ -> invocation_error "the resume session id is invalid"

let validate_transport_request ~attachment_delivery ~attachment_paths
    (spec : task_spec) =
  let* () = validate_resume_session_id spec.resume_session_id in
  let* () =
    match (attachment_delivery, spec.resume_session_id) with
    | Reuse_session_attachments, None ->
        invocation_error "session attachment reuse requires a resumed session"
    | (Upload_attachments | Reuse_session_attachments), (None | Some _) -> Ok ()
  in
  if spec.attachments <> [] || attachment_paths <> [] then
    invocation_error
      "media transport is not enabled without authenticated proof at the \
       pinned baseline"
  else if spec.web_access <> Web_disabled then
    invocation_error "web transport is not enabled without authenticated proof"
  else Ok ()

let task_prompt (spec : task_spec) =
  if String.length spec.instructions > 0 then
    Printf.sprintf
      "%s\n\n---\nProject Instructions:\n%s"
      spec.prompt
      spec.instructions
  else spec.prompt

(* Exact static provenance: @anthropic-ai/claude-agent-sdk 0.2.117 declares
   parity with Claude Code 2.1.117 and constructs this SDKUserMessage envelope
   for string prompts before writing one JSON value per stdin line. *)
let stream_json_input spec =
  `Assoc
    [
      ("type", `String "user");
      ( "message",
        `Assoc
          [
            ("role", `String "user");
            ( "content",
              `List
                [
                  `Assoc
                    [("type", `String "text"); ("text", `String (task_prompt spec))];
                ] );
          ] );
      ("parent_tool_use_id", `Null);
      ("session_id", `String "");
    ]
  |> Yojson.Safe.to_string ~std:true
  |> fun json -> json ^ "\n"

let fixed_tools read_only =
  if read_only then ["Read"; "Glob"; "Grep"]
  else ["Read"; "Glob"; "Grep"; "Bash"; "Edit"; "Write"; "Task"]

let build_invocation ?(attachment_paths = [])
    ?(attachment_delivery = Upload_attachments)
    ?(project_config_path = None) ~mcp_config_path (spec : task_spec) =
  let* () =
    validate_transport_request ~attachment_delivery ~attachment_paths spec
  in
  let tools = String.concat "," (fixed_tools spec.read_only) in
  let resume_args, redacted_resume_args =
    match spec.resume_session_id with
    | Some id -> (["--resume"; id], ["--resume"; "<session-id>"])
    | None -> ([], [])
  in
  (* [--tools] fixes the built-in availability set, so permissions loaded from
     user settings cannot restore WebSearch/WebFetch. Keep the historical
     read-only deny list as defense in depth and do not combine it with
     [--allowedTools], which is known to produce empty output in 2.1.70+. *)
  let tool_args =
    if spec.read_only then
      [
        "--dangerously-skip-permissions";
        "--tools";
        tools;
        "--disallowedTools";
        "Bash,Edit,Write,NotebookEdit,WebSearch,WebFetch";
      ]
    else
      [
        "--dangerously-skip-permissions";
        "--tools";
        tools;
        "--allowedTools";
        tools;
      ]
  in
  let base =
    [
      "claude";
      "--print";
      "--input-format";
      "stream-json";
      "--output-format";
      "stream-json";
      "--verbose";
    ]
    @ resume_args @ tool_args @ ["--setting-sources"; "user"]
  in
  let redacted_base =
    [
      "claude";
      "--print";
      "--input-format";
      "stream-json";
      "--output-format";
      "stream-json";
      "--verbose";
    ]
    @ redacted_resume_args @ tool_args @ ["--setting-sources"; "user"]
  in
  let mcp_args, redacted_mcp_args =
    match mcp_config_path with
    | Some path ->
        (["--mcp-config"; path; "--strict-mcp-config"],
         ["--mcp-config"; "<mcp-config>"; "--strict-mcp-config"])
    | None -> (["--strict-mcp-config"], ["--strict-mcp-config"])
  in
  let config_args, redacted_config_args =
    match project_config_path with
    | Some path -> (["--settings"; path], ["--settings"; "<settings>"])
    | None -> ([], [])
  in
  let model_args, redacted_model_args =
    match spec.model with
    | Some model -> (["--model"; model], ["--model"; "<model>"])
    | None -> ([], [])
  in
  let max_turns_args =
    match spec.max_turns with
    | Some turns -> ["--max-turns"; string_of_int turns]
    | None -> []
  in
  let schema_args, redacted_schema_args =
    match spec.json_schema with
    | Some schema ->
        ( [
            "--json-schema";
            Yojson.Safe.to_string ~std:true (strip_meta_schema schema);
          ],
          ["--json-schema"; "<schema>"] )
    | None -> ([], [])
  in
  Ok
    {
      argv =
        base @ mcp_args @ config_args @ model_args @ max_turns_args @ schema_args;
      stdin = stream_json_input spec;
      redacted_argv =
        redacted_base @ redacted_mcp_args @ redacted_config_args
        @ redacted_model_args @ max_turns_args @ redacted_schema_args;
      redacted_stdin = "<stream-json-input:text-only>";
    }

let build_command ?streaming:_ ?(project_config_path = None) ~mcp_config_path
    spec =
  match build_invocation ~project_config_path ~mcp_config_path spec with
  | Ok invocation -> (invocation.argv, invocation.stdin)
  | Error message -> invalid_arg message

let safe_protocol_identifier value =
  let safe_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | ':' -> true
    | _ -> false
  in
  if
    value <> ""
    && String.length value <= 128
    && String.for_all safe_character value
  then Some value
  else None

let nonnegative_int_member name json =
  match Yojson.Safe.Util.member name json with
  | `Int value when value >= 0 -> Some value
  | _ -> None

let nonnegative_float_member name json =
  match Yojson.Safe.Util.member name json with
  | `Float value when Float.is_finite value && value >= 0.0 -> Some value
  | `Int value when value >= 0 -> Some (float_of_int value)
  | _ -> None

let public_usage json =
  let usage = Yojson.Safe.Util.member "usage" json in
  match usage with
  | `Assoc _ ->
      let tokens_input = nonnegative_int_member "input_tokens" usage in
      let tokens_output = nonnegative_int_member "output_tokens" usage in
      let cache_creation_input_tokens =
        nonnegative_int_member "cache_creation_input_tokens" usage
      in
      let cache_read_input_tokens =
        nonnegative_int_member "cache_read_input_tokens" usage
      in
      let cost_usd = nonnegative_float_member "total_cost_usd" json in
      if
        Option.is_none tokens_input
        && Option.is_none tokens_output
        && Option.is_none cache_creation_input_tokens
        && Option.is_none cache_read_input_tokens
        && Option.is_none cost_usd
      then None
      else
        Some
          {
            tokens_input;
            tokens_output;
            cost_usd;
            cache_creation_input_tokens;
            cache_read_input_tokens;
          }
  | _ -> None

let public_success_subtype json =
  match Yojson.Safe.Util.(json |> member "subtype" |> to_string_option) with
  | None | Some "success" -> true
  | Some _ -> false

let public_result_text json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string_option with
  | Some "result"
    when public_success_subtype json
         && json |> member "is_error" |> to_bool_option = Some false
    ->
      let structured = json |> member "structured_output" in
      if structured <> `Null then
        Some (Yojson.Safe.to_string ~std:true structured)
      else json |> member "result" |> to_string_option
  | Some _ | None -> None

let normalized_events_of_stream_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    match json |> member "type" |> to_string_option with
    | Some "assistant"
      when json |> member "message" |> member "role" |> to_string_option
           = Some "assistant"
           && json |> member "error" = `Null
           && json |> member "message" |> member "error" = `Null ->
        json |> member "message" |> member "content" |> to_list
        |> List.filter_map (fun block ->
            match block |> member "type" |> to_string_option with
            | Some "text" ->
                Option.map
                  (fun text -> Task_event.Agent_text_delta text)
                  (block |> member "text" |> to_string_option)
            | Some "tool_use" -> (
                match
                  ( Option.bind
                      (block |> member "id" |> to_string_option)
                      safe_protocol_identifier,
                    Option.bind
                      (block |> member "name" |> to_string_option)
                      safe_protocol_identifier )
                with
                | Some id, Some name ->
                    Some (Task_event.Tool_started {id = Some id; name})
                | (None | Some _), (None | Some _) -> None)
            | Some _ | None -> None)
    | Some "system"
      when json |> member "subtype" |> to_string_option = Some "init" ->
        Option.to_list
          (Option.map
             (fun id -> Task_event.Session_id id)
             (Option.bind
                (json |> member "session_id" |> to_string_option)
                canonical_session_id))
    | Some "result"
      when public_success_subtype json
           && json |> member "is_error" |> to_bool_option = Some false ->
        let text =
          Option.to_list
            (Option.map
               (fun text -> Task_event.Agent_text_delta text)
               (public_result_text json))
        in
        let session =
          Option.to_list
            (Option.map
               (fun id -> Task_event.Session_id id)
               (Option.bind
                  (json |> member "session_id" |> to_string_option)
                  canonical_session_id))
        in
        let usage =
          Option.to_list
            (Option.map (fun cost -> Task_event.Token_usage cost) (public_usage json))
        in
        text @ session @ usage
    | Some _ | None -> []
  with _ -> []

let normalized_events_of_stdout stdout =
  String.split_on_char '\n' stdout
  |> List.concat_map normalized_events_of_stream_line

let parse_public_stdout_text stdout =
  let lines = String.split_on_char '\n' stdout in
  let result_text =
    List.rev lines
    |> List.find_map (fun line ->
        try public_result_text (Yojson.Safe.from_string line) with _ -> None)
  in
  match result_text with
  | Some text -> text
  | None ->
      lines
      |> List.concat_map normalized_events_of_stream_line
      |> List.filter_map (function
           | Task_event.Agent_text_delta text -> Some text
           | _ -> None)
      |> String.concat ""

let parse_public_session_id stdout =
  normalized_events_of_stdout stdout
  |> List.find_map (function Task_event.Session_id id -> Some id | _ -> None)

let parse_public_cost stdout =
  normalized_events_of_stdout stdout
  |> List.find_map (function Task_event.Token_usage cost -> Some cost | _ -> None)

(** Parse one stream event into display-safe public text. *)
let parse_stream_event line =
  let rendered =
    normalized_events_of_stream_line line
    |> List.filter_map (function
         | Task_event.Agent_text_delta text -> Some text
         | Task_event.Tool_started {name; _} -> Some ("\xe2\x86\x92 " ^ name)
         | Task_event.Session_id _ -> Some "[Session started]"
         | _ -> None)
  in
  match rendered with [] -> None | _ -> Some (String.concat "\n" rendered)

(* Extract response text from Claude Code JSON stdout *)
let parse_stdout_text stdout =
  try
    let json = Yojson.Safe.from_string stdout in
    let text, _ = parse_json_output json in
    text
  with _ -> stdout

let failed_result message =
  make_task_result ~status:(Failed message) ~stderr:message ~exit_code:1 ()

let requested_attachment_delivery ?context spec =
  match context with
  | None -> Ok Upload_attachments
  | Some context -> (
      match Task_execution_context.requested_delivery context with
      | None -> Ok Upload_attachments
      | Some delivery
        when delivery.attachment_references = spec.attachments
             && delivery.web_access_policy = spec.web_access ->
          Ok delivery.attachment_delivery
      | Some _ ->
          invocation_error "the execution delivery context does not match the task")

let run_invocation ~sw ~env ~spec ?context ?on_raw_line ?on_display invocation =
  Diagnostics.debug "backend command: %s stdin=%s"
    (String.concat " " invocation.redacted_argv)
    invocation.redacted_stdin ;
  let on_stdout line =
    Option.iter (fun callback -> callback line) on_raw_line ;
    Option.iter
      (fun context ->
        List.iter
          (Task_execution_context.emit context)
          (normalized_events_of_stream_line line))
      context ;
    Option.iter
      (fun display -> Option.iter display (parse_stream_event line))
      on_display
  in
  Option.iter Task_execution_context.claim_structured_text context ;
  let result =
    Backend_process.run_process
      ~sw
      ~env
      ~cmd:invocation.argv
      ~stdin_content:(Some invocation.stdin)
      ~working_dir:spec.working_dir
      ~timeout_seconds:(duration_to_seconds spec.timeout)
      ?context
      ~parse_cost:parse_public_cost
      ~on_stdout
      ()
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

let run_task_common ~sw ~env ?context ?on_raw_line ?on_display spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None -> (
      match requested_attachment_delivery ?context spec with
      | Error message -> failed_result message
      | Ok attachment_delivery -> (
          match
            validate_transport_request ~attachment_delivery ~attachment_paths:[]
              spec
          with
          | Error message -> failed_result message
          | Ok () ->
              (* Sensitive inputs and caller-controlled resume values have been
                 rejected before any config I/O. *)
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
              let mcp_config_path = Backend_process.setup_mcp_config ~env spec in
              Fun.protect
                ~finally:(fun () ->
                  Option.iter (Backend_process.cleanup_mcp_config ~env)
                    mcp_config_path)
                (fun () ->
                  match
                    build_invocation ~attachment_delivery ~project_config_path
                      ~mcp_config_path spec
                  with
                  | Error message -> failed_result message
                  | Ok invocation ->
                      run_invocation ~sw ~env ~spec ?context ?on_raw_line
                        ?on_display invocation)))

let run_task ~sw ~env ?context ?on_raw_line spec =
  run_task_common ~sw ~env ?context ?on_raw_line spec

let run_task_streaming ~sw ~env ~on_stdout ?context ?on_raw_line spec =
  run_task_common ~sw ~env ?context ?on_raw_line ~on_display:on_stdout spec
