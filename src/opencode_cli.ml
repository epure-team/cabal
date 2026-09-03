(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Backend_types

let id = "opencode"

let name = "OpenCode"

(* OpenCode is provider-agnostic; the canonical pair below mirrors the
   model slugs documented for `opencode run --model`. *)
let models =
  [
    "claude-opus-4-7";
    "claude-sonnet-4-6";
    "claude-haiku-4-5-20251001";
    "gpt-4o";
    "gpt-4o-mini";
    "gpt-5";
  ]

(* `opencode models` lists every provider/model slug the local CLI knows
   about, one per line, without requiring authentication.  Use it as the
   live enumeration source, parsing non-empty lines and falling back when
   the binary is absent, the call times out, or the output is empty. *)
let parse_models_output stdout =
  stdout |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      let trimmed = String.trim line in
      if String.length trimmed = 0 then None else Some trimmed)

let models_probe =
  Some
    (fun ~sw:_ ~env ->
      match
        Backend_process.capture_version_output
          ~env
          ~timeout_seconds:5.0
          ["opencode"; "models"]
      with
      | Error msg -> Error msg
      | Ok output -> (
          match parse_models_output output with
          | [] -> Error "opencode models returned no parseable lines"
          | models -> Ok models))

let available ~sw:_ ~env =
  Backend_process.check_available ~env ["opencode"; "--version"]

let supports_session_resume = false

let native_json_schema_output = false

let is_resume_failure (_result : task_result) = false

let status_text = function
  | Success -> "success"
  | Failed msg -> msg
  | Timeout -> "timed out"
  | Cancelled -> "cancelled"

let config_applied setup_result =
  match setup_result.Backend_config_writer.write_outcome with
  | Some result -> Backend_config_writer.write_result_was_applied result
  | None -> false

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
  if not (config_applied setup_result) then
    Agentic_backend.Config_check_unsupported
      "OpenCode config was not applied; refusing to validate user-authored \
       opencode.json"
  else if not (available ~sw ~env) then
    Agentic_backend.Config_check_unsupported
      "OpenCode CLI is not available on PATH; cannot run native debug config \
       validation"
  else
    let cmd = ["opencode"; "debug"; "config"] in
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
           "OpenCode native config validation could not run: %s"
           (Printexc.to_string e))

(* Shared json_string_map encoding lives in Backend_json_helpers. *)
type json_string_map = Backend_json_helpers.json_string_map

let json_string_map_to_yojson = Backend_json_helpers.json_string_map_to_yojson

let json_string_map_of_yojson = Backend_json_helpers.json_string_map_of_yojson

type local_mcp_server_settings = {
  type_ : string; [@key "type"]
  command : string list;
  enabled : bool;
  environment : json_string_map;
}
[@@deriving yojson]

type mcp_server_map = (string * local_mcp_server_settings) list

let mcp_server_map_to_yojson servers =
  `Assoc
    (List.map
       (fun (name, server) ->
         (name, local_mcp_server_settings_to_yojson server))
       servers)

let mcp_server_map_of_yojson = function
  | `Assoc fields ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (name, json) :: rest -> (
            match local_mcp_server_settings_of_yojson json with
            | Ok server -> loop ((name, server) :: acc) rest
            | Error msg -> Error (Printf.sprintf "%s: %s" name msg))
      in
      loop [] fields
  | _ -> Error "expected JSON object"

type opencode_lsp_server_settings = {
  command : string list;
  extensions : string list;
}
[@@deriving yojson]

type opencode_lsp_server_map = (string * opencode_lsp_server_settings) list

let opencode_lsp_server_map_to_yojson servers =
  `Assoc
    (List.map
       (fun (name, server) ->
         (name, opencode_lsp_server_settings_to_yojson server))
       servers)

let opencode_lsp_server_map_of_yojson = function
  | `Assoc fields ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (name, json) :: rest -> (
            match opencode_lsp_server_settings_of_yojson json with
            | Ok server -> loop ((name, server) :: acc) rest
            | Error msg -> Error (Printf.sprintf "%s: %s" name msg))
      in
      loop [] fields
  | _ -> Error "expected JSON object"

type project_config_json = {
  mcp : mcp_server_map option; [@yojson.option]
  lsp : opencode_lsp_server_map option; [@yojson.option]
}
[@@deriving yojson]

let lsp_entry_for_server (cfg : Backend_types.lsp_server_config) =
  match List.map (fun a -> a.Backend_types.extension) cfg.file_associations with
  | [] -> None
  | extensions ->
      Some (cfg.name, {command = cfg.command :: cfg.args; extensions})

let lsp_map_for_servers lsp_servers =
  match List.filter_map lsp_entry_for_server lsp_servers with
  | [] -> None
  | entries -> Some entries

let project_config_body lsp_servers =
  let json =
    match
      project_config_json_to_yojson
        {mcp = None; lsp = lsp_map_for_servers lsp_servers}
    with
    | `Assoc fields ->
        `Assoc (List.filter (fun (_name, value) -> value <> `Null) fields)
    | other -> other
  in
  json |> Yojson.Safe.pretty_to_string |> fun s -> s ^ "\n"

let project_config_artifacts ~managed_namespace ~mcp_servers:_ ~lsp_servers =
  let body = project_config_body lsp_servers in
  [
    {
      Backend_config_writer.backend_id = id;
      ownership = Backend_config_writer.Backend_project;
      managed_namespace;
      project_relative_path = "opencode.json";
      content =
        Backend_config_writer.with_managed_header
          ~managed_namespace
          Backend_config_writer.Slash
          ~backend_id:id
          body;
    };
  ]

(* OpenCode reads MCP config from opencode.json in the project directory,
   not via CLI flags. This function ensures the host-supplied MCP server
   entry is present and up-to-date in opencode.json before each run. *)
let strip_jsonc_comments content =
  let len = String.length content in
  let buf = Buffer.create len in
  let rec skip_comment i =
    if i >= len then i
    else if content.[i] = '\n' then begin
      Buffer.add_char buf '\n' ;
      i + 1
    end
    else skip_comment (i + 1)
  in
  let rec skip_block_comment i =
    if i + 1 >= len then failwith "unterminated JSONC block comment"
    else if content.[i] = '*' && content.[i + 1] = '/' then i + 2
    else begin
      if content.[i] = '\n' then Buffer.add_char buf '\n' ;
      skip_block_comment (i + 1)
    end
  in
  let rec loop i in_string escaped =
    if i >= len then ()
    else
      let c = content.[i] in
      if in_string then begin
        Buffer.add_char buf c ;
        if escaped then loop (i + 1) true false
        else if c = '\\' then loop (i + 1) true true
        else if c = '"' then loop (i + 1) false false
        else loop (i + 1) true false
      end
      else if c = '"' then begin
        Buffer.add_char buf c ;
        loop (i + 1) true false
      end
      else if c = '/' && i + 1 < len && content.[i + 1] = '/' then
        loop (skip_comment (i + 2)) false false
      else if c = '/' && i + 1 < len && content.[i + 1] = '*' then
        loop (skip_block_comment (i + 2)) false false
      else begin
        Buffer.add_char buf c ;
        loop (i + 1) false false
      end
  in
  loop 0 false false ;
  Buffer.contents buf

let remove_jsonc_trailing_commas content =
  let len = String.length content in
  let buf = Buffer.create len in
  let rec next_non_ws i =
    if i >= len then None
    else
      match content.[i] with
      | ' ' | '\t' | '\r' | '\n' -> next_non_ws (i + 1)
      | c -> Some c
  in
  let rec loop i in_string escaped =
    if i >= len then ()
    else
      let c = content.[i] in
      if in_string then begin
        Buffer.add_char buf c ;
        if escaped then loop (i + 1) true false
        else if c = '\\' then loop (i + 1) true true
        else if c = '"' then loop (i + 1) false false
        else loop (i + 1) true false
      end
      else if c = '"' then begin
        Buffer.add_char buf c ;
        loop (i + 1) true false
      end
      else if c = ',' then (
        match next_non_ws (i + 1) with
        | Some ('}' | ']') -> loop (i + 1) false false
        | _ ->
            Buffer.add_char buf c ;
            loop (i + 1) false false)
      else begin
        Buffer.add_char buf c ;
        loop (i + 1) false false
      end
  in
  loop 0 false false ;
  Buffer.contents buf

let parse_jsonc content =
  try
    content |> strip_jsonc_comments |> remove_jsonc_trailing_commas
    |> Yojson.Safe.from_string |> Option.some
  with _ -> None

let leading_comment_header content =
  let lines = String.split_on_char '\n' content in
  let rec take acc = function
    | line :: rest when String.trim line = "" -> take (line :: acc) rest
    | line :: rest when String.starts_with ~prefix:"//" (String.trim line) ->
        take (line :: acc) rest
    | _ -> List.rev acc
  in
  match take [] lines with
  | [] -> ""
  | header -> String.concat "\n" header ^ "\n"

let is_managed_comment_header ~managed_namespace header =
  Backend_config_writer.is_managed_content ~managed_namespace header
  || Option.is_some
       (Backend_config_writer.extract_hash ~managed_namespace header)

let is_legacy_epure_field = function
  | "_epure_attribution" | "_epure-managed" | "_epure-hash" -> true
  | _ -> false

let has_legacy_epure_fields fields =
  List.exists (fun (name, _) -> is_legacy_epure_field name) fields

let managed_header_for_body ~managed_namespace body =
  let hash =
    Backend_config_writer.managed_body_hash
      ~managed_namespace
      ~backend_id:"opencode"
      body
  in
  Printf.sprintf
    "// %s\n// %s\n// %s: %s\n"
    (Backend_config_writer.attribution_text_for managed_namespace)
    (Backend_config_writer.managed_marker_for managed_namespace)
    (Backend_config_writer.hash_marker_for managed_namespace)
    hash

let header_for_updated_body ~managed_namespace ~existing_content
    ~existing_fields body =
  match existing_content with
  | None -> managed_header_for_body ~managed_namespace body
  | Some content ->
      let comment_header = leading_comment_header content in
      if is_managed_comment_header ~managed_namespace comment_header then
        managed_header_for_body ~managed_namespace body
      else if has_legacy_epure_fields existing_fields then
        managed_header_for_body ~managed_namespace body
      else comment_header

let ensure_mcp_in_opencode_json ~env (spec : task_spec) =
  match spec.mcp_servers with
  | [] -> ()
  | servers -> (
      let fs = Eio.Stdenv.fs env in
      let config_path = Filename.concat spec.working_dir "opencode.json" in
      let existing_content =
        try Some (Eio.Path.load Eio.Path.(fs / config_path)) with _ -> None
      in
      let existing =
        match existing_content with
        | None -> Some (`Assoc [])
        | Some content -> parse_jsonc content
      in
      match existing with
      | None -> ()
      | Some (`Assoc _ as existing) ->
          (* Build the MCP entries for our servers *)
          let new_mcp_entries =
            List.map
              (fun (cfg : mcp_server_config) ->
                ( cfg.name,
                  local_mcp_server_settings_to_yojson
                    {
                      type_ = "local";
                      command = cfg.command :: cfg.args;
                      enabled = true;
                      environment = cfg.env;
                    } ))
              servers
          in
          (* Merge into existing config: update "mcp" key, preserve everything else *)
          let raw_fields =
            match existing with `Assoc fields -> fields | _ -> []
          in
          let existing_fields =
            List.filter
              (fun (name, _) -> not (is_legacy_epure_field name))
              raw_fields
          in
          let existing_mcp =
            match List.assoc_opt "mcp" existing_fields with
            | Some (`Assoc entries) -> entries
            | _ -> []
          in
          (* Replace our server entries, keep any other user MCP servers *)
          let our_names = List.map (fun (name, _) -> name) new_mcp_entries in
          let kept_mcp =
            List.filter
              (fun (name, _) -> not (List.mem name our_names))
              existing_mcp
          in
          let merged_mcp = kept_mcp @ new_mcp_entries in
          let updated_fields =
            List.map
              (fun (k, v) ->
                if k = "mcp" then (k, `Assoc merged_mcp) else (k, v))
              existing_fields
          in
          let final_fields =
            if List.mem_assoc "mcp" updated_fields then updated_fields
            else updated_fields @ [("mcp", `Assoc merged_mcp)]
          in
          let json = `Assoc final_fields in
          let body = Yojson.Safe.pretty_to_string json in
          let header =
            header_for_updated_body
              ~managed_namespace:spec.managed_namespace
              ~existing_content
              ~existing_fields:raw_fields
              body
          in
          let content = header ^ body ^ "\n" in
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(fs / config_path)
            content
      | Some _ -> ())

let write_outcome_reason = function
  | None -> "no setup outcome was recorded"
  | Some (Backend_config_writer.Skipped_user_content _) ->
      "user-authored opencode.json was skipped"
  | Some (Backend_config_writer.Refused_hash_mismatch _) ->
      "opencode.json was refused because of a hash mismatch"
  | Some Backend_config_writer.Already_current -> "opencode.json is current"
  | Some (Backend_config_writer.Written _) -> "opencode.json was written"
  | Some (Backend_config_writer.Backed_up_and_written _) ->
      "opencode.json was backed up and written"
  | Some (Backend_config_writer.Invalid_managed_namespace _) ->
      "managed namespace was invalid"

let ensure_mcp_if_config_applied ~env ~setup_outcome spec =
  match spec.mcp_servers with
  | [] -> Ok ()
  | _ -> (
      match setup_outcome with
      | Some result when Backend_config_writer.write_result_was_applied result
        ->
          ensure_mcp_in_opencode_json ~env spec ;
          Ok ()
      | _ ->
          Error
            (Printf.sprintf
               "OpenCode MCP servers were requested, but opencode.json was not \
                updated (%s). Refusing to run without the requested MCP \
                config."
               (write_outcome_reason setup_outcome)))

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

let canonical_prefixed_id prefix value =
  let lowercase_hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
  let alphanumeric = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false
  in
  String.length value = 30
  && String.starts_with ~prefix:(prefix ^ "_") value
  && String.for_all lowercase_hex (String.sub value 4 12)
  && String.for_all alphanumeric (String.sub value 16 14)

let canonical_opencode_session_id = canonical_prefixed_id "ses"

let canonical_opencode_message_id = canonical_prefixed_id "msg"

let canonical_opencode_part_id = canonical_prefixed_id "prt"

let nonnegative_int_member name json =
  match Yojson.Safe.Util.member name json with
  | `Int value when value >= 0 -> Some value
  | `Intlit value -> (
      match int_of_string_opt value with
      | Some value when value >= 0 -> Some value
      | Some _ | None -> None)
  | _ -> None

let finite_nonnegative_number = function
  | (`Int _ | `Intlit _ | `Float _) as value -> (
      match Yojson.Safe.Util.to_number_option value with
      | Some number
        when number >= 0.0
             && not (Float.is_nan number || Float.is_infinite number) ->
          Some number
      | Some _ | None -> None)
  | _ -> None

let nonnegative_float_member name json =
  finite_nonnegative_number (Yojson.Safe.Util.member name json)

let valid_completed_timing = function
  | `Assoc _ as timing -> (
      match
        ( finite_nonnegative_number (Yojson.Safe.Util.member "start" timing),
          finite_nonnegative_number (Yojson.Safe.Util.member "end" timing) )
      with
      | Some start, Some end_ -> end_ >= start
      | _ -> false)
  | _ -> false

let token_usage_of_part part =
  let open Yojson.Safe.Util in
  match
    ( part |> member "type",
      part |> member "reason",
      part |> member "tokens",
      nonnegative_float_member "cost" part )
  with
  | `String "step-finish", `String _, (`Assoc _ as tokens), Some cost_usd ->
      let cache = tokens |> member "cache" in
      let tokens_input = nonnegative_int_member "input" tokens in
      let tokens_output = nonnegative_int_member "output" tokens in
      let reasoning = nonnegative_int_member "reasoning" tokens in
      let cache_read_input_tokens =
        match cache with
        | `Assoc _ -> nonnegative_int_member "read" cache
        | _ -> None
      in
      let cache_creation_input_tokens =
        match cache with
        | `Assoc _ -> nonnegative_int_member "write" cache
        | _ -> None
      in
      let total_valid =
        match tokens |> member "total" with
        | `Null -> true
        | _ -> Option.is_some (nonnegative_int_member "total" tokens)
      in
      if
        total_valid
        && Option.is_some tokens_input
        && Option.is_some tokens_output
        && Option.is_some reasoning
        && Option.is_some cache_read_input_tokens
        && Option.is_some cache_creation_input_tokens
      then
        Some
          {
            Backend_types.tokens_input;
            tokens_output;
            cost_usd = Some cost_usd;
            cache_creation_input_tokens;
            cache_read_input_tokens;
          }
      else None
  | _ -> None

let completed_text_of_part part =
  let open Yojson.Safe.Util in
  match (part |> member "type", part |> member "text", part |> member "time") with
  | `String "text", `String text, timing when valid_completed_timing timing ->
      Some text
  | _ -> None

let completed_tool_of_part part =
  let open Yojson.Safe.Util in
  match (part |> member "type", part |> member "state") with
  | `String "tool", (`Assoc _ as state)
    when state |> member "status" = `String "completed" -> (
      match
        ( Option.bind
            (part |> member "tool" |> to_string_option)
            safe_protocol_identifier,
          Option.bind
            (part |> member "callID" |> to_string_option)
            safe_protocol_identifier,
          state |> member "input",
          state |> member "output",
          state |> member "title",
          state |> member "metadata",
          state |> member "time" )
      with
      | ( Some name,
          Some id,
          `Assoc _,
          `String _,
          `String _,
          `Assoc _,
          timing )
        when valid_completed_timing timing ->
          Some (Task_event.Tool_finished {id = Some id; name = Some name})
      | _ -> None)
  | _ -> None

type strict_record =
  | Ignored_record
  | Public_record of {
      session_id : string;
      message_id : string;
      starts_message : bool;
      payloads : Task_event.payload list;
    }

type protocol_failure =
  | Error_record
  | Invalid_jsonl
  | Invalid_envelope
  | Mixed_session
  | Mixed_message

let common_public_envelope json part =
  let open Yojson.Safe.Util in
  match
    ( finite_nonnegative_number (json |> member "timestamp"),
      json |> member "sessionID",
      part |> member "id",
      part |> member "sessionID",
      part |> member "messageID" )
  with
  | ( Some _,
      `String session_id,
      `String part_id,
      `String part_session_id,
      `String message_id )
    when canonical_opencode_session_id session_id
         && session_id = part_session_id
         && canonical_opencode_part_id part_id
         && canonical_opencode_message_id message_id ->
      Some (session_id, message_id)
  | _ -> None

(* Parse only the public OpenCode 1.14.20 JSONL surface. The exact source at
   v1.14.20 emits this top-level envelope from RunCommand.emit. Runtime stream
   state below binds timed text to the message ID introduced by the assistant
   processor's step-start. See the pinned source citations in the addendum. *)
let strict_record_of_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let event_type = json |> member "type" |> to_string_option in
    let part = json |> member "part" in
    match event_type with
    | Some "error" -> Error Error_record
    | Some ("step_start" | "text" | "tool_use" | "step_finish") -> (
        match common_public_envelope json part with
        | None -> Error Invalid_envelope
        | Some (session_id, message_id) -> (
            let public ~starts_message payloads =
              Ok
                (Public_record
                   {session_id; message_id; starts_message; payloads})
            in
            match event_type with
            | Some "step_start" -> (
                match part |> member "type" with
                | `String "step-start" ->
                    public ~starts_message:true
                      [Task_event.Session_id session_id]
                | _ -> Error Invalid_envelope)
            | Some "text" -> (
                match completed_text_of_part part with
                | Some text ->
                    public ~starts_message:false
                      [Task_event.Agent_text_delta text]
                | None -> Error Invalid_envelope)
            | Some "tool_use" -> (
                match completed_tool_of_part part with
                | Some payload -> public ~starts_message:false [payload]
                | None -> Error Invalid_envelope)
            | Some "step_finish" -> (
                match token_usage_of_part part with
                | Some usage ->
                    public ~starts_message:false [Task_event.Token_usage usage]
                | None -> Error Invalid_envelope)
            | Some _ | None -> assert false))
    | Some _ -> Ok Ignored_record
    | None -> Error Invalid_jsonl
  with _ -> Error Invalid_jsonl

let strict_normalized_events_of_line line =
  match strict_record_of_line line with
  | Ok (Public_record record) -> record.payloads
  | Ok Ignored_record | Error _ -> []

(* Retain the exact minimal fixture accepted by the original public utility.
   OpenCode runtime JSONL always adds timestamp/sessionID, so the runtime parser
   below deliberately does not use this compatibility projection. *)
let legacy_normalized_text_fixture line =
  try
    match Yojson.Safe.from_string line with
    | `Assoc fields when List.length fields = 2 -> (
        match (List.assoc_opt "type" fields, List.assoc_opt "part" fields) with
        | Some (`String "text"), Some (`Assoc [("text", `String text)]) ->
            [Task_event.Agent_text_delta text]
        | _ -> [])
    | _ -> []
  with _ -> []

let normalized_events_of_line line =
  match strict_normalized_events_of_line line with
  | [] -> legacy_normalized_text_fixture line
  | events -> events

type stream_parser = {
  mutable stream_session_id : string option;
  mutable stream_message_id : string option;
  mutable stream_failure : protocol_failure option;
}

let create_stream_parser () =
  {stream_session_id = None; stream_message_id = None; stream_failure = None}

let fail_stream parser failure =
  parser.stream_failure <- Some failure ;
  []

let consume_strict_line parser line =
  if Option.is_some parser.stream_failure || String.trim line = "" then []
  else
    match strict_record_of_line line with
    | Error failure -> fail_stream parser failure
    | Ok Ignored_record -> []
    | Ok (Public_record record) ->
        let session_valid =
          match parser.stream_session_id with
          | None ->
              parser.stream_session_id <- Some record.session_id ;
              true
          | Some expected -> expected = record.session_id
        in
        if not session_valid then fail_stream parser Mixed_session
        else
          let message_valid =
            match record.starts_message with
            | true ->
                parser.stream_message_id <- Some record.message_id ;
                true
            | false -> (
                match parser.stream_message_id with
                | Some expected -> expected = record.message_id
                | None -> false)
          in
          if message_valid then record.payloads
          else fail_stream parser Mixed_message

let normalized_stream stdout =
  let parser = create_stream_parser () in
  let events = ref [] in
  String.split_on_char '\n' stdout
  |> List.iter (fun line ->
      let next = consume_strict_line parser line in
      events := List.rev_append next !events) ;
  match parser.stream_failure with
  | Some failure -> Error failure
  | None -> Ok (List.rev !events, parser.stream_session_id)

let parse_json_events stdout =
  match normalized_stream stdout with
  | Error _ -> ("", None)
  | Ok (events, _) ->
      let text = Buffer.create 4096 in
      let costs = ref [] in
      List.iter
        (function
          | Task_event.Agent_text_delta delta -> Buffer.add_string text delta
          | Token_usage usage -> costs := Some usage :: !costs
          | _ -> ())
        events ;
      (Buffer.contents text, Backend_types.aggregate_costs (List.rev !costs))

let parse_cost_from_stdout stdout =
  let _, cost = parse_json_events stdout in
  cost

let parse_public_session_id stdout =
  match normalized_stream stdout with
  | Error _ -> None
  | Ok (_, session_id) -> session_id

type backend_invocation = {
  argv : string list;
  stdin : string option;
  redacted_argv : string list;
}

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let invocation_error message = Error ("OpenCode invocation rejected: " ^ message)

let staged_path_matches_media_type attachment path =
  path <> ""
  && (not (Filename.is_relative path))
  && (not (String.contains path '\000'))
  &&
  match attachment.Backend_types.media_type with
  | Backend_types.Png -> String.ends_with ~suffix:".png" path
  | Backend_types.Jpeg -> String.ends_with ~suffix:".jpg" path

let validate_transport_request ~attachment_delivery ~attachment_paths spec =
  let* () =
    match spec.Backend_types.resume_session_id with
    | None -> Ok ()
    | Some _ -> invocation_error "session resume is unsupported"
  in
  let* () =
    match attachment_delivery with
    | Backend_types.Upload_attachments -> Ok ()
    | Reuse_session_attachments ->
        invocation_error "session attachment reuse is unsupported"
  in
  if
    List.length attachment_paths <> List.length spec.attachments
    || not
         (List.for_all2 staged_path_matches_media_type spec.attachments
            attachment_paths)
  then invocation_error "the sealed attachment set does not match the task"
  else Ok ()

let web_permission_json = function
  | Backend_types.Web_disabled ->
      {|{"websearch":"deny","webfetch":"deny","codesearch":"deny","task":"deny"}|}
  | Web_search ->
      {|{"websearch":"allow","webfetch":"deny","codesearch":"deny","task":"deny"}|}
  | Web_search_and_fetch ->
      {|{"websearch":"allow","webfetch":"allow","codesearch":"deny","task":"deny"}|}

let web_config_json ?(agent = "build") web_access =
  let permission = web_permission_json web_access in
  Printf.sprintf
    {|{"share":"disabled","permission":%s,"agent":{"%s":{"mode":"primary","permission":%s}}}|}
    permission agent permission

let fixed_environment_args ~project_config_path web_access =
  [
    "env";
    "-u";
    "OPENCODE_DB";
    "OPENCODE_PERMISSION=" ^ web_permission_json web_access;
    "OPENCODE_CONFIG_CONTENT=" ^ web_config_json web_access;
    "OPENCODE_CONFIG=" ^ project_config_path;
    "OPENCODE_CONFIG_DIR=";
    "OPENCODE_DISABLE_PROJECT_CONFIG=1";
    "OPENCODE_EXPERIMENTAL=0";
    "OPENCODE_EXPERIMENTAL_EXA=0";
    ( "OPENCODE_ENABLE_EXA="
    ^ if web_access = Backend_types.Web_disabled then "0" else "1" );
    "OPENCODE_AUTO_SHARE=0";
    "OPENCODE_DISABLE_AUTOUPDATE=1";
    "OPENCODE_DISABLE_LSP_DOWNLOAD=1";
  ]

let image_args paths =
  List.concat_map (fun path -> ["--file"; path]) paths

let redacted_image_args paths =
  List.mapi
    (fun index _ -> ["--file"; Printf.sprintf "<attachment-%d>" (index + 1)])
    paths
  |> List.concat

let rec remove_tree path =
  try
    match (Unix.lstat path).st_kind with
    | Unix.S_DIR ->
        let children_removed =
          Sys.readdir path
          |> Array.fold_left
               (fun success child ->
                 remove_tree (Filename.concat path child) && success)
               true
        in
        if children_removed then (
          Unix.rmdir path ;
          true)
        else false
    | _ ->
        Unix.unlink path ;
        true
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> true
  | Unix.Unix_error _ | Sys_error _ -> false

let with_isolated_config_home f =
  match
    try
      Ok (Filename.temp_dir ~perms:0o700 "cabal-opencode-config-" "")
    with Sys_error _ | Unix.Unix_error _ -> Error ()
  with
  | Error () -> Error "OpenCode config isolation could not be created"
  | Ok directory ->
      let result =
        Fun.protect ~finally:(fun () -> ignore (remove_tree directory))
          (fun () -> f directory)
      in
      Ok result

let isolate_invocation directory web_access invocation =
  let runtime_agent = "cabal-" ^ Filename.basename directory in
  let config_prefix = "OPENCODE_CONFIG_CONTENT=" in
  let rewrite_config agent =
    List.map (fun arg ->
        if String.starts_with ~prefix:config_prefix arg then
          config_prefix ^ web_config_json ~agent web_access
        else arg)
  in
  let rec rewrite_agent agent = function
    | "--agent" :: _ :: rest -> "--agent" :: agent :: rest
    | item :: rest -> item :: rewrite_agent agent rest
    | [] -> []
  in
  let actual =
    [
      "XDG_CONFIG_HOME=" ^ directory;
      "OPENCODE_TEST_HOME=" ^ directory;
      ( "OPENCODE_TEST_MANAGED_CONFIG_DIR="
      ^ Filename.concat directory "managed" );
    ]
  in
  let redacted =
    [
      "XDG_CONFIG_HOME=<isolated-config>";
      "OPENCODE_TEST_HOME=<isolated-config>";
      "OPENCODE_TEST_MANAGED_CONFIG_DIR=<isolated-config>/managed";
    ]
  in
  let insert assignments = function
    | "env" :: "-u" :: name :: rest ->
        "env" :: "-u" :: name :: assignments @ rest
    | "env" :: rest -> "env" :: assignments @ rest
    | argv -> argv
  in
  {
    invocation with
    argv =
      invocation.argv |> rewrite_config runtime_agent
      |> rewrite_agent runtime_agent |> insert actual;
    redacted_argv =
      invocation.redacted_argv |> rewrite_config "<isolated-agent>"
      |> rewrite_agent "<isolated-agent>" |> insert redacted;
  }

let child_project_config_path working_dir =
  if working_dir = "" || String.contains working_dir '\000' then
    invocation_error "OpenCode working directory is invalid"
  else Ok "opencode.json"

let build_invocation ?(attachment_paths = [])
    ?(attachment_delivery = Backend_types.Upload_attachments) ~mcp_config_path:_
    (spec : task_spec) =
  let* () =
    validate_transport_request ~attachment_delivery ~attachment_paths spec
  in
  let* project_config_path = child_project_config_path spec.working_dir in
  let model_args, redacted_model_args =
    match spec.model with
    | Some model -> (["-m"; model], ["-m"; "<model>"])
    | None -> ([], [])
  in
  let root =
    fixed_environment_args ~project_config_path spec.web_access
    @ [
        "opencode";
        "run";
        "--pure";
        "--format";
        "json";
        "--agent";
        "build";
      ]
  in
  let redacted_root =
    fixed_environment_args ~project_config_path:"<project-config>" spec.web_access
    @ [
        "opencode";
        "run";
        "--pure";
        "--format";
        "json";
        "--agent";
        "build";
      ]
  in
  let full_prompt =
    if String.length spec.instructions > 0 then
      Printf.sprintf "%s\n\n---\nProject Instructions:\n%s" spec.prompt
        spec.instructions
    else spec.prompt
  in
  Ok
    {
      argv = root @ model_args @ image_args attachment_paths @ ["-"];
      stdin = Some full_prompt;
      redacted_argv =
        redacted_root @ redacted_model_args @ redacted_image_args attachment_paths
        @ ["-"];
    }

let build_command ~mcp_config_path spec =
  match build_invocation ~mcp_config_path spec with
  | Ok invocation ->
      ( invocation.argv,
        match invocation.stdin with Some stdin -> stdin | None -> "" )
  | Error message -> invalid_arg message

let parse_stdout_text stdout =
  let text, _ = parse_json_events stdout in
  text

let user_owned_backup_needed = function
  | Some (Backend_config_writer.Skipped_user_content _) -> true
  | _ -> false

let read_opencode_backup ~env ~config_path =
  let fs = Eio.Stdenv.fs env in
  try Ok (Eio.Path.load Eio.Path.(fs / config_path))
  with e ->
    Error
      (Printf.sprintf
         "Cannot read opencode.json before invocation: %s"
         (Printexc.to_string e))

let try_restore ~env ~config_path ~backup =
  let fs = Eio.Stdenv.fs env in
  try
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(fs / config_path)
      backup ;
    Ok ()
  with e -> Error (Printexc.to_string e)

let check_opencode_mutation ~env ~config_path ~backup result =
  let fs = Eio.Stdenv.fs env in
  let post_bytes =
    try Some (Eio.Path.load Eio.Path.(fs / config_path)) with _ -> None
  in
  match post_bytes with
  | Some post when post = backup -> result
  | changed ->
      let kind = match changed with None -> "deleted" | Some _ -> "mutated" in
      let restore_msg =
        match try_restore ~env ~config_path ~backup with
        | Ok () -> "original file has been restored"
        | Error exn_msg -> Printf.sprintf "restore failed: %s" exn_msg
      in
      {
        result with
        status =
          Failed
            (Printf.sprintf
               "OpenCode %s user-owned opencode.json; %s"
               kind
               restore_msg);
        stdout = "";
        agent_text = "";
        session_id = None;
      }

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
  | None -> Ok Backend_types.Upload_attachments
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
        | Error message -> invocation_error message
        | Ok sealed ->
            Ok
              {
                attachment_delivery = sealed.attachment_delivery;
                attachment_paths = sealed.attachment_paths;
              })

let protocol_failure_message = function
  | Error_record -> "OpenCode emitted an error event"
  | Invalid_jsonl | Invalid_envelope | Mixed_session | Mixed_message ->
      "OpenCode emitted an invalid structured response"

let run_invocation ~sw ~env ~spec ?context ?on_raw_line invocation =
  match
    with_isolated_config_home (fun config_home ->
        let invocation =
          isolate_invocation config_home spec.web_access invocation
        in
        Diagnostics.debug "backend command: %s"
          (String.concat " " invocation.redacted_argv) ;
        let stream_parser = create_stream_parser () in
        let on_stdout line =
          (match strict_record_of_line line with
          | Error Error_record -> ()
          | Error _ | Ok _ ->
              Option.iter (fun callback -> callback line) on_raw_line) ;
          let events = consume_strict_line stream_parser line in
          Option.iter
            (fun context ->
              List.iter (Task_execution_context.emit context) events)
            context
        in
        Option.iter Task_execution_context.claim_structured_text context ;
        let result =
          Backend_process.run_process
            ~sw
            ~env
            ~cmd:invocation.argv
            ~stdin_content:invocation.stdin
            ~working_dir:spec.working_dir
            ~timeout_seconds:(duration_to_seconds spec.timeout)
            ?context
            ~parse_cost:parse_cost_from_stdout
            ~on_stdout
            ()
        in
        let files_changed =
          Backend_process.get_git_diff ~sw ~env ~working_dir:spec.working_dir
        in
        let task_result =
          match normalized_stream result.stdout with
          | Error failure ->
              let message = protocol_failure_message failure in
              {
                status = Failed message;
                files_changed;
                report = None;
                elapsed = result.elapsed;
                cost = None;
                stdout = "";
                agent_text = "";
                stderr = message;
                exit_code = result.exit_code;
                session_id = None;
              }
          | Ok (_, session_id) ->
              {
                status = result.status;
                files_changed;
                report = None;
                elapsed = result.elapsed;
                cost = result.cost;
                stdout = result.stdout;
                agent_text = parse_stdout_text result.stdout;
                stderr = result.stderr;
                exit_code = result.exit_code;
                session_id;
              }
        in
        Option.iter Task_execution_context.mark_final_public_text context ;
        task_result)
  with
  | Ok result -> result
  | Error message ->
      make_task_result ~status:(Failed message) ~stderr:message ~exit_code:1 ()

let run_task ~sw ~env ?context ?on_raw_line spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None -> (
      match requested_transport_inputs ?context spec with
      | Error message -> failed_result message
      | Ok transport -> (
          match
            build_invocation
              ~attachment_paths:transport.attachment_paths
              ~attachment_delivery:transport.attachment_delivery
              ~mcp_config_path:None spec
          with
          | Error message -> failed_result message
          | Ok invocation ->
              (* Write initial Épure-managed opencode.json (attribution + empty
                 MCP) only after the complete transport request has passed. *)
              let setup =
                Backend_config_writer.setup_artifacts
                  ~project_dir:spec.working_dir
                  ~force:false
                  (project_config_artifacts
                     ~managed_namespace:spec.managed_namespace
                     ~mcp_servers:[]
                     ~lsp_servers:spec.lsp_servers)
              in
              (match
                 Backend_config_writer.precedence_warning_for
                   ~backend_id:id
                   ~write_outcome:setup.Backend_config_writer.write_outcome
               with
              | None -> ()
              | Some msg -> Diagnostics.user_warning "%s" msg) ;
              match
                ensure_mcp_if_config_applied
                  ~env
                  ~setup_outcome:setup.write_outcome
                  spec
              with
              | Error msg -> make_task_result ~status:(Failed msg) ()
              | Ok () ->
                  let runtime_spec = {spec with mcp_servers = []} in
                  let config_path =
                    Filename.concat spec.working_dir "opencode.json"
                  in
                  let run () =
                    run_invocation ~sw ~env ~spec:runtime_spec ?context
                      ?on_raw_line invocation
                  in
                  if user_owned_backup_needed setup.write_outcome then
                    match read_opencode_backup ~env ~config_path with
                    | Error msg -> make_task_result ~status:(Failed msg) ()
                    | Ok backup ->
                        check_opencode_mutation ~env ~config_path ~backup (run ())
                  else run ()))
