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

(* Parse Gemini CLI's JSON output format for a single event object.
   Gemini with --output-format json returns a JSON object with
   "response" or "result" containing the text, and optionally
   "usage" or "usageMetadata" with token counts. *)
let parse_gemini_json_output json =
  let open Yojson.Safe.Util in
  let result_text =
    (* Try "response" first, then "result", then stringify *)
    try json |> member "response" |> to_string
    with _ -> (
      try json |> member "result" |> to_string
      with _ -> ( try Yojson.Safe.to_string json with _ -> ""))
  in
  let cost =
    try
      (* Try "usageMetadata" (Gemini API style) then "usage" *)
      let usage =
        let u = json |> member "usageMetadata" in
        if u = `Null then json |> member "usage" else u
      in
      if usage = `Null then None
      else
        let input_tokens =
          try Some (usage |> member "promptTokenCount" |> to_int)
          with _ -> (
            try Some (usage |> member "input_tokens" |> to_int) with _ -> None)
        in
        let output_tokens =
          try Some (usage |> member "candidatesTokenCount" |> to_int)
          with _ -> (
            try Some (usage |> member "output_tokens" |> to_int)
            with _ -> None)
        in
        Some
          {
            tokens_input = input_tokens;
            tokens_output = output_tokens;
            cost_usd = None;
            cache_creation_input_tokens = None;
            cache_read_input_tokens = None;
          }
    with _ -> None
  in
  (result_text, cost)

(** Parse Gemini CLI's stream-json NDJSON output (one JSON event per line).
    Returns [(text, cost, session_id)] where [text] is assembled from
    [response]/[result]/[text] fields across all events, [cost] from
    [usageMetadata]/[usage] fields, and [session_id] from [session_id]. *)
let parse_gemini_stream_json stdout =
  let open Yojson.Safe.Util in
  let lines = String.split_on_char '\n' stdout in
  let text_chunks = ref [] in
  let final_response = ref None in
  let last_cost = ref None in
  let last_session_id = ref None in
  List.iter
    (fun line ->
      let trimmed = String.trim line in
      if String.length trimmed > 0 then
        try
          let json = Yojson.Safe.from_string trimmed in
          (* Prefer complete response/result fields over incremental text chunks *)
          (try
             let r = json |> member "response" |> to_string in
             final_response := Some r
           with _ -> (
             try
               let r = json |> member "result" |> to_string in
               final_response := Some r
             with _ -> (
               try
                 let t = json |> member "text" |> to_string in
                 text_chunks := t :: !text_chunks
               with _ -> ()))) ;
          (* Extract cost from this event if present *)
          (try
             let usage =
               let u = json |> member "usageMetadata" in
               if u = `Null then json |> member "usage" else u
             in
             if usage <> `Null then begin
               let input_tokens =
                 try Some (usage |> member "promptTokenCount" |> to_int)
                 with _ -> (
                   try Some (usage |> member "input_tokens" |> to_int)
                   with _ -> None)
               in
               let output_tokens =
                 try Some (usage |> member "candidatesTokenCount" |> to_int)
                 with _ -> (
                   try Some (usage |> member "output_tokens" |> to_int)
                   with _ -> None)
               in
               last_cost :=
                 Some
                   {
                     tokens_input = input_tokens;
                     tokens_output = output_tokens;
                     cost_usd = None;
                     cache_creation_input_tokens = None;
                     cache_read_input_tokens = None;
                   }
             end
           with _ -> ()) ;
          (* Extract session_id if present *)
          try
            let sid = json |> member "session_id" |> to_string in
            last_session_id := Some sid
          with _ -> ()
        with _ -> ())
    lines ;
  let text =
    match !final_response with
    | Some r -> r
    | None -> String.concat "" (List.rev !text_chunks)
  in
  (text, !last_cost, !last_session_id)

let parse_cost_from_stdout stdout =
  let _, cost, _ = parse_gemini_stream_json stdout in
  cost

(* Extract session_id from Gemini stream-json NDJSON stdout *)
let parse_session_id_from_stdout stdout =
  let _, _, session_id = parse_gemini_stream_json stdout in
  session_id

(* Build the gemini command for non-interactive stream-json execution *)
let build_command ~mcp_config_path:_ (spec : task_spec) =
  let model_args = match spec.model with Some m -> ["-m"; m] | None -> [] in
  let resume_args =
    match spec.resume_session_id with
    | Some sid -> ["--resume"; sid]
    | None -> []
  in
  let base =
    ["gemini"; "--output-format"; "stream-json"; "-y"; "--skip-trust"]
    @ model_args @ resume_args @ ["-p"; "-"]
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

(* Extract response text from Gemini stream-json NDJSON stdout *)
let parse_stdout_text stdout =
  let text, _, _ = parse_gemini_stream_json stdout in
  if String.length text = 0 then stdout else text

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

let run_task ~sw ~env ?on_raw_line spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None -> (
      (* Write workspace settings to .gemini/settings.json if absent or managed.
         Gemini CLI discovers this fixed path automatically.  GEMINI.md is project
         context and is intentionally left to humans/future project instructions. *)
      let setup =
        Backend_config_writer.setup_artifacts
          ~project_dir:spec.working_dir
          ~force:false
          (project_config_artifacts
             ~managed_namespace:spec.managed_namespace
             ~mcp_servers:spec.mcp_servers
             ~lsp_servers:spec.lsp_servers)
      in
      (* AC3/AC4 story #479: Gemini has limited precedence controls (Low
         confidence).  Emit non-fatal warning conditioned on whether the config
         was applied. *)
      (match
         Backend_config_writer.precedence_warning_for
           ~backend_id:id
           ~write_outcome:setup.Backend_config_writer.write_outcome
       with
      | None -> ()
      | Some msg -> Diagnostics.user_warning "%s" msg) ;
      match mcp_settings_error_if_needed setup spec.mcp_servers with
      | Some msg -> make_task_result ~status:(Failed msg) ()
      | None ->
          let runtime_spec = {spec with mcp_servers = []} in
          Backend_process.run_task_with
            ~sw
            ~env
            ~spec:runtime_spec
            ~build_command
            ~parse_cost:parse_cost_from_stdout
            ~parse_stdout:parse_stdout_text
            ~parse_session_id:parse_session_id_from_stdout
            ?on_stdout:on_raw_line
            ())
