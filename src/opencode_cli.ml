(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Backend_types

let id = "opencode"

let name = "OpenCode"

let available ~sw:_ ~env =
  Backend_process.check_available ~env ["opencode"; "--version"]

let supports_session_resume = false

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

(* Parse OpenCode's JSON event output.
   OpenCode with --format json emits JSONL events, one per line.
   Event types:
     {"type":"text", "part":{"text":"..."}} — text content (concatenated)
     {"type":"step_finish", "part":{"tokens":{"input":N,"output":N},"cost":F}}
   Text events are concatenated in order to build the full response. *)
let parse_json_events stdout =
  let lines =
    stdout |> String.split_on_char '\n'
    |> List.filter (fun s -> String.length (String.trim s) > 0)
  in
  let text_buf = Buffer.create 4096 in
  let total_input = ref 0 in
  let total_output = ref 0 in
  let has_usage = ref false in
  let total_cost = ref 0.0 in
  List.iter
    (fun line ->
      try
        let json = Yojson.Safe.from_string line in
        let open Yojson.Safe.Util in
        let event_type =
          try json |> member "type" |> to_string with _ -> ""
        in
        let part = json |> member "part" in
        match event_type with
        | "text" -> (
            (* Extract text from part.text *)
            try
              let t = part |> member "text" |> to_string in
              Buffer.add_string text_buf t
            with _ -> ())
        | "step_finish" -> (
            (* Extract usage from part.tokens *)
            (try
               let tokens = part |> member "tokens" in
               if tokens <> `Null then (
                 has_usage := true ;
                 (try
                    total_input :=
                      !total_input + (tokens |> member "input" |> to_int)
                  with _ -> ()) ;
                 try
                   total_output :=
                     !total_output + (tokens |> member "output" |> to_int)
                 with _ -> ())
             with _ -> ()) ;
            (* Extract cost (USD) from part.cost *)
            try
              let c = part |> member "cost" |> to_number in
              if c > 0.0 then total_cost := !total_cost +. c
            with _ -> ())
        | _ -> ()
      with _ -> ())
    lines ;
  let text = Buffer.contents text_buf in
  let cost =
    if !has_usage then
      Some
        {
          tokens_input = (if !total_input > 0 then Some !total_input else None);
          tokens_output =
            (if !total_output > 0 then Some !total_output else None);
          cost_usd = (if !total_cost > 0.0 then Some !total_cost else None);
          cache_creation_input_tokens = None;
          cache_read_input_tokens = None;
        }
    else None
  in
  (text, cost)

let parse_cost_from_stdout stdout =
  let _, cost = parse_json_events stdout in
  cost

(* Build the opencode run command for non-interactive execution *)
let build_command ~mcp_config_path:_ (spec : task_spec) =
  (* Use - to read prompt from stdin *)
  let model_args = match spec.model with Some m -> ["-m"; m] | None -> [] in
  let base = ["opencode"; "run"; "--format"; "json"] @ model_args @ ["-"] in
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

(* Extract response text from OpenCode JSON events stdout *)
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
        session_id = None;
      }

let run_task ~sw ~env ?on_raw_line spec =
  match Backend_process.validate_task_namespace spec with
  | Some result -> result
  | None -> (
      (* Write initial Épure-managed opencode.json (attribution + empty MCP) if
         absent or still managed.  Must run before ensure_mcp_in_opencode_json so
         the file exists and carries Épure markers before MCP entries are merged. *)
      let setup =
        Backend_config_writer.setup_artifacts
          ~project_dir:spec.working_dir
          ~force:false
          (project_config_artifacts
             ~managed_namespace:spec.managed_namespace
             ~mcp_servers:[]
             ~lsp_servers:spec.lsp_servers)
      in
      (* AC2/AC4 story #479: OpenCode has Medium precedence confidence.  Emit
         non-fatal warning conditioned on whether the config was applied. *)
      (match
         Backend_config_writer.precedence_warning_for
           ~backend_id:id
           ~write_outcome:setup.Backend_config_writer.write_outcome
       with
      | None -> ()
      | Some msg -> Diagnostics.user_warning "%s" msg) ;
      let mcp_ready =
        ensure_mcp_if_config_applied
          ~env
          ~setup_outcome:setup.write_outcome
          spec
      in
      match mcp_ready with
      | Error msg -> make_task_result ~status:(Failed msg) ()
      | Ok () ->
          let runtime_spec = {spec with mcp_servers = []} in
          let config_path = Filename.concat spec.working_dir "opencode.json" in
          if user_owned_backup_needed setup.write_outcome then
            (* Story #515 AC1/AC3: take pre-run backup of user-owned opencode.json *)
            match read_opencode_backup ~env ~config_path with
            | Error msg -> make_task_result ~status:(Failed msg) ()
            | Ok backup ->
                let result =
                  Backend_process.run_task_with
                    ~sw
                    ~env
                    ~spec:runtime_spec
                    ~build_command
                    ~parse_cost:parse_cost_from_stdout
                    ~parse_stdout:parse_stdout_text
                    ?on_stdout:on_raw_line
                    ()
                in
                (* Story #515 AC2: detect mutation and restore *)
                check_opencode_mutation ~env ~config_path ~backup result
          else
            Backend_process.run_task_with
              ~sw
              ~env
              ~spec:runtime_spec
              ~build_command
              ~parse_cost:parse_cost_from_stdout
              ~parse_stdout:parse_stdout_text
              ?on_stdout:on_raw_line
              ())
