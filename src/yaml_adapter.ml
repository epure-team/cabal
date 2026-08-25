(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Backend_types

type config = {
  name : string;
  display_name : string;
  invocation_command : string;
  template_set : string;
  env_mappings : (string * string) list;
  timeout_seconds : float;
  source : string;
  models : string list;
}

(* Maps backend id → config for config_of lookup. *)
let config_table : (string, config) Hashtbl.t = Hashtbl.create 8

(* Pi's [--mode json] is NDJSON.  The terminal assistant message contains a
   content array; retain only text blocks so callers receive the model answer,
   never its reasoning/event stream.  Malformed events are ignored and the
   empty result fails closed in the host's structured-output boundary. *)
let parse_pi_json_events stdout =
  String.split_on_char '\n' stdout
  |> List.filter_map (fun line ->
      try
        match Yojson.Safe.from_string line with
        | `Assoc fields -> (
            match List.assoc_opt "type" fields, List.assoc_opt "message" fields with
            | Some (`String "message_end"), Some (`Assoc message) -> (
                match List.assoc_opt "role" message, List.assoc_opt "content" message with
                | Some (`String "assistant"), Some (`List blocks) ->
                    blocks
                    |> List.filter_map (function
                        | `Assoc block -> (
                            match List.assoc_opt "type" block, List.assoc_opt "text" block with
                            | Some (`String "text"), Some (`String text) -> Some text
                            | _ -> None)
                        | _ -> None)
                    |> fun parts -> Some (String.concat "" parts)
                | _ -> None)
            | _ -> None)
        | _ -> None
      with Yojson.Json_error _ -> None)
  |> String.concat "\n"

(* Pi emits a session header as the first NDJSON record. Preserve that stable
   identifier in [task_result] so an orchestrator can attest the association
   between its run ledger and Pi's independently stored transcript. *)
let parse_pi_session_id stdout =
  String.split_on_char '\n' stdout
  |> List.find_map (fun line ->
      try
        match Yojson.Safe.from_string line with
        | `Assoc fields -> (
            match List.assoc_opt "type" fields, List.assoc_opt "id" fields with
            | Some (`String "session"), Some (`String id) -> Some id
            | _ -> None)
        | _ -> None
      with Yojson.Json_error _ -> None)

let make_backend (cfg : config) : Agentic_backend.t =
  let module M = struct
    let id = cfg.name

    let name = cfg.display_name

    let models = cfg.models

    (* YAML-loaded adapters declare a static list only — they have no
       generic way to enumerate models from an arbitrary CLI invocation. *)
    let models_probe = None

    let available ~sw:_ ~env =
      (* Check availability by running the command with --version. *)
      let cmd =
        match
          List.filter
            (fun s -> String.length s > 0)
            (String.split_on_char ' ' (String.trim cfg.invocation_command))
        with
        | c :: _ -> c
        | [] -> cfg.invocation_command
      in
      Backend_process.check_available ~env [cmd; "--version"]

    let supports_session_resume = false

    let native_json_schema_output = false

    let is_resume_failure (_result : task_result) = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported
        (Printf.sprintf
           "YAML adapter '%s' from %s does not define a project config \
            validator"
           cfg.name
           cfg.source)

    (* Dispatch agent_text extraction to the matching hand-written adapter so
       YAML-loaded backends populate task_result.agent_text instead of leaving
       it empty. Without this, every host using a YAML-registered adapter sees
       agent_text = "" and has to re-parse raw stdout itself. *)
    let parse_stdout_for_id =
      match cfg.name with
      | "claude-code" -> Some Claude_code.parse_stdout_text
      | "codex" -> Some Codex_cli.parse_stdout_text
      | "gemini-cli" -> Some Gemini_cli.parse_stdout_text
      | "copilot-cli" -> Some Copilot_cli.parse_stdout_text
      | "opencode" -> Some Opencode_cli.parse_stdout_text
      | "pi" -> Some parse_pi_json_events
      | _ -> None

    let run_task ~sw ~env ?on_raw_line:_ (spec : task_spec) =
      let args =
        List.filter
          (fun s -> String.length s > 0)
          (String.split_on_char ' ' (String.trim cfg.invocation_command))
      in
      let full_prompt =
        if String.length spec.instructions > 0 then
          spec.prompt ^ "\n\n---\nProject Instructions:\n" ^ spec.instructions
        else spec.prompt
      in
      let build_command ~mcp_config_path:_ s =
        if cfg.name = "pi" then
          let model_args = match s.model with None -> [] | Some m -> ["--model"; m] in
          (args @ ["--print"; "--mode"; "json"; "--approve"] @ model_args, full_prompt)
        else (args, full_prompt)
      in
      Backend_process.run_task_with
        ~sw
        ~env
        ~spec:{spec with timeout = cfg.timeout_seconds}
        ~build_command
        ?parse_stdout:parse_stdout_for_id
        ?parse_session_id:(if cfg.name = "pi" then Some parse_pi_session_id else None)
        ()
  end in
  Hashtbl.replace config_table cfg.name cfg ;
  (module M : Agentic_backend.S)

let config_of backend =
  let id = Agentic_backend.id backend in
  Hashtbl.find_opt config_table id
