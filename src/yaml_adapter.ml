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

(* Preserve only the latest exact package/config association per id. Physical
   package identity avoids misclassifying handwritten replacements, while
   same-id replacement and [Registry.clear] keep retention lifecycle-bounded. *)
let backend_configs : (Agentic_backend.t * config) list ref = ref []

let clear_config_cache () = backend_configs := []

let invocation_argv cfg =
  List.filter
    (fun value -> value <> "")
    (String.split_on_char ' ' (String.trim cfg.invocation_command))

let safe_binary_token value =
  value <> ""
  && value.[0] <> '-'
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x20 && code <> 0x7f)
       value

let binary_name cfg =
  match invocation_argv cfg with
  | command :: _ when safe_binary_token command -> Some command
  | _ -> None

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

(* Ollama occasionally closes an OpenAI-compatible streamed response from a
   local coding model without a terminal [finish_reason].  Pi retries a turn
   internally, but ultimately exits non-zero and Cabal would otherwise turn a
   transient transport defect into a permanent CWR block.  Retrying the whole
   one-shot Pi invocation once is safe at this boundary: the agent works in
   the same directory, so any completed edit is visible to the retry, and CWR
   still owns the final schema check and all verification gates. *)
let pi_stream_ended_without_finish_reason (result : task_result) =
  match result.status with
  | Failed _ ->
      let needle = "stream ended without finish_reason" in
      let contains haystack =
        let haystack = String.lowercase_ascii haystack in
        let needle = String.lowercase_ascii needle in
        let hlen = String.length haystack and nlen = String.length needle in
        let rec loop i =
          if i + nlen > hlen then false
          else if String.sub haystack i nlen = needle then true
          else loop (i + 1)
        in
        loop 0
      in
      contains result.stdout || contains result.stderr
  | Success | Timeout | Cancelled -> false

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
      let cmd = Option.value ~default:cfg.invocation_command (binary_name cfg) in
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
      let args = invocation_argv cfg in
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
      let run_once () =
        Backend_process.run_task_with
          ~sw
          ~env
          ~spec:{spec with timeout = cfg.timeout_seconds}
          ~build_command
          ?parse_stdout:parse_stdout_for_id
          ?parse_session_id:(if cfg.name = "pi" then Some parse_pi_session_id else None)
          ()
      in
      let first = run_once () in
      if cfg.name = "pi" && pi_stream_ended_without_finish_reason first then
        run_once ()
      else first
  end in
  let backend = (module M : Agentic_backend.S) in
  backend_configs :=
    (backend, cfg)
    :: List.filter
         (fun (_, existing) -> existing.name <> cfg.name)
         !backend_configs ;
  backend

let config_of backend =
  List.find_map
    (fun (candidate, config) -> if candidate == backend then Some config else None)
    !backend_configs
