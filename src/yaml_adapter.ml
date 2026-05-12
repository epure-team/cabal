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
}

(* Maps backend id → config for config_of lookup. *)
let config_table : (string, config) Hashtbl.t = Hashtbl.create 8

let make_backend (cfg : config) : Agentic_backend.t =
  let module M = struct
    let id = cfg.name

    let name = cfg.display_name

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

    let is_resume_failure (_result : task_result) = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported
        (Printf.sprintf
           "YAML adapter '%s' from %s does not define a project config \
            validator"
           cfg.name
           cfg.source)

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
      let build_command ~mcp_config_path:_ _s = (args, full_prompt) in
      Backend_process.run_task_with
        ~sw
        ~env
        ~spec:{spec with timeout = cfg.timeout_seconds}
        ~build_command
        ()
  end in
  Hashtbl.replace config_table cfg.name cfg ;
  (module M : Agentic_backend.S)

let config_of backend =
  let id = Agentic_backend.id backend in
  Hashtbl.find_opt config_table id
