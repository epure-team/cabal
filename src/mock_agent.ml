(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Mock agent backend for integration tests — deterministic, no LLM calls. *)

let fixtures_env_var = "CABAL_MOCK_AGENT_FIXTURES"

let legacy_fixtures_env_var = "EPURE_MOCK_AGENT_FIXTURES"

let id = "mock-agent"

let name = "Mock Agent (integration tests only)"

(* mock-agent has no real model selection; expose an empty list so callers
   default to whatever upstream selector they normally use. *)
let models : string list = []

let resolve_fixtures_env () =
  match Sys.getenv_opt fixtures_env_var with
  | Some _ as v -> v
  | None -> Sys.getenv_opt legacy_fixtures_env_var

let available ~sw:_ ~env:_ =
  match resolve_fixtures_env () with
  | Some path when path <> "" -> Sys.file_exists path
  | _ -> false

let supports_session_resume = false

let is_resume_failure (_result : Backend_types.task_result) = false

let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
  Agentic_backend.Config_check_unsupported
    "mock-agent has no project-owned config surface to validate"

(* ── Fixture loading ─────────────────────────────────────────────────── *)

(* limit=0 means unlimited; limit>0 caps the number of times this rule fires.
   used tracks call count within the current process and is never persisted. *)
type rule = {
  contains : string;
  stdout : string;
  status : [`Success | `Failed];
  limit : int;
  mutable used : int;
}

(* Rules are cached per fixture-file path so that `used` counters persist
   across multiple run_task calls within a single host subprocess.  Each
   fresh subprocess starts with an empty cache. *)
let rules_cache : (string, (rule list, string) result) Hashtbl.t =
  Hashtbl.create 1

let parse_rule = function
  | `Assoc r ->
      let get_string key =
        match List.assoc_opt key r with
        | Some (`String s) -> Ok s
        | Some _ -> Error (Printf.sprintf "field %S must be a string" key)
        | None -> Error (Printf.sprintf "missing required field %S" key)
      in
      let get_limit () =
        match List.assoc_opt "limit" r with
        | None -> Ok 0
        | Some (`Int n) when n >= 0 -> Ok n
        | Some (`Int _) -> Error "field \"limit\" must be >= 0"
        | Some _ -> Error "field \"limit\" must be an integer"
      in
      let get_status () =
        match List.assoc_opt "status" r with
        | Some (`String "success") -> Ok `Success
        | Some (`String "failed") -> Ok `Failed
        | Some (`String other) ->
            Error
              (Printf.sprintf
                 "invalid status field %S in fixture (must be \"success\" or \
                  \"failed\")"
                 other)
        | Some _ -> Error "field \"status\" must be a string"
        | None -> Error "missing required field \"status\""
      in
      let ( let* ) = Result.bind in
      let* contains = get_string "contains" in
      let* stdout = get_string "stdout" in
      let* status = get_status () in
      let* limit = get_limit () in
      Ok {contains; stdout; status; limit; used = 0}
  | _ -> Error "each rule must be an object"

let parse_rules json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "rules" fields with
      | Some (`List items) ->
          let rec loop idx acc = function
            | [] -> Ok (List.rev acc)
            | item :: rest -> (
                match parse_rule item with
                | Ok rule -> loop (idx + 1) (rule :: acc) rest
                | Error msg ->
                    Error (Printf.sprintf "rule %d: %s" (idx + 1) msg))
          in
          loop 0 [] items
      | Some _ -> Error "field \"rules\" must be a list"
      | None -> Error "missing required field \"rules\"")
  | _ -> Error "fixture file must be a JSON object"

let load_rules ~env path =
  try
    let content = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) in
    parse_rules (Yojson.Safe.from_string content)
  with exn ->
    Error
      (Printf.sprintf
         "failed to load fixture file '%s': %s"
         path
         (Printexc.to_string exn))

(* ── Matching ────────────────────────────────────────────────────────── *)

let prompt_contains_ci ~needle ~haystack =
  let n = String.lowercase_ascii needle in
  let h = String.lowercase_ascii haystack in
  let ln = String.length n and lh = String.length h in
  if ln = 0 then true
  else if lh < ln then false
  else
    let rec check i =
      if i > lh - ln then false
      else if String.sub h i ln = n then true
      else check (i + 1)
    in
    check 0

(* ── Backend implementation ──────────────────────────────────────────── *)

let run_task ~sw:_ ~env ?on_raw_line:_ (spec : Backend_types.task_spec) =
  match resolve_fixtures_env () with
  | None | Some "" ->
      Backend_types.make_task_result
        ~status:
          (Backend_types.Failed
             (Printf.sprintf
                "mock-agent: %s is not set; cannot match request"
                fixtures_env_var))
        ()
  | Some path -> (
      let rules =
        match Hashtbl.find_opt rules_cache path with
        | Some r -> r
        | None ->
            let r = load_rules ~env path in
            Hashtbl.add rules_cache path r ;
            r
      in
      match rules with
      | Error msg ->
          Backend_types.make_task_result
            ~status:(Backend_types.Failed (Printf.sprintf "mock-agent: %s" msg))
            ()
      | Ok rules -> (
          let is_available r = r.limit = 0 || r.used < r.limit in
          let matched =
            List.find_opt
              (fun r ->
                is_available r
                && prompt_contains_ci ~needle:r.contains ~haystack:spec.prompt)
              rules
          in
          match matched with
          | None ->
              let preview =
                let n = min 120 (String.length spec.prompt) in
                String.sub spec.prompt 0 n
              in
              Backend_types.make_task_result
                ~status:
                  (Backend_types.Failed
                     (Printf.sprintf
                        "mock-agent: no rule matched prompt (len=%d, \
                         preview='%s'). Review fixture file: %s"
                        (String.length spec.prompt)
                        preview
                        path))
                ()
          | Some rule ->
              rule.used <- rule.used + 1 ;
              let status =
                match rule.status with
                | `Success -> Backend_types.Success
                | `Failed -> Backend_types.Failed "mock-agent: scripted failure"
              in
              Backend_types.make_task_result
                ~status
                ~stdout:rule.stdout
                ~agent_text:rule.stdout
                ()))
