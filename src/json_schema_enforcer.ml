(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** JSON Schema enforcement — validate-and-retry wrapper. *)

(* Template strings exported for inspection.  Placeholders are written as
   {schema}, {error}, and {original_prompt} for readability. The actual
   prompt building uses direct string concatenation below. *)

let resume_retry_template =
  "## Required output schema\n\n\
   {schema}\n\n\
   Your previous response did not conform to the required JSON schema.\n\
   Validation error: {error}\n\
   Please produce a response that is valid JSON conforming exactly to the \
   schema shown under \"## Required output schema\" above."

let fresh_retry_template =
  "{original_prompt}\n\n\
   ## Required output schema\n\n\
   {schema}\n\n\
   Your previous response did not conform to the required JSON schema.\n\
   Validation error: {error}\n\
   Please produce a response that is valid JSON conforming exactly to the \
   schema shown under \"## Required output schema\" above."

let compliance_suffix err =
  "\n\n\
   Your previous response did not conform to the required JSON schema.\n\
   Validation error: " ^ err
  ^ "\n\
     Please produce a response that is valid JSON conforming exactly to the \
     schema shown under \"## Required output schema\" above."

let build_resume_prompt ~schema_json ~err =
  "## Required output schema\n\n" ^ schema_json ^ compliance_suffix err

let build_fresh_prompt ~original_prompt ~schema_json ~err =
  original_prompt ^ "\n\n## Required output schema\n\n" ^ schema_json
  ^ compliance_suffix err

let make_resume_retry_spec ~base ~session_id ~schema_json ~err =
  let resume =
    Backend_types.make_resume_task_spec ~base ~resume_session_id:session_id ()
  in
  let prompt = build_resume_prompt ~schema_json ~err in
  {resume with Backend_types.prompt; json_schema = None}

let make_fresh_retry_spec ~base ~schema_json ~err =
  let prompt =
    build_fresh_prompt
      ~original_prompt:base.Backend_types.prompt
      ~schema_json
      ~err
  in
  {base with Backend_types.prompt; resume_session_id = None; json_schema = None}

let backend_status_error_text = function
  | Backend_types.Failed msg -> "Failed: " ^ msg
  | Backend_types.Timeout -> "Timeout"
  | Backend_types.Cancelled -> "Cancelled"
  | Backend_types.Success -> "Success"

let both_attempts_error ~attempt1 ~attempt2 =
  Error
    ("Both schema enforcement attempts failed.\nAttempt 1: " ^ attempt1
   ^ "\nAttempt 2: " ^ attempt2)

let run_backend ~sw ~env ?context ?on_raw_line backend spec =
  match context with
  | None -> Agentic_backend.run_task ~sw ~env ?on_raw_line backend spec
  | Some context ->
      Agentic_backend.run_task_with_context
        ~sw
        ~env
        ~context
        ?on_raw_line
        backend
        spec

let run_task ~sw ~env ?context ?on_raw_line ~backend spec =
  Option.iter
    (fun value ->
      Task_execution_context.begin_attempt value Task_event.Initial_attempt)
    context ;
  match spec.Backend_types.json_schema with
  | None -> Ok (run_backend ~sw ~env ?context ?on_raw_line backend spec)
  | Some schema -> (
      if Agentic_backend.native_json_schema_output backend then
        (* Native path (Story #625): the schema is in spec.json_schema and the
           backend's run_task wires it to the CLI flag (e.g. --output-schema).
           The validate-and-retry loop is NOT executed on this path.
           Any Failed result is returned as Error immediately — no fallback,
           no retry (D-5). What the failure WAS is not asserted: this path is
           reached for any non-zero exit while a schema was in force, which
           includes a rejected schema but equally a rate limit, a network
           failure, a bad flag, or the process being killed. *)
        let result =
          run_backend ~sw ~env ?context ?on_raw_line backend spec
        in
        match result.Backend_types.status with
        | Backend_types.Failed msg ->
            (* The backend's own stderr is the only thing here that says what
               actually went wrong, and it was already on the result -- unused.
               Two separate diagnoses of a live failure were spent chasing the
               schema, because the message named the schema, while the schema
               replayed clean standalone. Bounded because a backend can emit an
               arbitrary amount on the way down. *)
            let detail =
              match String.trim result.Backend_types.stderr with
              | "" -> ""
              | stderr ->
                  let cap = 2000 in
                  let stderr =
                    if String.length stderr <= cap then stderr
                    else String.sub stderr 0 cap ^ "… (truncated)"
                  in
                  "\nbackend stderr: " ^ stderr
            in
            Error
              ("native-backend call failed with a schema in force: " ^ msg
             ^ detail)
        | Backend_types.Success | Backend_types.Timeout
        | Backend_types.Cancelled ->
            Ok result
      else
        (* Validate-and-retry path (Story #624): run the task, validate
           agent_text, and make at most one corrective re-invocation on failure.
           Hard cap of two backend calls per run_task invocation. *)
        let schema_json = Yojson.Safe.to_string ~std:true schema in
        let result1 =
          run_backend ~sw ~env ?context ?on_raw_line backend spec
        in
        (* Schema validation only makes sense for successful invocations.
           Propagate Failed/Timeout/Cancelled results directly so callers see
           the real backend error rather than a spurious "not valid JSON"
           schema-compliance failure. *)
        match result1.Backend_types.status with
        | Failed _ | Timeout | Cancelled -> Ok result1
        | Success -> (
            let agent_text1 = result1.Backend_types.agent_text in
            match
              Json_schema_validator.validate ~schema ~document:agent_text1
            with
            | Ok () -> Ok result1
            | Error err1 -> (
                let retry_spec, retry_kind =
                  if Agentic_backend.supports_session_resume backend then
                    match result1.Backend_types.session_id with
                    | Some sid ->
                        ( make_resume_retry_spec
                            ~base:spec
                            ~session_id:sid
                            ~schema_json
                            ~err:err1,
                          Task_event.Resume_retry )
                    | None ->
                        ( make_fresh_retry_spec ~base:spec ~schema_json ~err:err1,
                          Task_event.Fresh_retry )
                  else
                    ( make_fresh_retry_spec ~base:spec ~schema_json ~err:err1,
                      Task_event.Fresh_retry )
                in
                Option.iter
                  (fun value ->
                    Task_execution_context.transition_to_retry
                      value
                      ~kind:retry_kind
                      ~reason:err1)
                  context ;
                let deadline_expired =
                  match context with
                  | Some value -> Task_execution_context.deadline_expired value
                  | None -> false
                in
                if deadline_expired then
                  Ok
                    (Backend_types.make_task_result
                       ~status:Backend_types.Timeout
                       ())
                else
                let result2 =
                  run_backend
                    ~sw
                    ~env
                    ?context
                    ?on_raw_line
                    backend
                    retry_spec
                in
                match result2.Backend_types.status with
                | Backend_types.Timeout | Backend_types.Cancelled -> (
                    match context with
                    | Some _ -> Ok result2
                    | None ->
                        both_attempts_error
                          ~attempt1:err1
                          ~attempt2:
                            (backend_status_error_text result2.Backend_types.status))
                | Backend_types.Failed _ ->
                    both_attempts_error
                      ~attempt1:err1
                      ~attempt2:
                        (backend_status_error_text result2.Backend_types.status)
                | Backend_types.Success -> (
                    let agent_text2 = result2.Backend_types.agent_text in
                    match
                      Json_schema_validator.validate
                        ~schema
                        ~document:agent_text2
                    with
                    | Ok () -> Ok result2
                    | Error err2 ->
                        both_attempts_error ~attempt1:err1 ~attempt2:err2))))
