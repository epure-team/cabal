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
  Json_schema_prompt.block schema_json ^ compliance_suffix err

let build_fresh_prompt ~original_prompt ~schema_json ~err =
  original_prompt ^ "\n\n"
  ^ Json_schema_prompt.block schema_json
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

(* A backend's [native_json_schema_output] says it HAS a native schema flag,
   not that every schema fits it.  Codex forwards [--output-schema] to OpenAI
   strict structured outputs, which rejects (HTTP 400, exit 1) any schema
   outside the strict subset; such a schema must take validate-and-retry
   instead of failing every call.  Other native backends accept any schema. *)
let native_flag_rejection ~backend schema =
  if String.equal (Agentic_backend.id backend) Codex_cli.id then
    match Openai_strict_schema.check schema with
    | Ok () -> None
    | Error reason -> Some reason
  else None

let native_path_applies ~backend schema =
  Agentic_backend.native_json_schema_output backend
  && Option.is_none (native_flag_rejection ~backend schema)

let run_task ~sw ~env ?on_raw_line ~backend spec =
  match spec.Backend_types.json_schema with
  | None -> Ok (Agentic_backend.run_task ~sw ~env ?on_raw_line backend spec)
  | Some schema -> (
      if native_path_applies ~backend schema then
        (* Native path (Story #625): the schema is in spec.json_schema and the
           backend's run_task wires it to the CLI flag (e.g. --output-schema).
           The validate-and-retry loop is NOT executed on this path.
           Any non-success result is returned unchanged — no fallback, no
           retry (D-5). What a Failed result WAS is not asserted: this path is
           reached for any non-zero exit while a schema was in force, which
           includes a rejected schema but equally a rate limit, a network
           failure, a bad flag, or the process being killed.  Returning the
           task result preserves that distinction for callers instead of
           turning an operational backend failure into a schema error. *)
        let result =
          Agentic_backend.run_task ~sw ~env ?on_raw_line backend spec
        in
        Ok result
      else
        (* Validate-and-retry path (Story #624).  Also taken by a native
           backend whose native flag cannot carry this schema (see
           [native_path_applies]); [spec] keeps [json_schema] so that backend
           can still present the schema in its prompt. *)
        let () =
          match native_flag_rejection ~backend schema with
          | Some reason when Agentic_backend.native_json_schema_output backend
            ->
              Diagnostics.debug
                "[schema] %s: native schema flag cannot carry this schema \
                 (%s); using validate-and-retry"
                (Agentic_backend.id backend)
                reason
          | Some _ | None -> ()
        in
        (* Validate-and-retry path (Story #624): run the task, validate
           agent_text, and make at most one corrective re-invocation on failure.
           Hard cap of two backend calls per run_task invocation. *)
        let schema_json = Yojson.Safe.to_string ~std:true schema in
        let result1 =
          Agentic_backend.run_task ~sw ~env ?on_raw_line backend spec
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
                let retry_spec =
                  if Agentic_backend.supports_session_resume backend then
                    match result1.Backend_types.session_id with
                    | Some sid ->
                        make_resume_retry_spec
                          ~base:spec
                          ~session_id:sid
                          ~schema_json
                          ~err:err1
                    | None ->
                        make_fresh_retry_spec ~base:spec ~schema_json ~err:err1
                  else make_fresh_retry_spec ~base:spec ~schema_json ~err:err1
                in
                let result2 =
                  Agentic_backend.run_task
                    ~sw
                    ~env
                    ?on_raw_line
                    backend
                    retry_spec
                in
                match result2.Backend_types.status with
                | Backend_types.Failed _ | Backend_types.Timeout
                | Backend_types.Cancelled ->
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
