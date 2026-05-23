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

let run_task ~sw ~env ?on_raw_line ~backend spec =
  match spec.Backend_types.json_schema with
  | None -> Ok (Agentic_backend.run_task ~sw ~env ?on_raw_line backend spec)
  | Some schema -> (
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
      match Json_schema_validator.validate ~schema ~document:agent_text1 with
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
              | None -> make_fresh_retry_spec ~base:spec ~schema_json ~err:err1
            else make_fresh_retry_spec ~base:spec ~schema_json ~err:err1
          in
          let result2 =
            Agentic_backend.run_task ~sw ~env ?on_raw_line backend retry_spec
          in
          let agent_text2 = result2.Backend_types.agent_text in
          match
            Json_schema_validator.validate ~schema ~document:agent_text2
          with
          | Ok () -> Ok result2
          | Error err2 ->
              Error
                ("Both schema validation attempts failed.\nAttempt 1: " ^ err1
               ^ "\nAttempt 2: " ^ err2))))
