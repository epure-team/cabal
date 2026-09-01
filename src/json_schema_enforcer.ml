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

let bounded_stderr result =
  match String.trim result.Backend_types.stderr with
  | "" -> ""
  | stderr ->
      let cap = 2000 in
      let stderr =
        if String.length stderr <= cap then stderr
        else String.sub stderr 0 cap ^ "… (truncated)"
      in
      "\nbackend stderr: " ^ stderr

let render_error = function
  | Backend_types.Native_backend_failure_with_schema {execution; message} ->
      "native-backend call failed with a schema in force: " ^ message
      ^ bounded_stderr execution.Backend_types.final_result
  | Backend_types.Schema_retry_failed
      {attempt_1_validation_error; attempt_2_failure; _} ->
      let attempt_2 =
        match attempt_2_failure with
        | Backend_types.Schema_validation_failure message -> message
        | Backend_types.Transport_failure status
        | Backend_types.Resume_failure status ->
            backend_status_error_text status
      in
      "Both schema enforcement attempts failed.\nAttempt 1: "
      ^ attempt_1_validation_error ^ "\nAttempt 2: " ^ attempt_2

let attempt_outcome_of_status = function
  | Backend_types.Success -> Task_event.Attempt_succeeded
  | Backend_types.Failed _ -> Task_event.Attempt_failed
  | Backend_types.Timeout -> Task_event.Attempt_timed_out
  | Backend_types.Cancelled -> Task_event.Attempt_cancelled

let finish_attempt context outcome =
  Option.iter
    (fun value -> Task_execution_context.finish_attempt value outcome)
    context

let seconds_of_span span = Mtime.Span.to_float_ns span /. 1_000_000_000.0

let elapsed_since clock started_at =
  seconds_of_span (Mtime.span started_at (Eio.Time.Mono.now clock))

let run_backend ~sw ~env ~clock ?context ?on_raw_line backend spec =
  let started_at = Eio.Time.Mono.now clock in
  try
    let result =
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
    in
    finish_attempt context (attempt_outcome_of_status result.Backend_types.status) ;
    (result, elapsed_since clock started_at)
  with
  | Eio.Cancel.Cancelled _ as cancellation ->
      let outcome =
        match context with
        | Some value when Task_execution_context.deadline_expired value ->
            Task_event.Attempt_timed_out
        | Some _ | None -> Task_event.Attempt_cancelled
      in
      finish_attempt context outcome ;
      raise cancellation
  | (Out_of_memory | Stack_overflow | Sys.Break) as fatal ->
      finish_attempt context Task_event.Attempt_failed ;
      raise fatal
  | error ->
      finish_attempt context Task_event.Attempt_failed ;
      raise error

let delivery_for kind spec =
  let attachment_delivery =
    match kind with
    | Backend_types.Initial_attempt | Backend_types.Fresh_attempt ->
        Backend_types.Upload_attachments
    | Backend_types.Resumed_attempt ->
        Backend_types.Reuse_session_attachments
  in
  {
    Backend_types.attachment_references = spec.Backend_types.attachments;
    attachment_delivery;
    web_access_policy = spec.Backend_types.web_access;
  }

let prepare_delivery context kind spec =
  let delivery = delivery_for kind spec in
  Option.iter
    (fun value ->
      Task_execution_context.Private.set_requested_delivery value delivery)
    context ;
  delivery

let make_attempt ~number ~kind ~delivery ~result ~attempt_elapsed
    ?schema_validation_error () =
  {
    Backend_types.number;
    kind;
    result;
    attempt_elapsed;
    schema_validation_error;
    delivery;
  }

let nonblank_session_id = function
  | Some session_id -> (
      match String.trim session_id with "" -> None | trimmed -> Some trimmed)
  | None -> None

let final_session_id attempts =
  List.fold_left
    (fun selected (attempt : Backend_types.task_attempt) ->
      match nonblank_session_id attempt.result.session_id with
      | Some session_id -> Some session_id
      | None -> selected)
    None
    attempts

let make_execution ~clock ~started_at ~attempts ~final_result =
  {
    Backend_types.final_result;
    attempts;
    total_elapsed = elapsed_since clock started_at;
    total_cost =
      Backend_types.aggregate_costs
        (List.map
           (fun (attempt : Backend_types.task_attempt) -> attempt.result.cost)
           attempts);
    final_session_id = final_session_id attempts;
  }

let timeout_before_retry ~clock ~started_at attempts =
  let final_result =
    Backend_types.make_task_result ~status:Backend_types.Timeout ()
  in
  Ok (make_execution ~clock ~started_at ~attempts ~final_result)

let remaining_retry_spec context spec =
  match context with
  | None -> Some spec
  | Some value -> (
      match Task_execution_context.remaining_time value with
      | Some remaining when remaining <= 0.0 -> None
      | Some remaining -> Some {spec with Backend_types.timeout = remaining}
      | None -> Some spec)

let warn_resume_classifier_failure () =
  try
    Diagnostics.warn
      "resume failure classifier raised; treating the completed backend result \
       as a transport failure"
  with
  | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
  | (Out_of_memory | Stack_overflow | Sys.Break) as fatal -> raise fatal
  | _ -> ()

let is_resume_failure backend result =
  try Agentic_backend.is_resume_failure backend result with
  | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
  | (Out_of_memory | Stack_overflow | Sys.Break) as fatal -> raise fatal
  | _ ->
      warn_resume_classifier_failure () ;
      false

let run_task_detailed ~sw ~env ?context ?on_raw_line ~backend spec =
  let clock = Eio.Stdenv.mono_clock env in
  let started_at = Eio.Time.Mono.now clock in
  Option.iter
    (fun value ->
      Task_execution_context.begin_attempt value Task_event.Initial_attempt)
    context ;
  match spec.Backend_types.json_schema with
  | None ->
      let delivery =
        prepare_delivery context Backend_types.Initial_attempt spec
      in
      let result, attempt_elapsed =
        run_backend ~sw ~env ~clock ?context ?on_raw_line backend spec
      in
      let attempt =
        make_attempt
          ~number:1
          ~kind:Backend_types.Initial_attempt
          ~delivery
          ~result
          ~attempt_elapsed
          ()
      in
      Ok
        (make_execution
           ~clock
           ~started_at
           ~attempts:[attempt]
           ~final_result:result)
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
        let delivery =
          prepare_delivery context Backend_types.Initial_attempt spec
        in
        let result, attempt_elapsed =
          run_backend ~sw ~env ~clock ?context ?on_raw_line backend spec
        in
        let attempt =
          make_attempt
            ~number:1
            ~kind:Backend_types.Initial_attempt
            ~delivery
            ~result
            ~attempt_elapsed
            ()
        in
        let execution =
          make_execution
            ~clock
            ~started_at
            ~attempts:[attempt]
            ~final_result:result
        in
        match result.Backend_types.status with
        | Backend_types.Failed msg ->
            Error
              (Backend_types.Native_backend_failure_with_schema
                 {execution; message = msg})
        | Backend_types.Success | Backend_types.Timeout
        | Backend_types.Cancelled ->
            Ok execution
      else
        (* Validate-and-retry path (Story #624): run the task, validate
           agent_text, and make at most one corrective re-invocation on failure.
           Hard cap of two backend calls per run_task invocation. *)
        let schema_json = Yojson.Safe.to_string ~std:true schema in
        let delivery1 =
          prepare_delivery context Backend_types.Initial_attempt spec
        in
        let result1, attempt_elapsed1 =
          run_backend ~sw ~env ~clock ?context ?on_raw_line backend spec
        in
        (* Schema validation only makes sense for successful invocations.
           Propagate Failed/Timeout/Cancelled results directly so callers see
           the real backend error rather than a spurious "not valid JSON"
           schema-compliance failure. *)
        match result1.Backend_types.status with
        | Failed _ | Timeout | Cancelled ->
            let attempt1 =
              make_attempt
                ~number:1
                ~kind:Backend_types.Initial_attempt
                ~delivery:delivery1
                ~result:result1
                ~attempt_elapsed:attempt_elapsed1
                ()
            in
            Ok
              (make_execution
                 ~clock
                 ~started_at
                 ~attempts:[attempt1]
                 ~final_result:result1)
        | Success -> (
            let agent_text1 = result1.Backend_types.agent_text in
            match
              Json_schema_validator.validate ~schema ~document:agent_text1
            with
            | Ok () ->
                let attempt1 =
                  make_attempt
                    ~number:1
                    ~kind:Backend_types.Initial_attempt
                    ~delivery:delivery1
                    ~result:result1
                    ~attempt_elapsed:attempt_elapsed1
                    ()
                in
                Ok
                  (make_execution
                     ~clock
                     ~started_at
                     ~attempts:[attempt1]
                     ~final_result:result1)
            | Error err1 -> (
                let attempt1 =
                  make_attempt
                    ~number:1
                    ~kind:Backend_types.Initial_attempt
                    ~delivery:delivery1
                    ~result:result1
                    ~attempt_elapsed:attempt_elapsed1
                    ~schema_validation_error:err1
                    ()
                in
                let retry_spec, retry_kind, attempt_kind =
                  if Agentic_backend.supports_session_resume backend then
                    match
                      nonblank_session_id result1.Backend_types.session_id
                    with
                    | Some sid ->
                        ( make_resume_retry_spec
                            ~base:spec
                            ~session_id:sid
                            ~schema_json
                            ~err:err1,
                          Task_event.Resume_retry,
                          Backend_types.Resumed_attempt )
                    | None ->
                        ( make_fresh_retry_spec ~base:spec ~schema_json ~err:err1,
                          Task_event.Fresh_retry,
                          Backend_types.Fresh_attempt )
                  else
                    ( make_fresh_retry_spec ~base:spec ~schema_json ~err:err1,
                      Task_event.Fresh_retry,
                      Backend_types.Fresh_attempt )
                in
                (* This scheduling checkpoint lets cancellation requested after
                   attempt 1 finish before any retry transition is announced. *)
                Eio.Fiber.yield () ;
                match remaining_retry_spec context retry_spec with
                | None -> timeout_before_retry ~clock ~started_at [attempt1]
                | Some retry_spec ->
                    Option.iter
                      (fun value ->
                        Task_execution_context.transition_to_retry
                          value
                          ~kind:retry_kind
                          ~reason:err1)
                      context ;
                    let delivery2 =
                      prepare_delivery context attempt_kind retry_spec
                    in
                    let result2, attempt_elapsed2 =
                      run_backend
                        ~sw
                        ~env
                        ~clock
                        ?context
                        ?on_raw_line
                        backend
                        retry_spec
                    in
                    let attempt2 ?schema_validation_error () =
                      make_attempt
                        ~number:2
                        ~kind:attempt_kind
                        ~delivery:delivery2
                        ~result:result2
                        ~attempt_elapsed:attempt_elapsed2
                        ?schema_validation_error
                        ()
                    in
                    match result2.Backend_types.status with
                    | Backend_types.Failed _ | Backend_types.Timeout
                    | Backend_types.Cancelled ->
                        let attempts = [attempt1; attempt2 ()] in
                        let attempt_2_failure =
                          if
                            attempt_kind = Backend_types.Resumed_attempt
                            && is_resume_failure backend result2
                          then Backend_types.Resume_failure result2.status
                          else Backend_types.Transport_failure result2.status
                        in
                        let execution =
                          make_execution
                            ~clock
                            ~started_at
                            ~attempts
                            ~final_result:result2
                        in
                        Error
                          (Backend_types.Schema_retry_failed
                             {
                               execution;
                               attempt_1_validation_error = err1;
                               attempt_2_failure;
                             })
                    | Backend_types.Success -> (
                        let agent_text2 = result2.Backend_types.agent_text in
                        match
                          Json_schema_validator.validate
                            ~schema
                            ~document:agent_text2
                        with
                        | Ok () ->
                            let attempts = [attempt1; attempt2 ()] in
                            Ok
                              (make_execution
                                 ~clock
                                 ~started_at
                                 ~attempts
                                 ~final_result:result2)
                        | Error err2 ->
                            let attempts =
                              [
                                attempt1;
                                attempt2 ~schema_validation_error:err2 ();
                              ]
                            in
                            let execution =
                              make_execution
                                ~clock
                                ~started_at
                                ~attempts
                                ~final_result:result2
                            in
                            Error
                              (Backend_types.Schema_retry_failed
                                 {
                                   execution;
                                   attempt_1_validation_error = err1;
                                   attempt_2_failure =
                                     Backend_types.Schema_validation_failure
                                       err2;
                                 })))))

let run_task ~sw ~env ?context ?on_raw_line ~backend spec =
  match
    run_task_detailed ~sw ~env ?context ?on_raw_line ~backend spec
  with
  | Ok execution -> Ok execution.Backend_types.final_result
  | Error
      ((Backend_types.Schema_retry_failed
         {execution; attempt_2_failure; _}) as error) -> (
      match context, attempt_2_failure with
      | Some _, Backend_types.Transport_failure Backend_types.Timeout
      | Some _, Backend_types.Transport_failure Backend_types.Cancelled
      | Some _, Backend_types.Resume_failure Backend_types.Timeout
      | Some _, Backend_types.Resume_failure Backend_types.Cancelled ->
          Ok execution.Backend_types.final_result
      | None, _ | Some _, _ -> Error (render_error error))
  | Error error -> Error (render_error error)
