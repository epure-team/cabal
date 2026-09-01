(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type error =
  | Invalid_timeout
  | Backend_not_registered
  | Runtime_registration_untrusted
  | Preflight_failed of Task_preflight.error
  | Backend_version_unsupported
  | Version_check_failed
  | Backend_unavailable
  | Availability_check_failed
  | Backend_execution_failed
  | Schema_enforcement_failed of string

let render_error = function
  | Invalid_timeout -> "task timeout must be non-negative and not NaN"
  | Backend_not_registered -> "requested backend is not registered at runtime"
  | Runtime_registration_untrusted ->
      "requested backend is raw-registered and not trusted for central dispatch"
  | Preflight_failed error -> Task_preflight.render_error error
  | Backend_version_unsupported ->
      "installed backend version does not satisfy the required stable baseline"
  | Version_check_failed -> "backend version check failed"
  | Backend_unavailable -> "requested backend is not available"
  | Availability_check_failed -> "backend availability check failed"
  | Backend_execution_failed -> "backend execution failed"
  | Schema_enforcement_failed message ->
      "backend schema enforcement failed: "
      ^ Backend_event_redaction.redact_error_message message

let ( let* ) result continuation = Result.bind result continuation

let protect error f =
  try Ok (f ()) with
  | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
  | (Out_of_memory | Stack_overflow | Sys.Break) as fatal -> raise fatal
  | _ -> Error error

let check_version ~env descriptor =
  let command = [descriptor.Backend_registry.binary_name; "--version"] in
  let* probe =
    protect Version_check_failed (fun () ->
        Backend_process.probe_version_command ~env command)
  in
  let* () =
    match probe.Backend_process.output with
    | None -> Ok ()
    | Some output -> (
        match Backend_version.parse_from_output output with
        | Error _ -> Ok ()
        | Ok installed -> (
            match Backend_version.check_gate ~descriptor ~installed with
            | Ok () -> Ok ()
            | Error _ -> Error Backend_version_unsupported))
  in
  Ok probe

let check_availability ~sw ~env ~backend ~origin ~version_probe =
  match origin, version_probe with
  | (Runtime_entry.Handwritten | Runtime_entry.Yaml), Some version_probe ->
      if version_probe.Backend_process.command_available then Ok ()
      else Error Backend_unavailable
  | (Runtime_entry.Handwritten | Runtime_entry.Yaml | Runtime_entry.Custom), _ ->
      let* available =
        protect Availability_check_failed (fun () ->
            Agentic_backend.available ~sw ~env backend)
      in
      if available then Ok () else Error Backend_unavailable

type prepared = {
  backend : Agentic_backend.t;
  spec : Backend_types.task_spec;
}

let emit context payload =
  Option.iter (fun value -> Task_execution_context.emit value payload) context

let prepare ~sw ~env ~limits ~backend_id ?context spec =
  (* Give cancellation requested immediately after [start_task] a deterministic
     checkpoint before registry resolution or any preflight side effect. *)
  Eio.Fiber.yield () ;
  let* entry =
    match Registry.find_entry backend_id with
    | Some (Registry.Validated entry) -> Ok entry
    | Some (Registry.Raw _) -> Error Runtime_registration_untrusted
    | None -> Error Backend_not_registered
  in
  let backend = entry.Runtime_entry.backend in
  let descriptor = entry.effective_descriptor in
  emit context (Task_event.Backend_selected {backend_id}) ;
  Eio.Fiber.yield () ;
  emit context Task_event.Preflight_started ;
  let* () =
    match Task_preflight.validate_inputs ~limits spec with
    | Ok () -> Ok ()
    | Error error -> Error (Preflight_failed error)
  in
  let* () =
    match Task_preflight.validate_capabilities ~descriptor spec with
    | Ok () -> Ok ()
    | Error error -> Error (Preflight_failed error)
  in
  emit context Task_event.Preflight_completed ;
  Eio.Fiber.yield () ;
  let* version_probe =
    match entry.version_policy with
    | Runtime_entry.Enforce_baseline ->
        emit context Task_event.Version_probe_started ;
        let result = Result.map Option.some (check_version ~env descriptor) in
        if Result.is_ok result then
          emit context Task_event.Version_probe_completed ;
        result
    | Runtime_entry.No_version_gate -> Ok None
  in
  Eio.Fiber.yield () ;
  emit context Task_event.Availability_check_started ;
  let* () =
    check_availability ~sw ~env ~backend ~origin:entry.origin ~version_probe
  in
  emit context Task_event.Availability_check_completed ;
  Eio.Fiber.yield () ;
  Ok {backend; spec}

let execute_prepared ~sw ~env ?context ?on_raw_line prepared =
  let* execution =
    protect Backend_execution_failed (fun () ->
        Json_schema_enforcer.run_task
          ~sw
          ~env
          ?context
          ?on_raw_line
          ~backend:prepared.backend
          prepared.spec)
  in
  match execution with
  | Ok result -> Ok result
  | Error message -> Error (Schema_enforcement_failed message)

exception Task_cancelled

type outcome = (Backend_types.task_result, error) result

type task_handle = {
  cancellation : Eio.Cancel.t;
  outcome : outcome Eio.Promise.or_exn;
  delivery_complete : unit Eio.Promise.t;
  completed : bool Atomic.t;
}

let seconds_of_span span = Mtime.Span.to_float_ns span /. 1_000_000_000.0

let make_result status = Backend_types.make_task_result ~status ()

let emit_result_metadata context result =
  (match result.Backend_types.session_id with
  | Some session_id when not (Task_execution_context.session_id_emitted context) ->
      Task_execution_context.emit context (Task_event.Session_id session_id)
  | None | Some _ -> ()) ;
  if
    result.Backend_types.agent_text <> ""
    && not (Task_execution_context.agent_text_emitted context)
    &&
    (not (Task_execution_context.structured_text_claimed context)
    || Task_execution_context.final_public_text context)
  then
    Task_execution_context.emit
      context
      (Task_event.Agent_text_delta result.Backend_types.agent_text) ;
  match result.Backend_types.cost with
  | Some cost when not (Task_execution_context.token_usage_emitted context) ->
      Task_execution_context.emit context (Task_event.Token_usage cost)
  | None | Some _ -> ()

let emit_terminal sink context = function
  | Error error ->
      Task_event.emit_terminal sink (Task_event.Failed (render_error error))
  | Ok result ->
      emit_result_metadata context result ;
      let terminal =
        match result.Backend_types.status with
        | Backend_types.Success -> Task_event.Succeeded
        | Backend_types.Failed message -> Task_event.Failed message
        | Backend_types.Timeout -> Task_event.Timed_out
        | Backend_types.Cancelled -> Task_event.Cancelled
      in
      Task_event.emit_terminal sink terminal

let execute ~sw ~env ~limits ~backend_id ?on_raw_line spec sink deadline =
  let context =
    Task_execution_context.create
      ~remaining_time:(fun () -> Task_deadline.remaining deadline)
      sink
  in
  match
    Task_deadline.run deadline (fun () ->
        match prepare ~sw ~env ~limits ~backend_id ~context spec with
        | Error error -> Error error
        | Ok prepared ->
            execute_prepared ~sw ~env ~context ?on_raw_line prepared)
  with
  | `Completed result -> (result, context)
  | `Timeout -> (Ok (make_result Backend_types.Timeout), context)

let start_task ~sw ~env ~limits ~backend_id ?on_event ?on_raw_line spec =
  let outcome, resolve_outcome = Eio.Promise.create () in
  let ready, resolve_ready = Eio.Promise.create () in
  let completed = Atomic.make false in
  Eio.Fiber.fork ~sw (fun () ->
      let mono_clock = Eio.Stdenv.mono_clock env in
      let started_at = Eio.Time.Mono.now mono_clock in
      let sink =
        Task_event.create_sink
          ~sw
          ~now:(fun () ->
            seconds_of_span
              (Mtime.span started_at (Eio.Time.Mono.now mono_clock)))
          ?on_event
          ()
      in
      Task_event.emit sink Task_event.Task_started ;
      Eio.Cancel.sub (fun cancellation ->
          Eio.Promise.resolve
            resolve_ready
            (cancellation, Task_event.Private.delivery_complete sink) ;
          try
            let result, context =
              match Task_deadline.create mono_clock spec.Backend_types.timeout with
              | Error Task_deadline.Invalid_timeout ->
                  let context =
                    Task_execution_context.create
                      ~remaining_time:(fun () -> Some 0.0)
                      sink
                  in
                  (Error Invalid_timeout, context)
              | Ok deadline -> (
                  try
                    execute
                      ~sw
                      ~env
                      ~limits
                      ~backend_id
                      ?on_raw_line
                      spec
                      sink
                      deadline
                  with
                  | Eio.Cancel.Cancelled _ ->
                      let context =
                        Task_execution_context.create
                          ~remaining_time:(fun () ->
                            Task_deadline.remaining deadline)
                          sink
                      in
                      (Ok (make_result Backend_types.Cancelled), context)
                  | (Out_of_memory | Stack_overflow | Sys.Break) as fatal ->
                      raise fatal
                  | _ ->
                      let context =
                        Task_execution_context.create
                          ~remaining_time:(fun () ->
                            Task_deadline.remaining deadline)
                          sink
                      in
                      (Error Backend_execution_failed, context))
            in
            Eio.Cancel.protect (fun () ->
                emit_terminal sink context result ;
                Atomic.set completed true ;
                Eio.Promise.resolve_ok resolve_outcome result)
          with
          | (Out_of_memory | Stack_overflow | Sys.Break) as fatal ->
              Eio.Cancel.protect (fun () ->
                  (try
                     Task_event.emit_terminal
                       sink
                       (Task_event.Failed "backend execution failed")
                   with _ -> ()) ;
                  Atomic.set completed true ;
                  Eio.Promise.resolve_error resolve_outcome fatal))) ;
  let cancellation, delivery_complete = Eio.Promise.await ready in
  {cancellation; outcome; delivery_complete; completed}

let cancel handle =
  if not (Atomic.get handle.completed) then
    try Eio.Cancel.cancel handle.cancellation Task_cancelled
    with Invalid_argument _ -> ()

let await handle = Eio.Promise.await_exn handle.outcome

let await_event_delivery handle = Eio.Promise.await handle.delivery_complete

module Private = struct
  type nonrec task_handle = task_handle

  let start_task = start_task

  let cancel = cancel

  let await = await

  let await_event_delivery = await_event_delivery
end

let run_task ~sw ~env ~limits ~backend_id ?on_event ?on_raw_line spec =
  start_task
    ~sw
    ~env
    ~limits
    ~backend_id
    ?on_event
    ?on_raw_line
    spec
  |> await
