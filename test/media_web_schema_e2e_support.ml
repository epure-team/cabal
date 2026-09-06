(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Pure request planning and normalized-event assertions shared by CBL-08
    structural/live tests. *)

open Cabal

type protocol_requirements = {
  session : bool;
  usage : bool;
  exact_tools : (string * int) list;
}

type schema_execution = No_schema | Native_schema | Validate_and_retry

type media_task_plan = {
  schema_execution : schema_execution;
  spec : Backend_types.task_spec;
}

type trace_error =
  | Invalid_sequence
  | Invalid_terminal
  | Attempt_lifecycle_disagreement
  | Public_agent_output_missing
  | Public_agent_output_mismatch
  | Required_session_missing
  | Required_usage_missing
  | Required_tool_lifecycle_missing
  | Required_tool_lifecycle_mismatch
  | Tool_lifecycle_disagreement
  | Delivery_was_truncated

let protocol_requirements_for_backend = function
  | "codex" -> {session = true; usage = true; exact_tools = []}
  | "copilot-cli" ->
      (* Historical observation contract only: this is neither capability
         evidence nor an executable CBL-08 selection for quarantined Copilot. *)
      {session = true; usage = true; exact_tools = [("view", 2)]}
  | _ -> {session = false; usage = false; exact_tools = []}

let make_media_task_plan ~(descriptor : Backend_registry.descriptor) ~fixtures
    ~working_dir ~attachments ~model =
  let native = E2e_harness_config.valid_native_schema_descriptor descriptor in
  let schema_execution = if native then Native_schema else No_schema in
  let json_schema =
    if native then Some (Media_web_schema_fixture.schema fixtures) else None
  in
  let prompt =
    if native then Media_web_schema_fixture.prompt
    else Media_web_schema_fixture.prompt_without_native_schema fixtures
  in
  let spec =
    Backend_types.make_task_spec ~prompt ~working_dir ~timeout:180.0
      ~expected_outputs:[] ~attachments ~web_access:Backend_types.Web_disabled
      ~managed_namespace:E2e_harness_config.managed_namespace ?model
      ~read_only:descriptor.capabilities.read_only_support ?json_schema ()
  in
  {schema_execution; spec}

let outcome_of_status = function
  | Backend_types.Success -> Task_event.Attempt_succeeded
  | Backend_types.Failed _ -> Task_event.Attempt_failed
  | Backend_types.Timeout -> Task_event.Attempt_timed_out
  | Backend_types.Cancelled -> Task_event.Attempt_cancelled

let terminal_of_status = function
  | Backend_types.Success -> `Succeeded
  | Backend_types.Failed _ -> `Failed
  | Backend_types.Timeout -> `Timed_out
  | Backend_types.Cancelled -> `Cancelled

let same_terminal expected = function
  | Task_event.Succeeded -> expected = `Succeeded
  | Task_event.Failed _ -> expected = `Failed
  | Task_event.Timed_out -> expected = `Timed_out
  | Task_event.Cancelled -> expected = `Cancelled

let strictly_increasing_sequence events =
  let rec loop previous_seq previous_timestamp = function
    | [] -> true
    | event :: rest ->
        event.Task_event.seq > previous_seq
        && event.timestamp >= 0.0
        && event.timestamp >= previous_timestamp
        && loop event.seq event.timestamp rest
  in
  match events with
  | [] -> true
  | first :: rest ->
      first.Task_event.timestamp >= 0.0
      && loop first.Task_event.seq first.Task_event.timestamp rest

let terminal_is_exactly_last execution events =
  let terminals =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Terminal terminal -> Some terminal
        | _ -> None)
      events
  in
  match (terminals, List.rev events) with
  | [terminal], {Task_event.payload = Terminal final_terminal; _} :: _ ->
      terminal = final_terminal
      && same_terminal
           (terminal_of_status execution.Backend_types.final_result.status)
           terminal
  | _ -> false

let expected_attempt_lifecycle execution =
  List.map
    (fun (attempt : Backend_types.task_attempt) ->
      ( attempt.number,
        attempt.kind,
        outcome_of_status attempt.result.Backend_types.status ))
    execution.Backend_types.attempts

let actual_attempt_lifecycle events =
  let starts =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Attempt_started kind -> Some (event.attempt, kind)
        | _ -> None)
      events
  in
  let finishes =
    List.filter_map
      (fun event ->
        match event.Task_event.payload with
        | Task_event.Attempt_finished outcome -> Some (event.attempt, outcome)
        | _ -> None)
      events
  in
  (starts, finishes)

let attempt_lifecycle_agrees execution events =
  let expected = expected_attempt_lifecycle execution in
  let expected_starts =
    List.map (fun (number, kind, _) -> (number, kind)) expected
  in
  let expected_finishes =
    List.map (fun (number, _, outcome) -> (number, outcome)) expected
  in
  let actual_starts, actual_finishes = actual_attempt_lifecycle events in
  let well_ordered =
    List.for_all
      (fun (number, _, _) ->
        let start_index =
          List.find_index
            (fun event ->
              event.Task_event.attempt = number
              &&
              match event.Task_event.payload with
              | Task_event.Attempt_started _ -> true
              | _ -> false)
            events
        in
        let finish_index =
          List.find_index
            (fun event ->
              event.Task_event.attempt = number
              &&
              match event.Task_event.payload with
              | Task_event.Attempt_finished _ -> true
              | _ -> false)
            events
        in
        match (start_index, finish_index) with
        | Some started, Some finished -> started < finished
        | None, _ | _, None -> false)
      expected
  in
  expected_starts = actual_starts
  && expected_finishes = actual_finishes
  && well_ordered

let final_public_output execution events =
  match List.rev execution.Backend_types.attempts with
  | [] -> None
  | final_attempt :: _ ->
      events
      |> List.filter_map (fun event ->
             match event.Task_event.payload with
             | Task_event.Agent_text_delta text
               when event.attempt = final_attempt.number && text <> "" ->
                 Some text
             | _ -> None)
      |> List.rev |> List.find_opt (fun _ -> true)

let has_session events =
  List.exists
    (fun event ->
      match event.Task_event.payload with Task_event.Session_id _ -> true | _ -> false)
    events

let has_usage events =
  List.exists
    (fun event ->
      match event.Task_event.payload with Task_event.Token_usage _ -> true | _ -> false)
    events

type tool_identity = Stable_id of string | Fallback_name of string
type attempt_tool_state = Attempt_pending | Attempt_active | Attempt_finished

let nonempty value = String.trim value <> ""

let started_tool_identity (tool : Task_event.tool) =
  match tool.id with
  | Some id when nonempty id -> Some (Stable_id id)
  | Some _ | None ->
      if nonempty tool.name then Some (Fallback_name tool.name) else None

let finished_tool_identity id name =
  match id with
  | Some id when nonempty id -> Some (Stable_id id)
  | Some _ | None -> (
      match name with
      | Some name when nonempty name -> Some (Fallback_name name)
      | Some _ | None -> None)

let remove_active key active =
  let rec loop reversed = function
    | [] -> None
    | candidate :: rest when candidate = key ->
        Some (List.rev_append reversed rest)
    | candidate :: rest -> loop (candidate :: reversed) rest
  in
  loop [] active

let transition_attempt attempt expected replacement states =
  let rec loop reversed = function
    | [] -> None
    | (number, state) :: rest when number = attempt ->
        if state = expected then
          Some (List.rev_append reversed ((number, replacement) :: rest))
        else None
    | state :: rest -> loop (state :: reversed) rest
  in
  loop [] states

let attempt_is_active attempt states =
  List.assoc_opt attempt states = Some Attempt_active

let attempt_has_active_tool attempt active_tools =
  List.exists (fun (active_attempt, _) -> active_attempt = attempt) active_tools

let all_attempts_finished states =
  List.for_all (fun (_, state) -> state = Attempt_finished) states

let validate_tool_lifecycle ~attempt_numbers events =
  let attempt_states =
    List.map (fun number -> (number, Attempt_pending)) attempt_numbers
  in
  let rec loop attempt_states active_tools started = function
    | [] ->
        if active_tools = [] && all_attempts_finished attempt_states then Ok started
        else Error Tool_lifecycle_disagreement
    | event :: rest -> (
        match event.Task_event.payload with
        | Task_event.Attempt_started _ -> (
            match
              transition_attempt event.attempt Attempt_pending Attempt_active
                attempt_states
            with
            | None -> Error Tool_lifecycle_disagreement
            | Some states -> loop states active_tools started rest)
        | Task_event.Attempt_finished _ ->
            if attempt_has_active_tool event.attempt active_tools then
              Error Tool_lifecycle_disagreement
            else (
              match
                transition_attempt event.attempt Attempt_active Attempt_finished
                  attempt_states
              with
              | None -> Error Tool_lifecycle_disagreement
              | Some states -> loop states active_tools started rest)
        | Task_event.Tool_started tool -> (
            match
              ( attempt_is_active event.attempt attempt_states,
                started_tool_identity tool )
            with
            | false, _ | true, None -> Error Tool_lifecycle_disagreement
            | true, Some identity ->
                let key = (event.attempt, identity) in
                if List.mem key active_tools then Error Tool_lifecycle_disagreement
                else
                    loop attempt_states (key :: active_tools) (tool.name :: started)
                      rest)
        | Task_event.Tool_finished {id; name} -> (
            match
              ( attempt_is_active event.attempt attempt_states,
                finished_tool_identity id name )
            with
            | false, _ | true, None -> Error Tool_lifecycle_disagreement
            | true, Some identity -> (
                match remove_active (event.attempt, identity) active_tools with
                | None -> Error Tool_lifecycle_disagreement
                | Some remaining ->
                    loop attempt_states remaining started rest))
        | Task_event.Terminal _ ->
            if
              active_tools <> [] || not (all_attempts_finished attempt_states)
            then Error Tool_lifecycle_disagreement
            else loop attempt_states active_tools started rest
        | _ -> loop attempt_states active_tools started rest)
  in
  Result.map List.rev (loop attempt_states [] [] events)

let tool_counts names =
  List.fold_left
    (fun counts name ->
      let count = Option.value ~default:0 (List.assoc_opt name counts) in
      (name, count + 1) :: List.remove_assoc name counts)
    [] names
  |> List.sort compare

let expected_attachment_delivery = function
  | Backend_types.Initial_attempt | Backend_types.Fresh_attempt ->
      Backend_types.Upload_attachments
  | Backend_types.Resumed_attempt -> Backend_types.Reuse_session_attachments

let attempts_are_contiguous attempts =
  let rec loop expected = function
    | [] -> true
    | (attempt : Backend_types.task_attempt) :: rest ->
        attempt.number = expected && loop (expected + 1) rest
  in
  loop 1 attempts

let valid_attempt_delivery ~attachments (attempt : Backend_types.task_attempt) =
  attempt.delivery.attachment_references = attachments
  && attempt.delivery.web_access_policy = Backend_types.Web_disabled
  && attempt.delivery.attachment_delivery
     = expected_attachment_delivery attempt.kind

let valid_single_attempt ~attachments
    (execution : Backend_types.task_execution) =
  match execution.attempts with
  | [attempt] ->
      attempt.number = 1
      && attempt.kind = Backend_types.Initial_attempt
      && valid_attempt_delivery ~attachments attempt
      && attempt.schema_validation_error = None
      && attempt.result.status = Backend_types.Success
      && execution.final_result.status = Backend_types.Success
      && attempt.result = execution.final_result
  | [] | _ :: _ :: _ -> false

let valid_validate_and_retry_attempts ~attachments
    (execution : Backend_types.task_execution) =
  match execution.attempts with
  | [_] -> valid_single_attempt ~attachments execution
  | [first; second] ->
      attempts_are_contiguous execution.attempts
      && first.kind = Backend_types.Initial_attempt
      && first.result.status = Backend_types.Success
      && first.schema_validation_error <> None
      && valid_attempt_delivery ~attachments first
      && (second.kind = Backend_types.Fresh_attempt
         || second.kind = Backend_types.Resumed_attempt)
      && second.result.status = Backend_types.Success
      && second.schema_validation_error = None
      && valid_attempt_delivery ~attachments second
      && execution.final_result.status = Backend_types.Success
      && second.result = execution.final_result
  | [] | _ :: _ :: _ -> false

let valid_attempts ~schema_execution ~attachments execution =
  match schema_execution with
  | No_schema | Native_schema -> valid_single_attempt ~attachments execution
  | Validate_and_retry ->
      valid_validate_and_retry_attempts ~attachments execution

let delivery_truncated events =
  List.exists
    (fun event ->
      match event.Task_event.payload with
      | Task_event.Event_delivery_truncated _ -> true
      | _ -> false)
    events

let validate_event_trace ~requirements execution events =
  if not (strictly_increasing_sequence events) then Error Invalid_sequence
  else if not (terminal_is_exactly_last execution events) then
    Error Invalid_terminal
  else if not (attempt_lifecycle_agrees execution events) then
    Error Attempt_lifecycle_disagreement
  else if delivery_truncated events then Error Delivery_was_truncated
  else
    match final_public_output execution events with
    | None -> Error Public_agent_output_missing
    | Some text
      when text <> execution.Backend_types.final_result.agent_text ->
        Error Public_agent_output_mismatch
    | Some _ ->
        if requirements.session && not (has_session events) then
          Error Required_session_missing
        else if requirements.usage && not (has_usage events) then
          Error Required_usage_missing
        else
          let attempt_numbers =
            List.map
              (fun (attempt : Backend_types.task_attempt) -> attempt.number)
              execution.Backend_types.attempts
          in
          match validate_tool_lifecycle ~attempt_numbers events with
          | Error _ as error -> error
          | Ok tools_started ->
              let expected = List.sort compare requirements.exact_tools in
              let actual = tool_counts tools_started in
              if expected = [] then Ok ()
              else if tools_started = [] then Error Required_tool_lifecycle_missing
              else if actual <> expected then
                Error Required_tool_lifecycle_mismatch
              else Ok ()
