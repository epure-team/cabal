(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Pure normalized-event assertions shared by CBL-08 structural/live tests. *)

open Cabal

type protocol_requirements = {
  session : bool;
  usage : bool;
  tool : bool;
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
  | Tool_lifecycle_disagreement
  | Delivery_was_truncated

let protocol_requirements_for_backend = function
  | "codex" -> {session = true; usage = true; tool = false}
  | _ -> {session = false; usage = false; tool = false}

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

let validate_tool_lifecycle events =
  let rec loop active started = function
    | [] -> if active = [] then Ok started else Error Tool_lifecycle_disagreement
    | event :: rest -> (
        match event.Task_event.payload with
        | Task_event.Tool_started tool -> (
            match started_tool_identity tool with
            | None -> Error Tool_lifecycle_disagreement
            | Some identity ->
                let key = (event.attempt, identity) in
                if event.attempt < 1 || List.mem key active then
                  Error Tool_lifecycle_disagreement
                else loop (key :: active) (started + 1) rest)
        | Task_event.Tool_finished {id; name} -> (
            match finished_tool_identity id name with
            | None -> Error Tool_lifecycle_disagreement
            | Some identity -> (
                match remove_active (event.attempt, identity) active with
                | None -> Error Tool_lifecycle_disagreement
                | Some remaining -> loop remaining started rest))
        | Task_event.Terminal _ when active <> [] ->
            Error Tool_lifecycle_disagreement
        | _ -> loop active started rest)
  in
  loop [] 0 events

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

let valid_native_attempt ~attachments
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

let valid_attempts ~native ~attachments
    (execution : Backend_types.task_execution) =
  let attempts = execution.attempts in
  attempts <> []
  && List.length attempts <= 2
  && attempts_are_contiguous attempts
  && List.for_all (valid_attempt_delivery ~attachments) attempts
  && (not native || valid_native_attempt ~attachments execution)

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
          match validate_tool_lifecycle events with
          | Error _ as error -> error
          | Ok tools_started ->
              if requirements.tool && tools_started = 0 then
                Error Required_tool_lifecycle_missing
              else Ok ()
