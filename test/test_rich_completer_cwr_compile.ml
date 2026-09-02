(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

(* A future workflow runner can submit the complete DTO and consume structured
   execution without constructing [task_spec] or knowing a CLI output format. *)
let run_step (complete : Backend_completer.rich_completer) attachment =
  let request =
    Backend_completer.make_completion_request
      ~system_prompt:"You are the workflow implementation agent."
      ~prompt:"Implement the assigned step."
      ~json_schema:(`Assoc [("type", `String "object")])
      ~resume_session_id:"workflow-session"
      ~attachments:[attachment]
      ~web_access:Backend_types.Web_search
      ~timeout:90.0
      ~max_turns:4
      ()
  in
  match complete request with
  | Ok response ->
      let execution = response.Backend_completer.execution in
      ( response.text,
        execution.Backend_types.final_result.status,
        execution.attempts,
        execution.final_session_id,
        execution.total_cost,
        response.event_trace.events,
        response.event_trace.omitted_events )
  | Error error ->
      let diagnostic = Backend_completer.render_rich_completion_error error in
      ( diagnostic,
        Backend_types.Failed diagnostic,
        [],
        None,
        None,
        error.event_trace.events,
        error.event_trace.omitted_events )

let () = ignore run_step
