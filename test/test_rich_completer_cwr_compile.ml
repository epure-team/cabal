(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

(* A future workflow runner can submit the complete DTO and consume structured
   execution without constructing [task_spec] or knowing a CLI output format. *)
let consume_cleanup_status = function
  | Backend_types.Cleanup_not_required -> Backend_types.Cleanup_not_required
  | Backend_types.Cleanup_succeeded -> Backend_types.Cleanup_succeeded
  | Backend_types.Cleanup_failed -> Backend_types.Cleanup_failed

let consume_execution
    ({
       Backend_types.final_result;
       attempts;
       final_session_id;
       total_cost;
       cleanup_status;
       _;
     } : Backend_types.task_execution) =
  ( final_result.status,
    attempts,
    final_session_id,
    total_cost,
    consume_cleanup_status cleanup_status )

let default_execution () =
  Backend_types.make_task_execution
    ~final_result:(Backend_types.make_task_result ~status:Backend_types.Success ())
    ()

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
      let status, attempts, session_id, cost, cleanup_status =
        consume_execution execution
      in
      ( response.text,
        status,
        attempts,
        session_id,
        cost,
        cleanup_status,
        response.event_trace.events,
        response.event_trace.omitted_events )
  | Error error ->
      let diagnostic = Backend_completer.render_rich_completion_error error in
      ( diagnostic,
        Backend_types.Failed diagnostic,
        [],
        None,
        None,
        Backend_types.Cleanup_not_required,
        error.event_trace.events,
        error.event_trace.omitted_events )

let () =
  let execution = default_execution () in
  assert (execution.cleanup_status = Backend_types.Cleanup_not_required);
  ignore run_step
