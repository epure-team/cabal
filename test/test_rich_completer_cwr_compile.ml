(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

(* The guarded API is additive: assigning every pre-guard entry point to its
   original explicit first-class function type must continue to compile. *)
type make_rich =
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_name:string ->
  working_dir:string ->
  ?model:string ->
  ?mcp_servers:Backend_types.mcp_server_config list ->
  ?read_only:bool ->
  unit ->
  (Backend_completer.rich_completer, string) result

type start_task =
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  Task_runtime.t

type run_task =
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  (Backend_types.task_result, Runtime_dispatch.error) result

type run_task_detailed =
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  Runtime_dispatch.detailed_outcome

type prepare =
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?context:Task_execution_context.t ->
  Backend_types.task_spec ->
  (Runtime_dispatch.prepared, Runtime_dispatch.error) result

type prepare_with_input_hooks =
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?context:Task_execution_context.t ->
  ?on_prepare_inputs:(unit -> unit) ->
  ?on_staging_directory:(string -> unit) ->
  ?on_staged_file:(string -> Unix.file_descr -> unit) ->
  ?on_cleanup_attempt:(unit -> unit) ->
  Backend_types.task_spec ->
  (Runtime_dispatch.prepared, Runtime_dispatch.error) result

type private_start_task =
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  Backend_types.task_spec ->
  Runtime_dispatch.Private.task_handle

type start_task_with_input_hooks =
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Task_preflight.limits ->
  backend_id:string ->
  ?on_event:(Task_event.t -> unit) ->
  ?on_raw_line:(string -> unit) ->
  ?on_prepare_inputs:(unit -> unit) ->
  ?on_staging_directory:(string -> unit) ->
  ?on_staged_file:(string -> Unix.file_descr -> unit) ->
  ?on_cleanup_attempt:(unit -> unit) ->
  Backend_types.task_spec ->
  Runtime_dispatch.Private.task_handle

let _legacy_make_rich : make_rich = Backend_completer.make_rich
let _legacy_task_start : start_task = Task_runtime.start_task
let _legacy_task_run : run_task = Task_runtime.run_task
let _legacy_task_run_detailed : run_task_detailed = Task_runtime.run_task_detailed
let _legacy_dispatch_prepare : prepare = Runtime_dispatch.prepare
let _legacy_dispatch_run : run_task = Runtime_dispatch.run_task

let _legacy_dispatch_run_detailed : run_task_detailed =
  Runtime_dispatch.run_task_detailed

let _legacy_private_prepare : prepare_with_input_hooks =
  Runtime_dispatch.Private.prepare_with_input_hooks

let _legacy_private_start : private_start_task =
  Runtime_dispatch.Private.start_task

let _legacy_private_start_with_hooks : start_task_with_input_hooks =
  Runtime_dispatch.Private.start_task_with_input_hooks

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

let make_guarded ~sw ~env ~limits ~backend_name ~working_dir
    (expected_entry : Runtime_entry.t) =
  Backend_completer.make_rich_with_entry ~sw ~env ~limits ~backend_name
    ~working_dir ~expected_entry ()

let () =
  let execution = default_execution () in
  assert (execution.cleanup_status = Backend_types.Cleanup_not_required);
  ignore run_step;
  ignore make_guarded
