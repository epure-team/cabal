(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Agentic backend module type implementation. *)

open Backend_types

type config_check_result =
  | Config_valid
  | Config_invalid of string
  | Config_check_unsupported of string

type implementation_origin = Handwritten | Yaml | Custom

module type S = sig
  val id : string

  val name : string

  val implementation_origin : implementation_origin

  val models : string list

  val models_probe :
    (sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    (string list, string) result)
    option

  val available : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> bool

  val supports_session_resume : bool

  val native_json_schema_output : bool

  val is_resume_failure : task_result -> bool

  val check_project_config :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    project_dir:string ->
    setup_result:Backend_config_writer.setup_result ->
    config_check_result

  val run_task :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    ?on_raw_line:(string -> unit) ->
    task_spec ->
    task_result
end

type t = (module S)

let id (module B : S) = B.id

let name (module B : S) = B.name

let implementation_origin (module B : S) = B.implementation_origin

let models (module B : S) = B.models

let models_probe (module B : S) = B.models_probe

let available ~sw ~env (module B : S) = B.available ~sw ~env

let supports_session_resume (module B : S) = B.supports_session_resume

let native_json_schema_output (module B : S) = B.native_json_schema_output

let is_resume_failure (module B : S) result = B.is_resume_failure result

let check_project_config ~sw ~env ~project_dir ~setup_result (module B : S) =
  B.check_project_config ~sw ~env ~project_dir ~setup_result

let run_task ~sw ~env ?on_raw_line (module B : S) spec =
  B.run_task ~sw ~env ?on_raw_line spec

let run_task_with_ctxt ~sw ~env ?on_raw_line backend request =
  let result = run_task ~sw ~env ?on_raw_line backend request.spec in
  Backend_types.make_task_response ~result ~ctxt:request.ctxt ()
