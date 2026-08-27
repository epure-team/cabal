(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Backend-owned diagnostics surface. *)

type level = Debug | Info | Warn | Error

type event = Log of level * string | User_warning of string

type handler = event -> unit

let prefix_of_level = function
  | Debug -> "[debug]"
  | Info -> "[info]"
  | Warn -> "[warn]"
  | Error -> "[error]"

let write_stderr_line line =
  Printf.eprintf
    "%s\n%!"
    line [@allow_forbidden "backend diagnostics default stderr sink"]

let default_handler = function
  | Log ((Debug | Info), _) -> ()
  | Log (level, msg) -> write_stderr_line (prefix_of_level level ^ " " ^ msg)
  | User_warning msg -> write_stderr_line msg

let current_handler : handler ref = ref default_handler

let set_handler handler = current_handler := handler

let reset_handler () = current_handler := default_handler

let emit event = !current_handler event

let log level fmt = Printf.ksprintf (fun msg -> emit (Log (level, msg))) fmt

let debug fmt = log Debug fmt

let info fmt = log Info fmt

let warn fmt = log Warn fmt

let error fmt = log Error fmt

let user_warning fmt = Printf.ksprintf (fun msg -> emit (User_warning msg)) fmt
