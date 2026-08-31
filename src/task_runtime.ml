(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type t = Runtime_dispatch.Private.task_handle

let start_task = Runtime_dispatch.Private.start_task

let cancel = Runtime_dispatch.Private.cancel

let await = Runtime_dispatch.Private.await

let run_task = Runtime_dispatch.run_task
