(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal
open Backend_types

(* CBL-05 source-compatibility guard: this projection intentionally has no
   annotation. Adding another public [elapsed] record label must not change its
   inference away from the legacy [task_result] field. *)
let legacy_elapsed result = result.elapsed

let _ = legacy_elapsed (make_task_result ~status:Success ~elapsed:1.0 ())

module Migrated_backend = struct
  let id = "custom-compile-fixture"
  let name = "Custom compile fixture"
  let models = []
  let models_probe = None
  let available ~sw:_ ~env:_ = true
  let supports_session_resume = false
  let native_json_schema_output = false
  let is_resume_failure _ = false

  let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
    Agentic_backend.Config_check_unsupported "compile fixture"

  (* Downstream modules must add [?context], even when they do not consume it. *)
  let run_task ~sw:_ ~env:_ ?context:_ ?on_raw_line:_ _spec =
    make_task_result ~status:Success ()
end

let backend = (module Migrated_backend : Agentic_backend.S)

(* Existing low-level call sites remain source-compatible because the public
   wrapper does not require a context argument. *)
let legacy_call_site ~sw ~env spec =
  Agentic_backend.run_task ~sw ~env backend spec

let () = ignore legacy_call_site
