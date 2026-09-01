(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal

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
    Backend_types.make_task_result ~status:Backend_types.Success ()
end

let backend = (module Migrated_backend : Agentic_backend.S)

(* Existing low-level call sites remain source-compatible because the public
   wrapper does not require a context argument. *)
let legacy_call_site ~sw ~env spec =
  Agentic_backend.run_task ~sw ~env backend spec

let () = ignore legacy_call_site
