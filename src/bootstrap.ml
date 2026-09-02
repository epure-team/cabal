(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(* [None] = never run. [Some pd] = last [project_dir] argument, itself an
   option, so [ensure ()] and [ensure ~project_dir] memoise separately. *)
let last_run : string option option ref = ref None

let reset () = last_run := None

let ensure ?project_dir () =
  if !last_run = Some project_dir then ()
  else begin
    (* Natives first. Adapter_loader's builtin pass skips an id that is
       already registered, so this order is what suppresses the five
       [Replacing existing backend] warnings and what keeps
       [Yaml_adapter.config_of] returning [None] for a native. *)
    Registry.register (module Claude_code) ;
    Registry.register (module Gemini_cli) ;
    Registry.register (module Codex_cli) ;
    Registry.register (module Opencode_cli) ;
    Registry.register (module Copilot_cli) ;
    Registry.register (module Mock_agent) ;
    Adapter_loader.register_all ?project_dir () ;
    last_run := Some project_dir
  end
