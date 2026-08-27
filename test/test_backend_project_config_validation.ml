(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Conformance tests for backend project-config validation hooks. *)

open Cabal

let with_tmpdir f =
  let dir = Filename.temp_dir "cabal_cfg_check_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let register_builtin_runtime_backends () =
  Registry.clear () ;
  Adapter_loader.register_all () ;
  Registry.register (module Claude_code) ;
  Registry.register (module Codex_cli) ;
  Registry.register (module Opencode_cli) ;
  Registry.register (module Gemini_cli) ;
  Registry.register (module Copilot_cli)

let epure_mcp_server () =
  Backend_types.make_mcp_server_config
    ~name:"epure"
    ~command:"epure-mcp-server"
    ~args:["--stdio"]
    ~env:[("EPURE_PROJECT", "/tmp/project")]
    ()

let backend_or_fail backend_id =
  match Registry.get backend_id with
  | Some backend -> backend
  | None -> Alcotest.failf "runtime backend not registered: %s" backend_id

let check_result backend_id = function
  | Agentic_backend.Config_valid -> ()
  | Agentic_backend.Config_check_unsupported reason ->
      Alcotest.(check bool)
        (backend_id ^ " unsupported check has documented reason")
        true
        (String.trim reason <> "")
  | Agentic_backend.Config_invalid msg ->
      Alcotest.failf "%s project config is invalid: %s" backend_id msg

let check_builtin_backend ~sw ~env (desc : Backend_registry.descriptor) =
  with_tmpdir (fun project_dir ->
      let setup_result =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:desc.id
          ~project_dir
          ~force:false
      in
      let backend = backend_or_fail desc.id in
      Agentic_backend.check_project_config
        ~sw
        ~env
        ~project_dir
        ~setup_result
        backend
      |> check_result desc.id)

let test_builtin_project_config_validation_conformance () =
  register_builtin_runtime_backends () ;
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Backend_registry.all () |> List.iter (check_builtin_backend ~sw ~env)

let () =
  Alcotest.run
    "Backend_project_config_validation"
    [
      ( "conformance",
        [
          Alcotest.test_case
            "built-in generated configs are valid or explicitly unsupported"
            `Quick
            test_builtin_project_config_validation_conformance;
        ] );
    ]
