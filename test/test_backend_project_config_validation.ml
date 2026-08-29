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

(* --- what the CLI's exit status is allowed to mean ------------------------ *)

(* The conformance test above cannot fail on the claude-code arm any more: with
   the settings file present and `claude` either absent or unable to answer,
   every path leads to Config_valid or Config_check_unsupported. That is the
   intended behaviour, but it means the arm no longer pins it.

   These three drive check_project_config against a stub `claude` on PATH and
   pin the contract that made the change necessary: only a clean success is a
   verdict, and a CLI that cannot answer must not be reported as a bad config.
   Before the fix the second case returned Config_invalid, which failed this
   suite on any machine where `claude` was installed but unauthenticated,
   rate-limited, or newer than the --init-only flag. *)

let with_stub_claude ~exit_code ~stderr f =
  with_tmpdir (fun bindir ->
      let script = Filename.concat bindir "claude" in
      let oc = open_out script in
      Printf.fprintf
        oc
        "#!/bin/sh\n\
         if [ \"$1\" = \"--version\" ]; then echo '9.9.9'; exit 0; fi\n\
         printf '%%s\\n' %s >&2\n\
         exit %d\n"
        (Filename.quote stderr)
        exit_code ;
      close_out oc ;
      Unix.chmod script 0o755 ;
      let old_path = Sys.getenv "PATH" in
      Unix.putenv "PATH" (bindir ^ ":" ^ old_path) ;
      Fun.protect
        ~finally:(fun () -> Unix.putenv "PATH" old_path)
        (fun () -> f ()))

let claude_verdict ~exit_code ~stderr =
  register_builtin_runtime_backends () ;
  with_stub_claude ~exit_code ~stderr @@ fun () ->
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_tmpdir (fun project_dir ->
      let setup_result =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[epure_mcp_server ()]
          ~backend_id:"claude-code"
          ~project_dir
          ~force:false
      in
      Agentic_backend.check_project_config
        ~sw
        ~env
        ~project_dir
        ~setup_result
        (backend_or_fail "claude-code"))

let test_clean_success_is_a_verdict () =
  match claude_verdict ~exit_code:0 ~stderr:"" with
  | Agentic_backend.Config_valid -> ()
  | Agentic_backend.Config_check_unsupported r ->
      Alcotest.failf
        "a CLI that answered cleanly was treated as unchecked: %s"
        r
  | Agentic_backend.Config_invalid m -> Alcotest.failf "reported invalid: %s" m

let test_non_zero_exit_is_not_a_verdict_against_the_config () =
  match
    claude_verdict ~exit_code:1 ~stderr:"error: unknown option '--init-only'"
  with
  | Agentic_backend.Config_invalid m ->
      Alcotest.failf
        "a CLI that could not answer was reported as a bad config: %s"
        m
  | Agentic_backend.Config_valid ->
      Alcotest.fail "a failing CLI was reported as a valid config"
  | Agentic_backend.Config_check_unsupported _ -> ()

(* The detail is the only thing a reader gets when the CLI does reject the
   settings for real, so losing it would make the softer verdict useless. *)
let test_the_process_output_survives_in_the_message () =
  match
    claude_verdict
      ~exit_code:1
      ~stderr:"Invalid settings: unknown key permisions"
  with
  | Agentic_backend.Config_check_unsupported reason ->
      let contains hay sub =
        let n = String.length sub in
        let rec go i =
          i + n <= String.length hay && (String.sub hay i n = sub || go (i + 1))
        in
        n = 0 || go 0
      in
      Alcotest.(check bool)
        "the CLI's own words reach the caller"
        true
        (contains reason "unknown key permisions")
  | other ->
      Alcotest.failf
        "expected unchecked, got %s"
        (match other with
        | Agentic_backend.Config_valid -> "valid"
        | Agentic_backend.Config_invalid m -> "invalid: " ^ m
        | _ -> "?")

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
      ( "claude-code exit status",
        [
          Alcotest.test_case
            "a clean success is a verdict"
            `Quick
            test_clean_success_is_a_verdict;
          Alcotest.test_case
            "a non-zero exit is not a verdict against the config"
            `Quick
            test_non_zero_exit_is_not_a_verdict_against_the_config;
          Alcotest.test_case
            "the process output survives in the message"
            `Quick
            test_the_process_output_survives_in_the_message;
        ] );
    ]
