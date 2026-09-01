(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** E2E tests for {!Json_schema_enforcer} against real backend CLIs.

    Story #627 — manual, small models, CI-excluded.

    These tests exercise the full [Json_schema_enforcer.run_task] path against
    a real backend CLI.  They are excluded from CI by the dune
    [(enabled_if (= %{env:CABAL_E2E_TESTS=0} 1))] guard and must be run
    manually by contributors who have the relevant backend installed and
    configured.

    {b Environment variables}

    {ul
      {li [CABAL_E2E_TESTS=1] — required to build and run this binary at all.}
      {li [CABAL_E2E_BACKEND] — optional backend id filter
          (e.g. ["claude-code"], ["codex"], ["opencode"],
          ["gemini-cli"], ["copilot-cli"]). If unset, all default E2E
          backends are exercised.}
      {li [CABAL_E2E_MODEL_<BACKEND>] — optional per-backend model override,
          where backend ids are uppercased and non-alphanumerics become
          underscores (for example [CABAL_E2E_MODEL_CLAUDE_CODE]). Defaults
          are backend-specific; Codex omits the model flag by default.}}

    {b Exact invocation}

{[
  CABAL_E2E_TESTS=1 \
    dune runtest libs/cabal/test/ --force
]}

    Or via the named alias:

{[
  CABAL_E2E_TESTS=1 \
    dune build @e2e
]}
*)

open Cabal
open Backend_types

let () = Process_test_helper.install_launcher ()

(* -------------------------------------------------------------------------
   Helpers — all process invocations use Eio.Process (no Sys.command)
   ---------------------------------------------------------------------- *)

(** Run a command via [Eio.Process.run].  Returns [Ok ()] on exit code 0,
    [Error msg] on non-zero exit or process spawn failure.  Never uses
    [Sys.command] or any other blocking Unix shell invocation. *)
let run_cmd_eio proc_mgr args =
  try
    Eio.Process.run proc_mgr args ;
    Ok ()
  with exn ->
    Error
      (Printf.sprintf
         "command (%s) failed: %s"
         (String.concat " " args)
         (Printexc.to_string exn))

(** Recursively remove [dir] via [rm -rf] using [Eio.Process].  Silently
    ignores failures (best-effort cleanup in [Fun.protect ~finally]). *)
let rmdir_r_eio proc_mgr dir =
  match run_cmd_eio proc_mgr ["rm"; "-rf"; dir] with Ok () | Error _ -> ()

(** Write a minimal README, then initialise a git repository in [dir] using
    [Eio.Process.run].  Returns [Ok ()] or [Error msg].  No [Sys.command]
    or shell string concatenation is used. *)
let init_git_repo_eio proc_mgr dir =
  let readme = Filename.concat dir "README.md" in
  let oc = open_out readme in
  output_string oc "# cabal e2e\n" ;
  close_out oc ;
  let run args = run_cmd_eio proc_mgr (["git"; "-C"; dir] @ args) in
  let git_cfg =
    ["-c"; "user.name=Cabal"; "-c"; "user.email=cabal@example.invalid"]
  in
  match run ["init"; "-q"] with
  | Error msg -> Error msg
  | Ok () -> (
      match run (git_cfg @ ["add"; "README.md"]) with
      | Error msg -> Error msg
      | Ok () -> run (git_cfg @ ["commit"; "-q"; "-m"; "init"]))

(* -------------------------------------------------------------------------
   Schema and prompt
   ---------------------------------------------------------------------- *)

(** Simple JSON Schema 2020-12 object with a required string field [answer].
    Deliberately minimal so that small models have the best chance of
    complying on the first attempt.  [additionalProperties: false] ensures
    the schema validator rejects any extra fields a model might add. *)
let answer_schema : Yojson.Safe.t =
  `Assoc
    [
      ("type", `String "object");
      ("properties", `Assoc [("answer", `Assoc [("type", `String "string")])]);
      ("required", `List [`String "answer"]);
      ("additionalProperties", `Bool false);
    ]

(** Prompt that instructs the backend to return a schema-compliant JSON object.
    Kept intentionally simple and explicit to maximise first-attempt compliance
    with small models such as haiku or gpt-4o-mini. *)
let enforcer_prompt =
  "Output exactly the following JSON object and nothing else.\n\
   Do not add markdown code fences, explanation, or extra whitespace.\n\n\
   {\"answer\":\"ok\"}"

let register_host_runtime_backends () =
  Registry.clear () ;
  match
    Runtime_bootstrap.register_runtime
      ~profile:Runtime_bootstrap.Hardened_builtins
      ()
  with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error)

(* -------------------------------------------------------------------------
   Tests
   ---------------------------------------------------------------------- *)

(** Exercises [Json_schema_enforcer.run_task] with a real backend CLI.

    Verifies that schema compliance is achieved by either the first attempt
    (happy path) or the corrective re-invocation (validate-and-retry path).
    On the native path ([native_json_schema_output = true]) the backend
    enforces the schema directly; on the validate-and-retry path the enforcer
    validates [agent_text] and, if needed, makes one corrective call.

    The test asserts that [run_task] returns [Ok result] with
    [status = Success] and that [result.agent_text] satisfies [answer_schema].

    All subprocess invocations (git, rm) use [Eio.Process.run] — no
    [Sys.command] or blocking Unix shell calls are made anywhere in this
    module.
*)
let run_enforcer_schema_compliance_for_backend ~sw ~env ~proc_mgr backend_id =
  let backend =
    match Registry.get backend_id with
    | Some b -> b
    | None ->
        Printf.eprintf
          "[e2e-627] SKIPPED: backend '%s' not found in registry\n%!"
          backend_id ;
        Alcotest.skip ()
  in
  if not (Agentic_backend.available ~sw ~env backend) then
    Printf.eprintf
      "[e2e-627] skipping %s: backend binary is not available on PATH\n%!"
      backend_id
  else
    let model = E2e_harness_config.model_for_backend backend_id in
    let model_label = E2e_harness_config.model_label model in
    let model_env_var =
      E2e_harness_config.model_env_var_for_backend backend_id
    in
    Printf.eprintf
      "[e2e-627] backend=%s model=%s (override env %s) native_schema=%b\n%!"
      backend_id
      model_label
      model_env_var
      (Agentic_backend.native_json_schema_output backend) ;
    let working_dir = Filename.temp_dir "cabal_e2e_627_" "" in
    Fun.protect
      ~finally:(fun () -> rmdir_r_eio proc_mgr working_dir)
      (fun () ->
        match init_git_repo_eio proc_mgr working_dir with
        | Error msg -> Alcotest.failf "git repo setup failed: %s" msg
        | Ok () -> (
            let spec =
              Backend_types.make_task_spec
                ~prompt:enforcer_prompt
                ~working_dir
                ~timeout:120.0
                ~expected_outputs:[]
                ~managed_namespace:E2e_harness_config.managed_namespace
                ?model
                ~read_only:true
                ~json_schema:answer_schema
                ()
            in
            let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
            match result with
            | Error msg ->
                Alcotest.failf
                  "Json_schema_enforcer returned Error (both attempts failed \
                   schema validation or native backend failure): %s"
                  msg
            | Ok task_result -> (
                match task_result.status with
                | Failed msg -> Alcotest.failf "backend returned Failed: %s" msg
                | Timeout -> Alcotest.fail "backend timed out"
                | Cancelled -> Alcotest.fail "backend was cancelled"
                | Success -> (
                    Printf.eprintf
                      "[e2e-627] agent_text: %s\n%!"
                      task_result.agent_text ;
                    match
                      Json_schema_validator.validate
                        ~schema:answer_schema
                        ~document:task_result.agent_text
                    with
                    | Ok () ->
                        Printf.eprintf
                          "[e2e-627] OK: agent_text passes schema validation\n\
                           %!"
                    | Error err ->
                        Alcotest.failf
                          "agent_text does not satisfy the schema.\n\
                           Schema error: %s\n\
                           agent_text: %s"
                          err
                          task_result.agent_text))))

let test_enforcer_schema_compliance () =
  register_host_runtime_backends () ;
  let backend_ids =
    E2e_harness_config.selected_backend_ids
      ~all_backend_ids:E2e_harness_config.all_backend_ids
      ()
  in
  if backend_ids = [] then (
    Printf.eprintf "[e2e-627] no backend ids selected; test skipped\n%!" ;
    Alcotest.skip ()) ;
  Printf.eprintf
    "[e2e-627] selected backends: %s\n%!"
    (String.concat ", " backend_ids) ;
  (* All Eio operations (process spawning for git, rm, and the enforcer
     itself) share a single Eio_posix.run + Switch.run scope so that
     proc_mgr is available for both setup and cleanup. *)
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  List.iter
    (run_enforcer_schema_compliance_for_backend ~sw ~env ~proc_mgr)
    backend_ids

(* -------------------------------------------------------------------------
   Entry point
   ---------------------------------------------------------------------- *)

let () =
  Alcotest.run
    "Json_schema_enforcer E2E"
    [
      ( "Enforcer",
        [
          ( "schema-compliant output (first or corrective call)",
            `Slow,
            test_enforcer_schema_compliance );
        ] );
    ]
