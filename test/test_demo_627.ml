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
      {li [CABAL_E2E_BACKEND] — backend id to exercise
          (e.g. ["claude-code"], ["codex"], ["opencode"],
          ["gemini-cli"], ["copilot-cli"]).
          If unset the test is skipped.}
      {li [CABAL_E2E_MODEL] — model name to pass to the backend
          (e.g. ["haiku"], ["gpt-4o-mini"], ["gemini-2.0-flash"]).
          If unset the test is skipped.}}

    {b Exact invocation}

{[
  CABAL_E2E_TESTS=1 CABAL_E2E_BACKEND=claude-code CABAL_E2E_MODEL=haiku \
    dune runtest libs/cabal/test/ --force
]}

    Or via the named alias:

{[
  CABAL_E2E_TESTS=1 CABAL_E2E_BACKEND=claude-code CABAL_E2E_MODEL=haiku \
    dune build @e2e
]}
*)

open Cabal
open Backend_types

(* -------------------------------------------------------------------------
   Helpers
   ---------------------------------------------------------------------- *)

let with_tmpdir f =
  let dir = Filename.temp_dir "cabal_e2e_627_" "" in
  Fun.protect
    ~finally:(fun () ->
      let _ = Sys.command ("rm -rf " ^ Filename.quote dir) in
      ())
    (fun () -> f dir)

let init_git_repo dir =
  let q = Filename.quote dir in
  let readme = Filename.concat dir "README.md" in
  let oc = open_out readme in
  output_string oc "# cabal e2e\n" ;
  close_out oc ;
  let cmd =
    Printf.sprintf
      "git -C %s init -q && \
       git -C %s -c user.name=Cabal -c user.email=cabal@example.invalid \
         add README.md && \
       git -C %s -c user.name=Cabal -c user.email=cabal@example.invalid \
         commit -q -m init"
      q q q
  in
  if Sys.command cmd <> 0 then Error "could not initialise temporary git repo"
  else Ok ()

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
      ( "properties",
        `Assoc [ ("answer", `Assoc [ ("type", `String "string") ]) ] );
      ("required", `List [ `String "answer" ]);
      ("additionalProperties", `Bool false);
    ]

(** Prompt that instructs the backend to return a schema-compliant JSON object.
    Kept intentionally simple and explicit to maximise first-attempt compliance
    with small models such as haiku or gpt-4o-mini. *)
let enforcer_prompt =
  "Output exactly the following JSON object and nothing else.\n\
   Do not add markdown code fences, explanation, or extra whitespace.\n\
   \n\
   {\"answer\":\"ok\"}"

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
*)
let test_enforcer_schema_compliance () =
  (* Resolve required env vars.  Unset → skip so contributors without a
     particular backend configured are not blocked. *)
  let backend_id =
    match Sys.getenv_opt "CABAL_E2E_BACKEND" with
    | Some v when v <> "" -> v
    | _ ->
        Printf.eprintf
          "[e2e-627] SKIPPED: CABAL_E2E_BACKEND not set \
           (set to a backend id such as 'claude-code')\n%!" ;
        Alcotest.skip ()
  in
  let model =
    match Sys.getenv_opt "CABAL_E2E_MODEL" with
    | Some v when v <> "" -> v
    | _ ->
        Printf.eprintf
          "[e2e-627] SKIPPED: CABAL_E2E_MODEL not set \
           (set to a model name such as 'haiku')\n%!" ;
        Alcotest.skip ()
  in
  (* Register all built-in and YAML-backed adapters so [Registry.get] can
     resolve any canonical backend id. *)
  Adapter_loader.register_all () ;
  let backend =
    match Registry.get backend_id with
    | Some b -> b
    | None ->
        Printf.eprintf
          "[e2e-627] SKIPPED: backend '%s' not found in registry\n%!"
          backend_id ;
        Alcotest.skip ()
  in
  Printf.eprintf
    "[e2e-627] backend=%s model=%s native_schema=%b\n%!"
    backend_id
    model
    (Agentic_backend.native_json_schema_output backend) ;
  with_tmpdir (fun working_dir ->
      match init_git_repo working_dir with
      | Error msg -> Alcotest.failf "git repo setup failed: %s" msg
      | Ok () ->
          let spec =
            Backend_types.make_task_spec
              ~prompt:enforcer_prompt
              ~working_dir
              ~timeout:120.0
              ~expected_outputs:[]
              ~model
              ~read_only:true
              ~json_schema:answer_schema
              ()
          in
          let result =
            Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            Json_schema_enforcer.run_task ~sw ~env ~backend spec
          in
          (match result with
          | Error msg ->
              Alcotest.failf
                "Json_schema_enforcer returned Error (both attempts failed \
                 schema validation or native rejection): %s"
                msg
          | Ok task_result -> (
              match task_result.status with
              | Failed msg ->
                  Alcotest.failf "backend returned Failed: %s" msg
              | Timeout -> Alcotest.fail "backend timed out"
              | Cancelled -> Alcotest.fail "backend was cancelled"
              | Success ->
                  Printf.eprintf
                    "[e2e-627] agent_text: %s\n%!"
                    task_result.agent_text ;
                  (match
                     Json_schema_validator.validate
                       ~schema:answer_schema
                       ~document:task_result.agent_text
                   with
                  | Ok () ->
                      Printf.eprintf
                        "[e2e-627] OK: agent_text passes schema validation\n%!"
                  | Error err ->
                      Alcotest.failf
                        "agent_text does not satisfy the schema.\n\
                         Schema error: %s\n\
                         agent_text: %s"
                        err
                        task_result.agent_text))))

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
