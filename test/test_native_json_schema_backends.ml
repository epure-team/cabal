(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Generic native-path E2E test — Story #628.

    Iterates every backend in [Backend_registry.all ()] whose
    [capabilities.native_json_schema_output = true], mirrors host runtime
    registration ([Adapter_loader.register_all ()] followed by handwritten
    built-in modules), fails closed if the runtime backend is not native, skips
    any backend whose required credential env var is absent, and exercises the
    native path via [Json_schema_enforcer.run_task] against a small model.

    {b Gate}: only compiled and executed when [CABAL_E2E_TESTS=1].

    Required env vars:
      - [CABAL_E2E_TESTS=1]   — enables building and running this binary
      - [CABAL_E2E_MODEL]     — model name to use (e.g. [haiku])

    Per-backend credential env vars (each native backend also requires its own
    API credential; the test skips the backend if the var is absent):
      - [ANTHROPIC_API_KEY]   — required by [claude-code]

    Run with:
    {[
      CABAL_E2E_TESTS=1 CABAL_E2E_MODEL=haiku dune runtest libs/cabal/test/
    ]}

    Or via the named alias (shares the [@e2e] alias with test_demo_627):
    {[
      CABAL_E2E_TESTS=1 CABAL_E2E_MODEL=haiku dune build @e2e
    ]} *)

open Cabal

(* -------------------------------------------------------------------------
   Credential env var mapping.  When a new backend is added to the registry
   with [native_json_schema_output = true], add its credential env var here.
   ------------------------------------------------------------------------- *)

let required_credential_env_var = function
  | "claude-code" -> Some "ANTHROPIC_API_KEY"
  | "codex" -> Some "OPENAI_API_KEY"
  | "gemini-cli" -> Some "GEMINI_API_KEY"
  | "opencode" -> Some "OPENAI_API_KEY"
  | "copilot-cli" -> None (* uses system auth; no isolated env var *)
  | _ -> None

let register_host_runtime_backends () =
  Registry.clear () ;
  Adapter_loader.register_all () ;
  Registry.register (module Claude_code) ;
  Registry.register (module Gemini_cli) ;
  Registry.register (module Codex_cli) ;
  Registry.register (module Opencode_cli) ;
  Registry.register (module Copilot_cli)

let runtime_backend_for_native_descriptor (d : Backend_registry.descriptor) =
  match Registry.get d.id with
  | None ->
      Alcotest.failf
        "[e2e-628] %s: descriptor declares native_json_schema_output=true but \
         no runtime backend is registered; the generic E2E cannot prove the \
         native path"
        d.id
  | Some backend ->
      let runtime_native = Agentic_backend.native_json_schema_output backend in
      if not runtime_native then
        Alcotest.failf
          "[e2e-628] %s: descriptor declares native_json_schema_output=true \
           but runtime backend native_json_schema_output=false; the generic \
           E2E would exercise validate-and-retry instead of the native path"
          d.id ;
      backend

(* -------------------------------------------------------------------------
   Version probe — advisory only.
   Runs [binary_name --version], parses the output, and emits drift warnings.
   Three cases from D-10:
     installed < descriptor.baseline_version  → warning (log + continue)
     baseline ≤ installed ≤ evidence.tested_at_version → no message
     installed > evidence.tested_at_version   → debug log only
   ------------------------------------------------------------------------- *)

let probe_installed_version binary_name =
  try
    let ic = Unix.open_process_in (binary_name ^ " --version 2>&1") in
    let output =
      let buf = Buffer.create 128 in
      (try
         while true do
           Buffer.add_char buf (input_char ic)
         done
       with End_of_file -> ()) ;
      Buffer.contents buf
    in
    let _ = Unix.close_process_in ic in
    Backend_version.parse_from_output output
  with _ -> Error "version probe failed (binary not found or not executable)"

let emit_version_drift_info backend_id binary_name
    (d : Backend_registry.descriptor) =
  match probe_installed_version binary_name with
  | Error msg ->
      Printf.eprintf
        "[e2e-628] %s: version probe failed (%s); skipping drift check\n%!"
        backend_id
        msg
  | Ok installed -> (
      let baseline_str = d.baseline_version in
      match
        ( Backend_version.of_string baseline_str,
          Backend_version.of_string
            (match d.capabilities.native_json_schema_output_evidence with
            | Some ev -> ev.Backend_types.tested_at_version
            | None -> baseline_str) )
      with
      | Error _, _ | _, Error _ ->
          Printf.eprintf
            "[e2e-628] %s: could not parse baseline or tested_at version\n%!"
            backend_id
      | Ok baseline, Ok tested_at ->
          let cmp_baseline = Backend_version.compare installed baseline in
          let cmp_tested = Backend_version.compare installed tested_at in
          if cmp_baseline < 0 then
            Printf.eprintf
              "[e2e-628] WARNING: %s installed version (%d.%d.%d) is BELOW \
               baseline %s — the native schema feature may not be present; the \
               E2E call is the real gate\n\
               %!"
              backend_id
              installed.Backend_version.major
              installed.Backend_version.minor
              installed.Backend_version.patch
              baseline_str
          else if cmp_tested > 0 then
            Printf.eprintf
              "[e2e-628] debug: %s installed version (%d.%d.%d) is ABOVE \
               tested_at_version %s — feature likely still works but has not \
               been re-verified\n\
               %!"
              backend_id
              installed.Backend_version.major
              installed.Backend_version.minor
              installed.Backend_version.patch
              (match d.capabilities.native_json_schema_output_evidence with
              | Some ev -> ev.Backend_types.tested_at_version
              | None -> "?"))

(* -------------------------------------------------------------------------
   Git repo setup helpers (same pattern as test_demo_627).
   ------------------------------------------------------------------------- *)

let rmdir_r proc_mgr dir =
  try Eio.Process.run proc_mgr ["rm"; "-rf"; dir] with _ -> ()

let init_git_repo proc_mgr dir =
  try
    Eio.Process.run proc_mgr ["git"; "-C"; dir; "init"] ;
    Eio.Process.run
      proc_mgr
      ["git"; "-C"; dir; "config"; "user.email"; "e2e-628@test.cabal"] ;
    Eio.Process.run
      proc_mgr
      ["git"; "-C"; dir; "config"; "user.name"; "Cabal E2E 628"] ;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

(* -------------------------------------------------------------------------
   JSON schema used for native-path verification.
   The backend must produce a JSON object with a single "answer" string field.
   ------------------------------------------------------------------------- *)

let answer_schema : Yojson.Safe.t =
  `Assoc
    [
      ("$schema", `String "https://json-schema.org/draft/2020-12/schema");
      ("type", `String "object");
      ("properties", `Assoc [("answer", `Assoc [("type", `String "string")])]);
      ("required", `List [`String "answer"]);
      ("additionalProperties", `Bool false);
    ]

let enforcer_prompt =
  "Reply ONLY with a JSON object matching the required output schema. Set the \
   'answer' field to the string 'pong'. No prose, no markdown fences — raw \
   JSON only."

(* -------------------------------------------------------------------------
   Exercise the native path for one backend.
   Returns unit on success; calls Alcotest.failf on hard failure.
   The caller already verified the runtime backend is registered and native.
   ------------------------------------------------------------------------- *)

let run_native_e2e_for_backend ~sw ~env ~proc_mgr ~model
    (d : Backend_registry.descriptor) backend =
  Printf.eprintf "[e2e-628] exercising native path for %s...\n%!" d.id ;
  emit_version_drift_info d.id d.binary_name d ;
  let working_dir = Filename.temp_dir ("cabal_e2e_628_" ^ d.id ^ "_") "" in
  Fun.protect
    ~finally:(fun () -> rmdir_r proc_mgr working_dir)
    (fun () ->
      match init_git_repo proc_mgr working_dir with
      | Error msg ->
          Alcotest.failf "[e2e-628] %s: git repo setup failed: %s" d.id msg
      | Ok () -> (
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
          let result = Json_schema_enforcer.run_task ~sw ~env ~backend spec in
          match result with
          | Error msg ->
              Alcotest.failf
                "[e2e-628] %s: Json_schema_enforcer returned Error (native \
                 rejection or schema compliance failure): %s"
                d.id
                msg
          | Ok task_result -> (
              match task_result.Backend_types.status with
              | Backend_types.Failed msg ->
                  Alcotest.failf
                    "[e2e-628] %s: backend returned Failed: %s"
                    d.id
                    msg
              | Backend_types.Timeout ->
                  Alcotest.failf "[e2e-628] %s: backend timed out" d.id
              | Backend_types.Cancelled ->
                  Alcotest.failf "[e2e-628] %s: backend was cancelled" d.id
              | Backend_types.Success -> (
                  Printf.eprintf
                    "[e2e-628] %s: OK — agent_text: %s\n%!"
                    d.id
                    task_result.Backend_types.agent_text ;
                  match
                    Json_schema_validator.validate
                      ~schema:answer_schema
                      ~document:task_result.Backend_types.agent_text
                  with
                  | Ok () ->
                      Printf.eprintf
                        "[e2e-628] %s: agent_text passes schema validation\n%!"
                        d.id
                  | Error err ->
                      Alcotest.failf
                        "[e2e-628] %s: agent_text does not satisfy the schema.\n\
                         Schema error: %s\n\
                         agent_text: %s"
                        d.id
                        err
                        task_result.Backend_types.agent_text))))

(* -------------------------------------------------------------------------
   Main test entry point.
   ------------------------------------------------------------------------- *)

let test_native_json_schema_backends () =
  let model =
    match Sys.getenv_opt "CABAL_E2E_MODEL" with
    | Some v when v <> "" -> v
    | _ ->
        Printf.eprintf
          "[e2e-628] SKIPPED: CABAL_E2E_MODEL not set (set to a model name \
           such as 'haiku')\n\
           %!" ;
        Alcotest.skip ()
  in
  register_host_runtime_backends () ;
  let native_backends =
    List.filter
      (fun (d : Backend_registry.descriptor) ->
        d.capabilities.native_json_schema_output)
      (Backend_registry.all ())
  in
  if native_backends = [] then (
    Printf.eprintf
      "[e2e-628] No backends with native_json_schema_output = true in \
       registry; test trivially passes.\n\
       %!" ;
    ())
  else
    let runtime_backends =
      List.map
        (fun (d : Backend_registry.descriptor) ->
          (d, runtime_backend_for_native_descriptor d))
        native_backends
    in
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    List.iter
      (fun ((d : Backend_registry.descriptor), backend) ->
        (* Credential gate *)
        let skip =
          match required_credential_env_var d.id with
          | Some env_var -> (
              match Sys.getenv_opt env_var with
              | None | Some "" ->
                  Printf.eprintf
                    "[e2e-628] skipping %s: required credential %s not set\n%!"
                    d.id
                    env_var ;
                  true
              | Some _ -> false)
          | None -> false
        in
        if not skip then
          run_native_e2e_for_backend ~sw ~env ~proc_mgr ~model d backend)
      runtime_backends

(** {1 Suite} *)

let () =
  Alcotest.run
    "Story_628_e2e"
    [
      ( "generic native-path E2E",
        [
          Alcotest.test_case
            "all native backends exercise the native schema path"
            `Slow
            test_native_json_schema_backends;
        ] );
    ]
