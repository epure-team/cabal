(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Generic native-path E2E test — Story #628.

    Iterates every backend in [Backend_registry.all ()] whose
    [capabilities.native_json_schema_output = true], mirrors host runtime
    registration through [Runtime_bootstrap.Hardened_builtins], fails closed if
    the runtime backend is not native, skips
    unavailable backend binaries, and exercises the native path via
    [Json_schema_enforcer.run_task] against backend-specific default models.

    {b Gate}: only compiled and executed when [CABAL_E2E_TESTS=1].

    Required env vars:
    - [CABAL_E2E_TESTS=1] — enables building and running this binary

    Optional env vars:
    - [CABAL_E2E_BACKEND] — backend id filter for manual debugging
    - [CABAL_E2E_MODEL_<BACKEND>] — per-backend model override; Codex omits the
      model flag by default and lets the CLI choose its configured default

    Backend credentials are owned by the CLIs themselves.  Installed but
    unauthenticated CLIs fail the E2E call; unavailable binaries are skipped
    with a diagnostic.

    Run with:
    {[
      CABAL_E2E_TESTS=1 dune runtest libs/cabal/test/
    ]}

    Or via the named alias (shares the [@e2e] alias with test_demo_627):
    {[
      CABAL_E2E_TESTS=1 dune build @e2e
    ]} *)

open Cabal

let () = Process_test_helper.install_launcher ()

(* -------------------------------------------------------------------------
   Harness setup.  Backend-specific model defaults live in
   [E2e_harness_config]; credentials are left to each CLI's own auth mechanism.
   ------------------------------------------------------------------------- *)

let emit_ambient_auth_note_if_needed backend_id =
  match backend_id with
  | "codex" ->
      Printf.eprintf
        "[e2e-628] codex: no API-key pre-gate; relying on an \
         already-authenticated Codex CLI session. For non-interactive setup, \
         provide CODEX_ACCESS_TOKEN via: printf '%%s' \"$CODEX_ACCESS_TOKEN\" \
         | codex login --with-access-token\n\
         %!"
  | _ -> ()

let register_host_runtime_backends () =
  Registry.clear () ;
  match
    Runtime_bootstrap.register_runtime
      ~profile:Runtime_bootstrap.Hardened_builtins
      ()
  with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error)

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

let run_native_e2e_for_backend ~sw ~env ~proc_mgr ?model
    (d : Backend_registry.descriptor) backend =
  Diagnostics.set_handler (function Log (_, msg) | User_warning msg ->
      Format.eprintf "%s" msg) ;
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
  register_host_runtime_backends () ;
  let native_backends =
    List.filter
      (fun (d : Backend_registry.descriptor) ->
        d.capabilities.native_json_schema_output)
      (Backend_registry.all ())
  in
  let selected_ids =
    E2e_harness_config.selected_backend_ids
      ~all_backend_ids:
        (List.map
           (fun (d : Backend_registry.descriptor) -> d.id)
           native_backends)
      ()
  in
  let native_backends =
    List.filter
      (fun (d : Backend_registry.descriptor) -> List.mem d.id selected_ids)
      native_backends
  in
  if native_backends = [] then (
    Printf.eprintf
      "[e2e-628] No backends with native_json_schema_output = true in registry \
       after applying the optional CABAL_E2E_BACKEND filter; test trivially \
       passes.\n\
       %!" ;
    ())
  else
    let runtime_backends =
      List.map
        (fun (d : Backend_registry.descriptor) ->
          (d, runtime_backend_for_native_descriptor d))
        native_backends
    in
    Eio_posix.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    List.iter
      (fun ((d : Backend_registry.descriptor), backend) ->
        if not (Agentic_backend.available ~sw ~env backend) then
          Printf.eprintf
            "[e2e-628] skipping %s: backend binary is not available on PATH\n%!"
            d.id
        else (
          emit_ambient_auth_note_if_needed d.id ;
          let backend_model = E2e_harness_config.model_for_backend d.id in
          Printf.eprintf
            "[e2e-628] %s model=%s (override env %s)\n%!"
            d.id
            (E2e_harness_config.model_label backend_model)
            (E2e_harness_config.model_env_var_for_backend d.id) ;
          run_native_e2e_for_backend
            ~sw
            ~env
            ~proc_mgr
            ?model:backend_model
            d
            backend))
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
