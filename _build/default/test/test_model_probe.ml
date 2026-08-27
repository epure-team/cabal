(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the dynamic-probe layer added on top of static model
    enumeration.

    Covers:
    - probe success path: [Ok non_empty] surfaces as [Registry.Probe]
    - probe error fallback: [Error _] reverts to the static list as
      [Registry.Static] without escaping the probe call
    - probe exception fallback: a raising probe reverts to the static list
      as [Registry.Static] without crashing [register_all]
    - probe empty fallback: [Ok []] reverts to the static list as
      [Registry.Static] with a diagnostics warning
    - no-probe contract: [models_probe = None] surfaces as [Registry.Static]
    - cache contract: a probe registered for a backend is invoked exactly
      once per [register_all] call
    - built-in adapters: every built-in id is reachable through
      [Registry.resolved_models] after [Adapter_loader.register_all] runs

    All test scenarios construct fresh [Agentic_backend.S] modules and
    install them via the public [Registry.register] entry point — no
    module-internal poking. *)

open Cabal

(* --- mock backend builder ------------------------------------------------- *)

(** [make_mock ~id ~models ~models_probe] builds a first-class backend with
    the requested static list and probe.  Everything else is stubbed so the
    backend never actually spawns a process or generates project config. *)
let make_mock ~id ~models ~models_probe : Agentic_backend.t =
  let module M = struct
    let id = id

    let name = "Mock probe backend (" ^ id ^ ")"

    let models = models

    let models_probe = models_probe

    let available ~sw:_ ~env:_ = true

    let supports_session_resume = false

    let native_json_schema_output = false

    let is_resume_failure (_result : Backend_types.task_result) = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "mock probe backend"

    let run_task ~sw:_ ~env:_ ?on_raw_line:_ (_spec : Backend_types.task_spec) =
      Backend_types.make_task_result
        ~status:Backend_types.Success
        ~stdout:"mock"
        ()
  end in
  (module M : Agentic_backend.S)

(* --- alcotest helpers ---------------------------------------------------- *)

let string_list = Alcotest.(list string)

let models_source =
  let pp fmt = function
    | Registry.Probe -> Format.pp_print_string fmt "Probe"
    | Registry.Static -> Format.pp_print_string fmt "Static"
    | Registry.Hybrid -> Format.pp_print_string fmt "Hybrid"
  in
  Alcotest.testable pp ( = )

(* Resolved-models pair tester for compact diffs in failure output. *)
let resolved_pair = Alcotest.(pair string_list models_source)

(* --- shared eio fixtures ------------------------------------------------- *)

(** Run [f] under a fresh Eio switch + env.  Probe-firing tests use this so
    [register_all] can dispatch real probes. *)
let with_eio f =
  Eio_posix.run (fun env -> Eio.Switch.run (fun sw -> f ~sw ~env))

(* --- tests --------------------------------------------------------------- *)

let teardown () = Registry.clear ()

let test_no_probe_yields_static_source () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      let backend =
        make_mock ~id:"mp-none" ~models:["a"; "b"] ~models_probe:None
      in
      Registry.register backend ;
      (* Even without an Eio env, the [register]-time seed publishes the
         static view tagged Static. *)
      match Registry.resolved_models "mp-none" with
      | None -> Alcotest.fail "expected resolved_models to return Some"
      | Some pair ->
          Alcotest.check
            resolved_pair
            "static seed"
            (["a"; "b"], Registry.Static)
            pair)

let test_probe_success_publishes_probe_tag () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      let probe ~sw:_ ~env:_ = Ok ["x"; "y"] in
      let backend =
        make_mock ~id:"mp-ok" ~models:["fallback"] ~models_probe:(Some probe)
      in
      Registry.register backend ;
      with_eio (fun ~sw ~env -> Adapter_loader.register_all ~sw ~env ()) ;
      match Registry.resolved_models "mp-ok" with
      | None -> Alcotest.fail "expected mp-ok to be resolved"
      | Some pair ->
          Alcotest.check
            resolved_pair
            "probe wins"
            (["x"; "y"], Registry.Probe)
            pair)

let test_probe_error_falls_back_to_static () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      let probe ~sw:_ ~env:_ = Error "boom" in
      let backend =
        make_mock ~id:"mp-err" ~models:["s1"; "s2"] ~models_probe:(Some probe)
      in
      Registry.register backend ;
      with_eio (fun ~sw ~env -> Adapter_loader.register_all ~sw ~env ()) ;
      match Registry.resolved_models "mp-err" with
      | None -> Alcotest.fail "expected mp-err to be resolved"
      | Some pair ->
          Alcotest.check
            resolved_pair
            "static fallback on Error"
            (["s1"; "s2"], Registry.Static)
            pair)

let test_probe_exception_falls_back_to_static () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      let probe ~sw:_ ~env:_ = failwith "probe raised" in
      let backend =
        make_mock ~id:"mp-raise" ~models:["s1"] ~models_probe:(Some probe)
      in
      Registry.register backend ;
      with_eio (fun ~sw ~env -> Adapter_loader.register_all ~sw ~env ()) ;
      match Registry.resolved_models "mp-raise" with
      | None -> Alcotest.fail "expected mp-raise to be resolved"
      | Some pair ->
          Alcotest.check
            resolved_pair
            "static fallback on exception"
            (["s1"], Registry.Static)
            pair)

let test_probe_empty_list_falls_back_to_static () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      let probe ~sw:_ ~env:_ = Ok [] in
      let backend =
        make_mock ~id:"mp-empty" ~models:["s1"; "s2"] ~models_probe:(Some probe)
      in
      Registry.register backend ;
      with_eio (fun ~sw ~env -> Adapter_loader.register_all ~sw ~env ()) ;
      match Registry.resolved_models "mp-empty" with
      | None -> Alcotest.fail "expected mp-empty to be resolved"
      | Some pair ->
          Alcotest.check
            resolved_pair
            "static fallback on Ok []"
            (["s1"; "s2"], Registry.Static)
            pair)

let test_probe_invoked_exactly_once () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      let calls = ref 0 in
      let probe ~sw:_ ~env:_ =
        incr calls ;
        Ok ["only-once"]
      in
      let backend =
        make_mock ~id:"mp-once" ~models:["s"] ~models_probe:(Some probe)
      in
      Registry.register backend ;
      with_eio (fun ~sw ~env -> Adapter_loader.register_all ~sw ~env ()) ;
      (* Multiple subsequent queries must not re-fire the probe. *)
      let _ = Registry.resolved_models "mp-once" in
      let _ = Registry.resolved_models "mp-once" in
      let _ = Registry.list_models "mp-once" in
      Alcotest.(check int) "probe fired exactly once" 1 !calls ;
      match Registry.resolved_models "mp-once" with
      | None -> Alcotest.fail "expected mp-once to be resolved"
      | Some (models, src) ->
          Alcotest.(check string_list) "probe output kept" ["only-once"] models ;
          Alcotest.check models_source "source is Probe" Registry.Probe src)

let test_list_models_is_resolved_models_first () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      let probe ~sw:_ ~env:_ = Ok ["live"; "live2"] in
      let backend =
        make_mock ~id:"mp-live" ~models:["stale"] ~models_probe:(Some probe)
      in
      Registry.register backend ;
      with_eio (fun ~sw ~env -> Adapter_loader.register_all ~sw ~env ()) ;
      Alcotest.(check (option string_list))
        "list_models returns the live list"
        (Some ["live"; "live2"])
        (Registry.list_models "mp-live"))

let test_unknown_id_resolved_models_returns_none () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      Alcotest.(check (option (pair string_list models_source)))
        "unknown id has no resolved view"
        None
        (Registry.resolved_models "no-such-backend"))

let test_register_all_no_eio_keeps_static_seed () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      (* Without ~sw/~env the probe layer is skipped — even backends with a
         probe surface as Static (their probe never fires). *)
      let probe ~sw:_ ~env:_ = Ok ["would-be-live"] in
      let backend =
        make_mock ~id:"mp-no-eio" ~models:["seeded"] ~models_probe:(Some probe)
      in
      Registry.register backend ;
      Adapter_loader.register_all () ;
      match Registry.resolved_models "mp-no-eio" with
      | None -> Alcotest.fail "expected mp-no-eio to be resolved"
      | Some pair ->
          Alcotest.check
            resolved_pair
            "probe skipped without ~sw/~env"
            (["seeded"], Registry.Static)
            pair)

let test_built_in_ids_resolve_after_register_all () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      Adapter_loader.register_all () ;
      let built_in_ids =
        ["claude-code"; "codex"; "gemini-cli"; "copilot-cli"; "opencode"]
      in
      List.iter
        (fun id ->
          match Registry.resolved_models id with
          | None ->
              Alcotest.failf
                "expected %s to have a resolved view after register_all"
                id
          | Some (models, _src) ->
              Alcotest.(check bool)
                (Printf.sprintf "%s has non-empty resolved list" id)
                true
                (List.length models > 0))
        built_in_ids)

let test_built_in_resolution_with_eio_keeps_non_none () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      with_eio (fun ~sw ~env -> Adapter_loader.register_all ~sw ~env ()) ;
      let built_in_ids =
        ["claude-code"; "codex"; "gemini-cli"; "copilot-cli"; "opencode"]
      in
      List.iter
        (fun id ->
          match Registry.resolved_models id with
          | None -> Alcotest.failf "expected %s to be resolved" id
          | Some (models, src) ->
              (* For probe-less adapters the source must stay Static.  Either
                 way the list must be non-empty so callers can render a
                 selectable set. *)
              (match src with
              | Registry.Probe | Registry.Static -> ()
              | Registry.Hybrid ->
                  Alcotest.failf "%s emitted reserved Hybrid tag" id) ;
              Alcotest.(check bool)
                (Printf.sprintf "%s non-empty after probe layer" id)
                true
                (List.length models > 0))
        built_in_ids)

let test_clear_drops_resolved_models () =
  Registry.clear () ;
  let backend = make_mock ~id:"mp-clear" ~models:["x"] ~models_probe:None in
  Registry.register backend ;
  (match Registry.resolved_models "mp-clear" with
  | Some _ -> ()
  | None -> Alcotest.fail "expected resolved view before clear") ;
  Registry.clear () ;
  match Registry.resolved_models "mp-clear" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected clear to wipe resolved cache"

let test_overlapping_register_replaces_resolved () =
  Registry.clear () ;
  Fun.protect ~finally:teardown (fun () ->
      let first =
        make_mock ~id:"mp-shadow" ~models:["first"] ~models_probe:None
      in
      Registry.register first ;
      Alcotest.(check (option string_list))
        "first register publishes its static list"
        (Some ["first"])
        (Registry.list_models "mp-shadow") ;
      let second =
        make_mock ~id:"mp-shadow" ~models:["second"] ~models_probe:None
      in
      Registry.register second ;
      Alcotest.(check (option string_list))
        "second register replaces the resolved list"
        (Some ["second"])
        (Registry.list_models "mp-shadow"))

(* --- Suite --------------------------------------------------------------- *)

let () =
  Alcotest.run
    "Model_probe"
    [
      ( "Probe success / failure modes",
        [
          Alcotest.test_case
            "no probe surfaces as Static"
            `Quick
            test_no_probe_yields_static_source;
          Alcotest.test_case
            "Ok non-empty wins as Probe"
            `Quick
            test_probe_success_publishes_probe_tag;
          Alcotest.test_case
            "Error falls back to Static"
            `Quick
            test_probe_error_falls_back_to_static;
          Alcotest.test_case
            "exception falls back to Static"
            `Quick
            test_probe_exception_falls_back_to_static;
          Alcotest.test_case
            "Ok [] falls back to Static"
            `Quick
            test_probe_empty_list_falls_back_to_static;
        ] );
      ( "Cache + accessor contract",
        [
          Alcotest.test_case
            "probe fires exactly once per register_all"
            `Quick
            test_probe_invoked_exactly_once;
          Alcotest.test_case
            "list_models surfaces probe output"
            `Quick
            test_list_models_is_resolved_models_first;
          Alcotest.test_case
            "unknown id yields None"
            `Quick
            test_unknown_id_resolved_models_returns_none;
          Alcotest.test_case
            "register_all without ~sw/~env keeps Static seed"
            `Quick
            test_register_all_no_eio_keeps_static_seed;
          Alcotest.test_case
            "clear drops the resolved cache"
            `Quick
            test_clear_drops_resolved_models;
          Alcotest.test_case
            "re-register replaces the resolved view"
            `Quick
            test_overlapping_register_replaces_resolved;
        ] );
      ( "Built-in adapters",
        [
          Alcotest.test_case
            "all built-in ids resolvable after register_all"
            `Quick
            test_built_in_ids_resolve_after_register_all;
          Alcotest.test_case
            "built-in adapters resolve under Eio probe layer"
            `Quick
            test_built_in_resolution_with_eio_keeps_non_none;
        ] );
    ]
