(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** {!Bootstrap} is the single seam that makes backends available.  What is
    pinned here is not "registration happens" -- the previous generation of
    this test proved only that, and a mutant that dropped the OCaml layer
    survived because the bundled YAML adapters define the same five ids and
    answered in its place.  What is pinned is the four things that differ
    between the two layers, each of which was a live defect:

    - a native wins the id collision, and it wins by the YAML builtin being
      SKIPPED rather than overwritten (so no warning, and no poisoned
      [config_table]);
    - a user/project adapter still overrides a native, and still warns;
    - the memo actually memoises;
    - [mock-agent]'s read-only capability does not depend on registration. *)

open Cabal

(* ── warning capture ──────────────────────────────────────────────────── *)

let captured : string list ref = ref []

let with_captured_warnings f =
  captured := [] ;
  Diagnostics.set_handler (function
    | Diagnostics.Log (Diagnostics.Warn, msg) -> captured := msg :: !captured
    | Diagnostics.User_warning msg -> captured := msg :: !captured
    | _ -> ()) ;
  Fun.protect ~finally:Diagnostics.reset_handler f ;
  List.rev !captured

let replacement_warnings ws =
  List.filter
    (fun w ->
      let needle = "Replacing existing backend" in
      let n = String.length needle in
      let rec scan i =
        i + n <= String.length w && (String.sub w i n = needle || scan (i + 1))
      in
      scan 0)
    ws

(* Every test starts from a genuinely empty world.  [Registry.clear] alone is
   not enough: it empties the registry without [Bootstrap] noticing, so the
   next [ensure] would be a no-op over nothing.  That asymmetry is exactly
   why [reset] is exported. *)
let fresh () =
  Registry.clear () ;
  Bootstrap.reset ()

(* A developer's own ~/.cabal/adapters must not be able to decide any outcome
   below -- it would register a third claude-code and quietly win. *)
let with_isolated_home f =
  let saved = Sys.getenv_opt "HOME" in
  let tmp = Filename.temp_file "cabal_bootstrap_home_" "" in
  Sys.remove tmp ;
  Unix.mkdir tmp 0o755 ;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "HOME" (Option.value saved ~default:"") ;
      try Unix.rmdir tmp with _ -> ())
    (fun () ->
      Unix.putenv "HOME" tmp ;
      f tmp)

(* ── FR-012: the order is a contract, not an observation ──────────────── *)

let test_registration_order_is_pinned () =
  with_isolated_home @@ fun _home ->
  fresh () ;
  Bootstrap.ensure () ;
  Alcotest.(check (list string))
    "natives in declaration order, then YAML-only adapters"
    [
      "claude-code";
      "gemini-cli";
      "codex";
      "opencode";
      "copilot-cli";
      "mock-agent";
      "pi";
    ]
    (Registry.list_ids ())

(* ── FR-003a/b: the native wins, by the builtin being skipped ─────────── *)

(* [supports_session_resume] is the discriminator.  [Yaml_adapter.config_of]
   cannot serve as one: it is keyed by backend id, so before this change it
   returned the YAML config for the native too -- which is the very bug
   [test_config_of_is_none_for_natives] below pins. *)
let test_native_wins_the_id_collision () =
  with_isolated_home @@ fun _home ->
  fresh () ;
  Bootstrap.ensure () ;
  List.iter
    (fun id ->
      match Registry.get id with
      | None -> Alcotest.failf "%s not registered" id
      | Some b ->
          Alcotest.(check bool)
            (Printf.sprintf "%s is the native backend (session_resume)" id)
            true
            (Agentic_backend.supports_session_resume b))
    ["claude-code"; "gemini-cli"]

let test_no_replacement_warning_for_builtins () =
  with_isolated_home @@ fun _home ->
  let ws =
    with_captured_warnings (fun () ->
        fresh () ;
        Bootstrap.ensure ())
  in
  match replacement_warnings ws with
  | [] -> ()
  | ws ->
      Alcotest.failf
        "expected no id-collision warnings, got %d:\n%s"
        (List.length ws)
        (String.concat "\n" ws)

(* ── FR-004: config_of's documented contract holds again ──────────────── *)

(* The skip in [Adapter_loader] is placed BEFORE [Yaml_adapter.make_backend]
   precisely because that function's side effect is
   [Hashtbl.replace config_table cfg.name cfg].  Move the skip after it and
   this test is the one that fails -- the warnings would stay gone, so
   nothing else here would notice. *)
let test_config_of_is_none_for_natives () =
  with_isolated_home @@ fun _home ->
  fresh () ;
  Bootstrap.ensure () ;
  List.iter
    (fun id ->
      match Registry.get id with
      | None -> Alcotest.failf "%s not registered" id
      | Some b ->
          Alcotest.(check bool)
            (Printf.sprintf "config_of %s is None (native)" id)
            true
            (Yaml_adapter.config_of b = None))
    ["claude-code"; "gemini-cli"; "codex"; "opencode"; "copilot-cli"] ;
  (* ...and the YAML-only adapter still reports its config, so the test above
     cannot pass by config_of having been broken to always return None. *)
  match Registry.get "pi" with
  | None -> Alcotest.fail "pi not registered"
  | Some b ->
      Alcotest.(check bool)
        "config_of pi is Some (genuinely YAML)"
        true
        (Yaml_adapter.config_of b <> None)

(* ── FR-003c: an explicit override still overrides, and still warns ───── *)

let test_project_local_adapter_still_overrides_a_native () =
  with_isolated_home @@ fun _home ->
  let pd = Filename.temp_file "cabal_bootstrap_proj_" "" in
  Sys.remove pd ;
  Unix.mkdir pd 0o755 ;
  let dir = Filename.concat pd ".cabal" in
  Unix.mkdir dir 0o755 ;
  let dir = Filename.concat dir "adapters" in
  Unix.mkdir dir 0o755 ;
  let oc = open_out (Filename.concat dir "claude-code.yaml") in
  output_string
    oc
    "name: claude-code\n\
     display_name: Overridden By Project\n\
     invocation_command: /bin/true\n\
     template_set: default\n" ;
  close_out oc ;
  let ws =
    with_captured_warnings (fun () ->
        fresh () ;
        Bootstrap.ensure ~project_dir:pd ())
  in
  (match Registry.get "claude-code" with
  | None -> Alcotest.fail "claude-code not registered"
  | Some b ->
      (* A YAML-built backend hard-codes session_resume = false. *)
      Alcotest.(check bool)
        "the project-local adapter displaced the native"
        false
        (Agentic_backend.supports_session_resume b) ;
      Alcotest.(check bool)
        "and config_of now reports it, correctly"
        true
        (Yaml_adapter.config_of b <> None)) ;
  Alcotest.(check int)
    "a deliberate override warns exactly once"
    1
    (List.length (replacement_warnings ws))

(* ── FR-002: the memo ─────────────────────────────────────────────────── *)

let test_second_ensure_is_a_no_op () =
  with_isolated_home @@ fun _home ->
  fresh () ;
  Bootstrap.ensure () ;
  let ws = with_captured_warnings (fun () -> Bootstrap.ensure ()) in
  Alcotest.(check int)
    "re-running ensure registers nothing again"
    0
    (List.length (replacement_warnings ws))

let test_a_different_project_dir_re_runs () =
  with_isolated_home @@ fun _home ->
  fresh () ;
  Bootstrap.ensure ~project_dir:"/nonexistent-a" () ;
  (* A second root with a project-local adapter: if the memo wrongly keyed on
     "have I ever run" rather than on the argument, this adapter is never
     seen and [zzz-bootstrap] does not resolve. *)
  let pd = Filename.temp_file "cabal_bootstrap_proj2_" "" in
  Sys.remove pd ;
  Unix.mkdir pd 0o755 ;
  let dir = Filename.concat pd ".cabal" in
  Unix.mkdir dir 0o755 ;
  let dir = Filename.concat dir "adapters" in
  Unix.mkdir dir 0o755 ;
  let oc = open_out (Filename.concat dir "zzz.yaml") in
  output_string
    oc
    "name: zzz-bootstrap\n\
     display_name: Second Root\n\
     invocation_command: /bin/true\n\
     template_set: default\n" ;
  close_out oc ;
  Bootstrap.ensure ~project_dir:pd () ;
  Alcotest.(check bool)
    "a different project_dir re-runs and picks up its adapters"
    true
    (Registry.get "zzz-bootstrap" <> None)

(* ── FR-005: the capability is static, not registration-dependent ─────── *)

let test_mock_agent_read_only_without_any_registration () =
  fresh () ;
  Alcotest.(check bool)
    "supports_read_only mock-agent, with an empty registry"
    true
    (Backend_registry.supports_read_only "mock-agent") ;
  (* Selectable when named, never suggested.  [read_only_safe_backend_ids] is
     the list a human is shown as "use one of these instead"; a stub that
     replays canned fixture text would approve whatever the fixture says, so
     advertising it as a safe validator is the fail-open this task is about.
     The two facts are genuinely different and this pins both. *)
  Alcotest.(check bool)
    "but it is NOT advertised as a read-only-safe alternative"
    false
    (List.mem "mock-agent" (Backend_registry.read_only_safe_backend_ids ()))

let () =
  Alcotest.run
    "bootstrap"
    [
      ( "registration",
        [
          Alcotest.test_case
            "order is a pinned contract"
            `Quick
            test_registration_order_is_pinned;
          Alcotest.test_case
            "native wins the id collision"
            `Quick
            test_native_wins_the_id_collision;
          Alcotest.test_case
            "builtins are skipped, not replaced"
            `Quick
            test_no_replacement_warning_for_builtins;
          Alcotest.test_case
            "config_of is None for natives"
            `Quick
            test_config_of_is_none_for_natives;
          Alcotest.test_case
            "project-local adapter still overrides, and warns"
            `Quick
            test_project_local_adapter_still_overrides_a_native;
        ] );
      ( "idempotence",
        [
          Alcotest.test_case
            "second ensure is a no-op"
            `Quick
            test_second_ensure_is_a_no_op;
          Alcotest.test_case
            "a different project_dir re-runs"
            `Quick
            test_a_different_project_dir_re_runs;
        ] );
      ( "capabilities",
        [
          Alcotest.test_case
            "mock-agent read-only needs no registration"
            `Quick
            test_mock_agent_read_only_without_any_registration;
        ] );
    ]
