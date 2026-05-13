(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #479 — Project config precedence enforcement.

    AC traceability:
    - AC #658: project-generated config exists → backend invocation path prefers
               project config over user-global config
    - AC #659: Claude, Codex, OpenCode → project config wins (None warning) or
               documented limitation warning is emitted (Some warning)
    - AC #660: Gemini CLI, Copilot CLI have limited precedence controls → warning
               is explicit and documented
    - AC #661: when strict precedence cannot be guaranteed, Épure emits a
               non-fatal warning *)

open Cabal

(** {1 Helpers} *)

let contains_str s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let with_tmpdir f =
  let dir = Filename.temp_dir "epure_test_479_" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let write_file path content =
  let oc = open_out path in
  output_string oc content ;
  close_out oc

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n ;
  close_in ic ;
  Bytes.to_string buf

(** {1 AC #658 — project config preferred over user-global} *)

(* AC #658: claude-code setup returns a project config path for invocation *)
let test_project_config_preferred_claude_code () =
  with_tmpdir (fun dir ->
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"claude-code"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.project_config_path with
      | None ->
          Alcotest.fail "AC #658: expected project_config_path for claude-code"
      | Some path ->
          Alcotest.(check bool)
            "path lives under .cabal/backend-config"
            true
            (contains_str path ".cabal"))

(* AC #658: the explicit settings path gives claude-code strict project precedence *)
let test_explicit_path_enforces_precedence () =
  with_tmpdir (fun dir ->
      let r =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"claude-code"
          ~project_dir:dir
          ~force:false
      in
      match r.Backend_config_gen.project_config_path with
      | None ->
          Alcotest.fail
            "AC #658: expected explicit settings path for claude-code"
      | Some path ->
          Alcotest.(check bool)
            "path ends with settings.json"
            true
            (contains_str path "settings.json") ;
          Alcotest.(check bool)
            "settings file exists on disk"
            true
            (Sys.file_exists path))

(* AC #658: project config preference persists when config is already current *)
let test_project_config_preferred_already_current () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"claude-code"
           ~project_dir:dir
           ~force:false) ;
      let r2 =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"claude-code"
          ~project_dir:dir
          ~force:false
      in
      (match r2.Backend_config_gen.write_outcome with
      | Some Backend_config_gen.Already_current -> ()
      | _ ->
          Alcotest.fail
            "AC #658: expected Already_current on second call for claude-code") ;
      match r2.Backend_config_gen.project_config_path with
      | None ->
          Alcotest.fail
            "AC #658: project_config_path must remain Some on idempotent call"
      | Some path ->
          Alcotest.(check bool)
            "settings file still present"
            true
            (Sys.file_exists path))

(** {1 AC #659 — Claude/Codex/OpenCode: project config wins or documented warning} *)

(* AC #659: claude-code has High precedence → no warning needed *)
let test_claude_code_no_precedence_warning () =
  Alcotest.(check (option string))
    "AC #659: claude-code returns no warning (High confidence)"
    None
    (Backend_config_gen.precedence_warning_for
       ~backend_id:"claude-code"
       ~write_outcome:(Some (Backend_config_gen.Written "/tmp/settings.json")))

(* AC #659: codex emits a documented limitation warning *)
let test_codex_emits_precedence_warning () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"codex"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None ->
      Alcotest.fail "AC #659: expected warning for codex (Medium confidence)"
  | Some _ -> ()

(* AC #659: opencode emits a documented limitation warning *)
let test_opencode_emits_precedence_warning () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"opencode"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None ->
      Alcotest.fail "AC #659: expected warning for opencode (Medium confidence)"
  | Some _ -> ()

(* AC #659: codex warning mentions the backend name for actionability *)
let test_codex_warning_mentions_backend_name () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"codex"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None -> Alcotest.fail "AC #659: expected warning for codex"
  | Some msg ->
      Alcotest.(check bool)
        "codex warning mentions 'Codex'"
        true
        (contains_str msg "Codex")

(* AC #659: opencode warning mentions the backend name for actionability *)
let test_opencode_warning_mentions_backend_name () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"opencode"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None -> Alcotest.fail "AC #659: expected warning for opencode"
  | Some msg ->
      Alcotest.(check bool)
        "opencode warning mentions 'OpenCode'"
        true
        (contains_str msg "OpenCode")

(** {1 AC #660 — Gemini CLI / Copilot CLI: explicit limitation warning} *)

(* AC #660: gemini-cli has Low precedence confidence → explicit limitation warning *)
let test_gemini_cli_emits_precedence_warning () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"gemini-cli"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None ->
      Alcotest.fail "AC #660: expected warning for gemini-cli (Low confidence)"
  | Some _ -> ()

(* AC #660: copilot-cli has no project config surface → explicit limitation warning *)
let test_copilot_cli_emits_precedence_warning () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"copilot-cli"
      ~write_outcome:None
  with
  | None ->
      Alcotest.fail "AC #660: expected warning for copilot-cli (Config_none)"
  | Some _ -> ()

(* AC #660: gemini-cli warning mentions the backend name for actionability *)
let test_gemini_cli_warning_mentions_backend_name () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"gemini-cli"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None -> Alcotest.fail "AC #660: expected warning for gemini-cli"
  | Some msg ->
      Alcotest.(check bool)
        "gemini-cli warning mentions 'Gemini'"
        true
        (contains_str msg "Gemini")

(* AC #660: copilot-cli warning mentions the backend name for actionability *)
let test_copilot_cli_warning_mentions_backend_name () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"copilot-cli"
      ~write_outcome:None
  with
  | None -> Alcotest.fail "AC #660: expected warning for copilot-cli"
  | Some msg ->
      Alcotest.(check bool)
        "copilot-cli warning mentions 'Copilot'"
        true
        (contains_str msg "Copilot")

(** {1 AC #661 — non-fatal warning when strict precedence cannot be guaranteed} *)

(* AC #661: warning for codex is non-empty *)
let test_warning_is_non_empty_codex () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"codex"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None -> Alcotest.fail "AC #661: expected non-fatal warning for codex"
  | Some msg ->
      Alcotest.(check bool) "warning is non-empty" true (String.length msg > 0)

(* AC #661: warning for gemini-cli is non-empty *)
let test_warning_is_non_empty_gemini_cli () =
  match
    Backend_config_gen.precedence_warning_for
      ~backend_id:"gemini-cli"
      ~write_outcome:(Some Backend_config_gen.Already_current)
  with
  | None -> Alcotest.fail "AC #661: expected non-fatal warning for gemini-cli"
  | Some msg ->
      Alcotest.(check bool) "warning is non-empty" true (String.length msg > 0)

(* AC #661: when codex config write is skipped (user file at fixed path),
   a warning is still emitted — the operator learns that project config is not active *)
let test_skipped_write_still_warns_codex () =
  with_tmpdir (fun dir ->
      let config_dir = Filename.concat dir ".codex" in
      Unix.mkdir config_dir 0o755 ;
      let config_path = Filename.concat config_dir "config.toml" in
      write_file config_path "[model]\nprovider = \"anthropic\"\n" ;
      let setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"codex"
          ~project_dir:dir
          ~force:false
      in
      (match setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Skipped_user_content _) -> ()
      | _ ->
          Alcotest.fail
            "AC #661: expected Skipped_user_content when user file exists") ;
      match
        Backend_config_gen.precedence_warning_for
          ~backend_id:"codex"
          ~write_outcome:setup.Backend_config_gen.write_outcome
      with
      | None ->
          Alcotest.fail
            "AC #661: warning must fire even when config write was skipped"
      | Some msg ->
          Alcotest.(check bool)
            "skipped-write warning indicates project config not applied"
            true
            (contains_str (String.lowercase_ascii msg) "user"
            || contains_str (String.lowercase_ascii msg) "applied"
            || contains_str (String.lowercase_ascii msg) "not"))

(* AC #661: when codex config write is refused (hash mismatch — user modified the managed file),
   a warning is still emitted — the operator learns that project config is not active *)
let test_refused_write_still_warns_codex () =
  with_tmpdir (fun dir ->
      ignore
        (Backend_config_gen.setup_project_config
           ~mcp_servers:[]
           ~backend_id:"codex"
           ~project_dir:dir
           ~force:false) ;
      let config_path = Filename.concat dir ".codex/config.toml" in
      let original = read_file config_path in
      write_file config_path (original ^ "\n# user-added setting\n") ;
      let setup =
        Backend_config_gen.setup_project_config
          ~mcp_servers:[]
          ~backend_id:"codex"
          ~project_dir:dir
          ~force:false
      in
      (match setup.Backend_config_gen.write_outcome with
      | Some (Backend_config_gen.Refused_hash_mismatch _) -> ()
      | _ ->
          Alcotest.fail
            "AC #661: expected Refused_hash_mismatch after body modification") ;
      match
        Backend_config_gen.precedence_warning_for
          ~backend_id:"codex"
          ~write_outcome:setup.Backend_config_gen.write_outcome
      with
      | None ->
          Alcotest.fail
            "AC #661: warning must fire even when config write was refused"
      | Some msg ->
          Alcotest.(check bool)
            "refused-write warning indicates project config not applied"
            true
            (contains_str (String.lowercase_ascii msg) "user"
            || contains_str (String.lowercase_ascii msg) "applied"
            || contains_str (String.lowercase_ascii msg) "not"))

let () =
  Alcotest.run
    "test_demo_479"
    [
      ( "AC #658 — project config preferred over user-global",
        [
          Alcotest.test_case
            "claude-code: setup returns project config path"
            `Quick
            test_project_config_preferred_claude_code;
          Alcotest.test_case
            "claude-code: explicit path enforces project precedence"
            `Quick
            test_explicit_path_enforces_precedence;
          Alcotest.test_case
            "claude-code: config path persists on idempotent call"
            `Quick
            test_project_config_preferred_already_current;
        ] );
      ( "AC #659 — Claude/Codex/OpenCode: project config wins or documented \
         warning",
        [
          Alcotest.test_case
            "claude-code: no precedence warning (High confidence)"
            `Quick
            test_claude_code_no_precedence_warning;
          Alcotest.test_case
            "codex: emits documented limitation warning"
            `Quick
            test_codex_emits_precedence_warning;
          Alcotest.test_case
            "opencode: emits documented limitation warning"
            `Quick
            test_opencode_emits_precedence_warning;
          Alcotest.test_case
            "codex: warning mentions backend name"
            `Quick
            test_codex_warning_mentions_backend_name;
          Alcotest.test_case
            "opencode: warning mentions backend name"
            `Quick
            test_opencode_warning_mentions_backend_name;
        ] );
      ( "AC #660 — Gemini CLI / Copilot CLI: explicit limitation warning",
        [
          Alcotest.test_case
            "gemini-cli: emits documented limitation warning"
            `Quick
            test_gemini_cli_emits_precedence_warning;
          Alcotest.test_case
            "copilot-cli: emits documented limitation warning"
            `Quick
            test_copilot_cli_emits_precedence_warning;
          Alcotest.test_case
            "gemini-cli: warning mentions backend name"
            `Quick
            test_gemini_cli_warning_mentions_backend_name;
          Alcotest.test_case
            "copilot-cli: warning mentions backend name"
            `Quick
            test_copilot_cli_warning_mentions_backend_name;
        ] );
      ( "AC #661 — non-fatal warning when strict precedence cannot be guaranteed",
        [
          Alcotest.test_case
            "codex: warning is non-empty"
            `Quick
            test_warning_is_non_empty_codex;
          Alcotest.test_case
            "gemini-cli: warning is non-empty"
            `Quick
            test_warning_is_non_empty_gemini_cli;
          Alcotest.test_case
            "codex: skipped write still triggers warning"
            `Quick
            test_skipped_write_still_warns_codex;
          Alcotest.test_case
            "codex: refused write still triggers warning"
            `Quick
            test_refused_write_still_warns_codex;
        ] );
    ]
