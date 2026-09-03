(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Adapter_loader and Yaml_adapter. *)

open Cabal

let () = Process_test_helper.run_if_requested ()

(* --- helpers --------------------------------------------------------------- *)

let with_temp_dir f =
  let dir = Filename.temp_file "epure_adapter_test_" "" in
  Unix.unlink dir ;
  Unix.mkdir dir 0o700 ;
  Fun.protect
    ~finally:(fun () ->
      let rm_rf d =
        let rec go path =
          if Sys.is_directory path then begin
            Array.iter
              (fun name -> go (Filename.concat path name))
              (Sys.readdir path) ;
            Unix.rmdir path
          end
          else Unix.unlink path
        in
        go d
      in
      if Sys.file_exists dir then rm_rf dir)
    (fun () -> f dir)

let write_file path content =
  let dir = Filename.dirname path in
  let rec mkdir_p d =
    if Sys.file_exists d then ()
    else begin
      mkdir_p (Filename.dirname d) ;
      Unix.mkdir d 0o700
    end
  in
  mkdir_p dir ;
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value ;
  Fun.protect
    ~finally:(fun () -> Unix.putenv name (Option.value ~default:"" previous))
    f

(* --- load_string tests ----------------------------------------------------- *)

let test_valid_yaml () =
  let yaml =
    {|
name: test-adapter
display_name: Test Adapter
invocation_command: "test-tool --flag -p -"
template_set: default
timeout_seconds: 120.0
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Error msg -> Alcotest.failf "unexpected error: %s" msg
  | Ok cfg ->
      Alcotest.(check string) "name" "test-adapter" cfg.name ;
      Alcotest.(check string) "display_name" "Test Adapter" cfg.display_name ;
      Alcotest.(check string)
        "invocation_command"
        "test-tool --flag -p -"
        cfg.invocation_command ;
      Alcotest.(check string) "template_set" "default" cfg.template_set ;
      Alcotest.(check string) "source" "test" cfg.source ;
      Alcotest.(check bool)
        "timeout approx"
        true
        (Float.abs (cfg.timeout_seconds -. 120.0) < 0.001)

let test_missing_name () =
  let yaml =
    {|
display_name: No Name Adapter
invocation_command: "some-tool -p -"
template_set: default
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error for missing name"
  | Error msg ->
      Alcotest.(check bool)
        "mentions name"
        true
        (let open String in
         let len = length "name" in
         let mlen = length msg in
         mlen >= len
         &&
         let rec find i =
           if i + len > mlen then false
           else if sub msg i len = "name" then true
           else find (i + 1)
         in
         find 0)

let test_missing_invocation_command () =
  let yaml =
    {|
name: no-command
display_name: No Command
template_set: default
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error for missing invocation_command"
  | Error msg ->
      Alcotest.(check bool)
        "mentions invocation_command"
        true
        (let open String in
         let needle = "invocation_command" in
         let nlen = length needle in
         let mlen = length msg in
         mlen >= nlen
         &&
         let rec find i =
           if i + nlen > mlen then false
           else if sub msg i nlen = needle then true
           else find (i + 1)
         in
         find 0)

let test_missing_template_set () =
  let yaml =
    {|
name: no-template-set
display_name: No Template Set
invocation_command: "tool -p -"
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error for missing template_set"
  | Error _ -> ()

let test_default_display_name () =
  (* When display_name is absent, name is used *)
  let yaml =
    {|
name: minimal
invocation_command: "tool -p -"
template_set: default
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Error msg -> Alcotest.failf "unexpected error: %s" msg
  | Ok cfg ->
      Alcotest.(check string)
        "display_name defaults to name"
        "minimal"
        cfg.display_name

let test_malformed_yaml () =
  let yaml = "{{ not valid yaml" in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error for malformed YAML"
  | Error _ -> ()

let test_top_level_not_a_mapping () =
  let yaml = "- just\n- a\n- list\n" in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok _ -> Alcotest.fail "expected Error when top level is a sequence"
  | Error _ -> ()

let test_env_field_not_a_mapping () =
  (* env must be a mapping; supplying a scalar should produce an empty
     mapping (no env), but the loader should still succeed for other fields. *)
  let yaml =
    {|
name: bad-env
display_name: Bad Env
invocation_command: "tool -p -"
template_set: default
env: "not a mapping"
|}
  in
  match Adapter_loader.load_string ~source:"test" yaml with
  | Ok cfg ->
      Alcotest.(check int)
        "env scalar gives empty mapping"
        0
        (List.length cfg.env_mappings)
  | Error msg -> Alcotest.failf "loader rejected scalar env: %s" msg

(* ---- diagnostics: non-string env values must surface a warning ---------- *)

let with_captured_diagnostics f =
  let events = ref [] in
  let handler ev = events := ev :: !events in
  Diagnostics.set_handler handler ;
  Fun.protect
    ~finally:(fun () -> Diagnostics.reset_handler ())
    (fun () ->
      f () ;
      List.rev !events)

let warn_count =
  List.fold_left
    (fun acc ev ->
      match ev with Diagnostics.Log (Warn, _) -> acc + 1 | _ -> acc)
    0

let test_non_string_env_values_emit_warning () =
  let yaml =
    {|
name: tricky-env
display_name: Tricky Env
invocation_command: "tool -p -"
template_set: default
env:
  GOOD: "ok"
  BAD: 1234
  ALSO_BAD: true
|}
  in
  let events =
    with_captured_diagnostics (fun () ->
        match Adapter_loader.load_string ~source:"test" yaml with
        | Ok cfg ->
            Alcotest.(check int)
              "only string-valued env mappings survive"
              1
              (List.length cfg.env_mappings)
        | Error e -> Alcotest.failf "loader rejected mixed env: %s" e)
  in
  Alcotest.(check int)
    "two non-string env values produce two warnings"
    2
    (warn_count events)

(* --- load_dir tests -------------------------------------------------------- *)

let test_load_dir_empty () =
  with_temp_dir (fun dir ->
      let results = Adapter_loader.load_dir dir in
      Alcotest.(check int) "empty dir = no results" 0 (List.length results))

let test_load_dir_valid () =
  with_temp_dir (fun dir ->
      write_file
        (Filename.concat dir "my-tool.yaml")
        {|
name: my-tool
display_name: My Tool
invocation_command: "my-tool -p -"
template_set: default
|} ;
      let results = Adapter_loader.load_dir dir in
      Alcotest.(check int) "one result" 1 (List.length results) ;
      match results with
      | [("my-tool.yaml", Ok cfg)] ->
          Alcotest.(check string) "name" "my-tool" cfg.name
      | [("my-tool.yaml", Error msg)] ->
          Alcotest.failf "unexpected error: %s" msg
      | _ -> Alcotest.fail "unexpected result structure")

let test_load_dir_mixed () =
  with_temp_dir (fun dir ->
      write_file
        (Filename.concat dir "good.yaml")
        {|
name: good
invocation_command: "good -p -"
template_set: default
|} ;
      write_file (Filename.concat dir "bad.yaml") "{{ invalid yaml" ;
      let results = Adapter_loader.load_dir dir in
      Alcotest.(check int) "two results" 2 (List.length results) ;
      let successes =
        List.filter
          (fun (_, r) -> match r with Ok _ -> true | _ -> false)
          results
      in
      let errors =
        List.filter
          (fun (_, r) -> match r with Error _ -> true | _ -> false)
          results
      in
      Alcotest.(check int) "one success" 1 (List.length successes) ;
      Alcotest.(check int) "one error" 1 (List.length errors))

(* --- register_all and project-local override ------------------------------- *)

let test_project_local_override () =
  with_temp_dir (fun tmpdir ->
      (* Create a project-local adapter under id "gemini". The builtin
         registry exposes "gemini-cli" — this test exercises arbitrary
         user-named adapters being layered on top of the builtins. *)
      let adapters_dir =
        Filename.concat tmpdir (Filename.concat ".cabal" "adapters")
      in
      write_file
        (Filename.concat adapters_dir "gemini.yaml")
        {|
name: gemini
display_name: Gemini Override
invocation_command: "gemini-custom --flag -p -"
template_set: custom
|} ;
      Registry.clear () ;
      Adapter_loader.register_all ~project_dir:tmpdir () ;
      (* Project-local "gemini" should have overridden the builtin *)
      match Registry.get "gemini" with
      | None -> Alcotest.fail "gemini not registered"
      | Some backend ->
          Alcotest.(check string)
            "gemini name overridden"
            "Gemini Override"
            (Agentic_backend.name backend))

let test_builtin_adapters_registered () =
  Registry.clear () ;
  Adapter_loader.register_all () ;
  let expected =
    ["claude-code"; "gemini-cli"; "copilot-cli"; "codex"; "opencode"]
  in
  List.iter
    (fun name ->
      match Registry.get name with
      | None -> Alcotest.failf "builtin '%s' not registered" name
      | Some _ -> ())
    expected

let rec flag_value flag = function
  | candidate :: value :: _ when candidate = flag -> Some value
  | _ :: rest -> flag_value flag rest
  | [] -> None

let test_builtin_claude_yaml_caps_native_web_tools () =
  let backends =
    match Adapter_loader.embedded_backends () with
    | Ok backends -> backends
    | Error message -> Alcotest.fail message
  in
  let backend =
    match
      List.find_opt
        (fun backend -> Agentic_backend.id backend = "claude-code")
        backends
    with
    | Some backend -> backend
    | None -> Alcotest.fail "embedded Claude YAML backend missing"
  in
  let config =
    match Yaml_adapter.config_of backend with
    | Some config -> config
    | None -> Alcotest.fail "embedded Claude backend lost YAML config"
  in
  let argv =
    String.split_on_char ' ' (String.trim config.invocation_command)
    |> List.filter (fun value -> value <> "")
  in
  let fixed_native_tools = "Read,Glob,Grep,Bash,Edit,Write,Task" in
  Alcotest.(check (option string))
    "--tools is the global-config-proof availability ceiling"
    (Some fixed_native_tools)
    (flag_value "--tools" argv) ;
  Alcotest.(check (option string))
    "allowed tools cannot widen the native ceiling"
    (Some fixed_native_tools)
    (flag_value "--allowedTools" argv) ;
  Alcotest.(check (option string))
    "project settings are not a setting source"
    (Some "user")
    (flag_value "--setting-sources" argv) ;
  Alcotest.(check bool)
    "unrelated MCP discovery is disabled" true
    (List.mem "--strict-mcp-config" argv) ;
  Alcotest.(check bool)
    "native WebSearch is unavailable" false
    (List.exists
       (fun value -> List.mem "WebSearch" (String.split_on_char ',' value))
       argv) ;
  Alcotest.(check bool)
    "native WebFetch is unavailable" false
    (List.exists
       (fun value -> List.mem "WebFetch" (String.split_on_char ',' value))
       argv) ;
  Alcotest.(check bool)
    "Bash remains available; native web policy is not a total-egress promise"
    true
    (String.split_on_char ',' fixed_native_tools |> List.mem "Bash")

let test_extensible_claude_invocation_caps_config_web_tools () =
  Process_test_helper.install_launcher () ;
  with_temp_dir @@ fun root ->
  let home = Filename.concat root "home" in
  let working_dir = Filename.concat root "workspace" in
  let bin_dir = Filename.concat root "bin" in
  List.iter (fun path -> Unix.mkdir path 0o700) [home; working_dir; bin_dir] ;
  let permissive_settings =
    {|{"permissions":{"allow":["WebSearch","WebFetch"]}}|}
  in
  write_file
    (Filename.concat home ".claude/settings.json")
    permissive_settings ;
  write_file
    (Filename.concat working_dir ".claude/settings.json")
    permissive_settings ;
  let args_path = Filename.concat root "claude-args" in
  let script =
    String.concat ""
      [
        "#!/bin/sh\nset -eu\n";
        Printf.sprintf
          "for arg in \"$@\"; do printf '%%s\\n' \"$arg\"; done > %s\n"
          (Filename.quote args_path);
        "cat >/dev/null\n";
        "printf '%s\\n' '{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"result\":\"ok\"}'\n";
      ]
  in
  let claude = Filename.concat bin_dir "claude" in
  write_file claude script ;
  Unix.chmod claude 0o700 ;
  let path =
    match Sys.getenv_opt "PATH" with
    | Some current when current <> "" -> bin_dir ^ ":" ^ current
    | Some _ | None -> bin_dir
  in
  with_env "HOME" home @@ fun () ->
  with_env "PATH" path @@ fun () ->
  Registry.clear () ;
  (match
     Runtime_bootstrap.register_runtime ~project_dir:working_dir
       ~profile:Runtime_bootstrap.Extensible ()
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error)) ;
  let backend =
    match Registry.get "claude-code" with
    | Some backend -> backend
    | None -> Alcotest.fail "Extensible Claude backend missing"
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec = Backend_types.make_task_spec ~prompt:"test" ~working_dir () in
  let result = Agentic_backend.run_task ~sw ~env backend spec in
  Alcotest.(check bool)
    "fake Extensible Claude succeeds" true
    (result.Backend_types.status = Backend_types.Success) ;
  let argv =
    read_file args_path |> String.split_on_char '\n'
    |> List.filter (fun value -> value <> "")
  in
  let fixed_native_tools = "Read,Glob,Grep,Bash,Edit,Write,Task" in
  Alcotest.(check (option string))
    "global permissions cannot widen --tools"
    (Some fixed_native_tools)
    (flag_value "--tools" argv) ;
  Alcotest.(check (option string))
    "project permissions cannot widen --allowedTools"
    (Some fixed_native_tools)
    (flag_value "--allowedTools" argv) ;
  Alcotest.(check (option string))
    "project settings remain excluded"
    (Some "user")
    (flag_value "--setting-sources" argv) ;
  Alcotest.(check bool)
    "native web tools absent despite permissive configs" false
    (List.exists
       (fun value ->
         let tools = String.split_on_char ',' value in
         List.mem "WebSearch" tools || List.mem "WebFetch" tools)
       argv) ;
  Registry.clear ()

let test_extensible_builtin_claude_requires_exact_terminal () =
  Process_test_helper.install_launcher () ;
  let protocol_failure =
    "Claude Code protocol failure: missing or invalid terminal result"
  in
  let cases =
    [
      ( "assistant-only",
        [
          {|{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":"not final"}]}}|};
        ] );
      ("malformed", ["not-json-private-output"]);
      ( "error",
        [
          {|{"type":"result","subtype":"error_during_execution","is_error":true,"result":"private error"}|};
        ] );
      ( "duplicate-result",
        [
          {|{"type":"result","subtype":"success","is_error":false,"result":"first"}|};
          {|{"type":"result","subtype":"success","is_error":false,"result":"second"}|};
        ] );
      ( "record-after-result",
        [
          {|{"type":"result","subtype":"success","is_error":false,"result":"not terminal"}|};
          {|{"type":"system","subtype":"status","message":"private trailing record"}|};
        ] );
    ]
  in
  List.iter
    (fun (label, output_lines) ->
      with_temp_dir @@ fun root ->
      let home = Filename.concat root "home" in
      let working_dir = Filename.concat root "workspace" in
      let bin_dir = Filename.concat root "bin" in
      List.iter (fun path -> Unix.mkdir path 0o700) [home; working_dir; bin_dir] ;
      let output =
        output_lines
        |> List.map (fun line ->
            Printf.sprintf "printf '%%s\\n' %s\n" (Filename.quote line))
        |> String.concat ""
      in
      let claude = Filename.concat bin_dir "claude" in
      write_file claude ("#!/bin/sh\nset -eu\ncat >/dev/null\n" ^ output) ;
      Unix.chmod claude 0o700 ;
      let path =
        match Sys.getenv_opt "PATH" with
        | Some current when current <> "" -> bin_dir ^ ":" ^ current
        | Some _ | None -> bin_dir
      in
      with_env "HOME" home @@ fun () ->
      with_env "PATH" path @@ fun () ->
      Registry.clear () ;
      (match
         Runtime_bootstrap.register_runtime ~project_dir:working_dir
           ~profile:Runtime_bootstrap.Extensible ()
       with
      | Ok () -> ()
      | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error)) ;
      let backend =
        match Registry.get "claude-code" with
        | Some backend -> backend
        | None -> Alcotest.fail "Extensible Claude backend missing"
      in
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let spec = Backend_types.make_task_spec ~prompt:"test" ~working_dir () in
      let result = Agentic_backend.run_task ~sw ~env backend spec in
      (match result.Backend_types.status with
      | Backend_types.Failed message ->
          Alcotest.(check string)
            (label ^ " fixed failure") protocol_failure message
      | Success -> Alcotest.fail (label ^ " unexpectedly succeeded")
      | Timeout -> Alcotest.fail (label ^ " unexpectedly timed out")
      | Cancelled -> Alcotest.fail (label ^ " unexpectedly cancelled")) ;
      Alcotest.(check string)
        (label ^ " suppresses final text") "" result.agent_text ;
      Alcotest.(check string)
        (label ^ " sanitizes stderr") protocol_failure result.stderr ;
      Registry.clear ())
    cases

let test_claude_user_override_retains_generic_semantics () =
  Process_test_helper.install_launcher () ;
  with_temp_dir @@ fun working_dir ->
  let executable = Filename.concat working_dir "custom-claude" in
  write_file executable
    {|#!/bin/sh
set -eu
cat >/dev/null
printf '%s\n' '{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":"custom output"}]}}'
|} ;
  Unix.chmod executable 0o700 ;
  let config : Yaml_adapter.config =
    {
      name = "claude-code";
      display_name = "Custom Claude";
      invocation_command = executable;
      template_set = "custom";
      env_mappings = [];
      timeout_seconds = 3.0;
      source = "project adapter";
      models = [];
    }
  in
  let backend = Yaml_adapter.make_backend config in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec = Backend_types.make_task_spec ~prompt:"test" ~working_dir () in
  let result = Agentic_backend.run_task ~sw ~env backend spec in
  Alcotest.(check bool)
    "custom same-id adapter keeps generic exit-zero success" true
    (result.status = Backend_types.Success) ;
  Alcotest.(check string)
    "custom same-id adapter keeps generic structured parser"
    "custom output" result.agent_text

(* --- validate tests -------------------------------------------------------- *)

let test_validate_ok () =
  let cfg : Yaml_adapter.config =
    {
      name = "test";
      display_name = "Test";
      invocation_command = "test -p -";
      template_set = "default";
      env_mappings = [];
      timeout_seconds = 300.0;
      source = "test";
      models = [];
    }
  in
  match Adapter_loader.validate cfg with
  | Ok () -> ()
  | Error field -> Alcotest.failf "unexpected validation error: %s" field

let test_validate_empty_name () =
  let cfg : Yaml_adapter.config =
    {
      name = "";
      display_name = "Test";
      invocation_command = "test -p -";
      template_set = "default";
      env_mappings = [];
      timeout_seconds = 300.0;
      source = "test";
      models = [];
    }
  in
  match Adapter_loader.validate cfg with
  | Error "name" -> ()
  | Error f -> Alcotest.failf "expected 'name' error, got '%s'" f
  | Ok () -> Alcotest.fail "expected Error"

let test_yaml_config_metadata_is_bounded_and_cleared () =
  Registry.clear () ;
  let config source : Yaml_adapter.config =
    {
      name = "metadata-lifecycle";
      display_name = source;
      invocation_command = "false";
      template_set = "default";
      env_mappings = [];
      timeout_seconds = 1.0;
      source;
      models = [];
    }
  in
  let first = Yaml_adapter.make_backend (config "first") in
  let second = Yaml_adapter.make_backend (config "second") in
  Alcotest.(check bool)
    "replacement drops stale same-id package metadata"
    true
    (Option.is_none (Yaml_adapter.config_of first)) ;
  Alcotest.(check bool)
    "latest same-id package metadata remains"
    true
    (Option.is_some (Yaml_adapter.config_of second)) ;
  Registry.clear () ;
  Alcotest.(check bool)
    "registry reset clears YAML package metadata"
    true
    (Option.is_none (Yaml_adapter.config_of second))

let test_builtin_id_yaml_override_keeps_private_json_raw_only () =
  Process_test_helper.install_launcher () ;
  let run_case name action expected_raw =
    with_temp_dir @@ fun working_dir ->
    let invocation_command =
      String.concat
        " "
        [
          Unix.realpath Sys.executable_name;
          "--process-descendant-helper";
          action;
        ]
    in
    let adapter_path =
      Filename.concat
        working_dir
        (Filename.concat ".cabal/adapters" (name ^ ".yaml"))
    in
    write_file
      adapter_path
      (Printf.sprintf
         "name: %s\ndisplay_name: Structured override\ninvocation_command: \"%s\"\ntemplate_set: default\ntimeout_seconds: 3.0\n"
         name
         invocation_command) ;
    Registry.clear () ;
    (match
       Runtime_bootstrap.register_runtime
         ~project_dir:working_dir
         ~profile:Runtime_bootstrap.Extensible
         ()
     with
    | Ok () -> ()
    | Error error -> Alcotest.fail (Runtime_bootstrap.render_error error)) ;
    let backend =
      match Registry.get name with
      | Some backend -> backend
      | None -> Alcotest.failf "%s override was not registered" name
    in
    Eio_posix.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let events = ref [] in
    let raw = ref [] in
    let sink =
      Task_event.create_sink
        ~sw
        ~now:(fun () -> 0.0)
        ~on_event:(fun event -> events := event :: !events)
        ()
    in
    let context =
      Task_execution_context.create ~remaining_time:(fun () -> None) sink
    in
    let spec =
      Backend_types.make_task_spec ~prompt:"test" ~working_dir ()
    in
    let result =
      Agentic_backend.run_task_with_context
        ~sw
        ~env
        ~context
        ~on_raw_line:(fun line -> raw := line :: !raw)
        backend
        spec
    in
    Task_event.emit_terminal sink Task_event.Succeeded ;
    Eio.Promise.await (Task_event.Private.delivery_complete sink) ;
    Alcotest.(check bool)
      (name ^ " process succeeds")
      true
      (result.Backend_types.status = Backend_types.Success) ;
    Alcotest.(check string) (name ^ " private JSON is not result text") "" result.agent_text ;
    Alcotest.(check (list string))
      (name ^ " raw callback is unchanged")
      [expected_raw]
      (List.rev !raw) ;
    let normalized_text =
      List.filter_map
        (fun event ->
          match event.Task_event.payload with
          | Task_event.Agent_text_delta text -> Some text
          | _ -> None)
        !events
    in
    Alcotest.(check (list string))
      (name ^ " private JSON is never normalized")
      []
      normalized_text ;
    Registry.clear ()
  in
  run_case
    "claude-code"
    "emit-private-claude"
    {|{"type":"result","is_error":true,"result":"private claude failure"}|} ;
  run_case
    "codex"
    "emit-private-codex"
    {|{"type":"item.completed","item":{"type":"reasoning","text":"private codex chain"}}|} ;
  run_case
    "claude-code"
    "emit-malformed-claude"
    "not-json-private-claude-output"

(* --- test suite ------------------------------------------------------------ *)

let () =
  Alcotest.run
    "Adapter_loader"
    [
      ( "load_string",
        [
          Alcotest.test_case "valid YAML" `Quick test_valid_yaml;
          Alcotest.test_case "missing name" `Quick test_missing_name;
          Alcotest.test_case
            "missing invocation_command"
            `Quick
            test_missing_invocation_command;
          Alcotest.test_case
            "missing template_set"
            `Quick
            test_missing_template_set;
          Alcotest.test_case
            "default display_name"
            `Quick
            test_default_display_name;
          Alcotest.test_case "malformed YAML" `Quick test_malformed_yaml;
          Alcotest.test_case
            "top level not a mapping"
            `Quick
            test_top_level_not_a_mapping;
          Alcotest.test_case
            "env field not a mapping"
            `Quick
            test_env_field_not_a_mapping;
          Alcotest.test_case
            "non-string env values emit warnings"
            `Quick
            test_non_string_env_values_emit_warning;
        ] );
      ( "load_dir",
        [
          Alcotest.test_case "empty dir" `Quick test_load_dir_empty;
          Alcotest.test_case "valid file" `Quick test_load_dir_valid;
          Alcotest.test_case "mixed valid/invalid" `Quick test_load_dir_mixed;
        ] );
      ( "register_all",
        [
          Alcotest.test_case
            "builtins registered"
            `Quick
            test_builtin_adapters_registered;
          Alcotest.test_case
            "built-in Claude caps native web tools"
            `Quick
            test_builtin_claude_yaml_caps_native_web_tools;
          Alcotest.test_case
            "Extensible Claude caps permissive config web tools"
            `Quick
            test_extensible_claude_invocation_caps_config_web_tools;
          Alcotest.test_case
            "Extensible built-in Claude requires exact terminal"
            `Quick
            test_extensible_builtin_claude_requires_exact_terminal;
          Alcotest.test_case
            "same-id Claude user override keeps generic semantics"
            `Quick
            test_claude_user_override_retains_generic_semantics;
          Alcotest.test_case
            "project-local override"
            `Quick
            test_project_local_override;
        ] );
      ( "validate",
        [
          Alcotest.test_case "valid config" `Quick test_validate_ok;
          Alcotest.test_case "empty name" `Quick test_validate_empty_name;
        ] );
      ( "metadata lifecycle",
        [
          Alcotest.test_case
            "same-id replacement is bounded and clear resets"
            `Quick
            test_yaml_config_metadata_is_bounded_and_cleared;
        ] );
      ( "structured override privacy",
        [
          Alcotest.test_case
            "private JSON remains raw-only"
            `Quick
            test_builtin_id_yaml_override_keeps_private_json_raw_only;
        ] );
    ]
