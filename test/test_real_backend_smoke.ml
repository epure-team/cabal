(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Opt-in smoke tests for real agentic backend CLIs. *)

open Cabal
open Backend_types

let () = Process_test_helper.install_launcher ()

type model_source =
  | Static_model of string option
  | Codex_openai_mini
  | Opencode_openai_mini

type backend_case = {
  id : string;
  name : string;
  backend : Agentic_backend.t;
  model_source : model_source;
}

let backends =
  [
    {
      id = Claude_code.id;
      name = Claude_code.name;
      backend = (module Claude_code : Agentic_backend.S);
      model_source = Static_model (Some "haiku");
    };
    {
      id = Codex_cli.id;
      name = Codex_cli.name;
      backend = (module Codex_cli : Agentic_backend.S);
      model_source = Codex_openai_mini;
    };
    {
      id = Opencode_cli.id;
      name = Opencode_cli.name;
      backend = (module Opencode_cli : Agentic_backend.S);
      model_source = Opencode_openai_mini;
    };
    {
      id = Gemini_cli.id;
      name = Gemini_cli.name;
      backend = (module Gemini_cli : Agentic_backend.S);
      model_source = Static_model (Some "gemini-3-flash-preview");
    };
    {
      id = Copilot_cli.id;
      name = Copilot_cli.name;
      backend = (module Copilot_cli : Agentic_backend.S);
      model_source = Static_model None;
    };
  ]

let smoke_enabled () = Sys.getenv_opt "EPURE_REAL_BACKEND_SMOKE" = Some "1"

let sanitize_env_fragment id =
  String.map
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> Char.uppercase_ascii c
      | _ -> '_')
    id

let model_env_var id =
  "EPURE_REAL_BACKEND_SMOKE_MODEL_" ^ sanitize_env_fragment id

let command_line_of_argv argv =
  argv |> List.map Filename.quote |> String.concat " "

let contains_substring haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    i + nlen <= hlen && (String.sub haystack i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let first_mini_model models =
  List.find_opt (fun model -> contains_substring model "mini") models

let choose_codex_smoke_model models =
  if List.exists (( = ) "gpt-5.4-mini") models then Ok "gpt-5.4-mini"
  else
    match first_mini_model models with
    | Some model -> Ok model
    | None ->
        Error
          ("no Codex model id containing `mini` found in `codex debug models`; "
         ^ "set EPURE_REAL_BACKEND_SMOKE_MODEL_CODEX to override")

let choose_opencode_smoke_model models =
  if List.exists (( = ) "openai/gpt-5.4-mini") models then
    Ok "openai/gpt-5.4-mini"
  else
    match first_mini_model models with
    | Some model -> Ok model
    | None ->
        Error
          ("no OpenCode OpenAI model containing `mini` found in `opencode "
         ^ "models openai`; set EPURE_REAL_BACKEND_SMOKE_MODEL_OPENCODE to "
         ^ "override")

let dedupe_preserve_order values =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | value :: rest when List.mem value seen -> loop seen acc rest
    | value :: rest -> loop (value :: seen) (value :: acc) rest
  in
  loop [] [] values

let codex_model_ids_from_json output =
  let rec collect = function
    | `Assoc fields ->
        let direct =
          List.filter_map
            (function
              | ("slug" | "id"), `String value when String.trim value <> "" ->
                  Some (String.trim value)
              | _ -> None)
            fields
        in
        let nested =
          List.concat_map (fun (_key, value) -> collect value) fields
        in
        direct @ nested
    | `List items -> List.concat_map collect items
    | _ -> []
  in
  try Ok (Yojson.Safe.from_string output |> collect |> dedupe_preserve_order)
  with Yojson.Json_error msg -> Error ("invalid JSON: " ^ msg)

let opencode_model_ids_from_lines output =
  output |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun line -> String.starts_with ~prefix:"openai/" line)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let remove_if_exists path = try Sys.remove path with Sys_error _ -> ()

let process_status_text = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

let run_command_capture argv =
  match argv with
  | [] -> Error "empty command"
  | program :: _ ->
      let stdout_path = Filename.temp_file "epure_smoke_stdout_" ".txt" in
      let stderr_path = Filename.temp_file "epure_smoke_stderr_" ".txt" in
      Fun.protect
        ~finally:(fun () ->
          remove_if_exists stdout_path ;
          remove_if_exists stderr_path)
        (fun () ->
          try
            let stdin_fd = Unix.openfile Filename.null [Unix.O_RDONLY] 0 in
            let stdout_fd =
              Unix.openfile stdout_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
            in
            let stderr_fd =
              Unix.openfile stderr_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
            in
            let pid_result =
              try
                Ok
                  (Unix.create_process
                     program
                     (Array.of_list argv)
                     stdin_fd
                     stdout_fd
                     stderr_fd)
              with e -> Error (Printexc.to_string e)
            in
            List.iter
              (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
              [stdin_fd; stdout_fd; stderr_fd] ;
            match pid_result with
            | Error msg -> Error msg
            | Ok pid ->
                let _pid, status = Unix.waitpid [] pid in
                let stdout = read_file stdout_path in
                let stderr = String.trim (read_file stderr_path) in
                if status = Unix.WEXITED 0 then Ok stdout
                else
                  Error
                    (Printf.sprintf
                       "%s returned %s%s"
                       (command_line_of_argv argv)
                       (process_status_text status)
                       (if stderr = "" then "" else ": " ^ stderr))
          with e -> Error (Printexc.to_string e))

let discover_codex_smoke_model () =
  match run_command_capture ["codex"; "debug"; "models"] with
  | Error msg ->
      Error
        ("could not run `codex debug models` for smoke model discovery: " ^ msg)
  | Ok output -> (
      match codex_model_ids_from_json output with
      | Error msg ->
          Error ("could not parse `codex debug models` output: " ^ msg)
      | Ok models -> choose_codex_smoke_model models)

let discover_opencode_smoke_model () =
  match run_command_capture ["opencode"; "models"; "openai"] with
  | Error msg ->
      Error
        ("could not run `opencode models openai` for smoke model discovery: "
       ^ msg)
  | Ok output ->
      output |> opencode_model_ids_from_lines |> choose_opencode_smoke_model

let resolve_model ?(getenv = Sys.getenv_opt)
    ?(discover_codex = discover_codex_smoke_model)
    ?(discover_opencode = discover_opencode_smoke_model) case =
  match getenv (model_env_var case.id) with
  | Some model when String.trim model <> "" -> Ok (Some model)
  | _ -> (
      match case.model_source with
      | Static_model model -> Ok model
      | Codex_openai_mini -> (
          match discover_codex () with
          | Ok model -> Ok (Some model)
          | Error msg -> Error msg)
      | Opencode_openai_mini -> (
          match discover_opencode () with
          | Ok model -> Ok (Some model)
          | Error msg -> Error msg))

let test_choose_codex_prefers_gpt_54_mini () =
  Alcotest.(check (result string string))
    "preferred Codex mini"
    (Ok "gpt-5.4-mini")
    (choose_codex_smoke_model ["gpt-5.5"; "gpt-5.4-mini"; "gpt-5.4"])

let test_choose_codex_falls_back_to_any_mini () =
  Alcotest.(check (result string string))
    "fallback Codex mini"
    (Ok "gpt-5.3-codex-mini")
    (choose_codex_smoke_model ["gpt-5.5"; "gpt-5.3-codex-mini"])

let test_choose_codex_errors_without_mini () =
  match choose_codex_smoke_model ["gpt-5.5"; "gpt-5.4"] with
  | Ok model -> Alcotest.failf "unexpected Codex model %s" model
  | Error msg ->
      Alcotest.(check bool)
        "diagnostic mentions discovery command"
        true
        (contains_substring msg "codex debug models")

let test_parse_codex_model_ids_from_json () =
  let output =
    {|{"models":[{"slug":"gpt-5.4-mini"},{"id":"gpt-5.5"},{"nested":{"slug":"gpt-5.2"}}]}|}
  in
  Alcotest.(check (result (list string) string))
    "Codex model ids"
    (Ok ["gpt-5.4-mini"; "gpt-5.5"; "gpt-5.2"])
    (codex_model_ids_from_json output)

let test_choose_opencode_prefers_openai_gpt_54_mini () =
  Alcotest.(check (result string string))
    "preferred OpenCode mini"
    (Ok "openai/gpt-5.4-mini")
    (choose_opencode_smoke_model
       ["openai/gpt-5.5"; "openai/gpt-5.4-mini"; "openai/gpt-5.4"])

let test_parse_opencode_model_lines () =
  Alcotest.(check (list string))
    "OpenCode OpenAI models"
    ["openai/gpt-5.4-mini"; "openai/gpt-5.5"]
    (opencode_model_ids_from_lines
       "Models\nopenai/gpt-5.4-mini\nanthropic/claude\nopenai/gpt-5.5\n")

let test_choose_opencode_errors_without_mini () =
  match choose_opencode_smoke_model ["openai/gpt-5.5"; "openai/gpt-5.4"] with
  | Ok model -> Alcotest.failf "unexpected OpenCode model %s" model
  | Error msg ->
      Alcotest.(check bool)
        "diagnostic mentions discovery command"
        true
        (contains_substring msg "opencode models openai")

let test_resolve_model_override_skips_discovery () =
  let discover_not_called () = Alcotest.fail "discovery should not run" in
  Alcotest.(check (result (option string) string))
    "override model"
    (Ok (Some "manual-model"))
    (resolve_model
       ~getenv:(fun _ -> Some "manual-model")
       ~discover_codex:discover_not_called
       ~discover_opencode:discover_not_called
       (List.nth backends 1))

let backend_case id =
  match List.find_opt (fun case -> case.id = id) backends with
  | Some case -> case
  | None -> Alcotest.failf "smoke case not registered: %s" id

let copilot_case () = backend_case Copilot_cli.id

let command_argv_for_case_unsafe case spec =
  let command_from build =
    let argv, _stdin = build ~mcp_config_path:None spec in
    argv
  in
  if case.id = Claude_code.id then
    (* Claude's run_task writes an Épure-owned settings file and passes that
       generated path into build_command.  Do the same setup here so the smoke
       diagnostic shows the same argv path run_task will use. *)
    let setup =
      Backend_config_writer.setup_artifacts
        ~project_dir:spec.working_dir
        ~force:false
        (Claude_code.project_config_artifacts
           ~managed_namespace:Backend_types.default_managed_namespace
           ~mcp_servers:[]
           ~lsp_servers:[])
    in
    let argv, _stdin =
      Claude_code.build_command
        ~project_config_path:setup.project_config_path
        ~mcp_config_path:None
        spec
    in
    argv
  else if case.id = Codex_cli.id then command_from Codex_cli.build_command
  else if case.id = Opencode_cli.id then command_from Opencode_cli.build_command
  else if case.id = Gemini_cli.id then command_from Gemini_cli.build_command
  else if case.id = Copilot_cli.id then command_from Copilot_cli.build_command
  else invalid_arg ("no smoke command preview for backend " ^ case.id)

let command_argv_for_case case spec =
  try Ok (command_argv_for_case_unsafe case spec)
  with e -> Error ("could not build command preview: " ^ Printexc.to_string e)

let test_resolve_model_keeps_static_explicit_models () =
  let discover_not_called () = Alcotest.fail "discovery should not run" in
  let check_model label expected case =
    Alcotest.(check (result (option string) string))
      label
      (Ok (Some expected))
      (resolve_model
         ~getenv:(fun _ -> None)
         ~discover_codex:discover_not_called
         ~discover_opencode:discover_not_called
         case)
  in
  check_model "Claude explicit model" "haiku" (backend_case Claude_code.id) ;
  check_model
    "Gemini explicit model"
    "gemini-3-flash-preview"
    (backend_case Gemini_cli.id)

let test_resolve_model_dynamic_discovery_is_explicit () =
  Alcotest.(check (result (option string) string))
    "Codex discovered model"
    (Ok (Some "gpt-5.4-mini"))
    (resolve_model
       ~getenv:(fun _ -> None)
       ~discover_codex:(fun () -> Ok "gpt-5.4-mini")
       (backend_case Codex_cli.id)) ;
  Alcotest.(check (result (option string) string))
    "OpenCode discovered model"
    (Ok (Some "openai/gpt-5.4-mini"))
    (resolve_model
       ~getenv:(fun _ -> None)
       ~discover_opencode:(fun () -> Ok "openai/gpt-5.4-mini")
       (backend_case Opencode_cli.id))

let test_resolve_model_uses_backend_default_for_copilot () =
  Alcotest.(check (result (option string) string))
    "Copilot backend default"
    (Ok None)
    (resolve_model ~getenv:(fun _ -> None) (copilot_case ()))

let test_resolve_model_override_sets_copilot_model () =
  let env_var = model_env_var Copilot_cli.id in
  Alcotest.(check (result (option string) string))
    "Copilot override model"
    (Ok (Some "gpt-5.4-mini"))
    (resolve_model
       ~getenv:(fun name ->
         if name = env_var then Some "gpt-5.4-mini" else None)
       (copilot_case ()))

let model_discovery_tests =
  [
    ( "Codex prefers exact gpt-5.4-mini",
      `Quick,
      test_choose_codex_prefers_gpt_54_mini );
    ( "Codex falls back to any mini",
      `Quick,
      test_choose_codex_falls_back_to_any_mini );
    ("Codex errors without mini", `Quick, test_choose_codex_errors_without_mini);
    ("Codex parses debug JSON", `Quick, test_parse_codex_model_ids_from_json);
    ( "OpenCode prefers exact openai/gpt-5.4-mini",
      `Quick,
      test_choose_opencode_prefers_openai_gpt_54_mini );
    ("OpenCode parses model lines", `Quick, test_parse_opencode_model_lines);
    ( "OpenCode errors without mini",
      `Quick,
      test_choose_opencode_errors_without_mini );
    ( "model env override skips discovery",
      `Quick,
      test_resolve_model_override_skips_discovery );
    ( "static smoke models stay explicit",
      `Quick,
      test_resolve_model_keeps_static_explicit_models );
    ( "dynamic smoke discovery returns explicit models",
      `Quick,
      test_resolve_model_dynamic_discovery_is_explicit );
    ( "Copilot smoke uses backend default model",
      `Quick,
      test_resolve_model_uses_backend_default_for_copilot );
    ( "Copilot smoke model env override is explicit",
      `Quick,
      test_resolve_model_override_sets_copilot_model );
  ]

let with_tmpdir case f =
  let dir =
    Filename.temp_dir ("epure_smoke_" ^ sanitize_env_fragment case.id) ""
  in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let write_file path content =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let init_git_repo dir =
  write_file (Filename.concat dir "README.md") "# Epure backend smoke\n" ;
  let q = Filename.quote dir in
  let cmd =
    Printf.sprintf
      "git -C %s init -q && git -C %s -c user.name=Epure -c \
       user.email=epure@example.invalid add README.md && git -C %s -c \
       user.name=Epure -c user.email=epure@example.invalid commit -q -m init"
      q
      q
      q
  in
  if Sys.command cmd <> 0 then Error "could not initialise temporary git repo"
  else Ok ()

let task_spec ?(read_only = true) ~working_dir ~model () =
  Backend_types.make_task_spec
    ~prompt:"Reply exactly: EPURE_SMOKE_OK"
    ~working_dir
    ~timeout:120.0
    ~expected_outputs:[]
    ?model
    ~read_only
    ()

let command_argv_or_fail case spec =
  match command_argv_for_case case spec with
  | Ok argv -> argv
  | Error msg -> Alcotest.fail msg

let has_adjacent_args flag value argv =
  let rec loop = function
    | first :: second :: _ when first = flag && second = value -> true
    | _ :: rest -> loop rest
    | [] -> false
  in
  loop argv

let test_copilot_command_preview_omits_model_when_none () =
  let argv =
    command_argv_or_fail
      (backend_case Copilot_cli.id)
      (task_spec ~read_only:false ~working_dir:"/tmp/epure-smoke-copilot"
         ~model:None ())
  in
  Alcotest.(check bool) "omits --model" false (List.mem "--model" argv)

let test_gemini_command_preview_includes_model_and_omits_skip_trust () =
  let argv =
    command_argv_or_fail
      (backend_case Gemini_cli.id)
      (task_spec
         ~working_dir:"/tmp/epure-smoke-gemini"
         ~model:(Some "gemini-3-flash-preview") ())
  in
  Alcotest.(check bool)
    "omits unsupported --skip-trust"
    false
    (List.mem "--skip-trust" argv) ;
  Alcotest.(check bool)
    "includes -m model"
    true
    (has_adjacent_args "-m" "gemini-3-flash-preview" argv)

let command_preview_tests =
  [
    ( "Copilot default preview omits model flag",
      `Quick,
      test_copilot_command_preview_omits_model_when_none );
    ( "Gemini preview includes smoke model and omits skip-trust",
      `Quick,
      test_gemini_command_preview_includes_model_and_omits_skip_trust );
  ]

let availability_with_timeout ~sw ~env case =
  let clock = Eio.Stdenv.clock env in
  match
    Eio.Time.with_timeout clock 10.0 (fun () ->
        try Ok (Agentic_backend.available ~sw ~env case.backend)
        with e -> Error (`Available_exn (Printexc.to_string e)))
  with
  | Ok true -> Ok ()
  | Ok false -> Error "available returned false"
  | Error `Timeout -> Error "available timed out"
  | Error (`Available_exn msg) -> Error ("available raised " ^ msg)

let report_skip case reason =
  Printf.eprintf
    "[real-backend-smoke] %s (%s): SKIPPED unavailable%s\n%!"
    case.id
    case.name
    (if reason = "" then "" else " — " ^ reason)

let status_text = function
  | Backend_types.Success -> "success"
  | Failed msg -> "failed: " ^ msg
  | Timeout -> "timeout"
  | Cancelled -> "cancelled"

let fail_backend case msg = Alcotest.failf "%s (%s): %s" case.id case.name msg

let run_backend ~sw ~env case =
  match availability_with_timeout ~sw ~env case with
  | Error reason ->
      report_skip case reason ;
      Alcotest.skip ()
  | Ok () -> (
      match resolve_model case with
      | Error msg -> fail_backend case msg
      | Ok model ->
          with_tmpdir case (fun dir ->
              let model_label =
                match model with
                | Some model -> model
                | None -> "backend default"
              in
              Printf.eprintf
                "[real-backend-smoke] %s (%s): available; running model %s\n%!"
                case.id
                case.name
                model_label ;
              match init_git_repo dir with
              | Error msg -> fail_backend case msg
              | Ok () -> (
                  let spec =
                    task_spec ~read_only:(case.id <> Copilot_cli.id)
                      ~working_dir:dir ~model ()
                  in
                  match command_argv_for_case case spec with
                  | Error msg -> fail_backend case msg
                  | Ok argv -> (
                      let command_line = command_line_of_argv argv in
                      Printf.eprintf
                        "[real-backend-smoke] %s (%s): command %s\n%!"
                        case.id
                        case.name
                        command_line ;
                      let result =
                        try Agentic_backend.run_task ~sw ~env case.backend spec
                        with e ->
                          Backend_types.make_task_result
                            ~status:(Failed (Printexc.to_string e))
                            ()
                      in
                      match
                        ( result.Backend_types.status,
                          contains_substring
                            result.Backend_types.stdout
                            "EPURE_SMOKE_OK" )
                      with
                      | Success, true ->
                          Printf.printf
                            "[real-backend-smoke] %s (%s): OK\n%!"
                            case.id
                            case.name ;
                          ()
                      | _ ->
                          fail_backend
                            case
                            (Printf.sprintf
                               "command %s\n\
                                expected stdout containing EPURE_SMOKE_OK, got \
                                status=%s exit=%d stdout=%S stderr=%S"
                               command_line
                               (status_text result.status)
                               result.exit_code
                               result.stdout
                               result.stderr)))))

let test_real_backend case () =
  if not (smoke_enabled ()) then begin
    prerr_endline
      "[real-backend-smoke] disabled; set EPURE_REAL_BACKEND_SMOKE=1 to run \
       real backend smoke tests" ;
    Alcotest.skip ()
  end
  else
    Eio_posix.run @@ fun env ->
    Eio.Switch.run @@ fun sw -> run_backend ~sw ~env case

let smoke_tests =
  List.map (fun case -> (case.id, `Slow, test_real_backend case)) backends

let () =
  Alcotest.run
    "Real_backend_smoke"
    [
      ("Model discovery", model_discovery_tests);
      ("Command preview", command_preview_tests);
      ("Smoke", smoke_tests);
    ]
