(******************************************************************************)
(* Copyright (c) 2026 Epure Team                                               *)
(* All rights reserved.                                                       *)
(******************************************************************************)

open Cabal

let envelope =
  {|{"type":"result","session_id":"usage-test","usage":{"input_tokens":0,"output_tokens":7,"cache_read_input_tokens":3,"cache_creation_input_tokens":2}}|}

let parser stdout =
  stdout |> String.split_on_char '\n'
  |> List.find_map (fun line ->
         try snd (Claude_code.parse_json_output (Yojson.Safe.from_string line))
         with _ -> None)

let check_failure suffix timeout expected =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spec =
    Backend_types.make_task_spec ~prompt:"fixture" ~working_dir:"/tmp"
      ~timeout:(Backend_types.duration_of_seconds timeout) ()
  in
  let result =
    Backend_process.run_task_with ~sw ~env ~spec
      ~build_command:(fun ~mcp_config_path:_ _ ->
        (["sh"; "-c"; "printf '%s\\n' '" ^ envelope ^ "'; " ^ suffix], ""))
      ~parse_cost:parser
      ~parse_session_id:Claude_code.parse_session_id_from_stdout ()
  in
  Alcotest.(check bool) "failure status preserved" true (expected result.status);
  Alcotest.(check (option string)) "session survives" (Some "usage-test") result.session_id;
  Alcotest.(check bool) "elapsed measured" true (result.elapsed >= 0.);
  match result.cost with
  | None -> Alcotest.fail "observed usage lost on process failure"
  | Some cost ->
      Alcotest.(check (option int)) "observed zero input" (Some 0) cost.tokens_input;
      Alcotest.(check (option int)) "output" (Some 7) cost.tokens_output;
      Alcotest.(check (option int)) "cache read" (Some 3) cost.cache_read_input_tokens;
      Alcotest.(check (option int)) "cache write" (Some 2) cost.cache_creation_input_tokens

let () =
  Alcotest.run "provider usage"
    [ ("terminal usage", [
        Alcotest.test_case "JSONL terminal report counted once" `Quick (fun () ->
          let stream = "{\"type\":\"system\",\"session_id\":\"usage-test\"}\n" ^ envelope ^ "\n" ^ envelope ^ "\n" in
          match Claude_code.parse_cost_from_stdout stream with
          | None -> Alcotest.fail "terminal JSONL usage missing"
          | Some cost ->
              Alcotest.(check (option int)) "zero survives" (Some 0) cost.tokens_input;
              Alcotest.(check (option int)) "no double sum" (Some 7) cost.tokens_output);
        Alcotest.test_case "partial message usage is not complete" `Quick (fun () ->
          Alcotest.(check bool) "unknown" true
            (Option.is_none (Claude_code.parse_cost_from_stdout
              {|{"type":"assistant","message":{"usage":{"input_tokens":6}}}|})));
      ]);
      ("failure metadata", [
        Alcotest.test_case "non-display aggregate stdout bound" `Quick (fun () ->
          let rejected =
            try
              Eio_posix.run (fun env ->
                Eio.Switch.run (fun sw ->
                  ignore (Backend_process.run_process ~sw ~env
                    ~cmd:["head"; "-c"; "134217728"; "/dev/zero"]
                    ~working_dir:"/tmp" ~timeout_seconds:10. ()))) ;
              false
            with Eio.Buf_read.Buffer_limit_exceeded -> true
          in
          Alcotest.(check bool) "128 MiB capture refused as before" true rejected);
        Alcotest.test_case "nonzero" `Quick (fun () ->
          check_failure "exit 9" 2. (function Backend_types.Failed _ -> true | _ -> false));
        Alcotest.test_case "signal" `Quick (fun () ->
          check_failure "kill -TERM $$" 2. (function Backend_types.Failed _ -> true | _ -> false));
        Alcotest.test_case "timeout without display callback" `Quick (fun () ->
          check_failure "exec sleep 5" 0.1 (function Backend_types.Timeout -> true | _ -> false));
      ]) ]
