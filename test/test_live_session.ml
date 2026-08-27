(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal
open Live_session

let test_new_session_argv () =
  Alcotest.(check (list string))
    "full form"
    [
      "tmux";
      "-L";
      "cabal-mcp";
      "new-session";
      "-d";
      "-s";
      "s";
      "-x";
      "200";
      "-y";
      "50";
      "-c";
      "/tmp";
      "claude";
    ]
    (new_session_argv ~name:"s" ~size:(200, 50) ~working_dir:"/tmp" "claude") ;
  Alcotest.(check (list string))
    "minimal form"
    ["tmux"; "-L"; "cabal-mcp"; "new-session"; "-d"; "-s"; "s"; "claude"]
    (new_session_argv ~name:"s" "claude")

let test_send_argv () =
  Alcotest.(check (list string))
    "set-buffer (named, -- guards text)"
    ["tmux"; "-L"; "cabal-mcp"; "set-buffer"; "-b"; "s"; "--"; "multi\nline"]
    (set_buffer_argv ~name:"s" "multi\nline") ;
  Alcotest.(check (list string))
    "paste-buffer (delete after)"
    ["tmux"; "-L"; "cabal-mcp"; "paste-buffer"; "-d"; "-b"; "s"; "-t"; "=s"]
    (paste_buffer_argv ~name:"s") ;
  Alcotest.(check (list string))
    "enter"
    ["tmux"; "-L"; "cabal-mcp"; "send-keys"; "-t"; "=s"; "Enter"]
    (enter_argv ~name:"s")

let test_query_argv () =
  Alcotest.(check (list string))
    "capture"
    ["tmux"; "-L"; "cabal-mcp"; "capture-pane"; "-t"; "=s"; "-p"]
    (capture_argv ~name:"s") ;
  Alcotest.(check (list string))
    "has-session"
    ["tmux"; "-L"; "cabal-mcp"; "has-session"; "-t"; "=s"]
    (has_session_argv ~name:"s") ;
  Alcotest.(check (list string))
    "kill-session"
    ["tmux"; "-L"; "cabal-mcp"; "kill-session"; "-t"; "=s"]
    (kill_session_argv ~name:"s") ;
  Alcotest.(check (list string))
    "list-sessions"
    ["tmux"; "-L"; "cabal-mcp"; "list-sessions"; "-F"; "#{session_name}"]
    (list_sessions_argv ())

(* Integration: exercised only when tmux is present; a no-op skip otherwise so
   CI without tmux still passes. *)
let test_live_lifecycle () =
  Eio_main.run @@ fun env ->
  let name = "cabal-livesession-selftest" in
  let s = open_ ~env ~name "sleep 30" in
  if has_session ~env s then begin
    let names = list ~env () in
    Alcotest.(check bool) "session appears in list" true (List.mem name names) ;
    close ~env s ;
    Alcotest.(check bool) "closed" false (has_session ~env s)
  end
  else Alcotest.(check pass) "tmux unavailable — integration skipped" () ()

let () =
  Alcotest.run
    "live_session"
    [
      ( "argv builders",
        [
          Alcotest.test_case "new_session_argv" `Quick test_new_session_argv;
          Alcotest.test_case "send argv" `Quick test_send_argv;
          Alcotest.test_case "query argv" `Quick test_query_argv;
        ] );
      ( "integration",
        [Alcotest.test_case "lifecycle" `Quick test_live_lifecycle] );
    ]
