(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** [cabal-mcp] — a stateful MCP server exposing cabal's live tmux-backed
    sessions and client-neutral session composition as MCP tools.

    Statefulness comes for free: the sessions live in {b tmux}, which persists
    across restarts of this server, so an agent can manipulate sessions that
    were opened by a previous process (or by a human).  This slice uses the
    stdio transport; mcp-kit also ships an HTTP transport
    ([mcp-kit.cohttp-eio]) for multi-client/remote deployments. *)

open Cabal
module K = Mcp_kit

let ok_text s =
  Ok { K.Tool.content = [ K.Tool.Text s ]; is_error = false; structured_content = None }

let ok_json ~text j =
  Ok
    {
      K.Tool.content = [ K.Tool.Text text ];
      is_error = false;
      structured_content = Some j;
    }

let ( let* ) = Result.bind
let sstr = K.Schema.string
let obj = K.Schema.object_

(* All handlers close over the Eio [env] so tmux subprocesses can be spawned. *)
let tools env =
  let session_list =
    K.Tool.make ~description:"List live tmux-backed sessions." ~input_schema:(obj ())
      "session_list" (fun _args ->
        let names = Live_session.list ~env () in
        ok_json
          ~text:(if names = [] then "(no live sessions)" else String.concat "\n" names)
          (`Assoc [ ("sessions", `List (List.map (fun n -> `String n) names)) ]))
  in
  let session_open =
    K.Tool.make
      ~description:
        "Open a detached tmux session running an agentic CLI command (kept \
         alive in its real TUI, human-attachable via `tmux attach`)."
      ~input_schema:
        (obj
           ~properties:
             [
               ("name", sstr ~description:"tmux session name" ());
               ("command", sstr ~description:"CLI command to run" ());
               ("working_dir", sstr ~description:"start directory" ());
             ]
           ~required:[ "name"; "command" ] ())
      "session_open" (fun args ->
        let* name = K.Tool.Arg.string "name" args in
        let* command = K.Tool.Arg.string "command" args in
        let* working_dir = K.Tool.Arg.(optional string) "working_dir" args in
        let s = Live_session.open_ ~env ?working_dir ~name command in
        if Live_session.has_session ~env s then
          ok_text
            (Printf.sprintf "opened '%s' (attach: tmux attach -t %s)"
               (Live_session.target s) name)
        else Ok (K.Tool.error_text (Printf.sprintf "failed to open session '%s'" name)))
  in
  let session_send =
    K.Tool.make
      ~description:"Send one turn (arbitrary multi-line text) to a session."
      ~input_schema:
        (obj
           ~properties:[ ("name", sstr ()); ("text", sstr ()) ]
           ~required:[ "name"; "text" ] ())
      "session_send" (fun args ->
        let* name = K.Tool.Arg.string "name" args in
        let* text = K.Tool.Arg.string "text" args in
        let s = Live_session.of_name name in
        if Live_session.has_session ~env s then (
          Live_session.send ~env s text;
          ok_text (Printf.sprintf "sent %d chars to '%s'" (String.length text) name))
        else Ok (K.Tool.error_text (Printf.sprintf "no such session '%s'" name)))
  in
  let session_capture =
    K.Tool.make ~description:"Capture a session's current pane as plain text."
      ~input_schema:(obj ~properties:[ ("name", sstr ()) ] ~required:[ "name" ] ())
      "session_capture" (fun args ->
        let* name = K.Tool.Arg.string "name" args in
        ok_text (Live_session.capture ~env (Live_session.of_name name)))
  in
  let session_close =
    K.Tool.make ~description:"Kill a session." ~input_schema:(obj ~properties:[ ("name", sstr ()) ] ~required:[ "name" ] ())
      "session_close" (fun args ->
        let* name = K.Tool.Arg.string "name" args in
        Live_session.close ~env (Live_session.of_name name);
        ok_text (Printf.sprintf "closed '%s'" name))
  in
  let session_compose_claude =
    K.Tool.make
      ~description:
        "Ingest a Claude Code session into the client-neutral model, compose \
         it (dedup + chronological reorder), and return Claude-Code JSONL \
         suitable for reseeding another session."
      ~input_schema:
        (obj
           ~properties:[ ("working_dir", sstr ()); ("session_id", sstr ()) ]
           ~required:[ "working_dir"; "session_id" ] ())
      "session_compose_claude" (fun args ->
        let* working_dir = K.Tool.Arg.string "working_dir" args in
        let* session_id = K.Tool.Arg.string "session_id" args in
        let evs = Session_ingest.load_claude_code_session ~working_dir ~session_id in
        let composed = Session_composition.(run [ Dedup; Reorder ]) evs in
        let jsonl = Session_render.claude_code composed in
        ok_json
          ~text:
            (Printf.sprintf "ingested %d events, composed to %d"
               (List.length evs) (List.length composed))
          (`Assoc
             [
               ("ingested", `Int (List.length evs));
               ("composed", `Int (List.length composed));
               ("rendered_jsonl", `String jsonl);
             ]))
  in
  [
    session_list;
    session_open;
    session_send;
    session_capture;
    session_close;
    session_compose_claude;
  ]

let build_server env =
  let base =
    K.Server.create ~name:"cabal-mcp" ~version:"0.1.0"
      ~instructions:
        "Drive live tmux-backed agentic CLI sessions and compose \
         client-neutral session context for cross-client continuation."
      ()
  in
  match K.Server.add_tools base (tools env) with
  | Ok server -> server
  | Error (K.Server.Duplicate_tool name) -> failwith ("duplicate tool: " ^ name)
  | Error _ -> failwith "unexpected duplicate declaration"

let () =
  Eio_main.run @@ fun env ->
  let server = build_server env in
  Mcp_kit_stdio.run_channels server stdin stdout
