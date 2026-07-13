(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type t = {name : string}

let of_name name = {name}

(* {1 Pure command builders} *)

let new_session_argv ~name ?size ?working_dir cmd =
  let size_args =
    match size with
    | None -> []
    | Some (w, h) -> ["-x"; string_of_int w; "-y"; string_of_int h]
  in
  let wd_args = match working_dir with None -> [] | Some dir -> ["-c"; dir] in
  ["tmux"; "new-session"; "-d"; "-s"; name] @ size_args @ wd_args @ [cmd]

(* A per-session named buffer avoids races between concurrent sends and
   between the load and the paste. *)
let set_buffer_argv ~name text = ["tmux"; "set-buffer"; "-b"; name; "--"; text]

let paste_buffer_argv ~name =
  ["tmux"; "paste-buffer"; "-d"; "-b"; name; "-t"; name]

let enter_argv ~name = ["tmux"; "send-keys"; "-t"; name; "Enter"]

let capture_argv ~name = ["tmux"; "capture-pane"; "-t"; name; "-p"]

let has_session_argv ~name = ["tmux"; "has-session"; "-t"; name]

let kill_session_argv ~name = ["tmux"; "kill-session"; "-t"; name]

let list_sessions_argv () = ["tmux"; "list-sessions"; "-F"; "#{session_name}"]

(* {1 Effectful operations} *)

(* Run [argv], returning (exit_code, combined stdout+stderr).  stderr is merged
   into the captured stream but only consumed when the exit code is 0, so error
   text never leaks into successful captures. *)
let run_capture env argv =
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let r, w = Eio.Process.pipe mgr ~sw in
  let proc = Eio.Process.spawn ~sw mgr ~stdout:w ~stderr:w argv in
  Eio.Flow.close w ;
  let out = Eio.Buf_read.(parse_exn take_all) r ~max_size:max_int in
  Eio.Flow.close r ;
  let code =
    match Eio.Process.await proc with `Exited n -> n | `Signaled n -> 128 + n
  in
  (code, out)

let run_ok env argv =
  match run_capture env argv with code, _ -> code = 0 | exception _ -> false

let target t = t.name

let open_ ~env ?size ?working_dir ~name cmd =
  let _ = run_ok env (new_session_argv ~name ?size ?working_dir cmd) in
  of_name name

let send ~env t text =
  let _ = run_ok env (set_buffer_argv ~name:t.name text) in
  let _ = run_ok env (paste_buffer_argv ~name:t.name) in
  let _ = run_ok env (enter_argv ~name:t.name) in
  ()

let capture ~env t =
  match run_capture env (capture_argv ~name:t.name) with
  | 0, out -> out
  | _ -> ""
  | exception _ -> ""

let has_session ~env t = run_ok env (has_session_argv ~name:t.name)

let close ~env t = ignore (run_ok env (kill_session_argv ~name:t.name))

let list ~env () =
  match run_capture env (list_sessions_argv ()) with
  | 0, out ->
      String.split_on_char '\n' out
      |> List.filter (fun s -> String.trim s <> "")
  | _ -> []
  | exception _ -> []
