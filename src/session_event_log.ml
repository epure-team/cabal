(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Per-session append-only NDJSON event log for live observability.

    Each session gets a file at [.epure/agent-sessions/<session_uuid>.ndjson].
    All writes are best-effort: failures are swallowed so they never block a
    build or agentic turn.  The log is independent of the DB-backed replay
    mechanism ([agentic_turns] / [reconstruct_agentic_context]).

    I/O uses [Eio.Path] throughout (no blocking stdlib calls). *)

(* -------------------------------------------------------------------------- *)
(* Session ID validation                                                       *)
(* -------------------------------------------------------------------------- *)

(** [is_safe_session_id s] returns [true] iff [s] is non-empty and contains
    only characters that are safe in a filename with no path-separator risk
    ([a-zA-Z0-9_-]).  Rejects empty strings, dots, slashes, and any character
    that could enable path traversal or injection. *)
let is_safe_session_id s =
  s <> ""
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '-' || c = '_')
       s

(* -------------------------------------------------------------------------- *)
(* Eio path helpers                                                            *)
(* -------------------------------------------------------------------------- *)

(** Open or create the session NDJSON file for appending, write [line] plus a
    newline, then close.  Uses [Eio.Path.with_open_out] — no separate stat,
    no TOCTOU.  The directory is created on-demand if absent. *)
let append_line ~fs ~session_logs_dir ~session_id line =
  let dir_path = Eio.Path.(fs / session_logs_dir) in
  (* Create directory atomically; swallow Already_exists *)
  (try Eio.Path.mkdir ~perm:0o750 dir_path with Eio.Io _ -> ()) ;
  let file_path = Eio.Path.(dir_path / (session_id ^ ".ndjson")) in
  Eio.Path.with_open_out
    ~append:true
    ~create:(`If_missing 0o600)
    file_path
    (fun flow -> Eio.Flow.copy_string (line ^ "\n") flow)

(** Best-effort wrapper: swallows all exceptions so log failures never
    propagate to the caller.  Logs warnings via epure diagnostics. *)
let try_append ~fs ~session_logs_dir ~session_id line =
  if not (is_safe_session_id session_id) then ()
  else
    try append_line ~fs ~session_logs_dir ~session_id line
    with exn ->
      Diagnostics.warn
        "[session_event_log] write failed for %s: %s"
        session_id
        (Printexc.to_string exn)

(* -------------------------------------------------------------------------- *)
(* JSON helpers                                                                *)
(* -------------------------------------------------------------------------- *)

let now_utc () =
  let t = Unix.gettimeofday () in
  let {Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _} =
    Unix.gmtime t
  in
  Printf.sprintf
    "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm_year + 1900)
    (tm_mon + 1)
    tm_mday
    tm_hour
    tm_min
    tm_sec

let cost_to_json (c : Backend_types.cost) =
  let field k = function None -> [] | Some v -> [(k, `Int v)] in
  let float_field k = function None -> [] | Some v -> [(k, `Float v)] in
  let tokens_total =
    match (c.tokens_input, c.tokens_output) with
    | Some i, Some o -> [("tokens_total", `Int (i + o))]
    | _ -> []
  in
  `Assoc
    (field "tokens_input" c.tokens_input
    @ field "tokens_output" c.tokens_output
    @ tokens_total
    @ field "cache_creation_input_tokens" c.cache_creation_input_tokens
    @ field "cache_read_input_tokens" c.cache_read_input_tokens
    @ float_field "cost_usd" c.cost_usd)

let make_envelope ~session_id ~backend ~event_type ~payload =
  `Assoc
    [
      ("ts", `String (now_utc ()));
      ("session_id", `String session_id);
      ("backend", `String backend);
      ("type", `String event_type);
      ("payload", payload);
    ]

(* -------------------------------------------------------------------------- *)
(* Write API (all best-effort)                                                 *)
(* -------------------------------------------------------------------------- *)

let write_session_start ~fs ~session_logs_dir ~session_id ~backend ~story_id
    ~agent_role () =
  let payload =
    `Assoc [("story_id", `Int story_id); ("agent_role", `String agent_role)]
  in
  let envelope =
    make_envelope ~session_id ~backend ~event_type:"session_start" ~payload
  in
  try_append ~fs ~session_logs_dir ~session_id (Yojson.Safe.to_string envelope)

let write_session_end ~fs ~session_logs_dir ~session_id ~backend ~status ~cost
    () =
  let cost_json = match cost with None -> `Null | Some c -> cost_to_json c in
  let payload = `Assoc [("status", `String status); ("cost", cost_json)] in
  let envelope =
    make_envelope ~session_id ~backend ~event_type:"session_end" ~payload
  in
  try_append ~fs ~session_logs_dir ~session_id (Yojson.Safe.to_string envelope)

let write_turn_start ~fs ~session_logs_dir ~session_id ~backend ~turn_number
    ~agent_role () =
  let payload =
    `Assoc
      [("turn_number", `Int turn_number); ("agent_role", `String agent_role)]
  in
  let envelope =
    make_envelope ~session_id ~backend ~event_type:"turn_started" ~payload
  in
  try_append ~fs ~session_logs_dir ~session_id (Yojson.Safe.to_string envelope)

let write_turn_end ~fs ~session_logs_dir ~session_id ~backend ~turn_number
    ~agent_role ~status ~cost () =
  let cost_json = match cost with None -> `Null | Some c -> cost_to_json c in
  let payload =
    `Assoc
      [
        ("turn_number", `Int turn_number);
        ("agent_role", `String agent_role);
        ("status", `String status);
        ("cost", cost_json);
      ]
  in
  let envelope =
    make_envelope ~session_id ~backend ~event_type:"turn_completed" ~payload
  in
  try_append ~fs ~session_logs_dir ~session_id (Yojson.Safe.to_string envelope)

let write_turn_failed ~fs ~session_logs_dir ~session_id ~backend ~turn_number
    ~agent_role ~error () =
  let payload =
    `Assoc
      [
        ("turn_number", `Int turn_number);
        ("agent_role", `String agent_role);
        ("error", `String error);
      ]
  in
  let envelope =
    make_envelope ~session_id ~backend ~event_type:"turn_failed" ~payload
  in
  try_append ~fs ~session_logs_dir ~session_id (Yojson.Safe.to_string envelope)

let write_raw_event ~fs ~session_logs_dir ~session_id ~backend ~turn_number line
    =
  match Yojson.Safe.from_string line with
  | raw ->
      let redacted =
        Backend_event_redaction.redact_event ~backend_id:backend raw
      in
      let payload =
        `Assoc
          [
            ("turn_number", `Int turn_number);
            ("redacted_event", Backend_event_redaction.to_json redacted);
          ]
      in
      let envelope =
        make_envelope ~session_id ~backend ~event_type:"raw" ~payload
      in
      try_append
        ~fs
        ~session_logs_dir
        ~session_id
        (Yojson.Safe.to_string envelope)
  | exception _ -> ()

(* -------------------------------------------------------------------------- *)
(* Read API                                                                    *)
(* -------------------------------------------------------------------------- *)

(** [list_sessions ~fs ~session_logs_dir ()] returns the list of session IDs
    (filenames without the [.ndjson] extension) found in the agent-sessions
    directory.  Returns [[]] on any error or when the directory does not exist.
    Sorted in descending order by filename (most recently modified file name
    alphabetically last). *)
let list_sessions ~fs ~session_logs_dir () =
  let dir_path = Eio.Path.(fs / session_logs_dir) in
  match Eio.Path.read_dir dir_path with
  | entries ->
      entries
      |> List.filter_map (fun name ->
          if
            String.length name > 7
            && String.sub name (String.length name - 7) 7 = ".ndjson"
          then
            let id = String.sub name 0 (String.length name - 7) in
            if is_safe_session_id id then Some id else None
          else None)
      |> List.sort (fun a b -> String.compare b a)
  | exception Eio.Io _ -> []

(** [read_events ~fs ~session_logs_dir ~session_id ()] reads all NDJSON lines from
    the session log and returns them as parsed JSON values.  Returns [[]] when
    the file does not exist or on any error.

    Rejects [session_id] values that fail [is_safe_session_id] before opening
    any file (path traversal guard).  Uses [Eio.Path.with_open_in] — no
    separate stat, no TOCTOU.  Uses [Eio.Path.load] — single atomic open. *)
let read_events ~fs ~session_logs_dir ~session_id () =
  if not (is_safe_session_id session_id) then []
  else
    let file_path =
      Eio.Path.(fs / session_logs_dir / (session_id ^ ".ndjson"))
    in
    match Eio.Path.load file_path with
    | contents ->
        let lines =
          String.split_on_char '\n' contents
          |> List.filter (fun l -> String.trim l <> "")
        in
        List.filter_map
          (fun line ->
            match Yojson.Safe.from_string line with
            | json -> Some json
            | exception _ -> None)
          lines
    | exception Eio.Io _ -> []
