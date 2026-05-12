(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** CMV-style session trimmer for Claude Code.

    Reimplements the core trim+fork logic from
    {{:https://github.com/CosmoNaught/claude-code-cmv}claude-code-cmv}
    in OCaml. Between Builder rounds, the session JSONL is copied with
    a new UUID and trimmed: tool results are stubbed, thinking blocks
    removed, and images stripped. The model retains its synthesised
    understanding while the context is 50-70% smaller. *)

(** Metrics from a trimming operation. *)
type trim_metrics = {
  original_bytes : int;
  trimmed_bytes : int;
  tool_results_stubbed : int;
  thinking_blocks_removed : int;
  images_stripped : int;
  tool_inputs_stubbed : int;
}

(** Serialize trim metrics to JSON for DB storage.

    {pre}
    (none)

    {post}
    Returns a [Yojson.Safe.t] JSON object encoding all fields of the given [trim_metrics].

    {violators}
    (none)

    {violates}
    (none) *)
val trim_metrics_to_json : trim_metrics -> Yojson.Safe.t

(** Encode a working directory path the way Claude Code does:
    every non-[a-zA-Z0-9-] character (including the leading [/]) is replaced
    with a single [-].  For example [/home/user/project] becomes
    [-home-user-project] and [/path/.hidden/sub] becomes [-path--hidden-sub].

    {pre}
    (none)

    {post}
    Returns the encoded string with every non-alphanumeric-or-hyphen character replaced by [-].

    {violators}
    (none)

    {violates}
    (none) *)
val encode_working_dir : string -> string

(** [find_session_file ~working_dir ~session_id] locates the JSONL file
    for a Claude Code session.  Searches
    [~/.claude/projects/<encoded-working-dir>/<session_id>.jsonl].

    {pre}
    (none)

    {post}
    Returns [Some path] if the session JSONL file exists, [None] otherwise.

    {violators}
    (none)

    {violates}
    (none) *)
val find_session_file : working_dir:string -> session_id:string -> string option

(** [fork_trimmed ~env ~working_dir ~session_id ?threshold ()]
    copies a session JSONL, applies trimming rules, writes the result
    with a fresh UUID, and updates [sessions-index.json] so
    [--resume] discovers it.

    @param threshold Content length (chars) above which tool results /
      inputs are stubbed (default: 500).
    @return [(new_session_id, trim_metrics)] on success.

    {pre}
    [working_dir] must be an existing directory. The session file for [session_id] must exist.

    {post}
    Returns [Ok (new_session_id, metrics)] with the new session ID and trim statistics on success, or [Error msg] if the session file cannot be found or written.

    {violators}
    (none)

    {violates}
    (none) *)
val fork_trimmed :
  env:Eio_unix.Stdenv.base ->
  working_dir:string ->
  session_id:string ->
  ?threshold:int ->
  unit ->
  (string * trim_metrics, string) result

(** [trim_line ~threshold json_line] applies all trimming rules to a
    single JSONL line.  Returns [None] if the line should be dropped
    entirely, or [Some (trimmed_line, metrics_delta)] otherwise.
    Exported for unit testing.

    {pre}
    [json_line] must be a valid JSON string representing a single JSONL entry.

    {post}
    Returns [Some (trimmed_line, delta)] with the processed line and incremental metrics, or [None] if the line should be dropped.

    {violators}
    (none)

    {violates}
    (none) *)
val trim_line : threshold:int -> string -> (string * trim_metrics) option
