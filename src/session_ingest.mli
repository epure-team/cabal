(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Ingest native backend transcripts into the {!Portable_session} model.

    Each backend stores conversation history in its own on-disk format.  This
    module normalizes those into client-neutral {!Portable_session.event}s.

    Only Claude Code is implemented in this slice (its JSONL is the reference
    format).  Other backends (codex/gemini/opencode/copilot) are documented
    follow-ups. *)

(** [claude_code content] parses Claude Code session JSONL (one record per
    line) into ordered, oldest-first portable events.

    - [user]/[assistant] records become events; other record types
      (summaries, snapshots, mode markers) are ignored.
    - [thinking] content blocks are dropped.
    - [text] blocks become {!Portable_session.User}/{!Portable_session.Assistant}
      events; [tool_use]/[tool_result] blocks become
      {!Portable_session.Tool} events with best-effort summaries.
    - Malformed or non-JSON lines are skipped, never raise.

    {pre} (none)
    {post} Returns the events in file order; each carries provenance
    [{ client = Some "claude-code"; source_session = <sessionId if present> }].
    {violators} (none)
    {violates} (none) *)
val claude_code : string -> Portable_session.event list

(** [load_claude_code_session ~working_dir ~session_id] locates the Claude Code
    JSONL for [session_id] under [~/.claude/projects/...] (via
    {!Session_trimmer.find_session_file}), reads it, and ingests it.

    {pre} (none)
    {post} Returns the ingested events, or [[]] if the session file does not
    exist or cannot be read.  Never raises.
    {violators} (none)
    {violates} (none) *)
val load_claude_code_session :
  working_dir:string -> session_id:string -> Portable_session.event list
