(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Render {!Portable_session} events back to a native backend transcript.

    Rendering a composed session into a client's native format lets that
    client resume it with its own [--resume] mechanism, which is how a session
    is {b continued in a different client}.

    Rendering is {b conversation-only}: it emits [user]/[assistant] records and
    does {e not} synthesize tool-call blocks (tool interactions from other
    clients are not portable as executable calls).  Only Claude Code is
    implemented in this slice. *)

(** [claude_code ?session_id evs] renders [evs] to a Claude-Code-shaped JSONL
    string (one record per line, trailing newline).

    - Only {!Portable_session.User} and {!Portable_session.Assistant} events
      are emitted; {!Portable_session.Tool}/{!Portable_session.System} events
      are omitted (conversation-only).
    - Records carry a deterministic [uuid], a [parentUuid] chain, [sessionId]
      (defaulting to an all-zero placeholder), [timestamp], and
      [message.{role,content}] where user content is a string and assistant
      content is a single [text] block.
    - Output is deterministic (no clock/random), so
      [Session_ingest.claude_code (claude_code evs)] round-trips role and text
      for user/assistant events.

    {pre} (none)
    {post} Returns valid JSONL whose records preserve the order and
    role/text of the emitted events; empty input yields [""].
    {violators} (none)
    {violates} (none) *)
val claude_code : ?session_id:string -> Portable_session.event list -> string
