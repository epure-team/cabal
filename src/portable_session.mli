(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Client-neutral conversation model.

    A [Portable_session] is an ordered list of {!event}s that captures a
    conversation independently of the agentic client that produced it.
    Backends ingest their native transcript into this model
    ({!Session_ingest}) and render it back to a native transcript
    ({!Session_render}); {!Session_composition} transforms it.

    The goal is {b continuation via curated injection}, not faithful replay:
    a session captured from client A can be composed and re-seeded into a
    (possibly different) client B.  Working-directory state (git) is already
    shared, so this model only carries the conversational thread. *)

(** Speaker/kind of an event. *)
type role =
  | User  (** A human/caller turn. *)
  | Assistant  (** A model turn. *)
  | System  (** System/context material. *)
  | Tool  (** A tool invocation or its result. *)
[@@deriving show, eq, yojson]

(** A flattened tool interaction.  Summaries are best-effort textual
    renderings; the model does not attempt to preserve exact tool schemas
    across clients (see module docs). *)
type tool_ref = {
  name : string;  (** Tool name (e.g. "Bash", "exec_command"). *)
  input_summary : string;  (** Textual summary of the tool input. *)
  output_summary : string;  (** Textual summary of the tool output. *)
}
[@@deriving show, eq, yojson]

(** Where an event came from, so composed sessions remain traceable and
    dedup can span sources. *)
type provenance = {
  source_session : string option;  (** Originating session id, if known. *)
  client : string option;  (** Originating backend id (e.g. "claude-code"). *)
}
[@@deriving show, eq, yojson]

(** A single client-neutral conversation event. *)
type event = {
  role : role;
  text : string;  (** Flattened textual content ([""] for pure tool events). *)
  tool : tool_ref option;  (** Present when this event is a tool interaction. *)
  model : string option;  (** Model that produced the event, if known. *)
  provenance : provenance;
  timestamp : string option;  (** ISO-8601 timestamp, if known. *)
  tokens : Backend_types.cost option;  (** Token/cost data, if known. *)
}
[@@deriving show, eq, yojson]

(** An ordered (oldest-first) client-neutral conversation. *)
type t = event list [@@deriving show, eq, yojson]

(** A provenance with no known source. *)
val empty_provenance : provenance

(** [make_event ?tool ?model ?provenance ?timestamp ?tokens role text]
    constructs an event.  [provenance] defaults to {!empty_provenance}.

    {pre} (none)
    {post} Returns an event with the given fields; optional fields default to
    [None] (and [provenance] to {!empty_provenance}).
    {violators} (none)
    {violates} (none) *)
val make_event :
  ?tool:tool_ref ->
  ?model:string ->
  ?provenance:provenance ->
  ?timestamp:string ->
  ?tokens:Backend_types.cost ->
  role ->
  string ->
  event

(** [normalized_text e] returns [e.text] with surrounding whitespace trimmed
    and internal runs of whitespace collapsed to single spaces.  Used as the
    identity basis for {!Session_composition.dedup}.

    {pre} (none)
    {post} Returns a whitespace-normalized copy of [e.text]; never raises.
    {violators} (none)
    {violates} (none) *)
val normalized_text : event -> string
