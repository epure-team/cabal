(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Backend event redaction — Story #484.

    Pure module (no I/O, no Eio).  Redacts sensitive fields from raw backend
    JSON events before they are persisted to NDJSON logs or the DB.

    Sensitive field names whose string values are always removed:
    [prompt], [instructions], [diff], [patch], [log], [logs], [stdout],
    [stderr], [content], [body], [file_content], [file_contents], [text],
    [message], [messages], [response], [result], [authorization], [auth], [auth_header],
    [token], [access_token], [refresh_token], [bearer_token], [id_token],
    [api_key], [password], [credential], [credentials], [secret],
    [private_key].

    String values longer than 256 characters are also redacted regardless of
    field name (precautionary payload truncation).

    Numbers, booleans, and null are always preserved.  Objects and arrays are
    recursed into; individual string fields inside them are checked by the
    rules above.

    The preserved output contains: event type, backend id, sanitized payload
    (with sensitive strings replaced by [[redacted:N chars]] markers), count
    of redacted fields, human-readable redaction summary, and an optional MD5
    shape hash computed from the field structure (keys only, not values). *)

(** Result of redacting a backend event. *)
type redacted = {
  backend_id : string;  (** Backend that emitted the event. *)
  event_type : string;
      (** Event type extracted from the [type] or [event_type] field. *)
  sanitized : Yojson.Safe.t;
      (** Redacted JSON with sensitive string fields replaced. *)
  fields_redacted : int;  (** Number of string fields that were redacted. *)
  redaction_summary : string;  (** Human-readable redaction summary. *)
  shape_hash : string option;
      (** MD5 hex hash of the event structure (field names only). *)
}

(** [redact_event ~backend_id json] redacts sensitive fields from a raw backend
    JSON event.

    {pre}
    [json] is a valid [Yojson.Safe.t] value.

    {post}
    Returns a [redacted] value where all fields matching the sensitive list
    (or containing strings longer than 256 chars) have their values replaced
    by [[redacted:N chars]] markers.  Numbers, booleans, and null are
    unchanged.  The [event_type] field is extracted from the top-level [type]
    or [event_type] key before redaction.

    {violators}
    (none)

    {violates}
    (none) *)
val redact_event : backend_id:string -> Yojson.Safe.t -> redacted

(** [to_json r] serializes a [redacted] event as a JSON object suitable for
    NDJSON storage.

    {pre}
    (none)

    {post}
    Returns a [Yojson.Safe.t] [Assoc] containing [backend_id], [event_type],
    [sanitized], [fields_redacted], [redaction_summary], and [shape_hash].

    {violators}
    (none)

    {violates}
    (none) *)
val to_json : redacted -> Yojson.Safe.t

(** [redact_error_message msg] redacts a plain error string before persistence.

    Wraps [msg] in a synthetic sensitive field and applies the same redaction
    rules as [redact_event]: the entire value is replaced by a
    [[redacted:N chars]] marker to prevent API keys, tokens, or prompt content
    from leaking through error messages (e.g. [unauthorized: api_key=sk-...]).
*)
val redact_error_message : string -> string
