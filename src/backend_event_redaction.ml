(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Backend event redaction — Story #484.

    Pure module: no I/O, no Eio.  All functions are deterministic given the
    same input. *)

(* -------------------------------------------------------------------------- *)
(* Redaction policy                                                            *)
(* -------------------------------------------------------------------------- *)

(** Field names whose string values must always be redacted, regardless of
    value length. *)
let sensitive_fields =
  [
    "prompt";
    "instructions";
    "instruction";
    "diff";
    "patch";
    "hunk";
    "log";
    "logs";
    "stdout";
    "stderr";
    "content";
    "body";
    "file_content";
    "file_contents";
    "text";
    "message";
    "messages";
    "response";
    "result";
    "authorization";
    "auth";
    "auth_header";
    "headers";
    "token";
    "access_token";
    "refresh_token";
    "bearer_token";
    "id_token";
    "api_key";
    "password";
    "credential";
    "credentials";
    "secret";
    "private_key";
  ]

(** Field names whose string values are always preserved as-is (identifiers,
    event type tags, status codes, model names, etc.). *)
let safe_string_fields =
  [
    "type";
    "event_type";
    "kind";
    "subtype";
    "id";
    "session_id";
    "tool_use_id";
    "turn_id";
    "ts";
    "timestamp";
    "created_at";
    "updated_at";
    "status";
    "role";
    "model";
    "backend";
    "provider";
    "stop_reason";
    "stop_sequence";
    "delta_type";
    "schema_version";
    "version";
    "language";
    "error";
    "error_code";
    "tool_id";
    "url";
    "path";
  ]

(** String values longer than this are redacted even if the field name is not
    in [sensitive_fields] (precautionary payload truncation). *)
let max_safe_str_len = 256

let is_sensitive_field name =
  List.mem (String.lowercase_ascii name) sensitive_fields

let is_safe_string_field name =
  List.mem (String.lowercase_ascii name) safe_string_fields

(* -------------------------------------------------------------------------- *)
(* Shape hash                                                                  *)
(* -------------------------------------------------------------------------- *)

(** Compute a compact representation of the JSON structure (field names and
    types only, no values) for hashing. *)
let rec shape_of_json (json : Yojson.Safe.t) =
  match json with
  | `Assoc fields ->
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
      "{"
      ^ String.concat
          ","
          (List.map (fun (k, v) -> k ^ ":" ^ shape_of_json v) sorted)
      ^ "}"
  | `List items -> (
      "[" ^ match items with [] -> "]" | h :: _ -> shape_of_json h ^ "...]")
  | `String _ -> "str"
  | `Int _ | `Float _ | `Intlit _ -> "num"
  | `Bool _ -> "bool"
  | `Null -> "null"

let compute_shape_hash json =
  let shape = shape_of_json json in
  Digest.to_hex (Digest.string shape)

(* -------------------------------------------------------------------------- *)
(* Recursive redaction                                                         *)
(* -------------------------------------------------------------------------- *)

(** Walk the JSON tree and redact sensitive string values.  [parent_field] is
    the field name under which this value appears (used for policy lookup).
    [count] accumulates the number of redactions performed. *)
let rec redact_json ~parent_field (count : int ref) (json : Yojson.Safe.t) :
    Yojson.Safe.t =
  match json with
  | `Null | `Bool _ | `Int _ | `Float _ | `Intlit _ ->
      (* Scalar non-string values are always safe. *)
      json
  | `String s ->
      if is_safe_string_field parent_field then
        (* Whitelisted field: keep regardless of value. *)
        json
      else if is_sensitive_field parent_field then begin
        incr count ;
        `String (Printf.sprintf "[redacted:%d chars]" (String.length s))
      end
      else if String.length s > max_safe_str_len then begin
        incr count ;
        `String (Printf.sprintf "[redacted:%d chars]" (String.length s))
      end
      else json
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (k, v) -> (k, redact_json ~parent_field:k count v))
           fields)
  | `List items ->
      (* Items in a list inherit the parent field name for policy lookup. *)
      `List (List.map (fun item -> redact_json ~parent_field count item) items)

(* -------------------------------------------------------------------------- *)
(* Public API                                                                  *)
(* -------------------------------------------------------------------------- *)

type redacted = {
  backend_id : string;
  event_type : string;
  sanitized : Yojson.Safe.t;
  fields_redacted : int;
  redaction_summary : string;
  shape_hash : string option;
}

let extract_event_type json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "type" fields with
      | Some (`String t) -> t
      | _ -> (
          match List.assoc_opt "event_type" fields with
          | Some (`String t) -> t
          | _ -> "unknown"))
  | _ -> "unknown"

let redact_event ~backend_id json =
  let count = ref 0 in
  let event_type = extract_event_type json in
  let shape_hash = Some (compute_shape_hash json) in
  let sanitized = redact_json ~parent_field:"" count json in
  let fields_redacted = !count in
  let redaction_summary =
    if fields_redacted = 0 then "no fields redacted"
    else Printf.sprintf "%d field(s) redacted" fields_redacted
  in
  {
    backend_id;
    event_type;
    sanitized;
    fields_redacted;
    redaction_summary;
    shape_hash;
  }

let to_json r =
  `Assoc
    [
      ("backend_id", `String r.backend_id);
      ("event_type", `String r.event_type);
      ("sanitized", r.sanitized);
      ("fields_redacted", `Int r.fields_redacted);
      ("redaction_summary", `String r.redaction_summary);
      ( "shape_hash",
        match r.shape_hash with None -> `Null | Some h -> `String h );
    ]

let redact_error_message msg =
  let count = ref 0 in
  let wrapper = `Assoc [("message", `String msg)] in
  match redact_json ~parent_field:"" count wrapper with
  | `Assoc [("message", `String s)] -> s
  | `Assoc [("message", v)] -> Yojson.Safe.to_string v
  | _ -> "[redacted]"
