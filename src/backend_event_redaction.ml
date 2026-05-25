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
    "bearer";
    "id_token";
    "oauth_token";
    "oauth";
    "jwt";
    "api_key";
    "api_keys";
    "client_secret";
    "client_id_secret";
    "password";
    "passwd";
    "pwd";
    "credential";
    "credentials";
    "secret";
    "secrets";
    "private_key";
    "ssh_key";
    "signature";
    "signing_key";
    "aws_secret_access_key";
    "aws_access_key_id";
    "aws_session_token";
    "gcp_service_account";
    "cookie";
    "set_cookie";
    "session";
    "session_token";
    "connection_string";
    "dsn";
    "environment";
    "env";
    "env_vars";
    "environ";
  ]

(** Field names whose string values are always preserved as-is (identifiers,
    event type tags, status codes, model names, etc.).

    NOTE: [url] and [error] were intentionally removed; both can legitimately
    contain credentials (a [postgres://user:pw@host] URL leaking through
    [url], or an error message echoing such a URL). They are now subject to
    the default policy plus the pattern-based fallback below. *)
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
    "error_code";
    "tool_id";
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
(* Value-pattern fallbacks                                                     *)
(* -------------------------------------------------------------------------- *)

(** [contains_url_credentials s] is true when [s] has a substring of the form
    [scheme://user:password@host], the classic in-URL credential leak. *)
let contains_url_credentials s =
  let needle = "://" in
  let nlen = String.length needle in
  let slen = String.length s in
  let rec scan i =
    if i > slen - nlen then false
    else if String.sub s i nlen = needle then begin
      let start = i + nlen in
      let stop =
        let rec find_slash j =
          if j >= slen then slen
          else if s.[j] = '/' || s.[j] = '?' || s.[j] = '#' then j
          else find_slash (j + 1)
        in
        find_slash start
      in
      let segment = String.sub s start (stop - start) in
      let has_at = String.contains segment '@' in
      let has_colon = String.contains segment ':' in
      let colon_before_at =
        match (String.index_opt segment ':', String.index_opt segment '@') with
        | Some c, Some a -> c < a
        | _ -> false
      in
      if has_at && has_colon && colon_before_at then true else scan (i + 1)
    end
    else scan (i + 1)
  in
  scan 0

(** [is_base64_url_segment s] — characters are URL-safe base64
    ([A-Za-z0-9_-]) and length is non-trivial.  No padding (JWT segments
    are unpadded). *)
let is_base64_url_segment s =
  let len = String.length s in
  if len < 4 then false
  else
    let ok = ref true in
    String.iter
      (fun c ->
        if
          not
            ((c >= 'A' && c <= 'Z')
            || (c >= 'a' && c <= 'z')
            || (c >= '0' && c <= '9')
            || c = '_' || c = '-')
        then ok := false)
      s ;
    !ok

(** [is_jwt_like s] — three dot-separated URL-safe base64 segments where
    header and payload both start with [eyJ] (the base64 prefix every JWT
    inherits from the leading open-brace + quote of its JSON header and
    payload).  Conservative: requires the conventional JWT shape. *)
let is_jwt_like s =
  if String.length s < 30 then false
  else
    match String.split_on_char '.' s with
    | [h; p; sig_]
      when String.length h >= 4
           && String.length p >= 4
           && String.length sig_ >= 4
           && is_base64_url_segment h && is_base64_url_segment p
           && is_base64_url_segment sig_
           && String.length h >= 3
           && String.sub h 0 3 = "eyJ"
           && String.sub p 0 3 = "eyJ" ->
        true
    | _ -> false

let value_pattern_redactable s = contains_url_credentials s || is_jwt_like s

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
  | `Tuple items -> (
      "(" ^ match items with [] -> ")" | h :: _ -> shape_of_json h ^ ",...)")
  | `Variant (tag, None) -> "variant:" ^ tag
  | `Variant (tag, Some payload) ->
      "variant:" ^ tag ^ "(" ^ shape_of_json payload ^ ")"
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
      if is_sensitive_field parent_field then begin
        incr count ;
        `String (Printf.sprintf "[redacted:%d chars]" (String.length s))
      end
      else if value_pattern_redactable s then begin
        (* Even if the field is in the safe list, a value matching a known
           credential pattern (URL with embedded user:password, JWT shape)
           must not be echoed. *)
        incr count ;
        `String (Printf.sprintf "[redacted:%d chars]" (String.length s))
      end
      else if is_safe_string_field parent_field then
        (* Whitelisted field with a clean value: keep as-is for observability. *)
        json
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
  | `Tuple items ->
      `Tuple (List.map (fun item -> redact_json ~parent_field count item) items)
  | `Variant (tag, payload) ->
      `Variant (tag, Option.map (fun v -> redact_json ~parent_field count v) payload)

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
