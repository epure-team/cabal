(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Pure JSON Schema validator — no I/O, no subprocess, no LLM.
    Backed by the [jsonschema] opam package (v0.1.0). *)

(** Convert [Yojson.Safe.t] to [Yojson.Basic.t] via JSON-string round-trip.
    [Jsonschema] requires [Yojson.Basic.t]; the public API uses [Yojson.Safe.t]
    so callers do not need to depend on a specific Yojson variant. *)
let safe_to_basic (v : Yojson.Safe.t) : Yojson.Basic.t =
  Yojson.Basic.from_string (Yojson.Safe.to_string v)

let format_instance_location (loc : Jsonschema.instance_location) : string =
  match loc.tokens with
  | [] -> "document root"
  | tokens ->
      let parts =
        List.map
          (function
            | Jsonschema.Prop s -> s
            | Jsonschema.Item i -> string_of_int i)
          tokens
      in
      "/" ^ String.concat "/" parts

let format_type_set (want : Jsonschema.type_set) : string =
  let buf = Buffer.create 32 in
  Jsonschema.Types.iter
    (fun t ->
      if Buffer.length buf > 0 then Buffer.add_string buf " | ";
      Buffer.add_string buf
        (match t with
        | Jsonschema.Types.Null -> "null"
        | Jsonschema.Types.Boolean -> "boolean"
        | Jsonschema.Types.Number -> "number"
        | Jsonschema.Types.Integer -> "integer"
        | Jsonschema.Types.String -> "string"
        | Jsonschema.Types.Array -> "array"
        | Jsonschema.Types.Object -> "object"))
    want;
  Buffer.contents buf

let format_error_kind (kind : Jsonschema.error_kind) (loc : string) : string =
  match kind with
  | Jsonschema.Group -> Printf.sprintf "validation failed at %s" loc
  | Jsonschema.Schema { url } ->
      Printf.sprintf "schema error at %s (schema: %s)" loc url
  | Jsonschema.Content_schema ->
      Printf.sprintf "content schema validation failed at %s" loc
  | Jsonschema.Property_name { prop } ->
      Printf.sprintf "invalid property name %S at %s" prop loc
  | Jsonschema.Reference { kw; url } ->
      Printf.sprintf "reference %s -> %s failed at %s" kw url loc
  | Jsonschema.Ref_cycle { url; _ } ->
      Printf.sprintf "reference cycle detected: %s" url
  | Jsonschema.False_schema -> Printf.sprintf "false schema at %s" loc
  | Jsonschema.Type { want; _ } ->
      Printf.sprintf "type mismatch at %s: expected %s" loc
        (format_type_set want)
  | Jsonschema.Enum { want } ->
      Printf.sprintf "value not in enum at %s: expected one of [%s]" loc
        (String.concat ", " (List.map Yojson.Basic.to_string want))
  | Jsonschema.Const { want } ->
      Printf.sprintf "const mismatch at %s: expected %s" loc
        (Yojson.Basic.to_string want)
  | Jsonschema.Format { want; _ } ->
      Printf.sprintf "format validation failed at %s: expected %s format" loc
        want
  | Jsonschema.Min_properties { got; want } ->
      Printf.sprintf "too few properties at %s: got %d, min %d" loc got want
  | Jsonschema.Max_properties { got; want } ->
      Printf.sprintf "too many properties at %s: got %d, max %d" loc got want
  | Jsonschema.Additional_properties { got } ->
      Printf.sprintf
        "additional properties not allowed at %s: [%s]" loc
        (String.concat ", " got)
  | Jsonschema.Unevaluated_properties { got } ->
      Printf.sprintf "unevaluated properties at %s: [%s]" loc
        (String.concat ", " got)
  | Jsonschema.Unevaluated_items { got } ->
      Printf.sprintf "unevaluated items at %s: %d item(s)" loc got
  | Jsonschema.Required { want } ->
      Printf.sprintf "missing required properties at %s: [%s]" loc
        (String.concat ", " want)
  | Jsonschema.Dependency { prop; missing } ->
      Printf.sprintf
        "dependency error at %s: property %S requires [%s]" loc prop
        (String.concat ", " missing)
  | Jsonschema.Dependent_required { prop; missing } ->
      Printf.sprintf
        "dependent required error at %s: property %S requires [%s]" loc prop
        (String.concat ", " missing)
  | Jsonschema.Min_items { got; want } ->
      Printf.sprintf "too few items at %s: got %d, min %d" loc got want
  | Jsonschema.Max_items { got; want } ->
      Printf.sprintf "too many items at %s: got %d, max %d" loc got want
  | Jsonschema.Contains ->
      Printf.sprintf "contains constraint not satisfied at %s" loc
  | Jsonschema.Min_contains { want; _ } ->
      Printf.sprintf "minContains not satisfied at %s: need at least %d" loc
        want
  | Jsonschema.Max_contains { want; _ } ->
      Printf.sprintf "maxContains not satisfied at %s: need at most %d" loc
        want
  | Jsonschema.Unique_items { got = (i, j) } ->
      Printf.sprintf "duplicate items at %s: items %d and %d are equal" loc i j
  | Jsonschema.Additional_items { got } ->
      Printf.sprintf
        "additional items not allowed at %s: %d extra item(s)" loc got
  | Jsonschema.Min_length { got; want } ->
      Printf.sprintf "string too short at %s: got %d chars, min %d" loc got
        want
  | Jsonschema.Max_length { got; want } ->
      Printf.sprintf "string too long at %s: got %d chars, max %d" loc got want
  | Jsonschema.Pattern { want; _ } ->
      Printf.sprintf "string at %s does not match pattern %S" loc want
  | Jsonschema.Content_encoding { want; _ } ->
      Printf.sprintf "content encoding error at %s: expected %s" loc want
  | Jsonschema.Content_media_type { want; _ } ->
      Printf.sprintf "content media type error at %s: expected %s" loc want
  | Jsonschema.Minimum { got; want } ->
      Printf.sprintf "value too small at %s: got %g, min %g" loc got want
  | Jsonschema.Maximum { got; want } ->
      Printf.sprintf "value too large at %s: got %g, max %g" loc got want
  | Jsonschema.Exclusive_minimum { got; want } ->
      Printf.sprintf
        "value not above exclusive minimum at %s: got %g, exclusive min %g"
        loc got want
  | Jsonschema.Exclusive_maximum { got; want } ->
      Printf.sprintf
        "value not below exclusive maximum at %s: got %g, exclusive max %g"
        loc got want
  | Jsonschema.Multiple_of { got; want } ->
      Printf.sprintf "value %g is not a multiple of %g at %s" got want loc
  | Jsonschema.Not -> Printf.sprintf "not constraint violated at %s" loc
  | Jsonschema.All_of -> Printf.sprintf "allOf constraint violated at %s" loc
  | Jsonschema.Any_of -> Printf.sprintf "anyOf constraint violated at %s" loc
  | Jsonschema.One_of _ -> Printf.sprintf "oneOf constraint violated at %s" loc

let rec format_validation_error (e : Jsonschema.validation_error) : string =
  let loc = format_instance_location e.instance_location in
  let msg = format_error_kind e.kind loc in
  match e.causes with
  | [] -> msg
  | causes ->
      Printf.sprintf "%s [%s]" msg
        (String.concat "; " (List.map format_validation_error causes))

let format_compile_error (e : Jsonschema.compile_error) : string =
  match e with
  | Jsonschema.Parse_url_error { url; _ } ->
      Printf.sprintf "schema compile error: cannot parse URL %s" url
  | Jsonschema.Load_url_error { url; _ } ->
      Printf.sprintf "schema compile error: cannot load URL %s" url
  | Jsonschema.Unsupported_url_scheme { url } ->
      Printf.sprintf "schema compile error: unsupported URL scheme in %s" url
  | Jsonschema.Invalid_meta_schema_url { url; _ } ->
      Printf.sprintf "schema compile error: invalid meta-schema URL %s" url
  | Jsonschema.Unsupported_draft { url } ->
      Printf.sprintf "schema compile error: unsupported draft %s" url
  | Jsonschema.MetaSchema_cycle { url } ->
      Printf.sprintf "schema compile error: meta-schema cycle at %s" url
  | Jsonschema.Validation_error { url; _ } ->
      Printf.sprintf "schema compile error: schema validation failed at %s" url
  | Jsonschema.Parse_id_error { loc } ->
      Printf.sprintf "schema compile error: invalid $id at %s" loc
  | Jsonschema.Parse_anchor_error { loc } ->
      Printf.sprintf "schema compile error: invalid $anchor at %s" loc
  | Jsonschema.Duplicate_id { id; _ } ->
      Printf.sprintf "schema compile error: duplicate $id %s" id
  | Jsonschema.Duplicate_anchor { anchor; _ } ->
      Printf.sprintf "schema compile error: duplicate $anchor %s" anchor
  | Jsonschema.Invalid_json_pointer ptr ->
      Printf.sprintf "schema compile error: invalid JSON pointer %s" ptr
  | Jsonschema.Json_pointer_not_found ptr ->
      Printf.sprintf "schema compile error: JSON pointer not found %s" ptr
  | Jsonschema.Anchor_not_found { reference; _ } ->
      Printf.sprintf "schema compile error: anchor not found %s" reference
  | Jsonschema.Unsupported_vocabulary { vocabulary; _ } ->
      Printf.sprintf "schema compile error: unsupported vocabulary %s"
        vocabulary
  | Jsonschema.Invalid_regex { regex; _ } ->
      Printf.sprintf "schema compile error: invalid regex %s" regex
  | Jsonschema.Bug exn ->
      Printf.sprintf "schema compile error: internal error: %s"
        (Printexc.to_string exn)

let validate ~schema ~document =
  match Yojson.Safe.from_string document with
  | exception Yojson.Json_error msg ->
      Error (Printf.sprintf "not valid JSON: %s" msg)
  | exception exn ->
      Error (Printf.sprintf "not valid JSON: %s" (Printexc.to_string exn))
  | doc_safe ->
      let schema_basic = safe_to_basic schema in
      let doc_basic = safe_to_basic doc_safe in
      (* Default draft is 2020-12 per D-2 (Story #623 AC2).  When the schema
         carries a [$schema] field, the [jsonschema] package reads it and
         overrides this default with the named draft (AC3). *)
      (match
         Jsonschema.create_validator_from_json
           ~draft:Jsonschema.Draft2020_12
           ~schema:schema_basic
           ()
       with
      | Error ce -> Error (format_compile_error ce)
      | Ok validator ->
          (match Jsonschema.validate validator doc_basic with
          | Ok () -> Ok ()
          | Error ve -> Error (format_validation_error ve)))
