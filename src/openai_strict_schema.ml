(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let max_depth = 10

(* Allowlist, not denylist: anything absent here fails the check.  [format]
   is deliberately excluded even though OpenAI documents a subset of formats:
   an unsupported format value is a request-level rejection, and dropping the
   native constraint is the cheaper failure. *)
let allowed_keywords =
  [
    "$schema";
    "type";
    "properties";
    "required";
    "additionalProperties";
    "items";
    "enum";
    "anyOf";
    "description";
    "title";
    "minimum";
    "maximum";
    "exclusiveMinimum";
    "exclusiveMaximum";
    "multipleOf";
    "minLength";
    "maxLength";
    "pattern";
    "minItems";
    "maxItems";
  ]

let known_types =
  ["string"; "number"; "integer"; "boolean"; "object"; "array"; "null"]

let ( let* ) = Result.bind

let fail path fmt = Printf.ksprintf (fun msg -> Error (path ^ ": " ^ msg)) fmt

let rec iter_result f = function
  | [] -> Ok ()
  | x :: rest ->
      let* () = f x in
      iter_result f rest

let types_of path = function
  | None -> Ok []
  | Some (`String t) -> Ok [t]
  | Some (`List ts) ->
      iter_result
        (function
          | `String _ -> Ok () | _ -> fail path "non-string in type list")
        ts
      |> Result.map (fun () ->
          List.filter_map (function `String t -> Some t | _ -> None) ts)
  | Some _ -> fail path "type is neither a string nor a list"

let check_keywords path fields =
  iter_result
    (fun (k, _) ->
      if List.mem k allowed_keywords then Ok ()
      else fail path "keyword %S is outside the strict subset" k)
    fields

let check_required_covers path props fields =
  let keys = List.map fst props in
  match List.assoc_opt "required" fields with
  | Some (`List req) ->
      let names =
        List.filter_map (function `String s -> Some s | _ -> None) req
      in
      if List.length names <> List.length req then
        fail path "required contains a non-string"
      else
        let missing = List.filter (fun k -> not (List.mem k names)) keys in
        let extra = List.filter (fun k -> not (List.mem k keys)) names in
        if missing <> [] then
          fail
            path
            "required omits propert%s %s"
            (if List.length missing = 1 then "y" else "ies")
            (String.concat ", " missing)
        else if extra <> [] then
          fail path "required names unknown %s" (String.concat ", " extra)
        else Ok ()
  | Some _ -> fail path "required is not an array"
  | None ->
      if keys = [] then Ok ()
      else fail path "object without required listing every property"

let rec check_node ~depth path (node : Yojson.Safe.t) =
  if depth > max_depth then fail path "nesting deeper than %d" max_depth
  else
    match node with
    | `Assoc fields ->
        let* () = check_keywords path fields in
        let* types = types_of path (List.assoc_opt "type" fields) in
        let* () =
          iter_result
            (fun t ->
              if List.mem t known_types then Ok ()
              else fail path "unknown type %S" t)
            types
        in
        let any_of = List.assoc_opt "anyOf" fields in
        let* () =
          if types = [] && any_of = None then
            fail path "schema has neither type nor anyOf"
          else Ok ()
        in
        let* () = check_any_of ~depth path any_of in
        let* () =
          if List.mem "object" types then check_object ~depth path fields
          else Ok ()
        in
        if List.mem "array" types then check_array ~depth path fields else Ok ()
    | _ -> fail path "schema is not a JSON object"

and check_any_of ~depth path = function
  | None -> Ok ()
  | Some (`List branches) ->
      List.mapi (fun i b -> (i, b)) branches
      |> iter_result (fun (i, b) ->
          check_node ~depth (Printf.sprintf "%s.anyOf[%d]" path i) b)
  | Some _ -> fail path "anyOf is not an array"

and check_object ~depth path fields =
  let* () =
    match List.assoc_opt "additionalProperties" fields with
    | Some (`Bool false) -> Ok ()
    | _ -> fail path "object without additionalProperties: false"
  in
  let* props =
    match List.assoc_opt "properties" fields with
    | Some (`Assoc props) -> Ok props
    | Some _ -> fail path "properties is not an object"
    | None -> fail path "object without properties"
  in
  let* () = check_required_covers path props fields in
  iter_result
    (fun (k, sub) ->
      check_node ~depth:(depth + 1) (path ^ ".properties." ^ k) sub)
    props

and check_array ~depth path fields =
  match List.assoc_opt "items" fields with
  | Some items -> check_node ~depth:(depth + 1) (path ^ ".items") items
  | None -> fail path "array without items"

let check (schema : Yojson.Safe.t) =
  match schema with
  | `Assoc fields -> (
      match List.assoc_opt "type" fields with
      | Some (`String "object") -> check_node ~depth:1 "$" schema
      | _ -> fail "$" "root is not a plain object schema")
  | _ -> fail "$" "schema is not a JSON object"
