(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Pure JSON Schema validator — no I/O, no subprocess, no LLM. *)

let check_type ~expected ~doc =
  match expected with
  | "object" -> (
      match doc with
      | `Assoc _ -> Ok ()
      | other ->
          Error
            (Printf.sprintf
               "expected JSON object but got %s"
               (Yojson.Safe.to_string other)))
  | "array" -> (
      match doc with
      | `List _ -> Ok ()
      | other ->
          Error
            (Printf.sprintf
               "expected JSON array but got %s"
               (Yojson.Safe.to_string other)))
  | "string" -> (
      match doc with
      | `String _ -> Ok ()
      | other ->
          Error
            (Printf.sprintf
               "expected string but got %s"
               (Yojson.Safe.to_string other)))
  | "number" -> (
      match doc with
      | `Float _ | `Int _ -> Ok ()
      | other ->
          Error
            (Printf.sprintf
               "expected number but got %s"
               (Yojson.Safe.to_string other)))
  | "integer" -> (
      match doc with
      | `Int _ -> Ok ()
      | other ->
          Error
            (Printf.sprintf
               "expected integer but got %s"
               (Yojson.Safe.to_string other)))
  | "boolean" -> (
      match doc with
      | `Bool _ -> Ok ()
      | other ->
          Error
            (Printf.sprintf
               "expected boolean but got %s"
               (Yojson.Safe.to_string other)))
  | "null" -> (
      match doc with
      | `Null -> Ok ()
      | other ->
          Error
            (Printf.sprintf
               "expected null but got %s"
               (Yojson.Safe.to_string other)))
  | other ->
      Error (Printf.sprintf "unsupported schema type %S" other)

let check_required ~required ~doc =
  let fields =
    match doc with `Assoc kv -> List.map fst kv | _ -> []
  in
  let missing = List.filter (fun r -> not (List.mem r fields)) required in
  match missing with
  | [] -> Ok ()
  | _ ->
      Error
        (Printf.sprintf
           "missing required properties: [%s]"
           (String.concat ", " (List.map (Printf.sprintf "%S") missing)))

let validate ~schema ~document =
  match Yojson.Safe.from_string document with
  | exception Yojson.Json_error msg ->
      Error (Printf.sprintf "not valid JSON: %s" msg)
  | exception exn ->
      Error (Printf.sprintf "not valid JSON: %s" (Printexc.to_string exn))
  | doc ->
      let schema_fields =
        match schema with `Assoc kv -> kv | _ -> []
      in
      let ( let* ) = Result.bind in
      let* () =
        match List.assoc_opt "type" schema_fields with
        | Some (`String expected) -> check_type ~expected ~doc
        | Some _ -> Error "schema field 'type' must be a string"
        | None -> Ok ()
      in
      let* () =
        match List.assoc_opt "required" schema_fields with
        | Some (`List reqs) ->
            let required =
              List.filter_map
                (function `String s -> Some s | _ -> None)
                reqs
            in
            check_required ~required ~doc
        | Some _ -> Error "schema field 'required' must be an array"
        | None -> Ok ()
      in
      Ok ()
