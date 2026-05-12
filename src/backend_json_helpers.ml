(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type json_string_map = (string * string) list

let json_string_map_to_yojson fields =
  `Assoc (List.map (fun (key, value) -> (key, `String value)) fields)

let json_string_map_of_yojson = function
  | `Assoc fields ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | (key, `String value) :: rest -> loop ((key, value) :: acc) rest
      | (key, _) :: _ -> Error (Printf.sprintf "expected string for %s" key)
    in
    loop [] fields
  | _ -> Error "expected JSON object"
