(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type role = User | Assistant | System | Tool [@@deriving show, eq, yojson]

type tool_ref = {
  name : string;
  input_summary : string;
  output_summary : string;
}
[@@deriving show, eq, yojson]

type provenance = {
  source_session : string option;
  client : string option;
}
[@@deriving show, eq, yojson]

type event = {
  role : role;
  text : string;
  tool : tool_ref option;
  model : string option;
  provenance : provenance;
  timestamp : string option;
  tokens : Backend_types.cost option;
}
[@@deriving show, eq, yojson]

type t = event list [@@deriving show, eq, yojson]

let empty_provenance = { source_session = None; client = None }

let make_event ?tool ?model ?(provenance = empty_provenance) ?timestamp ?tokens
    role text =
  { role; text; tool; model; provenance; timestamp; tokens }

let normalized_text e =
  (* Collapse any run of whitespace to a single space and trim the ends. *)
  let buf = Buffer.create (String.length e.text) in
  let in_ws = ref true (* leading whitespace is dropped *) in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\t' | '\n' | '\r' | '\012' -> in_ws := true
      | _ ->
          if !in_ws && Buffer.length buf > 0 then Buffer.add_char buf ' ';
          in_ws := false;
          Buffer.add_char buf c)
    e.text;
  Buffer.contents buf
