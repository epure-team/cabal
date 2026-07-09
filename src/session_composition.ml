(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Portable_session

type transform = event list -> event list

type stage =
  | Filter of (event -> bool)
  | Dedup
  | Reorder
  | Take of int
  | Drop of int
  | Compact of transform

let filter p evs = List.filter p evs

let dedup evs =
  (* Keep the first occurrence of each event identity.  Identity is
     (role, normalized text, tool payload): text-bearing turns dedup on their
     text, while distinct tool interactions (which carry empty text) stay
     distinct via their tool summary rather than all collapsing to one. *)
  let seen = Hashtbl.create 64 in
  let tool_key = function
    | None -> ""
    | Some { name; input_summary; output_summary } ->
        String.concat "\x00" [ name; input_summary; output_summary ]
  in
  List.filter
    (fun e ->
      let key = (show_role e.role, normalized_text e, tool_key e.tool) in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    evs

let reorder evs =
  (* Lexical order on ISO-8601 timestamps is chronological; [None] sorts
     first.  [List.stable_sort] keeps the relative order of equal keys. *)
  let key e = match e.timestamp with None -> "" | Some ts -> ts in
  List.stable_sort (fun a b -> String.compare (key a) (key b)) evs

let take n evs =
  let n = max 0 n in
  List.filteri (fun i _ -> i < n) evs

let drop n evs =
  let n = max 0 n in
  List.filteri (fun i _ -> i >= n) evs

let merge sources = List.concat sources

let apply_stage evs = function
  | Filter p -> filter p evs
  | Dedup -> dedup evs
  | Reorder -> reorder evs
  | Take n -> take n evs
  | Drop n -> drop n evs
  | Compact f -> f evs

let run stages evs = List.fold_left apply_stage evs stages
