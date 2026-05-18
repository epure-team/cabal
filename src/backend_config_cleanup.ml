(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Internal text-cleanup helpers used by Backend_config_writer. *)

let contains_substr s needle =
  let nlen = String.length needle in
  let slen = String.length s in
  if nlen = 0 then true
  else if nlen > slen then false
  else
    let rec scan i =
      i + nlen <= slen
      && (String.sub s i nlen = needle || scan (i + 1))
    in
    scan 0

let remove_trailing_comma_before_closer line =
  let len = String.length line in
  let rec last_non_ws i =
    if i < 0 then None
    else
      match line.[i] with
      | ' ' | '\t' | '\r' -> last_non_ws (i - 1)
      | _ -> Some i
  in
  match last_non_ws (len - 1) with
  | Some i when line.[i] = ',' ->
    String.sub line 0 i ^ String.sub line (i + 1) (len - i - 1)
  | _ -> line

let next_non_blank_starts_with_closer = function
  | None -> false
  | Some line ->
    (match String.trim line with
     | s when String.length s > 0 -> s.[0] = '}' || s.[0] = ']'
     | _ -> false)

let remove_dangling_commas_before_closers lines =
  let rec first_non_blank = function
    | [] -> None
    | line :: rest ->
      if String.trim line = "" then first_non_blank rest else Some line
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | line :: rest ->
      let line =
        if next_non_blank_starts_with_closer (first_non_blank rest) then
          remove_trailing_comma_before_closer line
        else line
      in
      loop (line :: acc) rest
  in
  loop [] lines

let strip_managed_mcp_block content =
  let lines = String.split_on_char '\n' content in
  let in_mcp = ref false in
  let depth = ref 0 in
  let filtered =
    List.filter
      (fun line ->
        let trimmed = String.trim line in
        if !in_mcp then begin
          String.iter
            (fun c ->
              if c = '{' || c = '[' then incr depth
              else if c = '}' || c = ']' then decr depth)
            line ;
          if !depth <= 0 then begin
            in_mcp := false ;
            depth := 0
          end ;
          false
        end
        else if contains_substr trimmed "\"mcp\"" && contains_substr trimmed ":"
        then begin
          in_mcp := true ;
          String.iter
            (fun c ->
              if c = '{' || c = '[' then incr depth
              else if c = '}' || c = ']' then decr depth)
            line ;
          if !depth <= 0 then begin
            in_mcp := false ;
            depth := 0
          end ;
          false
        end
        else true)
      lines
  in
  filtered
  |> remove_dangling_commas_before_closers
  |> String.concat "\n"
