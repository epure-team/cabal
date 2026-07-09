(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Portable_session
module U = Yojson.Safe.Util

let string_opt json field = json |> U.member field |> U.to_string_option

let usage_to_cost usage =
  match usage with
  | `Null -> None
  | _ ->
      let int_opt field = usage |> U.member field |> U.to_int_option in
      Some
        {
          Backend_types.tokens_input = int_opt "input_tokens";
          tokens_output = int_opt "output_tokens";
          cost_usd = None;
          cache_creation_input_tokens = int_opt "cache_creation_input_tokens";
          cache_read_input_tokens = int_opt "cache_read_input_tokens";
        }

(* Build the events contributed by a single content block. *)
let events_of_block ~role ~model ~provenance ~timestamp ~tokens block =
  let mk r ?tool text =
    make_event ?tool ?model ~provenance ?timestamp ?tokens r text
  in
  match block |> U.member "type" |> U.to_string_option with
  | Some "text" ->
      let text = block |> U.member "text" |> U.to_string_option in
      Option.fold ~none:[] ~some:(fun t -> [ mk role t ]) text
  | Some "thinking" -> [] (* dropped: internal reasoning is not portable *)
  | Some "tool_use" ->
      let name = Option.value ~default:"" (string_opt block "name") in
      let input = block |> U.member "input" in
      let input_summary =
        match input with `Null -> "" | j -> Yojson.Safe.to_string j
      in
      [ mk Tool ~tool:{ name; input_summary; output_summary = "" } "" ]
  | Some "tool_result" ->
      let name = Option.value ~default:"" (string_opt block "tool_use_id") in
      let content = block |> U.member "content" in
      let output_summary =
        match content with
        | `String s -> s
        | `Null -> ""
        | j -> Yojson.Safe.to_string j
      in
      [ mk Tool ~tool:{ name; input_summary = ""; output_summary } "" ]
  | _ -> []

let events_of_record json =
  match json |> U.member "type" |> U.to_string_option with
  | Some (("user" | "assistant") as typ) -> (
      let role = if typ = "assistant" then Assistant else User in
      let message = json |> U.member "message" in
      match message with
      | `Null -> []
      | _ ->
          let model = string_opt message "model" in
          let timestamp = string_opt json "timestamp" in
          let provenance =
            {
              source_session = string_opt json "sessionId";
              client = Some "claude-code";
            }
          in
          let tokens = usage_to_cost (message |> U.member "usage") in
          let mk ?tool text =
            make_event ?tool ?model ~provenance ?timestamp ?tokens role text
          in
          (match message |> U.member "content" with
          | `String s -> [ mk s ]
          | `List blocks ->
              List.concat_map
                (fun b ->
                  (* A single foreign/non-object block must not sink the whole
                     record; skip it and keep the rest. *)
                  try
                    events_of_block ~role ~model ~provenance ~timestamp ~tokens b
                  with _ -> [])
                blocks
          | _ -> []))
  | _ -> [] (* summaries, snapshots, mode markers, etc. *)

let claude_code content =
  String.split_on_char '\n' content
  |> List.concat_map (fun line ->
         let line = String.trim line in
         if line = "" then []
         else
           match Yojson.Safe.from_string line with
           (* Guard record processing too: a line can be valid JSON but a
              scalar/array (or carry a non-object [message]), where the Yojson
              [member] accessors would raise [Type_error].  Honor the "never
              raises, malformed lines skipped" contract. *)
           | json -> ( try events_of_record json with _ -> [])
           | exception _ -> [] (* skip non-JSON lines *))

let load_claude_code_session ~working_dir ~session_id =
  match Session_trimmer.find_session_file ~working_dir ~session_id with
  | None -> []
  | Some path -> (
      match In_channel.with_open_bin path In_channel.input_all with
      | content -> claude_code content
      | exception _ -> [])
