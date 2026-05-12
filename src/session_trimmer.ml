(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** CMV-style session trimmer for Claude Code.

    Reimplements the core trim+fork logic from claude-code-cmv in OCaml.
    Between Builder rounds, the session JSONL is copied with a new UUID
    and trimmed: tool results are stubbed, thinking blocks removed, and
    images stripped. *)

type trim_metrics = {
  original_bytes : int;
  trimmed_bytes : int;
  tool_results_stubbed : int;
  thinking_blocks_removed : int;
  images_stripped : int;
  tool_inputs_stubbed : int;
}

let empty_metrics =
  {
    original_bytes = 0;
    trimmed_bytes = 0;
    tool_results_stubbed = 0;
    thinking_blocks_removed = 0;
    images_stripped = 0;
    tool_inputs_stubbed = 0;
  }

let trim_metrics_to_json m =
  `Assoc
    [
      ("original_bytes", `Int m.original_bytes);
      ("trimmed_bytes", `Int m.trimmed_bytes);
      ("tool_results_stubbed", `Int m.tool_results_stubbed);
      ("thinking_blocks_removed", `Int m.thinking_blocks_removed);
      ("images_stripped", `Int m.images_stripped);
      ("tool_inputs_stubbed", `Int m.tool_inputs_stubbed);
    ]

let add_metrics a b =
  {
    original_bytes = a.original_bytes + b.original_bytes;
    trimmed_bytes = a.trimmed_bytes + b.trimmed_bytes;
    tool_results_stubbed = a.tool_results_stubbed + b.tool_results_stubbed;
    thinking_blocks_removed =
      a.thinking_blocks_removed + b.thinking_blocks_removed;
    images_stripped = a.images_stripped + b.images_stripped;
    tool_inputs_stubbed = a.tool_inputs_stubbed + b.tool_inputs_stubbed;
  }

(* --- Path encoding -------------------------------------------------------- *)

let encode_working_dir path =
  (* Claude Code encodes paths by replacing every non-[a-zA-Z0-9-] character
     with a single '-'.  The leading '/' also becomes '-', so the result
     always starts with '-' for absolute paths.

     Examples (verified against ~/.claude/projects/ directory names):
       /home/user/project       -> -home-user-project
       /home/user/dev/epure  -> -home-user-dev-epure
       /path/.hidden/sub        -> -path--hidden-sub  (dot -> -)
       /tmp/my-project          -> -tmp-my-project    (existing '-' kept)
  *)
  let buf = Buffer.create (String.length path) in
  String.iter
    (fun c ->
      let is_safe =
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '-'
      in
      if is_safe then Buffer.add_char buf c else Buffer.add_char buf '-')
    path ;
  Buffer.contents buf

let claude_projects_dir () =
  let home =
    try Sys.getenv "HOME" with Not_found -> Sys.getenv "USERPROFILE"
  in
  Filename.concat home ".claude/projects"

(* --- Session file location ------------------------------------------------ *)

let find_session_file ~working_dir ~session_id =
  let encoded = encode_working_dir working_dir in
  let dir = Filename.concat (claude_projects_dir ()) encoded in
  let path = Filename.concat dir (session_id ^ ".jsonl") in
  if Sys.file_exists path then Some path else None

(* --- UUID generation ------------------------------------------------------ *)

let () = Random.self_init ()

let generate_uuid () =
  let hex = "0123456789abcdef" in
  let seg len =
    let buf = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set buf i hex.[Random.int 16]
    done ;
    Bytes.to_string buf
  in
  Printf.sprintf "%s-%s-%s-%s-%s" (seg 8) (seg 4) (seg 4) (seg 4) (seg 12)

(* --- Trimming helpers ----------------------------------------------------- *)

(** Check if a JSON string is a write/edit tool name that should have
    its input stubbed. *)
let is_write_tool name =
  name = "Write" || name = "Edit" || name = "NotebookEdit"

(** Stub tool_result content blocks exceeding threshold. *)
let stub_tool_result ~threshold content =
  let open Yojson.Safe.Util in
  match content with
  | `List blocks ->
      let stubbed = ref 0 in
      let images = ref 0 in
      let blocks' =
        List.filter_map
          (fun block ->
            let btype =
              try block |> member "type" |> to_string with _ -> ""
            in
            if btype = "image" then (
              incr images ;
              None)
            else
              match btype with
              | "text" ->
                  let text =
                    try block |> member "text" |> to_string with _ -> ""
                  in
                  if String.length text > threshold then (
                    incr stubbed ;
                    let stub =
                      Printf.sprintf
                        "[Trimmed tool result: ~%d chars]"
                        (String.length text)
                    in
                    Some
                      (`Assoc [("type", `String "text"); ("text", `String stub)]))
                  else Some block
              | _ -> Some block)
          blocks
      in
      (`List blocks', !stubbed, !images)
  | _ -> (content, 0, 0)

(** Stub tool_use input for write/edit tools. *)
let stub_tool_input ~threshold block =
  let open Yojson.Safe.Util in
  let btype = try block |> member "type" |> to_string with _ -> "" in
  if btype <> "tool_use" then (block, false)
  else
    let name = try block |> member "name" |> to_string with _ -> "" in
    if not (is_write_tool name) then (block, false)
    else
      let input = block |> member "input" in
      let input_s = Yojson.Safe.to_string input in
      if String.length input_s <= threshold then (block, false)
      else
        let stub =
          `String
            (Printf.sprintf
               "[Trimmed input: ~%d chars]"
               (String.length input_s))
        in
        let block' =
          match block with
          | `Assoc fields ->
              `Assoc
                (List.map
                   (fun (k, v) -> if k = "input" then (k, stub) else (k, v))
                   fields)
          | _ -> block
        in
        (block', true)

(** Strip thinking blocks from content array. *)
let strip_thinking content =
  let open Yojson.Safe.Util in
  match content with
  | `List blocks ->
      let removed = ref 0 in
      let blocks' =
        List.filter
          (fun block ->
            let btype =
              try block |> member "type" |> to_string with _ -> ""
            in
            if btype = "thinking" || btype = "redacted_thinking" then (
              incr removed ;
              false)
            else true)
          blocks
      in
      (`List blocks', !removed)
  | _ -> (content, 0)

(** Strip usage metadata from a message JSON. *)
let strip_usage json =
  match json with
  | `Assoc fields -> `Assoc (List.filter (fun (k, _) -> k <> "usage") fields)
  | _ -> json

(* --- Per-line trimming ---------------------------------------------------- *)

let trim_line ~threshold line =
  let line_trimmed = String.trim line in
  if String.length line_trimmed = 0 then None
  else
    match Yojson.Safe.from_string line_trimmed with
    | exception _ -> Some (line, empty_metrics)
    | json ->
        let open Yojson.Safe.Util in
        (* Check for entries to drop entirely *)
        let entry_type =
          try json |> member "type" |> to_string with _ -> ""
        in
        (* Drop file-history-snapshot and queue-operation entries *)
        if
          entry_type = "file-history-snapshot" || entry_type = "queue-operation"
        then None
        else
          let role =
            try json |> member "message" |> member "role" |> to_string
            with _ -> ""
          in
          let content =
            try json |> member "message" |> member "content" with _ -> `Null
          in
          let metrics = ref empty_metrics in
          let update f =
            let m = !metrics in
            metrics := f m
          in
          (* Process based on role *)
          let json' =
            match role with
            | "assistant" ->
                (* Strip thinking blocks *)
                let content', thinking_removed = strip_thinking content in
                if thinking_removed > 0 then
                  update (fun m ->
                      {
                        m with
                        thinking_blocks_removed =
                          m.thinking_blocks_removed + thinking_removed;
                      }) ;
                (* Stub tool_use inputs for write tools *)
                let content'' =
                  match content' with
                  | `List blocks ->
                      let blocks' =
                        List.map
                          (fun block ->
                            let block', was_stubbed =
                              stub_tool_input ~threshold block
                            in
                            if was_stubbed then
                              update (fun m ->
                                  {
                                    m with
                                    tool_inputs_stubbed =
                                      m.tool_inputs_stubbed + 1;
                                  }) ;
                            block')
                          blocks
                      in
                      `List blocks'
                  | _ -> content'
                in
                (* Update message content *)
                let msg = try json |> member "message" with _ -> `Null in
                let msg' =
                  match msg with
                  | `Assoc fields ->
                      `Assoc
                        (List.map
                           (fun (k, v) ->
                             if k = "content" then (k, content'') else (k, v))
                           fields)
                  | _ -> msg
                in
                let json' =
                  match json with
                  | `Assoc fields ->
                      `Assoc
                        (List.map
                           (fun (k, v) ->
                             if k = "message" then (k, msg') else (k, v))
                           fields)
                  | _ -> json
                in
                strip_usage json'
            | "user" ->
                (* Stub tool_result content and strip images *)
                let content', stubbed, images =
                  stub_tool_result ~threshold content
                in
                if stubbed > 0 then
                  update (fun m ->
                      {
                        m with
                        tool_results_stubbed = m.tool_results_stubbed + stubbed;
                      }) ;
                if images > 0 then
                  update (fun m ->
                      {m with images_stripped = m.images_stripped + images}) ;
                let msg = try json |> member "message" with _ -> `Null in
                let msg' =
                  match msg with
                  | `Assoc fields ->
                      `Assoc
                        (List.map
                           (fun (k, v) ->
                             if k = "content" then (k, content') else (k, v))
                           fields)
                  | _ -> msg
                in
                let json' =
                  match json with
                  | `Assoc fields ->
                      `Assoc
                        (List.map
                           (fun (k, v) ->
                             if k = "message" then (k, msg') else (k, v))
                           fields)
                  | _ -> json
                in
                strip_usage json'
            | _ -> strip_usage json
          in
          let result_line = Yojson.Safe.to_string json' in
          let m = !metrics in
          Some
            ( result_line,
              {
                m with
                original_bytes = String.length line;
                trimmed_bytes = String.length result_line;
              } )

(* --- Fork + trim ---------------------------------------------------------- *)

let fork_trimmed ~env ~working_dir ~session_id ?(threshold = 500) () =
  match find_session_file ~working_dir ~session_id with
  | None ->
      Error
        (Printf.sprintf "Session file not found for session_id=%s" session_id)
  | Some source_path ->
      let fs = Eio.Stdenv.fs env in
      let new_id = generate_uuid () in
      let encoded = encode_working_dir working_dir in
      let project_dir = Filename.concat (claude_projects_dir ()) encoded in
      let dest_path = Filename.concat project_dir (new_id ^ ".jsonl") in
      (* Read source, trim, write destination *)
      let source_content = Eio.Path.load Eio.Path.(fs / source_path) in
      let lines = String.split_on_char '\n' source_content in
      let acc_metrics = ref empty_metrics in
      let trimmed_lines =
        List.filter_map
          (fun line ->
            match trim_line ~threshold line with
            | None -> None
            | Some (trimmed, m) ->
                acc_metrics := add_metrics !acc_metrics m ;
                Some trimmed)
          lines
      in
      let trimmed_content = String.concat "\n" trimmed_lines in
      (* Write trimmed session file *)
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(fs / dest_path)
        trimmed_content ;
      (* Update sessions-index.json *)
      let index_path = Filename.concat project_dir "sessions-index.json" in
      let index_json =
        try Yojson.Safe.from_string (Eio.Path.load Eio.Path.(fs / index_path))
        with _ -> `Assoc []
      in
      let updated_index =
        match index_json with
        | `Assoc entries ->
            let new_entry =
              `Assoc
                [
                  ("sessionId", `String new_id);
                  ("forkedFrom", `String session_id);
                  ( "createdAt",
                    `String
                      (let t = Unix.gettimeofday () in
                       let tm = Unix.gmtime t in
                       Printf.sprintf
                         "%04d-%02d-%02dT%02d:%02d:%02dZ"
                         (tm.tm_year + 1900)
                         (tm.tm_mon + 1)
                         tm.tm_mday
                         tm.tm_hour
                         tm.tm_min
                         tm.tm_sec) );
                ]
            in
            (* sessions-index.json maps session IDs to metadata *)
            `Assoc ((new_id, new_entry) :: entries)
        | _ ->
            (* If the index is not an object, create a new one *)
            `Assoc [(new_id, `Assoc [("sessionId", `String new_id)])]
      in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(fs / index_path)
        (Yojson.Safe.pretty_to_string updated_index) ;
      let final_metrics =
        {
          !acc_metrics with
          original_bytes = String.length source_content;
          trimmed_bytes = String.length trimmed_content;
        }
      in
      Ok (new_id, final_metrics)
