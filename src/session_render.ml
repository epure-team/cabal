(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Portable_session

let default_session_id = "00000000-0000-4000-8000-000000000000"
let default_timestamp = "1970-01-01T00:00:00.000Z"

(* Deterministic uuid-shaped id from an index, so rendering has no clock or
   randomness and round-trips reproducibly. *)
let synthetic_uuid i = Printf.sprintf "00000000-0000-4000-8000-%012d" i

let record ~index ~parent ~session_id ~role_str ~content ~timestamp =
  `Assoc
    [
      ("type", `String role_str);
      ("uuid", `String (synthetic_uuid index));
      ("parentUuid", match parent with None -> `Null | Some p -> `String p);
      ("isSidechain", `Bool false);
      ("sessionId", `String session_id);
      ("timestamp", `String timestamp);
      ("message", `Assoc [ ("role", `String role_str); ("content", content) ]);
    ]

let claude_code ?(session_id = default_session_id) evs =
  (* Conversation-only: keep user/assistant turns, drop tool/system events. *)
  let convo =
    List.filter (fun e -> e.role = User || e.role = Assistant) evs
  in
  let buf = Buffer.create 1024 in
  let _ =
    List.fold_left
      (fun (index, parent) e ->
        let role_str = if e.role = Assistant then "assistant" else "user" in
        let content =
          if e.role = Assistant then
            `List [ `Assoc [ ("type", `String "text"); ("text", `String e.text) ] ]
          else `String e.text
        in
        let timestamp = Option.value ~default:default_timestamp e.timestamp in
        let rec_json =
          record ~index ~parent ~session_id ~role_str ~content ~timestamp
        in
        Buffer.add_string buf (Yojson.Safe.to_string rec_json);
        Buffer.add_char buf '\n';
        (index + 1, Some (synthetic_uuid index)))
      (* Start at 1 so the first record's uuid never collides with the
         all-zero [default_session_id]. *)
      (1, None) convo
  in
  Buffer.contents buf
