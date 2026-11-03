(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type request = {custom_id : string; group : string; params : Yojson.Safe.t}

type ticket = {batch_id : string; keys : (string * string) list}

type transport = {
  call :
    meth:[`GET | `POST] ->
    path:string ->
    body:string option ->
    (int * string, string) result;
  stream_lines :
    path:string ->
    on_line:(string -> (unit, string) result) ->
    (int, string) result;
}

type status = In_progress | Canceling | Ended

type usage = {
  input_tokens : int;
  output_tokens : int;
  cache_creation_input_tokens : int option;
  cache_read_input_tokens : int option;
}

type outcome =
  | Succeeded of {message : Yojson.Safe.t; usage : usage}
  | Incomplete of {message : Yojson.Safe.t; usage : usage; stop_reason : string}
  | Errored of string
  | Canceled
  | Expired

type item = {custom_id : string; group : string; outcome : outcome}

type finalized = {ticket_id : string; results : item list}

let items finalized = finalized.results

type collection = Pending of status | Complete of finalized

type completeness = Require_all | Accept_partial

type reduce_plan = {ready : request list; blocked : (string * string list) list}

let ( let* ) = Result.bind

let field name = function `Assoc xs -> List.assoc_opt name xs | _ -> None

let str name j =
  match field name j with Some (`String s) -> Some s | _ -> None

let list name j =
  match field name j with Some (`List xs) -> Some xs | _ -> None

let positive = function Some (`Int n) when n > 0 -> true | _ -> false

let valid_message = function
  | `Assoc _ as message -> (
      match (str "role" message, field "content" message) with
      | Some ("user" | "assistant"), Some (`String s) -> s <> ""
      | Some ("user" | "assistant"), Some (`List (_ :: _ as blocks)) ->
          List.for_all
            (function
              | `Assoc _ as block -> str "type" block <> None | _ -> false)
            blocks
      | _ -> false)
  | _ -> false

let opaque label s =
  if String.length s = 0 || String.length s > 128 then
    Error (label ^ " must have 1..128 bytes")
  else if String.exists (fun c -> Char.code c < 32 || Char.code c = 127) s then
    Error (label ^ " contains a control byte")
  else Ok ()

let valid_custom_id s =
  let n = String.length s in
  n >= 1 && n <= 64
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
       s

let validate_keys keys =
  let seen = Hashtbl.create (List.length keys) in
  let rec loop = function
    | [] -> Ok ()
    | (id, group) :: rest ->
        let* () =
          if valid_custom_id id then Ok ()
          else Error "custom_id must match [A-Za-z0-9_-]{1,64}"
        in
        let* () = opaque "group" group in
        if Hashtbl.mem seen id then Error "duplicate custom_id"
        else (
          Hashtbl.add seen id () ;
          loop rest)
  in
  if keys = [] then Error "batch must contain at least one request"
  else if List.length keys > 100_000 then Error "batch exceeds 100000 requests"
  else loop keys

let validate_params = function
  | `Assoc _ as params -> (
      match (str "model" params, list "messages" params) with
      | Some model, Some (_ :: _ as messages)
        when String.trim model <> ""
             && positive (field "max_tokens" params)
             && List.for_all valid_message messages -> (
          match (field "stream" params, field "speed" params) with
          | (None | Some (`Bool false)), None -> Ok ()
          | _ -> Error "batch params contain unsupported stream or speed")
      | _ -> Error "batch params need model, positive max_tokens, and messages")
  | _ -> Error "batch params must be an object"

let validate_requests (requests : request list) =
  let* () =
    validate_keys
      (List.map (fun (x : request) -> (x.custom_id, x.group)) requests)
  in
  let rec loop = function
    | [] -> Ok ()
    | (x : request) :: xs ->
        let* () = validate_params x.params in
        loop xs
  in
  loop requests

let ticket_to_json t =
  `Assoc
    [
      ("version", `Int 1);
      ("batch_id", `String t.batch_id);
      ( "requests",
        `List
          (List.map
             (fun (id, group) ->
               `Assoc [("custom_id", `String id); ("group", `String group)])
             t.keys) );
    ]

let ticket_of_json j =
  match (field "version" j, str "batch_id" j, list "requests" j) with
  | Some (`Int 1), Some batch_id, Some rows ->
      let* () = opaque "batch_id" batch_id in
      let rec parse acc = function
        | [] -> Ok (List.rev acc)
        | row :: rest -> (
            match (str "custom_id" row, str "group" row) with
            | Some id, Some group -> parse ((id, group) :: acc) rest
            | _ -> Error "invalid ticket request entry")
      in
      let* keys = parse [] rows in
      let* () = validate_keys keys in
      Ok {batch_id; keys}
  | _ -> Error "invalid batch ticket"

let batch_id t = t.batch_id

let parse_json body =
  try Ok (Yojson.Safe.from_string body)
  with Yojson.Json_error _ -> Error "invalid provider JSON"

let call transport ~meth ~path ~body =
  let* status, response = transport.call ~meth ~path ~body in
  if status >= 200 && status < 300 then Ok response
  else Error (Printf.sprintf "provider HTTP %d" status)

let submit transport (requests : request list) =
  let* () = validate_requests requests in
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [
           ( "requests",
             `List
               (List.map
                  (fun (x : request) ->
                    `Assoc
                      [("custom_id", `String x.custom_id); ("params", x.params)])
                  requests) );
         ])
  in
  if String.length body > 256_000_000 then Error "batch exceeds 256 MB"
  else
    let* response =
      call transport ~meth:`POST ~path:"/v1/messages/batches" ~body:(Some body)
    in
    let* j = parse_json response in
    match (str "id" j, str "processing_status" j) with
    | Some id, Some ("in_progress" | "ended") ->
        let* () = opaque "batch_id" id in
        Ok
          {
            batch_id = id;
            keys =
              List.map (fun (x : request) -> (x.custom_id, x.group)) requests;
          }
    | _ -> Error "invalid batch submission response"

let encode_path_segment s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' ->
          Buffer.add_char b c
      | _ -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s ;
  Buffer.contents b

let path t = "/v1/messages/batches/" ^ encode_path_segment t.batch_id

let poll transport t =
  let* body = call transport ~meth:`GET ~path:(path t) ~body:None in
  let* j = parse_json body in
  match (str "id" j, str "processing_status" j) with
  | Some id, Some s when String.equal id t.batch_id -> (
      match s with
      | "in_progress" -> Ok In_progress
      | "canceling" -> Ok Canceling
      | "ended" -> Ok Ended
      | _ -> Error "unknown batch status")
  | _ -> Error "batch status identity mismatch"

let nonnegative_int name j =
  match field name j with
  | Some (`Int n) when n >= 0 -> Ok n
  | _ -> Error "invalid batch usage"

let optional_nonnegative_int name j =
  match field name j with
  | None | Some `Null -> Ok None
  | Some (`Int n) when n >= 0 -> Ok (Some n)
  | _ -> Error "invalid batch usage"

let parse_usage message =
  match field "usage" message with
  | None -> Error "missing batch usage"
  | Some j ->
      let* input_tokens = nonnegative_int "input_tokens" j in
      let* output_tokens = nonnegative_int "output_tokens" j in
      let* cache_creation_input_tokens =
        optional_nonnegative_int "cache_creation_input_tokens" j
      in
      let* cache_read_input_tokens =
        optional_nonnegative_int "cache_read_input_tokens" j
      in
      Ok
        {
          input_tokens;
          output_tokens;
          cache_creation_input_tokens;
          cache_read_input_tokens;
        }

let parse_outcome j =
  match str "type" j with
  | Some "succeeded" -> (
      match field "message" j with
      | Some (`Assoc _ as message) -> (
          let* usage = parse_usage message in
          match str "stop_reason" message with
          | Some ("end_turn" | "stop_sequence") ->
              Ok (Succeeded {message; usage})
          | Some reason ->
              Ok (Incomplete {message; usage; stop_reason = reason})
          | None -> Error "missing batch stop reason")
      | _ -> Error "missing batch message")
  | Some "errored" -> (
      match field "error" j with
      | Some error -> (
          match field "error" error with
          | Some inner -> (
              match str "type" inner with
              | Some typ -> Ok (Errored typ)
              | None -> Error "invalid batch error")
          | None -> Error "invalid batch error")
      | None -> Error "missing batch error")
  | Some "canceled" -> Ok Canceled
  | Some "expired" -> Ok Expired
  | _ -> Error "unknown batch result type"

let parse_results transport t =
  let wanted = Hashtbl.create (List.length t.keys) in
  List.iter (fun (id, group) -> Hashtbl.add wanted id group) t.keys ;
  let found = Hashtbl.create (List.length t.keys) in
  let parse_line line =
    if String.trim line = "" then Ok ()
    else
      let* j = parse_json line in
      let* id =
        match str "custom_id" j with
        | Some id -> Ok id
        | None -> Error "batch result lacks custom_id"
      in
      let* group =
        match Hashtbl.find_opt wanted id with
        | Some group -> Ok group
        | None -> Error "unknown batch result custom_id"
      in
      if Hashtbl.mem found id then Error "duplicate batch result custom_id"
      else
        let* outcome =
          match field "result" j with
          | Some result -> parse_outcome result
          | None -> Error "batch result lacks outcome"
        in
        Hashtbl.add found id {custom_id = id; group; outcome} ;
        Ok ()
  in
  let parse_failure = ref None in
  let on_line line =
    match parse_line line with
    | Ok () -> Ok ()
    | Error e as failed ->
        parse_failure := Some e ;
        failed
  in
  let streamed = transport.stream_lines ~path:(path t ^ "/results") ~on_line in
  let* status =
    match !parse_failure with Some e -> Error e | None -> streamed
  in
  let* () =
    if status >= 200 && status < 300 then Ok ()
    else Error (Printf.sprintf "provider HTTP %d" status)
  in
  let rec ordered acc = function
    | [] -> Ok (List.rev acc)
    | (id, _) :: rest -> (
        match Hashtbl.find_opt found id with
        | Some item -> ordered (item :: acc) rest
        | None -> Error "missing batch result custom_id")
  in
  ordered [] t.keys

let collect transport t =
  let* state = poll transport t in
  match state with
  | In_progress | Canceling -> Ok (Pending state)
  | Ended ->
      let* items = parse_results transport t in
      Ok (Complete {ticket_id = t.batch_id; results = items})

let is_success = function Succeeded _ -> true | _ -> false

let plan_reducers ~completeness ~ticket ~finalized ~reducer_id ~build =
  if not (String.equal finalized.ticket_id ticket.batch_id) then
    Error "reduce collection belongs to another batch"
  else
    let items = finalized.results in
    let by_id = Hashtbl.create (List.length items) in
    List.iter
      (fun item ->
        if Hashtbl.mem by_id item.custom_id then
          Hashtbl.replace by_id item.custom_id None
        else Hashtbl.add by_id item.custom_id (Some item))
      items ;
    let* ordered =
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | (id, group) :: rest -> (
            match Hashtbl.find_opt by_id id with
            | Some (Some item) when String.equal group item.group ->
                go (item :: acc) rest
            | _ -> Error "reduce inputs are not a complete correlated batch")
      in
      go [] ticket.keys
    in
    if Hashtbl.length by_id <> List.length ticket.keys then
      Error "reduce inputs contain unknown or duplicate IDs"
    else
      let groups = Hashtbl.create 16 in
      List.iter
        (fun item ->
          let xs =
            Option.value (Hashtbl.find_opt groups item.group) ~default:[]
          in
          Hashtbl.replace groups item.group (item :: xs))
        ordered ;
      let names =
        Hashtbl.fold (fun group _ acc -> group :: acc) groups []
        |> List.sort String.compare
      in
      let rec make ready blocked = function
        | [] ->
            let ready = List.rev ready and blocked = List.rev blocked in
            let* () = if ready = [] then Ok () else validate_requests ready in
            Ok {ready; blocked}
        | group :: rest ->
            let grouped = List.rev (Hashtbl.find groups group) in
            let failed =
              List.filter_map
                (fun item ->
                  if is_success item.outcome then None else Some item.custom_id)
                grouped
            in
            if completeness = Require_all && failed <> [] then
              make ready ((group, failed) :: blocked) rest
            else
              let* params = build ~group ~items:grouped in
              make
                ({custom_id = reducer_id group; group; params} :: ready)
                blocked
                rest
      in
      make [] [] names
