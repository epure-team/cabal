(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal
open Agent_batch

let params text =
  `Assoc
    [
      ("model", `String "claude-sonnet-5-5");
      ("max_tokens", `Int 100);
      ( "messages",
        `List [`Assoc [("role", `String "user"); ("content", `String text)]] );
    ]

let req id group = {custom_id = id; group; params = params id}

let status status =
  Printf.sprintf {|{"id":"msgbatch_1","processing_status":"%s"}|} status

let success id input output =
  Printf.sprintf
    {|{"custom_id":"%s","result":{"type":"succeeded","message":{"content":[{"type":"text","text":"ok"}],"stop_reason":"end_turn","usage":{"input_tokens":%d,"output_tokens":%d}}}}|}
    id
    input
    output

let errored id =
  Printf.sprintf
    {|{"custom_id":"%s","result":{"type":"errored","error":{"type":"error","error":{"type":"invalid_request_error","message":"private body"}}}}|}
    id

let incomplete id =
  Printf.sprintf
    {|{"custom_id":"%s","result":{"type":"succeeded","message":{"content":[{"type":"tool_use","id":"tool_1","name":"lookup","input":{}}],"stop_reason":"tool_use","usage":{"input_tokens":7,"output_tokens":8}}}}|}
    id

let fake ~state ~results () =
  let calls = ref [] in
  let transport =
    {
      call =
        (fun ~meth ~path ~body ->
          calls := (meth, path, body) :: !calls ;
          match (meth, path) with
          | `POST, "/v1/messages/batches" -> Ok (200, status "in_progress")
          | `GET, "/v1/messages/batches/msgbatch_1" -> Ok (200, status state)
          | `GET, "/v1/messages/batches/msgbatch_1/results" -> Ok (200, results)
          | _ -> Error "unexpected call");
      stream_lines =
        (fun ~path ~on_line ->
          calls := (`GET, path, None) :: !calls ;
          if path <> "/v1/messages/batches/msgbatch_1/results" then
            Error "unexpected stream path"
          else
            let rec feed = function
              | [] -> Ok 200
              | line :: rest -> (
                  match on_line line with
                  | Ok () -> feed rest
                  | Error e -> Error e)
            in
            feed (String.split_on_char '\n' results));
    }
  in
  (transport, calls)

let get_ok = function Ok x -> x | Error e -> Alcotest.fail e

let submit_collect_and_reduce () =
  let results = success "b" 4 5 ^ "\n" ^ success "a" 2 3 ^ "\n" in
  let transport, calls = fake ~state:"ended" ~results () in
  let ticket = get_ok (submit transport [req "a" "kb1"; req "b" "kb2"]) in
  Alcotest.(check string) "batch id" "msgbatch_1" (batch_id ticket) ;
  let sent =
    match List.rev !calls with
    | [(`POST, _, Some body)] ->
        get_ok (Yojson.Safe.from_string body |> fun j -> Ok j)
    | _ -> Alcotest.fail "one POST expected"
  in
  (match Yojson.Safe.Util.member "requests" sent with
  | `List [_; _] -> ()
  | _ -> Alcotest.fail "provider request shape") ;
  let restored = get_ok (ticket_of_json (ticket_to_json ticket)) in
  let finalized =
    match get_ok (collect transport restored) with
    | Complete finalized -> finalized
    | Pending _ -> Alcotest.fail "ended batch stayed pending"
  in
  let items = items finalized in
  Alcotest.(check (list string))
    "submission order"
    ["a"; "b"]
    (List.map (fun x -> x.custom_id) items) ;
  (match (List.hd items).outcome with
  | Succeeded {usage; _} ->
      Alcotest.(check int) "input tokens" 2 usage.input_tokens ;
      Alcotest.(check int) "output tokens" 3 usage.output_tokens
  | _ -> Alcotest.fail "expected success") ;
  let plan =
    get_ok
      (plan_reducers
         ~completeness:Require_all
         ~ticket:restored
         ~finalized
         ~reducer_id:(fun group -> "reduce-" ^ group)
         ~build:(fun ~group ~items:_ -> Ok (params group)))
  in
  Alcotest.(check (list string))
    "one reducer per group"
    ["reduce-kb1"; "reduce-kb2"]
    (List.map (fun (x : request) -> x.custom_id) plan.ready) ;
  Alcotest.(check int) "no blocked groups" 0 (List.length plan.blocked)

let pending_and_preflight () =
  let transport, calls = fake ~state:"in_progress" ~results:"" () in
  let ticket = get_ok (submit transport [req "a" "kb"]) in
  (match get_ok (collect transport ticket) with
  | Pending In_progress -> ()
  | _ -> Alcotest.fail "expected pending") ;
  Alcotest.(check int) "submit and poll only" 2 (List.length !calls) ;
  let before = List.length !calls in
  (match submit transport [req "a" "kb"; req "a" "kb"] with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "duplicate ID accepted") ;
  (match submit transport [req "bad.id" "kb"] with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "punctuated ID accepted") ;
  (match submit transport [req (String.make 65 'a') "kb"] with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "long ID accepted") ;
  let malformed =
    {
      custom_id = "bad-message";
      group = "kb";
      params =
        `Assoc
          [
            ("model", `String "claude-sonnet-5-5");
            ("max_tokens", `Int 100);
            ("messages", `List [`Int 42]);
          ];
    }
  in
  (match submit transport [malformed] with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "malformed message accepted") ;
  Alcotest.(check int) "invalid batch not sent" before (List.length !calls)

let correlation_errors () =
  let check_bad name results =
    let transport, _ = fake ~state:"ended" ~results () in
    let ticket = get_ok (submit transport [req "a" "kb"; req "b" "kb"]) in
    match collect transport ticket with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail (name ^ " accepted")
  in
  check_bad "missing" (success "a" 1 1) ;
  check_bad "duplicate" (success "a" 1 1 ^ "\n" ^ success "a" 1 1) ;
  check_bad "unknown" (success "a" 1 1 ^ "\n" ^ success "c" 1 1)

let partial_policy () =
  let transport, _ =
    fake
      ~state:"ended"
      ~results:(success "a" 1 1 ^ "\n" ^ errored "b" ^ "\n" ^ success "c" 2 2)
      ()
  in
  let ticket =
    get_ok (submit transport [req "a" "kb1"; req "b" "kb1"; req "c" "kb2"])
  in
  let finalized =
    match get_ok (collect transport ticket) with
    | Complete xs -> xs
    | _ -> Alcotest.fail "not complete"
  in
  let build ~group ~items =
    Alcotest.(check int)
      ("all map outcomes seen by " ^ group)
      (if group = "kb1" then 2 else 1)
      (List.length items) ;
    Ok (params group)
  in
  let strict =
    get_ok
      (plan_reducers
         ~completeness:Require_all
         ~ticket
         ~finalized
         ~reducer_id:(fun x -> "r-" ^ x)
         ~build)
  in
  Alcotest.(check (list string))
    "unaffected group ready"
    ["kb2"]
    (List.map (fun (x : request) -> x.group) strict.ready) ;
  Alcotest.(check (list (pair string (list string))))
    "failed IDs preserved"
    [("kb1", ["b"])]
    strict.blocked ;
  let partial =
    get_ok
      (plan_reducers
         ~completeness:Accept_partial
         ~ticket
         ~finalized
         ~reducer_id:(fun x -> "r-" ^ x)
         ~build)
  in
  Alcotest.(check int) "both groups ready" 2 (List.length partial.ready) ;
  Alcotest.(check int) "no blocked groups" 0 (List.length partial.blocked)

let ticket_and_provider_guards () =
  let transport, _ = fake ~state:"ended" ~results:(success "a" 0 0) () in
  let ticket = get_ok (submit transport [req "a" "opaque-group"]) in
  (match Yojson.Safe.Util.member "requests" (ticket_to_json ticket) with
  | `List [row] ->
      Alcotest.(check bool)
        "ticket excludes params"
        false
        (Yojson.Safe.Util.member "params" row <> `Null)
  | _ -> Alcotest.fail "ticket request mapping missing") ;
  let finalized =
    match get_ok (collect transport ticket) with
    | Complete x -> x
    | _ -> Alcotest.fail "not complete"
  in
  let other_ticket =
    get_ok
      (ticket_of_json
         (`Assoc
            [
              ("version", `Int 1);
              ("batch_id", `String "msgbatch_2");
              ( "requests",
                `List
                  [
                    `Assoc
                      [
                        ("custom_id", `String "a");
                        ("group", `String "opaque-group");
                      ];
                  ] );
            ]))
  in
  (match
     plan_reducers
       ~completeness:Accept_partial
       ~ticket:other_ticket
       ~finalized
       ~reducer_id:(fun _ -> "r")
       ~build:(fun ~group:_ ~items:_ -> Ok (params "r"))
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "cross-batch reduce accepted") ;
  match ticket_of_json (`Assoc [("version", `Int 9)]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unknown ticket version accepted"

let incomplete_preserves_usage () =
  let transport, _ = fake ~state:"ended" ~results:(incomplete "a") () in
  let ticket = get_ok (submit transport [req "a" "kb"]) in
  let finalized =
    match get_ok (collect transport ticket) with
    | Complete x -> x
    | _ -> Alcotest.fail "not complete"
  in
  (match (List.hd (items finalized)).outcome with
  | Incomplete {usage; stop_reason; message = _} ->
      Alcotest.(check int) "incomplete input tokens" 7 usage.input_tokens ;
      Alcotest.(check string) "stop reason" "tool_use" stop_reason
  | _ -> Alcotest.fail "incomplete payload lost") ;
  let strict =
    get_ok
      (plan_reducers
         ~completeness:Require_all
         ~ticket
         ~finalized
         ~reducer_id:(fun _ -> "r")
         ~build:(fun ~group:_ ~items:_ -> Ok (params "r")))
  in
  Alcotest.(check (list (pair string (list string))))
    "incomplete blocks strict reduce"
    [("kb", ["a"])]
    strict.blocked

let () =
  Alcotest.run
    "agent batch"
    [
      ( "batch",
        [
          Alcotest.test_case
            "submit collect reduce"
            `Quick
            submit_collect_and_reduce;
          Alcotest.test_case
            "pending and preflight"
            `Quick
            pending_and_preflight;
          Alcotest.test_case "strict correlation" `Quick correlation_errors;
          Alcotest.test_case "partial policy" `Quick partial_policy;
          Alcotest.test_case
            "ticket and provider guards"
            `Quick
            ticket_and_provider_guards;
          Alcotest.test_case
            "incomplete preserves usage"
            `Quick
            incomplete_preserves_usage;
        ] );
    ]
