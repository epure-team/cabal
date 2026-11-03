(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Host-neutral asynchronous map/reduce batches for the Claude Messages Batch API.

    Cabal owns provider wire validation and result correlation. The host owns
    HTTP credentials, durable storage, scheduling and any webhook listener. No
    provider callback URL is assumed: a host may call {!collect} when woken by
    a local event, and must also poll for recovery. *)

(** One independent Messages request. [custom_id] and [group] should be opaque,
    non-sensitive identifiers. [params] is a Messages create body. *)
type request = {custom_id : string; group : string; params : Yojson.Safe.t}

(** Durable provider batch identity and exact request/group correlation. It
    excludes prompts, responses and credentials. *)
type ticket

val ticket_to_json : ticket -> Yojson.Safe.t

val ticket_of_json : Yojson.Safe.t -> (ticket, string) result

val batch_id : ticket -> string

(** The host supplies authenticated HTTPS transport to [api.anthropic.com].
    [path] is always a relative API path. The host must bound response size,
    redact credentials/errors, and never log request/response bodies.
    [stream_lines] feeds complete JSONL lines incrementally and stops on a
    callback error. It must not feed a non-2xx body to [on_line]. *)
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

(** Validate IDs, limits and the structural Messages envelope before one POST.
    The provider remains authoritative for the full Messages schema. A
    successful POST whose
    ticket cannot be persisted by the host may be orphaned; the provider does
    not offer an idempotency key here. Retry may create a duplicate paid batch. *)
val submit : transport -> request list -> (ticket, string) result

type status = In_progress | Canceling | Ended

(** Query provider status. This is the recovery path even if a host receives a
    completion notification. *)
val poll : transport -> ticket -> (status, string) result

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

(** A collection verified against the exact provider ticket. Only {!collect}
    can construct it. *)
type finalized

val items : finalized -> item list

type collection = Pending of status | Complete of finalized

(** Poll first. Once ended, fetch provider JSONL and require exactly one
    terminal outcome for every ticket ID. Results are returned in submission
    order, irrespective of provider order. Unknown, duplicate or missing IDs
    fail closed. [Succeeded] carries measured tokens; Cabal does not guess USD
    cost from mutable model pricing. *)
val collect : transport -> ticket -> (collection, string) result

type completeness = Require_all | Accept_partial

type reduce_plan = {ready : request list; blocked : (string * string list) list}

(** Pure, deterministic reduction planning. The caller groups maps by [group]
    and chooses a reducer request ID and Messages params. [Require_all] blocks
    only groups with failed maps; [Accept_partial] passes all successes and
    failures to [build]. Call {!submit} with [ready] to start the reduce batch.
    [finalized] must come from {!collect} for the same ticket. *)
val plan_reducers :
  completeness:completeness ->
  ticket:ticket ->
  finalized:finalized ->
  reducer_id:(string -> string) ->
  build:(group:string -> items:item list -> (Yojson.Safe.t, string) result) ->
  (reduce_plan, string) result
