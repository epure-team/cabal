(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Backend registry implementation.

    Thread-safety note: This registry uses a global mutable [Hashtbl].
    In OCaml 5 with domains, concurrent access to [Hashtbl] is not safe.
    Currently, Épure assumes single-threaded access to the registry
    (backends are registered at startup, then queried during execution).
    If multi-domain access is needed in the future, guard mutations with
    a mutex or use an immutable map behind [Atomic.t]. *)

(* Hashtable for O(1) lookup by ID *)
let backends : (string, Agentic_backend.t) Hashtbl.t = Hashtbl.create 8

(* Maintain registration order for deterministic iteration.
   Stored in reverse order (most recent first) for O(1) insertion. *)
let registration_order : string list ref = ref []

type models_source = Probe | Static | Hybrid

(* Resolved (live + static) model view, keyed by backend id.  Populated by
   {!Adapter_loader.register_all} via {!set_resolved_models}; the registry
   itself never invokes probes — that responsibility lives one layer up so
   the eio environment dependency stays out of [Registry]. *)
let resolved_models_tbl : (string, string list * models_source) Hashtbl.t =
  Hashtbl.create 8

let set_resolved_models id pair = Hashtbl.replace resolved_models_tbl id pair

let resolved_models id = Hashtbl.find_opt resolved_models_tbl id

let register backend =
  let id = Agentic_backend.id backend in
  if Hashtbl.mem backends id then
    Diagnostics.warn "Replacing existing backend with id '%s'" id
  else registration_order := id :: !registration_order ;
  Hashtbl.replace backends id backend ;
  (* Seed the resolved view with the static list so callers that query
     [resolved_models] right after [register] (without going through
     [Adapter_loader]) still get a sensible answer.  [Adapter_loader] may
     overwrite this with [Probe]-tagged data after invoking the probe. *)
  Hashtbl.replace resolved_models_tbl id (Agentic_backend.models backend, Static)

let register_from_adapter_loader backend =
  let id = Agentic_backend.id backend in
  if not (Hashtbl.mem backends id) then
    registration_order := id :: !registration_order ;
  Hashtbl.replace backends id backend ;
  Hashtbl.replace resolved_models_tbl id (Agentic_backend.models backend, Static)

let get id = Hashtbl.find_opt backends id

let get_exn id =
  match Hashtbl.find_opt backends id with
  | Some backend -> backend
  | None -> raise Not_found

(* Return backends in registration order for deterministic behavior.
   Since registration_order is stored reversed, we reverse before returning. *)
let list () =
  !registration_order |> List.rev
  |> List.filter_map (fun id -> Hashtbl.find_opt backends id)

let list_ids () =
  !registration_order |> List.rev
  |> List.filter (fun id -> Hashtbl.mem backends id)

let list_models id = Option.map fst (resolved_models id)

let available ~sw ~env =
  list () |> List.filter (Agentic_backend.available ~sw ~env)

let first_available ~sw ~env =
  list () |> List.find_opt (Agentic_backend.available ~sw ~env)

let clear () =
  Hashtbl.clear backends ;
  Hashtbl.clear resolved_models_tbl ;
  registration_order := []
