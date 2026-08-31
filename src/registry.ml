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

type entry = Raw of Agentic_backend.t | Validated of Runtime_entry.t

(* Hashtable for O(1) lookup by ID. A raw registration and a validated entry
   occupy the same slot, so replacement cannot retain stale trust metadata. *)
let entries : (string, entry) Hashtbl.t ref = ref (Hashtbl.create 8)

(* Maintain registration order for deterministic iteration.
   Stored in reverse order (most recent first) for O(1) insertion. *)
let registration_order : string list ref = ref []

type models_source = Probe | Static | Hybrid

(* Resolved (live + static) model view, keyed by backend id.  Populated by
   {!Adapter_loader.register_all} via {!set_resolved_models}; the registry
   itself never invokes probes — that responsibility lives one layer up so
   the eio environment dependency stays out of [Registry]. *)
let resolved_models_tbl : (string, string list * models_source) Hashtbl.t ref =
  ref (Hashtbl.create 8)

let set_resolved_models id pair = Hashtbl.replace !resolved_models_tbl id pair

let resolved_models id = Hashtbl.find_opt !resolved_models_tbl id

let backend_of_entry = function
  | Raw backend -> backend
  | Validated entry -> entry.Runtime_entry.backend

let replace_entry ?(warn = false) id entry =
  let backend = backend_of_entry entry in
  if Hashtbl.mem !entries id then begin
    if warn then Diagnostics.warn "Replacing existing backend with id '%s'" id
  end
  else registration_order := id :: !registration_order ;
  Hashtbl.replace !entries id entry ;
  Hashtbl.replace
    !resolved_models_tbl
    id
    (Agentic_backend.models backend, Static)

let register backend =
  let id = Agentic_backend.id backend in
  replace_entry ~warn:true id (Raw backend)

let register_validated validated =
  let id = Agentic_backend.id validated.Runtime_entry.backend in
  replace_entry id (Validated validated)

let replace_all_validated validated_entries =
  let next_entries = Hashtbl.create (List.length validated_entries) in
  let next_models = Hashtbl.create (List.length validated_entries) in
  let next_order =
    List.map
      (fun validated ->
        let backend = validated.Runtime_entry.backend in
        let id = Agentic_backend.id backend in
        Hashtbl.replace next_entries id (Validated validated) ;
        Hashtbl.replace next_models id (Agentic_backend.models backend, Static) ;
        id)
      validated_entries
  in
  entries := next_entries ;
  resolved_models_tbl := next_models ;
  registration_order := List.rev next_order

let find_entry id = Hashtbl.find_opt !entries id

let get id = Option.map backend_of_entry (find_entry id)

let get_exn id =
  match get id with
  | Some backend -> backend
  | None -> raise Not_found

(* Return backends in registration order for deterministic behavior.
   Since registration_order is stored reversed, we reverse before returning. *)
let list () =
  !registration_order |> List.rev
  |> List.filter_map (fun id -> Option.map backend_of_entry (find_entry id))

let list_ids () =
  !registration_order |> List.rev
  |> List.filter (fun id -> Hashtbl.mem !entries id)

let list_models id = Option.map fst (resolved_models id)

let available ~sw ~env =
  list () |> List.filter (Agentic_backend.available ~sw ~env)

let first_available ~sw ~env =
  list () |> List.find_opt (Agentic_backend.available ~sw ~env)

let clear () =
  entries := Hashtbl.create 8 ;
  resolved_models_tbl := Hashtbl.create 8 ;
  registration_order := [] ;
  Yaml_adapter.clear_config_cache ()
