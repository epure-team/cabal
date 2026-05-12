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

let register backend =
  let id = Agentic_backend.id backend in
  if Hashtbl.mem backends id then
    Diagnostics.warn "Replacing existing backend with id '%s'" id
  else registration_order := id :: !registration_order ;
  Hashtbl.replace backends id backend

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

let available ~sw ~env =
  list () |> List.filter (Agentic_backend.available ~sw ~env)

let first_available ~sw ~env =
  list () |> List.find_opt (Agentic_backend.available ~sw ~env)

let clear () =
  Hashtbl.clear backends ;
  registration_order := []
