(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Host-supplied virtual workspaces for agent prompts.

    Cabal does not perform authorization. Host applications must construct
    workspaces only from resources the current requester is allowed to read.
    This module is deliberately pure: it validates and renders bounded prompt
    context, but it does not fetch, log, persist, or authorize content. *)

type metadata = string * string [@@deriving eq]

type window = {
  citation_id : string;
  index : int;
  total : int;
  content : string;
}
[@@deriving eq]

type resource = {
  id : string;
  title : string;
  kind : string;
  metadata : metadata list;
  windows : window list;
}
[@@deriving eq]

type limits = {
  max_window_chars : int;
  max_metadata_value_chars : int;
  max_rendered_chars : int;
}
[@@deriving eq]

type workspace = {
  label : string;
  authorization_boundary : string;
  resources : resource list;
  limits : limits;
}
[@@deriving eq]

type completion_prompt = {
  system_prompt : string;
  prompt : string;
}
[@@deriving eq]

type resource_descriptor = {
  descriptor_id : string;
  descriptor_title : string;
  descriptor_kind : string;
  descriptor_metadata : metadata list;
}
[@@deriving eq]

type read_limits = {
  read_max_window_chars : int;
  read_overlap_chars : int;
  max_windows_per_resource : int;
}
[@@deriving eq]

type read_window_request = {
  resource_id : string;
  window_index : int;
  max_chars : int;
  overlap_chars : int;
}
[@@deriving eq]

type read_window_result = {
  citation_id : string;
  content : string;
  has_more : bool;
}
[@@deriving eq]

val default_limits : limits

val default_read_limits : read_limits

val make_window :
  citation_id:string -> index:int -> total:int -> content:string -> window

val make_resource :
  id:string ->
  title:string ->
  kind:string ->
  ?metadata:metadata list ->
  windows:window list ->
  unit ->
  resource

val make :
  ?authorization_boundary:string ->
  ?limits:limits ->
  label:string ->
  resources:resource list ->
  unit ->
  (workspace, string) result

val make_resource_descriptor :
  id:string ->
  title:string ->
  kind:string ->
  ?metadata:metadata list ->
  unit ->
  resource_descriptor

val split_text_windows : limit:int -> overlap:int -> string -> string list

val collect_resource_windows :
  ?limits:read_limits ->
  read_window:(read_window_request -> (read_window_result, string) result) ->
  resource_descriptor ->
  (resource, string) result

val collect_workspace :
  ?authorization_boundary:string ->
  ?limits:limits ->
  ?read_limits:read_limits ->
  label:string ->
  descriptors:resource_descriptor list ->
  read_window:(read_window_request -> (read_window_result, string) result) ->
  unit ->
  (workspace, string) result

val render_manifest : workspace -> string

val render_context : workspace -> string

val augment_prompt : workspace -> prompt:string -> string

val prepare_completion :
  workspace -> system_prompt:string -> prompt:string -> completion_prompt
