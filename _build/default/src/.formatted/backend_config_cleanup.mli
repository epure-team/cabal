(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Internal text-cleanup helpers used by {!Backend_config_writer}.

    Extracted to keep the writer's main module focused on artifact
    metadata, write policy, and atomic IO. These helpers are pure
    string-processing and have no Eio or IO dependency. *)

(** [remove_dangling_commas_before_closers lines] returns [lines] with
    trailing commas removed from any line whose next non-blank line
    starts with [}] or []]. Used to clean up JSON-with-comments where
    stripping a managed block leaves a trailing comma on the prior
    element. *)
val remove_dangling_commas_before_closers : string list -> string list

(** [strip_managed_mcp_block content] removes any inline ["mcp": { ... }]
    block (and its trailing comma if present) from [content], returning
    the cleaned text. Brace/bracket-aware so it tolerates nested values
    within the [mcp] block. *)
val strip_managed_mcp_block : string -> string
