(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** The one rendering of a JSON Schema inside a prompt.

    Shared by {!Json_schema_enforcer} (corrective retry prompts) and
    {!Codex_cli} (first attempt when [--output-schema] cannot carry the
    schema), so both present the schema under the same heading and a wording
    change cannot drift between them. *)

(** [block schema_json] is ["## Required output schema\n\n" ^ schema_json].

    {pre}
    [schema_json] is a serialised JSON Schema.

    {post}
    Pure; the result starts with the heading line.

    {violators}
    (none)

    {violates}
    (none) *)
val block : string -> string
