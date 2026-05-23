(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Pure JSON Schema validator — Story #623.

    Validates a JSON document against an inline JSON Schema without performing
    any I/O, subprocess call, or LLM invocation.  Backed by the [jsonschema]
    opam package (v0.1.0).

    {b Keyword coverage.}  All keywords defined in JSON Schema drafts 4, 6, 7,
    2019-09, and 2020-12 are supported (including [type], [required],
    [properties], [additionalProperties], [enum], [const], [minimum],
    [maximum], [minLength], [maxLength], [pattern], [items], [contains],
    [allOf], [anyOf], [oneOf], [not], [if/then/else], and more).

    {b Draft selection.}  The draft used during validation is chosen as
    follows:
    - When the schema document contains a [$schema] field, the draft named by
      that field is used (among the supported drafts: 4, 6, 7, 2019-09,
      2020-12).
    - When no [$schema] field is present, draft 2020-12 is used by default
      (Decision D-2).

    {b Scope.}  This module validates the structure and content of a JSON
    document against an inline schema.  It does not resolve external [$ref]
    URLs — schemas with remote references will produce a compile error.

    This module is used exclusively by the validate-and-retry path in
    {!Json_schema_enforcer}; the native path does not call this module. *)

(** [validate ~schema ~document] validates [document] against [schema].

    {pre}
    [schema] is an inline JSON Schema document as a parsed JSON value.
    [document] is the text string to validate (e.g. [task_result.agent_text]).

    {post}
    Returns [Ok ()] when [document] parses as valid JSON and satisfies all
    constraints expressed in [schema].  Returns [Error msg] with a
    human-readable diagnostic when [document] is not valid JSON or fails any
    schema keyword check.  Never performs I/O, subprocess calls, or LLM
    invocations.

    {violators}
    (none)

    {violates}
    (none) *)
val validate : schema:Yojson.Safe.t -> document:string -> (unit, string) result
