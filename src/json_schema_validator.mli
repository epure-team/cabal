(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Pure JSON Schema validator — Story #623.

    Validates a JSON document against an inline JSON Schema without performing
    any I/O, subprocess call, or LLM invocation.

    {b Implemented keywords.}  This validator checks the following JSON Schema
    keywords; all other keywords are silently accepted (unknown keywords do not
    cause validation to fail):
    - [type]: the document must have the JSON type named by the keyword
      ([object], [array], [string], [number], [integer], [boolean], [null]).
    - [required]: every string named in the array must be present as a key in
      the document object.

    {b Scope.}  This is a structural type-and-presence checker, not a full
    JSON Schema implementation.  Callers who need keyword coverage beyond [type]
    and [required] (e.g. [properties], [enum], [pattern],
    [additionalProperties]) should not rely on this module for those checks.

    This module is used exclusively by the validate-and-retry path in
    {!Json_schema_enforcer}; the native path does not call this module. *)

(** [validate ~schema ~document] validates [document] against [schema].

    {pre}
    [schema] is an inline JSON Schema document as a parsed JSON value.
    [document] is the text string to validate (e.g. [task_result.agent_text]).

    {post}
    Returns [Ok ()] when [document] parses as valid JSON and passes all
    supported keyword checks ([type] and [required]).  Returns [Error msg]
    with a human-readable diagnostic when [document] is not valid JSON or
    fails a supported keyword check.  Unknown schema keywords are silently
    accepted.  Never performs I/O, subprocess calls, or LLM invocations.

    {violators}
    (none)

    {violates}
    (none) *)
val validate :
  schema:Yojson.Safe.t -> document:string -> (unit, string) result
