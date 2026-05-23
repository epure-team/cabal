(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Pure JSON Schema validator — Story #623.

    Validates a JSON document against an inline JSON Schema without performing
    any I/O, subprocess call, or LLM invocation.  Draft 2020-12 semantics are
    used when no [$schema] field is present in the schema document; callers
    requiring a specific draft must embed the [$schema] URI in the document.

    This module is used exclusively by the validate-and-retry path in
    {!Json_schema_enforcer}; the native path does not call this module. *)

(** [validate ~schema ~document] validates [document] against [schema].

    {pre}
    [schema] is an inline JSON Schema document as a parsed JSON value.
    [document] is the text string to validate (e.g. [task_result.agent_text]).

    {post}
    Returns [Ok ()] when [document] parses as valid JSON and structurally
    conforms to [schema].  Returns [Error msg] with a human-readable
    diagnostic when [document] is not valid JSON or fails schema validation.
    Never performs I/O, subprocess calls, or LLM invocations.

    {violators}
    (none)

    {violates}
    (none) *)
val validate :
  schema:Yojson.Safe.t -> document:string -> (unit, string) result
