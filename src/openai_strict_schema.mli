(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Conservative checker for the JSON Schema subset OpenAI structured outputs
    accept in strict mode.

    [codex exec --output-schema] forwards the schema as
    [text.format = {type: json_schema, name: "codex_output_schema", strict}],
    and the API rejects the whole request with HTTP 400 [invalid_json_schema]
    when the schema leaves the strict subset (codex then exits 1 before the
    model produces anything).  This module decides, before invocation, whether
    a schema can be handed to that flag at all.

    The check is an allowlist, so it fails closed: a keyword it does not know
    makes the schema "not strict", which costs a native constraint, never a
    request.  The rules, each observed against codex-cli 0.160.0 /
    gpt-6.1-sol on 2026-10-03 (accepted: [$schema], [enum], [anyOf],
    [minimum]/[maximum], [minLength]/[maxLength]/[pattern], [maxItems],
    [["string","null"]]; rejected: free-form [{"type":"object"}], a
    [properties] key missing from [required], [oneOf], [const] without
    [type]):
    - every schema node is an object carrying [type] or [anyOf];
    - every object node has [additionalProperties: false], a [properties]
      object, and a [required] array naming exactly every property key;
    - every array node has an [items] schema;
    - the root is an object node (not an [anyOf]);
    - only allowlisted keywords appear ([oneOf], [allOf], [not], [const],
      [format], [$ref], [$defs], [patternProperties], [if]/[then]/[else], …
      are all outside it);
    - nesting depth stays within {!max_depth}. *)

(** Maximum nesting depth of object/array schemas accepted (OpenAI documents
    a limit of 10 levels). *)
val max_depth : int

(** [check schema] decides whether [schema] lies in the strict subset.

    {pre}
    (none)

    {post}
    [Ok ()] when every rule above holds; [Error reason] otherwise, where
    [reason] names the JSON path of the first offending node (e.g.
    ["$.properties.thinking: object without additionalProperties: false"]).
    Pure: no I/O.

    {violators}
    (none)

    {violates}
    (none) *)
val check : Yojson.Safe.t -> (unit, string) result
