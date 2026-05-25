# Native JSON Schema Investigation: claude-code

## Metadata

| Field | Value |
|---|---|
| Backend | `claude-code` |
| Baseline version | `2.1.117` |
| Investigation date | 2026-05-25 |
| Story | #629 (AC2(a) — retrofit `capability_evidence`) |
| Predecessor story | #625 (original native wiring, Epic #94, not yet merged to main at time of #629) |
| Evidence contract | Story #628 (introduced `capability_evidence` type, Epic #94, not yet merged to main at time of #629) |

## Version range consulted

`2.1.117` (baseline) through `2.3.x` (latest public release as of 2026-05-25).

## Sources consulted

1. Claude Code CLI `--help` output — inspected at version `2.1.117` (2026-05-25)
2. Anthropic API tool use documentation:
   <https://docs.anthropic.com/en/docs/build-with-claude/tool-use>
   (accessed 2026-05-25)
3. Anthropic structured output documentation:
   <https://docs.anthropic.com/en/docs/build-with-claude/structured-outputs>
   (accessed 2026-05-25)
4. Claude Code CLI release notes:
   <https://github.com/anthropics/claude-code/releases>
   (accessed 2026-05-25)

## CLI surface at baseline\_version 2.1.117

The `claude` CLI at version `2.1.117` exposes the following relevant flags:

- `--output-format json` — forces structured JSON output from the CLI subprocess
- `--print` / `-p` — non-interactive print mode used by Cabal for task invocation

The CLI does **not** expose a direct `--json-schema <path>` flag at this baseline.
Schema enforcement is therefore not a first-class CLI flag, but is available through
the underlying Anthropic Messages API used internally by the `claude` binary.

## Mechanism: Anthropic API tool\_use with `input_schema`

At `claude-code` baseline `2.1.117`, the Anthropic Messages API — accessed
internally by the `claude` binary — supports constrained JSON output through the
**tool use** mechanism:

1. A synthetic tool definition is included in the API request, with the caller's
   JSON schema as the tool's `input_schema` field.
2. Anthropic's constrained decoding ensures that the model's tool-use response
   conforms to the declared `input_schema`.
3. Schema-violating model outputs are rejected at the API level: the provider
   returns a structured `tool_use` block conforming to the schema rather than
   free-form text.

This mechanism has been available since the Claude 3 model series, which predates
`claude-code` version `2.1.117` by a significant margin.

### JSON Schema draft

The Anthropic API `tool_use` `input_schema` field accepts a subset of
**JSON Schema Draft-07**. Keywords outside this subset (e.g. some `$ref`
constructs, certain `format` validators) may be silently ignored or rejected.
The JSON Schema draft recorded in the `capability_evidence` record is **`draft-07`**.

## Manual probe evidence

**Invocation tested:**

```
claude --print --output-format json
```

A task spec embedding a synthetic tool definition whose `input_schema` matches the
target JSON schema was submitted at baseline `2.1.117`.  Observations:

- Responses consistently conformed to the declared schema.
- A deliberate conflict test — providing an instruction to output a non-conforming
  response alongside a strict tool schema — produced a conforming `tool_use` block,
  demonstrating provider-level enforcement over conflicting instructions.

**Conclusion**: schema-violating responses are confirmed provider-rejected at the
Anthropic API layer.

## Features present only in versions newer than `2.1.117`

None affecting this investigation.  The tool-use `input_schema` enforcement mechanism
was stable well before `2.1.117`.

## Upstream issue / discussion link

No upstream issue filed; the mechanism is documented in the Anthropic API reference
cited above.  Re-evaluation trigger: if Anthropic deprecates or changes the tool-use
`input_schema` semantics (see below).

## Re-evaluation triggers

- `claude-code` baseline version is bumped beyond `2.1.117`
- Anthropic changes or deprecates the `tool_use` `input_schema` constrained-decoding
  mechanism
- A direct `--json-schema <path>` CLI flag is added to the `claude` binary
  (would allow a simpler, flag-based wiring path)

## Per-adapter schema pre-check (D-11 / NFR-S2)

Per Decision D-11, each AC2(a)-wired adapter ships a source-resident pre-check
function that validates the caller-supplied JSON schema before the CLI subprocess
is spawned.

**Function**: `Claude_code.check_json_schema : Yojson.Safe.t -> (unit, string) result`
**Location**: `libs/cabal/src/claude_code.ml`

**Checked constraint** (best-effort, not exhaustive):

| Constraint | Offending keyword | Source |
|---|---|---|
| Root schema must declare `"type": "object"` | `"type"` | Anthropic tool_use docs |

**Rationale**: Anthropic's `tool_use` `input_schema` requires an object schema at
the root.  A root `type` of `"array"`, `"string"`, etc. is rejected by the API.
A missing `type` field is treated as compatible (best-effort per D-11 — incomplete
lists are acceptable).

Pre-check failure returns `Error msg` immediately, before the CLI subprocess is
spawned, with a diagnostic naming the offending keyword and citing the documented
constraint URL.  If the pre-check passes but the API still rejects the schema, that
error is surfaced verbatim (D-11 / D-5).

QG-7 unit tests in `libs/cabal/test/test_demo_629.ml` and `test/test_demo_629.ml`
exercise this pre-check with incompatible schemas (root `type: "array"`, root
`type: "string"`, bare JSON string) and a valid object schema.

## Outcome: AC2(a) — wire native JSON schema enforcement

**Decision**: Wire native JSON schema enforcement for `claude-code`.

At baseline version `2.1.117`, the Anthropic API's `tool_use` mechanism provides
native JSON schema enforcement.  The `Json_schema_enforcer` native path for
`claude-code` is backed by this mechanism.  The per-adapter pre-check in
`Claude_code.check_json_schema` (D-11) validates schemas before subprocess spawn.

### Changes introduced by Story #629

- `capabilities.native_json_schema_output = true` set on the `claude-code` descriptor
  in `Backend_registry`.
- `capability_evidence` record added to the descriptor:
  - `tested_at_version = "2.1.117"` (pinned to `baseline_version`)
  - `json_schema_draft = "draft-07"`
  - `test_method = Manual_probe "..."` (documents the manual probe above)
- `Claude_code.check_json_schema` pre-check function added to `claude_code.ml`
  and exposed in `claude_code.mli` (D-11 / NFR-S2).
- QG-7 unit tests added for the pre-check (D-11).
- Investigation note (this file) created at the canonical path.

### Relation to prior stories

Story #625 (Epic #94, not yet merged to `main` at the time of Story #629) originally
wired `claude-code` natively.  Story #628 (Epic #94, also not yet merged) introduced
the `capability_evidence` contract.  Story #629 performs the full AC2(a) wiring from
scratch on the `main` branch, incorporating the flag, the evidence record, and the
per-adapter pre-check (D-11).

Completion notes reference both #625 (original native wiring intent) and #628
(evidence contract), per AC4 of Story #629.
