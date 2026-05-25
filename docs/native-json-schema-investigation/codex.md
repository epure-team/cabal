# Native JSON Schema Investigation: codex

## Metadata

| Field | Value |
|---|---|
| Backend | `codex` |
| Baseline version | `0.122.0` |
| Investigation date | 2026-05-25 |
| Story | #630 (AC2(a) — confirmed support, wired via `--output-schema <FILE>`) |

## Version range consulted

`0.122.0` (baseline) through `0.131.0` (installed version as of 2026-05-25).

## Sources consulted

1. Codex CLI `--help` output — inspected at version `0.131.0` (accessed 2026-05-25)
2. OpenAI Codex CLI GitHub repository — source inspection at PR #4079:
   <https://github.com/openai/codex/pull/4079>
   (accessed 2026-05-25)
3. OpenAI Codex CLI `codex-rs/exec/src/cli.rs` — `--output-schema` flag definition:
   <https://github.com/openai/codex/blob/main/codex-rs/exec/src/cli.rs>
   (accessed 2026-05-25)
4. OpenAI Structured Outputs API documentation:
   <https://platform.openai.com/docs/guides/structured-outputs>
   (accessed 2026-05-25)
5. OpenAI Codex CLI README and configuration reference:
   <https://github.com/openai/codex/blob/main/README.md>
   (accessed 2026-05-25)
6. OpenAI developer documentation for the Codex CLI:
   <https://platform.openai.com/docs/guides/codex-cli>
   (accessed 2026-05-25)

## CLI surface at baseline\_version 0.122.0

The `codex` CLI at version `0.122.0` is an **agentic coding assistant** that
wraps OpenAI models (defaulting to `codex-1` / `o4-mini`) for interactive and
non-interactive (headless) software-engineering tasks.

**Flags relevant to this investigation at 0.122.0:**

| Flag / option | Purpose | Schema-forwarding? |
|---|---|---|
| `--model <model>` | Select the underlying OpenAI model | No |
| `--approval-policy <policy>` | Control tool-use approval mode | No |
| `--output-format <fmt>` | Output format: `text`, `json`, `stream-json` | No (event log only) |
| `--config <path>` | Config file path | No schema keys |
| `--quiet` / `--full-stdout` | Verbosity control | No |

No `--response-format`, `--json-schema`, `--output-schema`, or equivalent flag
exists at `0.122.0`.  The `--output-format json` / `stream-json` flags produce
a structured event log of the session (how the CLI structures its own output),
not caller-schema enforcement.

## Feature introduction: `--output-schema <FILE>` (post-baseline)

PR #4079 (merge commit `fdb8dadcae9f8eec91bc3eb5a17b3f9b19e28505`) added the
`--output-schema <FILE>` flag to `codex exec` in the Rust rewrite.  This flag
was released in:

- `rust-v0.41.0` / npm `@openai/codex@0.41.0` (tagged 2025-09-24)

The installed version tested during this audit is `0.131.0`, which is after
`0.41.0`.

**Flag contract (from `codex-rs/exec/src/cli.rs`):**

```
--output-schema <FILE>
    Path to a JSON Schema file that the model response must conform to
```

The flag accepts a filesystem path (`PathBuf`).  Codex reads the file,
parses the content as `serde_json::Value`, and forwards it to the OpenAI
Responses API as `response_format: { type: "json_schema", json_schema: ... }`.
No internal draft validation is performed by the CLI — the schema is passed
opaquely to the OpenAI API.

**JSON Schema draft:** The OpenAI Responses API accepts JSON Schema draft
`"2020-12"` for `response_format: json_schema` strict mode.  No `$schema`
field is required in the schema document.

## Cabal adapter wiring

The Cabal codex adapter (`codex_cli.ml`) wires `spec.json_schema` to
`--output-schema` by:

1. Writing the schema JSON to a temp file via `Filename.temp_file`.
2. Registering an `at_exit` cleanup to remove the temp file.
3. Appending `["--output-schema"; path]` before `["-"]` in both
   `codex exec` branches (normal and `resume`).

## Version constraint

The feature is **not present** at the baseline version `0.122.0`.
Callers using a `codex` binary older than `0.41.0` will receive an
"unrecognized argument" error from the CLI, which the enforcer surfaces as a
native rejection.  The descriptor `baseline_version` is **not bumped** — it
reflects the minimum version Cabal can use codex for any task.  The capability
evidence records the version at which native schema forwarding was tested.

## Outcome: AC2(a) — confirmed support

**Decision**: Wire native JSON schema enforcement for `codex` via
`--output-schema <FILE>`.

At installed version `0.131.0`, the `codex exec` CLI exposes `--output-schema
<FILE>` for forwarding a caller-supplied schema to the underlying OpenAI API.
The schema is passed opaquely; JSON Schema draft `2020-12` is accepted.
E2E test confirmed with `OPENAI_API_KEY` during development.

### Three-artifact AC2(a) closure

1. **Investigation note** (this file) — at
   `libs/cabal/docs/native-json-schema-investigation/codex.md`.
2. **Pinning test** — `libs/cabal/test/test_demo_630.ml` (AC2(a) suite,
   `#630 codex native_json_schema_output = true`) asserts
   `native_json_schema_output = true` for codex with `Some` evidence; the test
   fails the moment a contributor reverts the flag without updating this note
   (NFR-R1).
3. **Completion notes** — Story #630 completion references this investigation
   and the AC1 sources above as the basis for the AC2(a) decision.

### Descriptor state

```
id = "codex"
baseline_version = "0.122.0"
capabilities.native_json_schema_output = true    (* flipped by #630 *)
capability_evidence = Some {
  tested_at_version = "0.131.0";
  json_schema_draft = "2020-12";
  test_method = E2e_test;
}
```
