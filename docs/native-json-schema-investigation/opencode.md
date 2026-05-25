# Native JSON Schema Investigation — opencode

**Backend ID:** `opencode`
**Baseline version investigated:** `1.14.20`
**Version range consulted:** `1.14.20` (baseline) .. `1.14.20` (latest stable at time of investigation), i.e. `baseline_version..latest-public-release`
**Outcome:** AC2(b) — documented non-support
**Story:** #631
**Date:** 2026-05-25

---

## 1. Summary

At baseline version `1.14.20`, opencode does not expose a stable, machine-readable
schema-forwarding surface that allows the Cabal adapter to carry a JSON schema into
the CLI invocation in a way that guarantees provider-level enforcement.  opencode is
provider-agnostic: the active provider (OpenAI, Anthropic, Mistral, Bedrock, etc.) is
resolved at runtime from `opencode.json` or environment variables, and is not visible
to the Cabal adapter at descriptor-registration time.

Per Decision D-14 (Epic #95), this "refuse-at-call-time" shape routes to **AC2(b)**.
The descriptor cannot evidence schema enforcement at `baseline_version` without a
pinned provider, and therefore `native_json_schema_output` remains `false`.

---

## 2. CLI Surface Consulted

### 2a. `opencode --help` output (accessed 2026-05-25)

```
opencode [flags]

Flags:
  --version                 Print version and exit
  --config <path>           Path to opencode.json config file
  --model <model>           Override the active model
  --no-auto-share           Disable share prompt
  run <prompt>              Non-interactive run with a prompt
  ...
```

No flag accepting a JSON schema, `--response-format`, `--output-schema`, or any
structured-output control is documented.  The CLI surface at `1.14.20` exposes no
direct schema-forwarding mechanism.

Source: https://opencode.ai/docs/cli/ (accessed 2026-05-25)

### 2b. Source inspection

opencode is open-source at https://github.com/sst/opencode (accessed 2026-05-25).
At tag `v1.14.20`, the `packages/opencode/src/` directory was inspected for
`response_format`, `json_schema`, `structured_output`, and `generationConfig` usage:
this is the AC1 "source where open" surface consulted alongside `opencode --help`.

- The provider dispatch layer (e.g. `provider/openai.ts`, `provider/anthropic.ts`) wires
  provider-specific structured-output options internally when the provider SDK supports
  them, but this is not exposed as a CLI flag or config-file key at `1.14.20`.
- There is no documented `opencode.json` key for forwarding `response_format` or an
  equivalent schema constraint to the underlying provider.

Source: https://github.com/sst/opencode/tree/v1.14.20 (accessed 2026-05-25)

---

## 3. Provider-Resolution Contract

opencode resolves the active provider from (in order of precedence):

1. `--model` CLI flag (e.g. `--model openai/gpt-4o`)
2. `model` key in `opencode.json`
3. A default model from the first provider whose API key is present in the environment

Provider identity is therefore a runtime value, not a build-time constant.  The Cabal
adapter for opencode writes an `opencode.json` config file per invocation and passes
the prompt via `opencode run`; the resolved provider may differ across invocations
depending on the project config and environment.

This means:

- Even if a future version of opencode exposed `response_format` forwarding, the
  Cabal adapter would need to know the active provider at call-time in order to
  compose the correct schema enforcement arguments.
- At `1.14.20`, no such call-time provider-signal mechanism exists on the CLI surface.
- Wiring would therefore require the adapter to "refuse-at-call-time" (return `Error`)
  when the provider is not pinned — which D-14 classifies as AC2(b).

---

## 4. Provider API Documentation Consulted

The following authoritative sources were consulted, all pinned to behaviour at
`1.14.20` (i.e. only features present in the CLI at that version are considered):
this includes configured provider API documentation plus release notes and changelog
surfaces for the opencode baseline.

| Source | URL | Accessed |
|--------|-----|----------|
| opencode CLI reference | https://opencode.ai/docs/cli/ | 2026-05-25 |
| opencode configuration reference | https://opencode.ai/docs/config/ | 2026-05-25 |
| opencode source (v1.14.20 tag) | https://github.com/sst/opencode/tree/v1.14.20 | 2026-05-25 |
| opencode baseline release tag (v1.14.20) | https://github.com/sst/opencode/releases/tag/v1.14.20 | 2026-05-25 |
| opencode changelog / releases | https://github.com/sst/opencode/releases | 2026-05-25 |
| OpenAI structured outputs reference | https://platform.openai.com/docs/guides/structured-outputs | 2026-05-25 |
| Anthropic tool_use / structured outputs | https://docs.anthropic.com/en/docs/build-with-claude/tool-use | 2026-05-25 |

No feature in any of these sources provides a stable, machine-readable mechanism at
`1.14.20` for forwarding a full JSON schema from the Cabal caller into the opencode
CLI invocation such that the underlying provider enforces it natively.

---

## 5. Investigation Outcome

### 5a. AC1 binary mapping (Story #631)

- **Outcome (i) -> AC2(a) detect-and-dispatch:** Requires a stable,
  machine-readable provider signal at baseline plus a resolved provider whose
  schema subset is already characterised. Not satisfied at `1.14.20`.
- **Outcome (ii) -> AC2(b) documented non-support:** No usable
  schema-forwarding mechanism at baseline, and any attempted wiring would
  require refuse-at-call-time when no provider is pinned.

**AC1 binary outcome:** (ii) — routes to AC2(b).

Rationale:

- No stable machine-readable schema-forwarding surface exists at `1.14.20`.
- opencode's provider-resolution contract is runtime-dynamic; no pinned provider is
  characterised for the Cabal descriptor.
- Any wiring would have to refuse-at-call-time when no provider is pinned — which D-14
  classifies as AC2(b) (Story #628 evidence contract cannot be satisfied).

**Descriptor action:** `native_json_schema_output` stays `false`; `capability_evidence`
stays `None`.  No other backend descriptor is touched (NFR-U2).

---

## 6. Future-Work Note

Features newer than `1.14.20` are not considered here per NFR-U3.  If a future
opencode release introduces a stable per-invocation `--response-schema <path>` flag
(or equivalent config-file key) that forwards a JSON Schema to the active provider's
structured-output mechanism, this investigation should be re-opened against the new
`baseline_version`.

**Re-evaluation trigger:** A new opencode release that adds a documented
`response_format` / `--output-schema` CLI flag or a `opencode.json` key for
schema-constrained output.  Track at:
https://github.com/sst/opencode/releases (accessed 2026-05-25).

Upstream discussion link for tracking schema-forwarding support:
https://github.com/sst/opencode/issues/2802 (accessed 2026-05-25).
This issue discusses JSON schema surfaces in OpenCode and is the closest upstream
thread at the investigated baseline while no dedicated `opencode run` output-schema
issue exists yet.
