# Native JSON Schema Investigation — opencode

**Backend ID:** `opencode`
**Baseline version investigated:** `1.14.20`
**Version range consulted:** `1.14.20` (baseline) .. `1.14.20` (latest stable at time of investigation), i.e. `baseline_version..latest-public-release`
**Outcome:** AC2(b) — documented non-support
**Story:** #631
**Date:** 2026-05-25
**Media/web addendum:** CBL-07C / #29, 2026-09-03

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

---

## 7. CBL-07C Media and Web Transport Addendum

This addendum does not change the native JSON Schema conclusion above. It records
the separate content-dependent investigation of file delivery and per-invocation
web policy at the pinned `1.14.20` baseline.

### 7a. Reproducible evidence

- Baseline release: OpenCode `v1.14.20`, source commit
  `3175a3c61853e4666acb24fa435783826596665d`.
- Probe: `tools/probe_opencode_media_web.py`.
- Default authenticated model: `openai/gpt-5.4-mini` (overridable with
  `CABAL_E2E_MODEL_OPENCODE`).
- Baseline invocation used during the investigation:

  ```sh
  PATH="/tmp/cabal-cbl07c-opencode:$PATH" \
    ./tools/probe_opencode_media_web.py
  ```

The probe first requires the exact version and verifies that `opencode run
--help` still exposes `--format`, repeated `--file`, `--session`, `--model`, and
`--agent`. It then creates deterministic PNG/JPEG fixtures, including filenames
with spaces, and checks content rather than process exit alone.

| Probe mode | Content-dependent assertion at `1.14.20` | Result |
|------------|------------------------------------------|--------|
| `structured-output` | Public completed text is the exact requested JSON and public token usage is present. | PASS |
| `media-initial` | One repeated `--file` pair per PNG/JPEG; answer identifies blue PNG and red JPEG. | PASS |
| `resume-upload` | `--session` plus a new green PNG preserves the public session and identifies green. | PASS |
| `resume-reuse` | A later call without `--file` recalls the previously uploaded green image. | PASS |
| `schema-retry-media` | An intentionally non-JSON first response is retried in a fresh session with both images uploaded again; the second response matches the schema. | PASS |
| `web-disabled` | Fixed deny policy defeats hostile project allow rules; neither `websearch` nor `webfetch` completes and a local HTTP marker receives zero requests. | PASS |
| `web-search` | Search is allowed, fetch denied; completed `websearch` lifecycle yields the official CLI documentation URL without `webfetch`. | PASS |
| `web-search-fetch` | Both tools complete and the fetched official page yields its visible `CLI` heading. | PASS |

The same matrix was also run successfully as a forward-compatibility advisory
against installed OpenCode `1.18.25` (tag commit
`cb7d8b2f5e44876ef98b661dc10590c915af3a9f`). This does not replace the pinned
baseline evidence.

`./tools/probe_opencode_media_web.py --self-test` exercises the public JSONL,
fixed-policy, argv, timeout, and diagnostic-sanitization validators without
credentials or network access.

### 7b. Adapter contract resulting from the proof

- Initial and fresh-retry media delivery uses repeated direct argv pairs
  `--file <sealed-absolute-path>`. Paths come only from
  `Task_execution_context.sealed_attachment_delivery`; workspace-relative
  attachment references never enter argv.
- Any attachment or enabled web request without matching central authorization
  fails before project config I/O and before process spawn.
- `Web_disabled`, `Web_search`, and `Web_search_and_fetch` map to fixed
  `websearch`/`webfetch` allow-or-deny documents supplied through
  `OPENCODE_PERMISSION` and `OPENCODE_CONFIG_CONTENT`. The adapter invokes
  `env` directly (no shell), uses `--pure --agent build`, disables automatic
  sharing/update/LSP download, and never interpolates host config fragments.
- JSONL normalization admits only canonical session IDs, completed assistant
  text, sanitized completed tool lifecycle, and bounded non-negative usage.
  Reasoning, user text, error payloads, tool inputs/results, malformed records,
  and raw-output fallback remain private.
- The validate-and-retry path remains fresh-call only and hard-capped by
  `Json_schema_enforcer`; every fresh attempt receives the same authorized
  sealed image set.

Although the probe proves upstream `--session` upload and attachment reuse,
OpenCode's current Cabal descriptor advertises no session resume capability.
Low-level resume/reuse requests therefore fail closed. CBL-07C deliberately
does not alter shared capability descriptors, runtime snapshots, preflight,
dispatch, or the native JSON Schema evidence recorded above.
