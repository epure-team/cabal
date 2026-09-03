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
the separate investigation of file delivery and per-invocation web policy. No
CBL-07C observation currently qualifies as capability evidence.

### 7a. Provenance and evidence classification

- Descriptor baseline: OpenCode `v1.14.20`, source commit
  `3175a3c61853e4666acb24fa435783826596665d`.
- Authenticated observation version: OpenCode `1.2.24`, source commit
  `c6262f9d4002d86a1f1795c306aa329d45361d12`; the official Linux x64 archive
  used for the rerun had SHA-256
  `20644ef6b85975f0b49c3ea131c8d49cdee854419b3b8cfb24476e01787a871e`.
- Probe: `tools/probe_opencode_media_web.py`.
- Default authenticated observation model: `openai/gpt-5.4` (overridable with
  `CABAL_E2E_MODEL_OPENCODE`).

The authenticated binary is below the enforced descriptor baseline `1.14.20`.
These results are therefore observations only and are not capability evidence.
For a future positive `feature_evidence`, the version rule is:
`tested_at_version must be greater than or equal to baseline_version`.
The complete hardened probe must also pass at that version with a reproducible
authenticated configuration.

The corrected observation matrix is:

| Probe mode | Authenticated observation at `1.2.24` | Classification |
|------------|----------------------------------------|----------------|
| `structured-output` | Exact requested public JSON and public usage were observed. | Promising, below baseline; not evidence. |
| `media-initial` | Repeated PNG/JPEG delivery identified the blue PNG and red JPEG. | Promising, below baseline; not evidence. |
| `resume-upload` | A new green PNG on the resumed session was identified. | Promising, below baseline; not evidence. |
| `resume-reuse` | The resumed call without `--file` recalled the prior image. | Promising, below baseline; not evidence. |
| `schema-retry-media` | A fresh retry re-uploaded both images and returned the required object. | Promising, below baseline; not evidence. |
| `web-disabled` | The hardened rerun denied `websearch`, `webfetch`, and `codesearch`; the local marker received no request and no marker content returned. | Promising negative-policy observation below baseline; not evidence. |
| `web-search` | An earlier xAI/auth error event is classified as a failure. A separate exact-version rerun with `openai/gpt-5.4` completed search and returned the official URL. | Provider/model-specific and below baseline; unproven for the descriptor and not evidence. |
| `web-search-fetch` | An earlier xAI/auth error event is classified as a failure. A separate exact-version rerun with `openai/gpt-5.4` completed both lifecycles and returned the fetched `CLI` heading. | Provider/model-specific and below baseline; unproven for the descriptor and not evidence. |

The probe is deliberately locked to the authenticated observation version and
prints `OBSERVED-BELOW-BASELINE`, not `PASS`, for successful modes. It requires
completed public text and completion usage. A top-level error record, malformed
envelope, mixed session/message stream, failed tool record, missing text, or
missing usage fails with a fixed diagnostic that does not expose the provider
payload. The observation binary predates `opencode run --pure`, so the probe
relies on its explicit config isolation instead of passing that unsupported
flag; the baseline Cabal runtime continues to pass `--pure`.

`./tools/probe_opencode_media_web.py --self-test` exercises the public JSONL,
fixed-policy, all-native-network-tool marker, marker-server disconnect, argv,
timeout, and diagnostic-sanitization validators without credentials or network
access.

### 7b. Baseline source contract used by the parser

The exact `v1.14.20` source, not a moving branch, establishes the public JSONL
shape:

- `run.ts` constructs each JSON record with `type`, finite millisecond
  `timestamp`, invocation `sessionID`, and the public `part` payload, and emits
  completed tool, `step-start`, `step-finish`, and completed text records:
  https://github.com/sst/opencode/blob/3175a3c61853e4666acb24fa435783826596665d/packages/opencode/src/cli/cmd/run.ts#L429-L493
- JSON mode does not include the role from `message.updated`; therefore a text
  record alone is not an assistant discriminator. `processor.ts` creates
  `step-start` from `ctx.assistantMessage.id`, then creates text and finish parts
  with that same assistant message ID:
  https://github.com/sst/opencode/blob/3175a3c61853e4666acb24fa435783826596665d/packages/opencode/src/session/processor.ts#L346-L375 and
  https://github.com/sst/opencode/blob/3175a3c61853e4666acb24fa435783826596665d/packages/opencode/src/session/processor.ts#L406-L450
- Canonical IDs are generated as a three-letter prefix, underscore, 12 lowercase
  hexadecimal timestamp characters, and 14 base-62 random characters:
  https://github.com/sst/opencode/blob/3175a3c61853e4666acb24fa435783826596665d/packages/opencode/src/id/id.ts#L4-L16 and
  https://github.com/sst/opencode/blob/3175a3c61853e4666acb24fa435783826596665d/packages/opencode/src/id/id.ts#L57-L75
- `run.ts` emits `session.error` as a top-level `error` JSON record even though
  the process may otherwise complete normally:
  https://github.com/sst/opencode/blob/3175a3c61853e4666acb24fa435783826596665d/packages/opencode/src/cli/cmd/run.ts#L520-L529

The runtime parser consequently accepts session state only from `step_start` and
accepts text, tool lifecycle, and usage only when they retain that exact session
and the assistant message ID established by the latest `step_start`. A later
assistant tool turn may establish a new message ID while retaining the session.
This rejects adversarial completed user-shaped text and mixed streams rather
than inferring the role from attacker-controlled text metadata.

### 7c. Adapter transport and configuration contract

- Initial and fresh-retry media delivery uses repeated direct argv pairs
  `--file <sealed-absolute-path>`. Paths come only from
  `Task_execution_context.sealed_attachment_delivery`; workspace-relative
  attachment references never enter argv.
- Any attachment or enabled web request without matching central authorization
  fails before project config I/O and before process spawn.
- `Web_disabled`, `Web_search`, and `Web_search_and_fetch` map to fixed
  `websearch`/`webfetch` allow-or-deny documents, with `codesearch` always
  denied, supplied through
  `OPENCODE_PERMISSION` and `OPENCODE_CONFIG_CONTENT`. The adapter invokes
  `env` directly (no shell), uses `--pure` with an invocation-specific primary
  agent, disables automatic sharing/update/LSP download, and never interpolates
  host config fragments.
- Each process receives a private mode-`0700` config home and empty
  managed-config directory. Implicit home/global/project discovery is disabled;
  inherited config and experimental-network flags are replaced; only the exact
  task project `opencode.json` is named explicitly. An inherited `OPENCODE_DB`
  override is removed, while `XDG_DATA_HOME` and OpenCode's default database are
  intentionally preserved so provider and account authentication continue to
  work. The temporary directory is removed after the process.
- The invocation replaces the public `build` agent with an invocation-specific
  primary agent whose fixed policy is supplied in `OPENCODE_CONFIG_CONTENT`.
  Static project, account, or managed configuration therefore cannot append a
  late allow rule to the selected agent. The final `OPENCODE_PERMISSION` merge
  pins the same top-level rules after other config scopes.
- At baseline, the explicit project file is loaded before
  `OPENCODE_CONFIG_CONTENT`, and `OPENCODE_PERMISSION` is applied after managed
  config. The runtime additionally redirects `/etc/opencode`/MDM discovery with
  `OPENCODE_TEST_MANAGED_CONFIG_DIR`; OS administrator policy is not assumed
  trustworthy for this task's web boundary. Authenticated provider/account
  credential sources are trusted as authentication inputs, but not as the
  authority for the selected agent's web policy. See:
  https://github.com/sst/opencode/blob/3175a3c61853e4666acb24fa435783826596665d/packages/opencode/src/config/config.ts#L505-L584 and
  https://github.com/sst/opencode/blob/3175a3c61853e4666acb24fa435783826596665d/packages/opencode/src/config/config.ts#L626-L669
- JSONL normalization admits only canonical session IDs, completed assistant
  text, sanitized completed tool lifecycle, and bounded non-negative usage.
  Reasoning, user text, error payloads, tool inputs/results, malformed records,
  and raw-output fallback remain private.
- The validate-and-retry path remains fresh-call only and hard-capped by
  `Json_schema_enforcer`; every fresh attempt receives the same authorized
  sealed image set.

Although the below-baseline observation showed upstream `--session` upload and
attachment reuse,
OpenCode's current Cabal descriptor advertises no session resume capability.
Low-level resume/reuse requests therefore fail closed. CBL-07C deliberately
does not alter shared capability descriptors, runtime snapshots, preflight,
dispatch, or the native JSON Schema evidence recorded above. The descriptor
therefore remains media-disabled with no media evidence and remains
`Web_disabled` with `evidence = None`.
