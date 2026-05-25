# Native JSON Schema Investigation — gemini-cli

**Backend ID:** `gemini-cli`
**Baseline version investigated:** `0.38.2`
**Version range consulted:** `0.38.2` (baseline) .. `0.38.2` (latest stable at time of investigation), i.e. `baseline_version..latest-public-release`
**Outcome:** AC2(b) — documented non-support
**Story:** #632
**Date:** 2026-05-25

---

## 1. Summary

At baseline version `0.38.2`, gemini-cli does not expose a CLI surface for
forwarding a JSON schema or `generationConfig` payload into the Gemini API
invocation.  The CLI is a headless terminal agent that calls the Gemini API
internally but does not surface `response_schema`, `response_mime_type`, or any
`generationConfig` forwarding mechanism as CLI flags or config-file keys at
`0.38.2`.

This investigation consulted (a) the `gemini --help` CLI surface and the
open-source repository at the pinned `v0.38.2` tag (this is the AC1
"source where open" surface), and (b) the Gemini API structured output
documentation, release notes, and changelog pinned to `0.38.2`.  Neither
surface reveals a schema-forwarding path at `0.38.2`.

**Outcome:** AC2(b) — documented non-support.  `native_json_schema_output`
remains `false`; `capability_evidence` remains `None`.

---

## 2. CLI Surface Consulted

### 2a. `gemini --help` output (accessed 2026-05-25)

The gemini-cli binary at version `0.38.2` accepts the following flags relevant
to this investigation:

| Flag | Purpose | Schema-forwarding? |
|------|---------|-------------------|
| `--model <model>` | Select the Gemini model | No |
| `--all` | Show all tool output | No |
| `--debug` | Enable debug logging | No |
| `--resume <id>` | Resume a previous session | No |
| `--sandbox` | Enable sandboxed execution | No |
| `--checkpointing` | Enable checkpointing | No |
| `--yolo` | Approve all tool calls automatically | No |
| `--version` | Print version and exit | No |
| `--help` | Show help and exit | No |

No `--generation-config-file`, `--response-schema`, `--response-mime-type`,
`--json-schema`, or equivalent flag exists at `0.38.2`.

Source: https://github.com/google-gemini/gemini-cli (accessed 2026-05-25)

### 2b. Source inspection (source where open)

gemini-cli is open-source at https://github.com/google-gemini/gemini-cli
(accessed 2026-05-25).  At tag `v0.38.2`, the `packages/cli/src/` and
`packages/core/src/` directories were inspected for `response_schema`,
`responseSchema`, `generationConfig`, `responseMimeType`, and `json_schema`
usage.

Key findings at `v0.38.2`:

- The Gemini API client constructs a `GenerateContentRequest` payload
  internally but does not accept or forward a caller-supplied
  `response_schema` or `generationConfig` override via any CLI flag or
  config-file key.
- No `gemini_config.yaml` or equivalent config-file key accepts a JSON schema
  at `0.38.2`.
- The `response_schema` field is a Gemini API-level feature within
  `generationConfig` (introduced in the Gemini 1.5 series) but the CLI does
  not provide a forwarding surface for it at `0.38.2`.

Source: https://github.com/google-gemini/gemini-cli/tree/v0.38.2
(accessed 2026-05-25)

---

## 3. Authoritative Sources Consulted

The following authoritative sources were consulted, all pinned to behaviour
at `0.38.2` (i.e. only features present in the CLI at that version are
considered).  Release notes and changelog were inspected to confirm no
schema-forwarding feature was added between the baseline and the latest
public release at the time of this investigation.

| Source | URL | Accessed |
|--------|-----|----------|
| gemini-cli repository | https://github.com/google-gemini/gemini-cli | 2026-05-25 |
| gemini-cli source at v0.38.2 | https://github.com/google-gemini/gemini-cli/tree/v0.38.2 | 2026-05-25 |
| gemini-cli baseline release tag | https://github.com/google-gemini/gemini-cli/releases/tag/v0.38.2 | 2026-05-25 |
| gemini-cli releases / changelog | https://github.com/google-gemini/gemini-cli/releases | 2026-05-25 |
| Gemini API structured output docs | https://ai.google.dev/gemini-api/docs/structured-output | 2026-05-25 |
| Gemini API generationConfig reference | https://ai.google.dev/api/generate-content#v1beta.GenerationConfig | 2026-05-25 |

No feature in any of these sources provides a stable, machine-readable
mechanism at `0.38.2` for forwarding a full JSON schema from the Cabal caller
into the gemini-cli invocation such that the underlying Gemini API enforces it
natively via `generationConfig.response_schema`.

---

## 4. Options Evaluated

The AC1 investigation for Story #632 evaluates three candidate CLI surfaces,
as specified in the story's investigation criterion:

### Option 1 — Full `generationConfig` forwarding

A flag such as `--generation-config-file <path>` that accepts a JSON file
whose contents map to a Gemini API `generationConfig` payload.

**Finding:** Not present at `0.38.2`.  No such flag appears in `gemini --help`
at the baseline version, and source inspection of `v0.38.2` confirms no
`generationConfig` file-forwarding path exists.

### Option 2 — Curated subset as first-class flags

First-class CLI flags such as `--response-schema <path>` and
`--response-mime-type <type>` that correspond directly to the
`generationConfig.response_schema` and `generationConfig.responseMimeType`
Gemini API fields.

**Finding:** Not present at `0.38.2`.  Neither flag appears in `gemini --help`
at the baseline version.

### Option 3 — No schema-forwarding surface at `baseline_version`

**Finding:** Confirmed.  The CLI at `0.38.2` does not expose any mechanism for
forwarding a JSON schema into the underlying Gemini API `generationConfig`.

**Outcome:** Option 3 — routes to **AC2(b) documented non-support**.

---

## 5. Investigation Outcome

**AC1 binary outcome:** Option 3 (no schema-forwarding surface) — routes to
AC2(b) documented non-support.

**Descriptor action:** `native_json_schema_output` stays `false`;
`capability_evidence` stays `None`.  No other backend descriptor is touched
(NFR-U2).

---

## 6. Future-Work Note

Features newer than `0.38.2` are not considered here per NFR-U3.  If a future
gemini-cli release introduces a `--generation-config-file <path>` flag, a
`--response-schema <path>` flag, or an equivalent `generationConfig`-forwarding
mechanism in the CLI config file that routes to
`generationConfig.response_schema` in the Gemini API, this investigation should
be re-opened against the new `baseline_version`.

**Re-evaluation trigger:** A gemini-cli release that adds a documented
`--response-schema`, `--generation-config-file`, or equivalent CLI flag or
config-file key for schema-constrained output.  Track at:
https://github.com/google-gemini/gemini-cli/releases (accessed 2026-05-25).

Upstream discussion / issue link for tracking schema-forwarding support:
https://github.com/google-gemini/gemini-cli/issues/2768 (accessed 2026-05-25).
This thread discusses structured output and JSON schema support in gemini-cli
and is the closest upstream tracking issue at the investigated baseline.

---

## 7. Three-Artifact AC2(b) Closure

1. **Investigation note** (this file) — at
   `libs/cabal/docs/native-json-schema-investigation/gemini-cli.md`.
2. **Pinning test** — `libs/cabal/test/test_demo_632.ml` asserts
   `native_json_schema_output = false` for gemini-cli (NFR-R1);
   `libs/cabal/test/test_backend_registry.ml` (AC6 suite, `#632` entries)
   performs the same assertion.  Both tests fail the moment a contributor flips
   the flag without updating this investigation note.
3. **Completion notes** — Story #632 completion references this investigation
   and the AC1 sources above as the basis for the AC2(b) decision.

### Descriptor state

```
id = "gemini-cli"
baseline_version = "0.38.2"
capabilities.native_json_schema_output = false   (* unchanged *)
capability_evidence = None                        (* unchanged *)
```
