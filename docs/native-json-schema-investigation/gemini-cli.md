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

---

## 8. Media and Web Transport Investigation — CBL-07D

**Baseline version investigated:** `0.38.2`
**Installed comparison version:** `0.40.1`
**Date:** 2026-09-03
**Outcome:** media, search, and search-plus-fetch remain unsupported; enforce
`Web_disabled` on every invocation.

### 8.1 Surfaces inspected

The investigation inspected the baseline package itself through
`npx --yes @google/gemini-cli@0.38.2`, its bundled help/reference material, and
the source at the pinned `v0.38.2` tag:

- [`atCommandProcessor.ts`](https://github.com/google-gemini/gemini-cli/blob/v0.38.2/packages/cli/src/ui/hooks/atCommandProcessor.ts)
  parses backslash-escaped `@path` references and forwards files through
  `ReadManyFilesTool`.
- [`paths.ts`](https://github.com/google-gemini/gemini-cli/blob/v0.38.2/packages/core/src/utils/paths.ts)
  defines POSIX path escaping/unescaping.
- [`config.ts`](https://github.com/google-gemini/gemini-cli/blob/v0.38.2/packages/cli/src/config/config.ts)
  defines `--policy`, `--admin-policy`, `--include-directories`, `--prompt`,
  and stdin composition.
- [Headless mode reference](https://github.com/google-gemini/gemini-cli/blob/v0.38.2/docs/cli/headless.md)
  documents the public `init`, `message`, `tool_use`, `tool_result`, `error`,
  and `result` stream-JSON records.
- [Policy engine reference](https://github.com/google-gemini/gemini-cli/blob/v0.38.2/docs/reference/policy-engine.md)
  documents global deny rules, the priority range `0..999`, administrator
  policy precedence, and the standard administrator policy locations.

The source path reaches image `inlineData`, so `@path` is a plausible media
surface. Source reachability alone is not content-dependent proof that the
baseline correctly transports PNG/JPEG inputs, preserves them across a resumed
session, or behaves consistently under authentication and account policy.

### 8.2 Reproducible probe

The bounded probe is
[`tools/probe_gemini_media_web.py`](../../tools/probe_gemini_media_web.py). It:

- pins `npx --yes @google/gemini-cli@0.38.2` by default and rejects version
  drift;
- creates private deterministic blue PNG, red JPEG, and green PNG fixtures;
- requires exact content-dependent colors for initial and resumed uploads;
- requires resume reuse to recall the last image without another `@path`;
- tests disabled, search-only, and search-plus-fetch policy modes against public
  tool lifecycle records;
- uses direct argv plus stdin, bounds every subprocess, suppresses raw backend
  diagnostics, and prints only fixed errors;
- provides an offline `--self-test` covering fixtures, permissions, escaping,
  policies, public parsing, content rejection, malformed output, timeouts, and
  argument-error privacy.

Commands executed on 2026-09-03:

```text
tools/probe_gemini_media_web.py --self-test
# PASS self-test

tools/probe_gemini_media_web.py --mode media-initial
# ERROR: Gemini authentication is unavailable

tools/probe_gemini_media_web.py --mode web-disabled
# ERROR: Gemini authentication is unavailable

tools/probe_gemini_media_web.py --mode web-search
# ERROR: Gemini authentication is unavailable

tools/probe_gemini_media_web.py --mode web-live
# ERROR: Gemini authentication is unavailable
```

The same account failure occurred with the installed `0.40.1` binary. No API
key or Vertex AI environment was available. Resume-upload and resume-reuse could
not begin because obtaining the initial authenticated session failed.

### 8.3 Capability outcome

The probe did not establish any content-dependent media or positive web
behavior. Consequently the existing descriptor remains unchanged:

```text
file_reading = false
media_support.media_types = []
media_support.evidence = None
web_support.maximum = Web_disabled
web_support.evidence = None
native_json_schema_output = false
```

The adapter rejects attachments, sealed attachment paths, `Web_search`, and
`Web_search_and_fetch` before configuration I/O or process spawn. This is an
intentional proof-first result, not a claim that upstream source can never
support those features. Re-run the probe after baseline/account changes before
adding a positive descriptor claim or generating production `@path` references.

### 8.4 Fixed `Web_disabled` enforcement

Gemini exposes both search and fetch tools by default, so retaining
`Web_disabled` requires an active transport restriction. Each adapter runtime
invocation creates a private `0o600` TOML policy with these exact global rules:

```toml
[[rule]]
toolName = ["google_web_search", "web_fetch"]
decision = "deny"
priority = 999
```

The same invocation-scoped file is supplied through both `--policy` and
`--admin-policy`. The administrator-tier rule defeats user/workspace/default
settings. Gemini `0.38.2` ignores supplemental administrator policies when its
standard administrator policy directory contains a TOML file; Cabal therefore
checks the documented Linux, macOS, and Windows locations and fails closed
before config I/O or spawn when such a policy can supersede the scoped deny.
The policy file is unlinked on success, failure, timeout, cancellation, and
ordinary exceptions.

Baseline `0.38.2` rejects `--skip-trust`, so the adapter no longer emits that
flag. It sends caller text over stdin with `-p ""`; the empty option value forces
headless mode without appending a literal hyphen to stdin. Literal `@`
characters in prompt/instructions are backslash-escaped to prevent caller text
from becoming an implicit file-read directive. Schema prompts remain on the
existing non-native validate-and-retry path.

### 8.5 Re-evaluation gate

A future positive media or web claim requires all applicable probe modes to pass
against the exact proposed baseline with an authenticated account. Evidence must
include initial PNG/JPEG content, resumed new-image content, no-reference resume
reuse, and public tool lifecycle proof for the claimed web level. Source review
or a successful non-content-dependent invocation is insufficient.
