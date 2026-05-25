# Native JSON Schema Investigation — copilot-cli

**Backend ID:** `copilot-cli`
**Baseline version investigated:** `1.0.34`
**Version range consulted:** `1.0.34` (baseline) .. `1.0.34` (latest stable at time of investigation), i.e. `baseline_version..latest-public-release`
**Outcome:** AC2(b) — documented non-support
**Story:** #633
**Date:** 2026-05-25

---

## 1. Summary

At baseline version `1.0.34`, copilot-cli does not expose a CLI surface for
forwarding a JSON schema into the underlying GitHub Copilot API invocation.
The CLI is a closed-source GitHub CLI extension (`gh copilot`) that calls the
GitHub Copilot API internally but does not surface any `--json-schema`,
`--response-format`, `--response-schema`, or equivalent schema-forwarding
mechanism as a CLI flag or config-file key at `1.0.34`.

This investigation consulted (a) the `copilot --help` CLI surface and
source where open (the extension source is not publicly available at `1.0.34`;
the `gh-copilot` repository is a precompiled binary distribution — see
Section 2a for detail), and (b) the GitHub Copilot API documentation, release
notes, and changelog pinned to `1.0.34`.  Neither surface reveals a
schema-forwarding path at `1.0.34`.

Per Decision D-15, even if a hint-style or best-effort JSON guide were
discoverable in the future, it would not qualify for AC2(a): the
`capability_evidence` contract requires that schema-violating responses be
provider-rejected, and the GitHub Copilot Chat API does not document hard
schema enforcement in its CLI-facing surface at `1.0.34`.

**Outcome:** AC2(b) — documented non-support.  `native_json_schema_output`
remains `false`; `capability_evidence` remains `None`.

---

## 2. CLI Surface Consulted

### 2a. `copilot --help` output (accessed 2026-05-25)

The copilot-cli binary at version `1.0.34` is distributed as a GitHub CLI
extension.  It is invoked as `gh copilot` or, when installed standalone, as
`copilot`.  The relevant flags at this version:

| Flag | Purpose | Schema-forwarding? |
|------|---------|-------------------|
| `--help` | Show help and exit | No |
| `--version` | Print version and exit | No |
| `--target <agent\|shell\|git>` | Agent target mode | No |
| `--hostname <host>` | GitHub host override | No |

No `--json-schema`, `--response-format`, `--response-schema`,
`--generation-config`, or equivalent schema-forwarding flag exists at `1.0.34`.

Source: https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically (accessed 2026-05-25)

### 2b. Source inspection (source where open)

The copilot-cli extension binary at `1.0.34` is distributed as a
**closed-source precompiled binary** via the `github/gh-copilot` GitHub
repository.  The source code is not publicly available for inspection at this
version; the repository contains only release assets.

This investigation documents the absence of open source as required by the
AC1 "source where open" criterion: no source was available for inspection at
`v1.0.34`.

Source: https://github.com/github/gh-copilot/releases/tag/v1.0.34
(accessed 2026-05-25)

The GitHub CLI extension precompile machinery is at:
https://github.com/github/gh-extension-precompile (accessed 2026-05-25)
— confirms the closed-source distribution model for `gh-copilot`.

---

## 3. Authoritative Sources Consulted

The following authoritative sources were consulted, all pinned to behaviour
at `1.0.34` (i.e. only features present in the CLI at that version are
considered).  Release notes and changelog were inspected to confirm no
schema-forwarding feature was added between the baseline and the latest
public release at the time of this investigation.

| Source | URL | Accessed |
|--------|-----|----------|
| gh-copilot release tag v1.0.34 | https://github.com/github/gh-copilot/releases/tag/v1.0.34 | 2026-05-25 |
| gh-copilot releases / changelog | https://github.com/github/gh-copilot/releases | 2026-05-25 |
| Copilot CLI programmatic docs | https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically | 2026-05-25 |
| About Copilot CLI (concepts) | https://docs.github.com/en/copilot/concepts/agents/copilot-cli/about-copilot-cli | 2026-05-25 |
| GitHub Copilot API reference | https://docs.github.com/en/rest/copilot | 2026-05-25 |
| GitHub Copilot Chat extensions | https://docs.github.com/en/copilot/building-copilot-extensions/about-building-copilot-extensions | 2026-05-25 |

No feature in any of these sources provides a stable, machine-readable
mechanism at `1.0.34` for forwarding a full JSON schema from the Cabal caller
into the copilot-cli invocation such that the underlying GitHub Copilot API
enforces it natively via hard-constrained decoding.

---

## 4. Options Evaluated

The AC1 investigation for Story #633 evaluates candidate CLI surfaces for
schema-forwarding, as specified in the story's investigation criterion.

### Option 1 — JSON schema forwarding via CLI flag

A flag such as `--json-schema <path>` or `--response-format json-schema`
that accepts a JSON schema definition and forwards it to the underlying
GitHub Copilot API for enforced structured output.

**Finding:** Not present at `1.0.34`.  No such flag appears in `copilot --help`
at the baseline version.  The programmatic docs at
https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically
(accessed 2026-05-25) document only prompt flags and silent/non-interactive
invocation modes; no schema-forwarding flag is documented.

### Option 2 — JSON schema via config file

A config file key (analogous to `codex`'s `config.json` or `opencode`'s
`opencode.json`) that allows specifying a `response_format` or `json_schema`
field passed through to the GitHub Copilot API.

**Finding:** Not present at `1.0.34`.  The GitHub Copilot CLI config
surface at this version does not document a structured-output or
schema-enforcement key.  The GitHub Copilot API documentation (accessed
2026-05-25) does not expose a `response_format: json_schema` field for the
CLI-facing chat completions endpoint at `1.0.34`.

### Option 3 — Loose / hint-style enforcement (D-15 exclusion)

Some GitHub Copilot responses could be guided by a JSON example in the
prompt, but this would constitute best-effort / hint-style enforcement, not
hard-constrained decoding where schema-violating responses are
provider-rejected.

**Finding:** Even if discoverable, this option is excluded by Decision D-15:
loose / schema-as-hint / best-effort decoding does NOT qualify for AC2(a).
The `capability_evidence` contract requires that schema-violating responses
be provider-rejected; hint-style guidance cannot satisfy this requirement.

### Option 4 — No schema-forwarding surface at `baseline_version`

**Finding:** Confirmed.  The CLI at `1.0.34` does not expose any mechanism for
forwarding a JSON schema into the underlying GitHub Copilot API with hard
schema enforcement.

**Outcome:** Option 4 — routes to **AC2(b) documented non-support**.

---

## 5. Investigation Outcome

**AC1 binary outcome:** Option 4 (no schema-forwarding surface with hard
enforcement) — routes to AC2(b) documented non-support.

**Descriptor action:** `native_json_schema_output` stays `false`;
`capability_evidence` stays `None`.  No other backend descriptor is touched
(NFR-U2).

**AC3 note:** Since the descriptor stays at `false`, AC3 (E2E test via native
path) is not applicable.  The required credential env var for copilot-cli
would be `GH_TOKEN` (as documented in the programmatic invocation docs at
https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically,
accessed 2026-05-25), but no E2E test is registered because the native path
is not wired.

---

## 6. Future-Work Note

Features newer than `1.0.34` are not considered here per NFR-U3.  If a future
copilot-cli release introduces a `--json-schema <path>` flag, a
`--response-format json-schema` flag, or an equivalent config-file key that
routes to a hard-constrained GitHub Copilot API response schema, this
investigation should be re-opened against the new `baseline_version`.

**Re-evaluation trigger:** A copilot-cli release that adds a documented
`--json-schema`, `--response-format`, or equivalent flag or config key for
hard-constrained schema output.  Track at:
https://github.com/github/gh-copilot/releases (accessed 2026-05-25).

Upstream discussion / issue link for tracking schema-forwarding support in
GitHub Copilot CLI:
https://github.com/orgs/community/discussions/categories/copilot (accessed
2026-05-25).  GitHub Community discussions are the primary public forum for
GitHub Copilot feature requests; a structured-output / JSON schema enforcement
request for the Copilot CLI should be filed there if not already present.
See also: https://github.com/orgs/community/discussions/82756 (accessed
2026-05-25) — the closest existing GitHub Community discussion on Copilot CLI
output format.

The upstream tracking link for copilot-cli schema-forwarding is:
https://github.com/orgs/community/discussions/ (accessed 2026-05-25).

---

## 7. Three-Artifact AC2(b) Closure

1. **Investigation note** (this file) — at
   `libs/cabal/docs/native-json-schema-investigation/copilot-cli.md`.
2. **Pinning test** — `libs/cabal/test/test_demo_633.ml` asserts
   `native_json_schema_output = false` for copilot-cli (NFR-R1);
   `libs/cabal/test/test_backend_registry.ml` (AC6 suite, `#633` entries)
   performs the same assertion.  Both tests fail the moment a contributor flips
   the flag without updating this investigation note.
3. **Completion notes** — Story #633 completion references this investigation
   and the AC1 sources above as the basis for the AC2(b) decision.

### Descriptor state

```
id = "copilot-cli"
baseline_version = "1.0.34"
capabilities.native_json_schema_output = false   (* unchanged *)
capability_evidence = None                        (* unchanged *)
```
