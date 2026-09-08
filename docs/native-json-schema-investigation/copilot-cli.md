# Native JSON Schema Investigation — copilot-cli

**Backend ID:** `copilot-cli`
**Historical schema investigation:** legacy `gh-copilot` extension `1.0.34`
**Current transport re-evaluation:** standalone GitHub Copilot CLI `1.0.54`
**Surfaces consulted:** the two named releases; they are distinct CLI
architectures, not one continuous version range
**Outcome:** AC2(b) — documented non-support
**Story:** #633
**Date:** 2026-05-25

---

## 1. Summary

The original Story #633 result concerns the legacy `github/gh-copilot` `1.0.34`
GitHub CLI extension architecture, invoked as `gh copilot` (or its standalone
extension binary). That historical extension did not expose a schema-forwarding
surface. The current CBL-07E result separately concerns the standalone
`github/copilot-cli` npm CLI at `1.0.54`, invoked as `copilot` and pinned with
`--prefer-version 1.0.54`. The current CLI also exposes no native JSON Schema
surface and is quarantined for an independent MCP-isolation reason. Statements
about the old extension architecture do not describe the current CLI.

The historical investigation consulted (a) the legacy extension help surface and
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

## 2. Historical extension CLI surface consulted

### 2a. `gh copilot --help` output (accessed 2026-05-25)

The legacy `gh-copilot` binary at version `1.0.34` is distributed as a GitHub CLI
extension. It is invoked as `gh copilot` or through its standalone extension
binary. The relevant flags at this historical version:

| Flag | Purpose | Schema-forwarding? |
|------|---------|-------------------|
| `--help` | Show help and exit | No |
| `--version` | Print version and exit | No |
| `--target <agent\|shell\|git>` | Agent target mode | No |
| `--hostname <host>` | GitHub host override | No |

No `--json-schema`, `--response-format`, `--response-schema`,
`--generation-config`, or equivalent schema-forwarding flag exists at `1.0.34`.

Historical release source:
https://github.com/github/gh-copilot/releases/tag/v1.0.34
(accessed 2026-05-25)

### 2b. Source inspection (source where open)

The legacy `gh-copilot` extension binary at `1.0.34` is distributed as a
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
into the legacy extension invocation such that the underlying GitHub Copilot API
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

**Finding:** Confirmed. The legacy extension at `1.0.34` does not expose any
mechanism for forwarding a JSON schema into the underlying GitHub Copilot API
with hard schema enforcement.

**Outcome:** Option 4 — routes to **AC2(b) documented non-support**.

---

## 5. Investigation Outcome

**AC1 binary outcome:** Option 4 (no schema-forwarding surface with hard
enforcement) — routes to AC2(b) documented non-support.

**Current descriptor action, independently revalidated at `1.0.54`:**
`native_json_schema_output` stays `false`; `capability_evidence` stays `None`.
No other backend descriptor is touched (NFR-U2).

**AC3 note:** Since the descriptor stays at `false`, AC3 (E2E test via native
path) is not applicable. The required credential env var for the current
standalone CLI would be `GH_TOKEN` (as documented in the programmatic invocation docs at
https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically,
accessed 2026-05-25), but no E2E test is registered because the native path
is not wired.

---

## 6. Future-Work Note

The distinct standalone `github/copilot-cli` `1.0.54` surface was re-evaluated
for CBL-07E. If a future release of that current CLI introduces a
`--json-schema <path>` flag, a
`--response-format json-schema` flag, or an equivalent config-file key that
routes to a hard-constrained GitHub Copilot API response schema, this
investigation should be re-opened against the new `baseline_version`.

**Re-evaluation trigger:** A copilot-cli release that adds a documented
`--json-schema`, `--response-format`, or equivalent flag or config key for
hard-constrained schema output.  Track at:
https://github.com/github/copilot-cli/releases (accessed 2026-09-04). The legacy
extension releases remain at
https://github.com/github/gh-copilot/releases (accessed 2026-05-25) solely as the
historical Story #633 source.

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
baseline_version = "1.0.54"
capabilities.native_json_schema_output = false   (* unchanged *)
capability_evidence = None                        (* unchanged *)
```

---

## 8. CBL-07E media/web and transport re-evaluation

- **Accessed:** 2026-09-04
- **Exact authenticated binary:** GitHub Copilot CLI `1.0.54`
- **Current media outcome:** disabled; transport quarantined before spawn
- **Current web outcome:** `Web_disabled`
- **Current structured-output outcome:** false; the tested JSONL parser is dormant
- **Unchanged schema outcome:** no native JSON Schema

This section concerns the current standalone `github/copilot-cli` npm CLI. It
does not infer its architecture from the historical `github/gh-copilot` GitHub
CLI extension discussed in Sections 1–7.

### Authoritative public surfaces

- Pinned `1.0.54` distribution README:
  https://github.com/github/copilot-cli/blob/v1.0.54/README.md
- Pinned `1.0.54` changelog:
  https://github.com/github/copilot-cli/blob/v1.0.54/changelog.md
- GitHub's non-interactive/programmatic CLI documentation:
  https://docs.github.com/en/copilot/how-tos/copilot-cli/automate-copilot-cli/run-cli-programmatically
- Exact local `copilot --prefer-version 1.0.54 --help`, used to pin the stable
  argument surface for `--attachment`, `--output-format json`, `--stream off`,
  `--available-tools`, `--allow-all-tools`, `--deny-tool`, `--allow-url`,
  `--add-dir`, and `--disallow-temp-dir`.
- Exact local `copilot --prefer-version 1.0.54 mcp --help`, which identifies
  user `~/.copilot/mcp-config.json` and workspace `.mcp.json` sources.
- Installed `1.0.54` `app.js`, searched for `COPILOT_HOME`, `.mcp.json`,
  `disable-builtin-mcps`, and MCP loader construction. The loader merges user,
  workspace, installed-plugin, additional, and account-controlled ODR sources;
  built-ins are controlled separately.

The local npm distribution includes a bundled/minified `app.js` but not a
maintainable upstream source tree. The adapter therefore treats the documented
CLI and public JSONL contract as a strict boundary and fails closed on drift.
Before semantic parsing, both the OCaml candidate parser and Python evidence
probe recursively reject duplicate keys in every object reached through objects
or arrays, including opaque tool arguments, user attachment objects, and skills.
The failure is fixed and sanitized. The terminal record is then exact:
`type`, `timestamp`, `exitCode`, `sessionId`, and `usage`; `usage` and nested
`codeChanges` use exact field sets,
integer kinds/ranges where required, finite non-negative premium requests, and no
workspace changes. Nonterminal event envelopes require exactly `type`, `id`,
`parentId`, `timestamp`, and object `data`; every accepted session, user,
assistant-turn/message/reasoning, tool-start, and successful tool-complete payload
also has an exact field set. Event/turn/interaction/tool IDs, object-valued tool
arguments, allowlisted request/start/finish pairing, cumulative token overflow,
one lowercase canonical UUID terminal session, and one last zero-exit result are
checked before projection. Raw records and tool arguments are discarded;
callbacks are reconstructed only from validated assistant text, session identity,
and output-token usage.

### Historical authenticated observations and offline proof

Before the MCP isolation review, `tools/probe_copilot_media_web.py` produced a
bounded, non-sensitive, exact-version authenticated observation. Attachment
behavior was observed at `1.0.54`, but no positive media evidence is recorded
because complete MCP discovery isolation is unproven. Its `media` run
used deterministic blue PNG and red JPEG fixtures
with spaces in their paths, repeated `--attachment` flags in caller order,
explicit `--add-dir`, and `--disallow-temp-dir`. It passed the ordered answer,
paired successful `view` lifecycles, a swapped-order causal control, and an
omitted-media control after fixture removal. The final `web-disabled` run passed
with no web tool lifecycle. These observations prove attachment behavior, but
they do not prove that unrelated MCP servers were unable to load before task
execution and therefore no longer back a capability claim.

The probe's credential-free `--self-test` remains authoritative for its local
mechanics. It exercises every mode validator, deterministic fixture magic and
permissions, ordered/swapped/omitted media controls, exact URL web content,
strict public record shapes, forbidden/failed/cross-turn/outstanding tools,
nonempty MCP, workspace changes, missing/extra/duplicate fields, object-valued
arguments, recursive nested duplicate keys, numeric kinds/ranges and overflow,
malformed output, timeouts, process start/decode failures, interruption,
redaction, and a real subprocess invalid-argument check. Positional `selftest`
remains a compatibility alias.

The live `web-disabled` investigation now runs an exact-URL `web_fetch` positive
control first, proving credentials, network, and tool functionality, then a
separately configured denial requiring no web lifecycle. Cabal's policy is
hierarchical and the probe does not implement a complete negative unrelated-URL
matrix. Consequently the descriptor remains `Web_disabled`; the positive
control is not promoted to capability evidence.

### Runtime quarantine decision

Copilot prompt mode documents `--allow-all-tools` as required. The retained
candidate invocation bounds it with `--available-tools=view,grep,glob`, explicit
shell/write/memory/URL denials, and no `--allow-all`, `--yolo`,
`--allow-all-paths`, or `--allow-all-urls`. Hidden broadening environment
variables are unset. A fresh private `COPILOT_HOME` would isolate user config,
plugin state, and logs; descriptor/no-follow cleanup and fixed structural cleanup
failure are independently tested.

Those controls are insufficient at `1.0.54`: `--disable-builtin-mcps` disables
only built-ins, `COPILOT_HOME` does not disable workspace or account-controlled
ODR MCP sources, and `--additional-mcp-config` augments rather than replaces the
effective configuration. Server names cannot be known safely in advance for
per-server disable flags. Cabal therefore cannot establish an empty effective
MCP set before process start. Every task fails with a fixed diagnostic before
capability/input preflight, attachment staging, version/availability subprocesses,
project setup, or backend spawn: hardened bootstrap binds
`Dispatch_quarantined Incomplete_mcp_isolation`, which central dispatch checks
immediately after validated registry lookup. The direct adapter rejection remains
defense in depth. Project MCP files are never generated or overwritten. The
descriptor, effective entry, and independent runtime snapshot all advertise
`structured_output = false`, `media_types = []`, `media evidence = None`, and
`Web_disabled`. CBL-08 excludes Copilot from authenticated media selection.
Streaming, session resume, read-only, MCP, and native JSON Schema also remain
false.

The launcher installed on the probe host reported `1.0.54` while its default
cached application could select older behavior. The dormant candidate invocation
builder therefore pins `--prefer-version 1.0.54`; the descriptor baseline remains
the exact authenticated and investigated version rather than an inferred minimum
from earlier changelog entries.
