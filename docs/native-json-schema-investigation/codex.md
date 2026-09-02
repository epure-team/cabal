# Codex native schema, media, and web investigation

## Metadata

| Field | Value |
|---|---|
| Backend | `codex` |
| Cabal baseline | `0.131.0` |
| Installed/tested version | `codex-cli 0.131.0` |
| Probe date | 2026-09-02 |
| Sources accessed | 2026-09-02 |
| Stories | #630 native schema; #24 / CBL-07A media and web |

## Upstream contracts inspected

- Historical Codex `0.122.0` `exec` CLI source (commit `230dcade`):
  <https://github.com/openai/codex/blob/230dcadee609fa99d6162fe1107457030e5270a7/codex-rs/exec/src/cli.rs>
- Codex `0.131.0` `exec` CLI source:
  <https://github.com/openai/codex/blob/rust-v0.131.0/codex-rs/exec/src/cli.rs>
- Shared initial-image CLI options at `0.131.0`:
  <https://github.com/openai/codex/blob/rust-v0.131.0/codex-rs/utils/cli/src/shared_options.rs>
- Versioned config schema (`web_search = "disabled" | "cached" | "live"`):
  <https://github.com/openai/codex/blob/rust-v0.131.0/codex-rs/core/config.schema.json>
- Upstream web-mode request tests, including explicit-mode precedence over
  legacy feature flags:
  <https://github.com/openai/codex/blob/rust-v0.131.0/codex-rs/core/tests/suite/web_search.rs>

The tagged `0.131.0` baseline source contains all relevant surfaces: initial
`-i/--image`, resume `-i/--image`, `--output-schema <FILE>`, `--json`, and the
three `web_search` modes. The installed help confirms the same contracts; the
historical `0.122.0` source remains linked to preserve the earlier native-schema
investigation trail.

Two ordering details are significant on resume:

1. `--output-schema` and `-s/--sandbox` are root `exec` options and must occur
   before the `resume` subcommand.
2. Resume images are subcommand options and occur after the session id.

The first attempted resume probe placed `-s read-only` after `resume` and was
rejected by argument parsing with exit 2. The corrected ordering below exited
successfully; Cabal pins that ordering in exact-argv tests.

## Preserved native schema conclusion (Story #630)

Codex forwards a task-scoped schema file through `--output-schema <FILE>`; the
flag originally shipped in `0.41.0` ([PR #4079](https://github.com/openai/codex/pull/4079)).
The authenticated `0.131.0` probe accepted a draft 2020-12 schema and returned a
conforming response. The descriptor therefore remains independently pinned to:

```text
native_json_schema_output = true
tested_at_version = "0.131.0"
json_schema_draft = "2020-12"
test_method = E2e_test
```

CBL-07A does not weaken or couple that evidence to the media/web claims.

## Reproducible authenticated probes

The probes use deterministic solid-color images and prompts containing no
secrets. Raw JSONL and stderr are captured in memory and never displayed or
stored. The parser consumes only public thread, agent-message, and web lifecycle
records; reasoning, error payloads, raw tool arguments, credentials, and raw
backend output are never reported.

`tools/probe_codex_media_web.py` is the checked-in executable reproduction
artifact. It requires exactly `codex-cli 0.131.0`, creates and removes its own
deterministic 64×64 PNG/JPEG and draft-2020-12 schema fixtures, passes every
prompt on stdin, bounds version/task subprocesses, and prints only
`PASS <mode>` or a fixed sanitized failure. Its schemas constrain object shape
and color vocabulary but do not embed the expected image color or official-page
text. Run all evidence modes with:

```text
./tools/probe_codex_media_web.py
```

Individual evidence paths are named `media-initial`, `resume-upload`,
`resume-reuse`, `web-cached`, and `web-live`. The final authenticated rerun on
2026-09-02 passed all five content-dependent modes against exactly
`codex-cli 0.131.0`. `--self-test` executes every mode-specific validator and
negative assertion offline. Optional `--debug-public` reports only fixed
search/fetch/official-page booleans.

### Initial sealed absolute image plus native schema

Sanitized runtime argv:

```text
codex exec --json --skip-git-repo-check --ignore-user-config \
  -c 'web_search="disabled"' -s read-only \
  --output-schema '<task-schema-file>' \
  -i '<sealed-image-a.png>' -i '<sealed-image-b.jpg>' -
```

Outcome at `0.131.0`: exit 0 with a canonical public thread UUID and strict
agent-message JSON. The exact public result was
`{"png_dominant_color":"blue","jpeg_dominant_color":"red"}`. A constant
`{"ok":true}` answer, a missing attachment, or a wrong color now fails. The
private absolute fixture paths remain outside the process working directory,
matching Cabal's sealed runtime invocation shape.

### Multiple PNG/JPEG images plus native schema

Sanitized exact argv:

```text
codex exec --json --skip-git-repo-check --ignore-user-config \
  -c 'web_search="disabled"' -s read-only \
  --output-schema '<task-schema-file>' \
  -i '<sealed-image-1.png>' -i '<sealed-image-2.jpg>' \
  -
```

This transport shape is now covered by the initial content assertion above:
PNG, JPEG, repeated flags, schema composition, and both independently known
colors are exercised in one invocation.

### Resume PNG/JPEG upload plus native schema

After a probe-owned initial session emitted a canonical lowercase UUID in its
public `thread.started.thread_id`, the corrected sanitized argv was:

```text
codex exec --output-schema '<task-schema-file>' -s read-only \
  resume "$THREAD_ID" --json --skip-git-repo-check \
  --ignore-user-config -c 'web_search="disabled"' \
  -i '<new-sealed-image-c.png>' -
```

The newly uploaded fixture is distinct from the initial two. Outcome: exit 0;
the exact public result was `{"new_image_dominant_color":"green"}`. The schema
and sandbox are root `exec` options before `resume`; JSON/config and the new
image option are resume options after the UUID.

### Resume media reuse plus native schema

The `resume-reuse` evidence mode uses the same exact scoping but deliberately
emits no image options:

```text
codex exec --output-schema '<task-schema-file>' -s read-only \
  resume "$THREAD_ID" --json --skip-git-repo-check \
  --ignore-user-config -c 'web_search="disabled"' -
```

The prompt forbids file/tool use and asks for the dominant color of the most
recently inspected image. The exact answer is green after `resume-upload` (blue
when `resume-reuse` establishes its own initial session), and argv is
independently checked to contain no `-i`. This proves that a schema-bearing retry
can recall an image-derived fact from the already-fed session without duplicate
uploads.

### Live web search and page fetch

Sanitized exact argv:

```text
codex exec --json --skip-git-repo-check --ignore-user-config \
  -c 'web_search="live"' -s read-only \
  --output-schema '<task-schema-file>' -
```

The prompt requires search, opening
`https://developers.openai.com/codex/cli/`, and returning its visible primary
H1. Outcome: exit 0; paired public web lifecycles establish both search and the
official-page fetch, and the exact public answer is
`{"page_h1":"Inspect, edit, and run code from your terminal"}`. The tagged
schema/source separately pins `cached` as search without external live access
and `disabled` as no web tool; CLI `-c` overrides remain the highest-precedence
ordinary config layer. `web-cached` requires a complete cached-search lifecycle
and the same exact official-page fact; `web-live` additionally requires the
public search/fetch lifecycle and exact official URL.

## Adapter design and privacy

- Actual argv is a string list passed directly to the process launcher; no
  shell interpolation or host-provided config fragment is accepted.
- Public attachment references remain workspace-relative. Preflight streams the
  authorized descriptor once for size, digest, magic, and an exact sealed copy;
  Codex receives only the sealed absolute path and never reopens the caller path.
- `Upload_attachments` emits one `-i` pair per attachment.
  `Reuse_session_attachments` requires resume and emits none.
- Web policy maps to fixed values only:
  `Web_disabled -> disabled`, `Web_search -> cached`, and
  `Web_search_and_fetch -> live`.
- At `0.131.0`, `--ignore-user-config` suppresses only
  `$CODEX_HOME/config.toml` (authentication still uses `CODEX_HOME`); it does
  not suppress Cabal's managed project `.codex/config.toml`. The fixed task
  `-c web_search=...` override therefore isolates user defaults without
  disabling project-scoped MCP configuration.
- Native schema remains a task-scoped file passed through `--output-schema`.
- Diagnostic argv replaces attachment, schema, session, and model values with
  placeholders. Prompt/instructions remain on stdin and never enter argv.
- Normalized output accepts only protocol-proven public records:
  completed agent messages, thread ids, fixed-name tool lifecycle records, and
  completed-turn usage (including `cached_input_tokens`). There is no
  malformed/raw/reasoning/error fallback.
- Caller-supplied resume IDs and parsed `thread.started.thread_id` values must be
  canonical lowercase UUID strings (`8-4-4-4-12` hexadecimal form). Blank,
  option-like, control-containing, malformed, uppercase, and overlong values are
  rejected; caller rejection occurs before config I/O or process spawn.
- Token input/output/cache fields are accepted only as non-negative JSON
  integers. Zero is preserved, invalid sibling fields are ignored independently,
  and multi-turn totals saturate at `max_int` instead of wrapping.

## Sealed attachment handoff

`Task_preflight.prepare_inputs` creates one private task-scoped staging directory
outside the opened workspace. Each `0o600` PNG/JPEG file receives exactly the
bytes used by the same opened-descriptor validation stream; no post-validation
source reopen occurs. Runtime dispatch installs the opaque set and the resolved
entry's media/web authorization in the task execution context before version or
availability work. Codex fails closed for sensitive low-level calls without that
authorization and no longer reads `Backend_registry` during execution.

Fresh schema retries reuse the same staged paths. Resume reuse retains the public
attachment references/digests but emits no `-i`. Prepared dispatch values are
one-shot across both execute APIs, including attachment-free tasks. An atomic
pending/executing/released owner separates switch abandonment from active
execution cleanup. Cleanup runs after all attempts and backend process cleanup,
including timeout, cancellation, fatal exception, staging failure, cleanup
retry, and abandoned-prepared-value paths. Cleanup retries are bounded to three;
detailed execution exposes only `Cleanup_not_required`, `Cleanup_succeeded`, or
`Cleanup_failed`, and persistent failure emits a fixed warning without paths or
exception data. Non-success results, structured schema failures, and propagating
fatal identity remain authoritative.

The staging boundary defends against caller workspace path replacement and uses
private permissions to prevent accidental/cross-user reads. It does not defend
against a hostile same-UID process, privileged/system-level access, or a
compromised OS. Unsupported descriptor-backed staging platforms/workspace
layouts fail closed. Deterministic tests replace/symlink and delete caller paths
after `Preflight_completed`; the fake Codex process still observes only the
originally validated staged bytes, and successful cleanup removes the staged
paths before dispatch returns.

## Capability outcome and shared gate

| Capability | Transport proof | Advertised by the Codex descriptor in this branch |
|---|---:|---:|
| Native JSON schema | yes | yes (unchanged, independent evidence) |
| Sealed PNG media | yes | yes |
| Sealed JPEG media | yes | yes |
| Resume media upload | yes | yes |
| Session media reuse (no duplicate `-i`) | deterministic adapter test | yes |
| Web disabled override | source + deterministic argv | yes |
| Cached search | source + deterministic argv | yes |
| Live search/fetch | yes | yes |

The descriptor advertises exactly PNG and JPEG media plus the maximum
`Web_search_and_fetch` policy. The hierarchical web gate consequently permits
disabled, cached-search, and live search/fetch requests. Arbitrary prompt file
reading remains disabled because attachment transport does not establish that
separate capability.

`Runtime_bootstrap.approved_runtime_capabilities` remains an independent trusted
snapshot introduced by CBL-03. Its Codex media/web claims and versioned evidence
were updated atomically with `Backend_registry`; exact-equality validation still
rejects any future catalog/runtime drift. The enforced `0.131.0` baseline is the
lowest version covered by the authenticated media, resume, web, schema, and
strict-public-JSONL proof recorded here. Unsupported media kinds and malformed
inputs still fail before config I/O, version probing, or backend process spawn.

## Deterministic tests

`test/test_codex_cli.ml` pins:

- zero/one/multiple sealed absolute image argv;
- initial and resume upload versus session reuse;
- exact initial/resume image plus schema option scoping and ordering;
- canonical UUID-only session parsing and fail-before-config/spawn caller checks;
- disabled/cached/live web settings;
- redacted argv;
- fail-before-spawn behavior for malformed, unsupported, or unauthorized inputs;
- non-negative/zero token parsing and saturating aggregation;
- positive public JSONL agent/session/tool/usage records; and
- negative privacy fixtures for reasoning, errors, malformed input, raw
  fallback, tool arguments/results, and unsafe identifiers.

`test/test_backend_registry.ml` iterates the full registry for media/web
claim/evidence agreement and descriptor evidence validity, and pins the exact
Codex claims. `test/test_runtime_bootstrap.ml` verifies that the independent
snapshot matches and that drift fails closed. `test/test_task_preflight.ml` and
`test/test_runtime_dispatch.ml` cover positive PNG/JPEG/web acceptance, exact
adapter delivery intent, post-preflight namespace replacement/deletion, staged
byte identity, retry reuse, one-shot concurrent/sequential claims, abandonment
versus active cleanup ownership, bounded transient/persistent cleanup, sanitized
cleanup telemetry, timeout/cancellation/fatal preservation, capability-before-
staging ordering, the `0.131.0` version gate, and rejected-input no-side-effect
behavior. The probe's offline self-test executes strict validators for every
media, resume, cached-search, and live search/fetch mode.
