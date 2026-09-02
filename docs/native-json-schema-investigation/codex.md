# Codex native schema, media, and web investigation

## Metadata

| Field | Value |
|---|---|
| Backend | `codex` |
| Cabal baseline | `0.122.0` |
| Installed/tested version | `codex-cli 0.131.0` |
| Probe date | 2026-09-02 |
| Sources accessed | 2026-09-02 |
| Stories | #630 native schema; #24 / CBL-07A media and web |

## Upstream contracts inspected

- Codex `0.122.0` `exec` CLI source (commit `230dcade`):
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

The baseline source contains all relevant surfaces: initial `-i/--image`,
resume `-i/--image`, `--output-schema <FILE>`, `--json`, and the three
`web_search` modes. The installed help and tagged `0.131.0` source confirm the
same contracts.

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

The probes used public OS images and prompts containing no secrets. Raw JSONL
and stderr were captured in memory and filtered; only public agent/session/tool/
usage records were reported. Reasoning, error payloads, raw tool arguments, and
credentials were neither displayed nor stored.

### Initial workspace-relative image plus native schema

Sanitized exact argv (the process current directory contained the image):

```text
codex exec --json --skip-git-repo-check --ignore-user-config \
  -s read-only -c 'web_search="disabled"' \
  -i 'image with spaces.png' \
  --output-schema '<task-schema-file>' -
```

Outcome at `0.131.0`: exit 0; `thread.started`, `turn.started`,
`item.completed(agent_message)`, and `turn.completed(usage)` were emitted. The
schema-conforming public result was `{"media_seen":true}`. This proves that a
workspace-relative PNG path containing spaces composes with native schema and
strict JSONL.

### Multiple PNG/JPEG images plus native schema

Sanitized exact argv:

```text
codex exec --json --skip-git-repo-check --ignore-user-config \
  -s read-only -c 'web_search="disabled"' \
  -i '<workspace-image-1.png>' -i '<workspace-image-2.jpg>' \
  --output-schema '<task-schema-file>' -
```

Outcome: exit 0 with the same public protocol record classes and the
schema-conforming result `{"image_count":2}`. PNG, JPEG, repeated flags, and
image/schema composition were all exercised in one invocation.

### Resume image upload plus native schema

After a probe-owned initial session, the corrected sanitized argv was:

```text
codex exec --output-schema '<task-schema-file>' -s read-only \
  resume '<session-id>' --json --skip-git-repo-check \
  --ignore-user-config -c 'web_search="disabled"' \
  -i '<workspace-image-1>' -
```

Outcome: exit 0; a public session id, agent message, and usage were emitted; the
schema-conforming result was `{"media_seen":true}`. Cabal therefore has a
proven resume-upload syntax. For `Reuse_session_attachments`, Cabal deliberately
emits no `-i` flags, so a schema retry does not upload the already-fed media a
second time.

### Live web search and page fetch

Sanitized exact argv:

```text
codex exec --json --skip-git-repo-check --ignore-user-config \
  -s read-only -c 'web_search="live"' -
```

The public prompt required live search and opening an official OpenAI Codex
documentation page. Outcome: exit 0; paired
`item.started(web_search)` / `item.completed(web_search)` records were emitted,
followed by the public page-title answer and `turn.completed(usage)`. This
proves the `live` search-and-fetch setting. The tagged schema/source separately
pins `cached` as search without external live access and `disabled` as no web
tool; CLI `-c` overrides are the highest-precedence ordinary config layer, and
the upstream test pins explicit `web_search` precedence over legacy web flags.

## Adapter design and privacy

- Actual argv is a string list passed directly to the process launcher; no
  shell interpolation or host-provided config fragment is accepted.
- Attachments remain validated workspace-relative paths. Codex receives paths,
  not bytes or base64; the adapter does not reread media.
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

## Capability outcome and shared gate

| Capability | Transport proof | Advertised by the Codex descriptor in this branch |
|---|---:|---:|
| Native JSON schema | yes | yes (unchanged, independent evidence) |
| PNG path media | yes | no |
| JPEG path media | yes | no |
| Resume media upload | yes | no |
| Session media reuse (no duplicate `-i`) | deterministic adapter test | no |
| Web disabled override | source + deterministic argv | yes as the existing default |
| Cached search | source + deterministic argv | no |
| Live search/fetch | yes | no |

The media/web transport code intentionally remains fail-closed at runtime.
`Runtime_bootstrap.approved_runtime_capabilities` is an independent trusted
snapshot introduced by CBL-03 and still pins Codex to `no_media` / `no_web`.
Changing only `Backend_registry` makes hardened bootstrap reject Codex with
`effective descriptor capabilities differ from the trusted runtime snapshot`.
`src/runtime_bootstrap.ml` is outside CBL-07A's exclusive write scope, so this
branch does not bypass or weaken that gate.

Enabling the proven claims requires one coordinated shared-file change: update
the Codex entry in `Runtime_bootstrap.approved_runtime_capabilities` and the
Codex descriptor together with identical versioned `feature_evidence`, then run
the runtime/bootstrap/preflight suites. Until that integration is assigned,
unsupported media/web requests fail before config I/O or process spawn.

## Deterministic tests

`test/test_codex_cli.ml` pins:

- zero/one/multiple image argv, including spaces;
- initial and resume upload versus session reuse;
- image plus schema ordering;
- disabled/cached/live web settings;
- redacted argv;
- fail-before-spawn behavior while the capability gate remains closed;
- positive public JSONL agent/session/tool/usage records; and
- negative privacy fixtures for reasoning, errors, malformed input, raw
  fallback, tool arguments/results, and unsafe identifiers.

`test/test_backend_registry.ml` iterates the full registry for media/web
claim/evidence agreement and descriptor evidence validity; it also pins Codex
as unadvertised until the independent runtime trust snapshot is updated.
