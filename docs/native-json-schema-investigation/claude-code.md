# Claude Code schema, media, and web investigation

## Metadata

| Field | Value |
|---|---|
| Backend | `claude-code` |
| Cabal baseline | `2.1.117` |
| Investigation date | 2026-09-03 |
| Story | #30 / CBL-07B |
| Evidence level in this phase | Exact release artifacts, help, SDK source/types, offline probe self-test, and unit tests only |

No authenticated media or web probe completed in this phase. Media and positive
web capabilities therefore remain unsupported. The existing native JSON Schema
capability and its independent Story #625/#628 evidence are unchanged.

## Exact baseline artifacts

The inspected release is
[`v2.1.117`](https://github.com/anthropics/claude-code/releases/tag/v2.1.117),
published on 2026-04-22.

The Linux x64 package was checked against npm metadata before inspection:

```text
package:   @anthropic-ai/claude-code-linux-x64@2.1.117
tarball:   https://registry.npmjs.org/@anthropic-ai/claude-code-linux-x64/-/claude-code-linux-x64-2.1.117.tgz
integrity: sha512-bhN6qnc9xchKQqKWdwuZazEeSO+9NIhOPcoD/WgqTK5QRPSAwnvo5SZWIQUbkNbTKLaMwuxAu3u+Fj/jYbiidg==
```

The corresponding public TypeScript Agent SDK package provides the static wire
contract used below:

```text
package:            @anthropic-ai/claude-agent-sdk@0.2.117
claudeCodeVersion:  2.1.117
tarball:            https://registry.npmjs.org/@anthropic-ai/claude-agent-sdk/-/claude-agent-sdk-0.2.117.tgz
integrity:          sha512-pVBss1Vu0w87nKCBhWtjMggSgCh6GVUtdRmuE58ZvXv0E2q0JcnUCQHehmn92BAW0+VCwPY8q/k7uKWkgwz/gA==
```

Its versioned
[`CHANGELOG.md`](https://github.com/anthropics/claude-agent-sdk-typescript/blob/v0.2.117/CHANGELOG.md)
states that SDK `0.2.117` is at parity with Claude Code `2.1.117`. This is
static upstream evidence, not an authenticated behavior claim.

## Baseline CLI and SDK contracts

The exact `2.1.117` binary help exposes:

- `--print` for non-interactive execution;
- `--input-format text|stream-json`;
- `--output-format text|json|stream-json`;
- `--json-schema <schema-json>`;
- `--resume <session-id>`;
- `--tools <tools...>` and `--allowedTools` / `--disallowedTools`;
- `--setting-sources <sources>`; and
- `--mcp-config` plus `--strict-mcp-config`.

The exact SDK package confirms the composition used for text-only Cabal calls:

1. It launches the CLI with `--output-format stream-json --verbose
   --input-format stream-json`.
2. A string prompt becomes one `SDKUserMessage`:

   ```json
   {
     "type": "user",
     "session_id": "",
     "message": {
       "role": "user",
       "content": [{"type": "text", "text": "..."}]
     },
     "parent_tool_use_id": null
   }
   ```

3. The transport serializes each input message as one JSON value followed by a
   newline.
4. It maps structured output to `--json-schema`, resume to `--resume`, a tool
   array to `--tools`, settings sources to `--setting-sources`, and strict MCP
   isolation to `--strict-mcp-config`.
5. Its public declarations define assistant, `system/init`, successful result,
   session UUID, usage, and structured-output records consumed by Cabal's strict
   parser.

This is enough to retain the text/schema stream transport and deterministic
parsing changes without an authenticated request. It is not evidence that an
image block reaches the model or that web tools satisfy Cabal's requested
semantics.

## Probe artifact and authentication blocker

`tools/probe_claude_media_web.py` is the sole reproduction artifact. It:

- requires exactly `2.1.117 (Claude Code)`;
- creates deterministic 64×64 PNG/JPEG fixtures with non-descriptive names;
- constructs the public stream-JSON base64 image candidate;
- covers initial upload, resume upload, resume reuse without duplicate bytes,
  web-disabled isolation, WebSearch-only, and WebSearch+WebFetch;
- validates content-dependent structured answers and public tool/session
  records; and
- suppresses raw stdout, stderr, paths, prompts, image bytes, credentials, tool
  arguments/results, and tracebacks from diagnostics.

The offline validator and sanitization suite passes:

```text
./tools/probe_claude_media_web.py --self-test
PASS self-test
```

Authenticated execution was not available during this phase. No local
credential, configuration-directory, or raw failed-process detail is recorded as
evidence.

For a later manual rerun, authenticate outside this task with:

```text
CLAUDE_CONFIG_DIR="<dedicated-claude-config-dir>" claude auth login --claudeai
```

Then expose the verified `2.1.117` binary on `PATH` and run the checked-in probe.
Do not treat a run against another version as baseline evidence.

## Media result

The static SDK types establish that a user message uses Anthropic `MessageParam`
content and the probe pins the candidate base64 image block. They do not prove
that Claude Code `2.1.117` accepts, forwards, remembers, or schema-composes those
blocks in an authenticated session.

Consequently:

- no OCaml base64 encoder or dependency was added;
- initial and resumed uploads remain rejected before config I/O or process
  spawn;
- session attachment reuse also remains rejected; and
- `Backend_registry` continues to advertise no Claude media types and no media
  evidence.

Central preflight already validates each attachment's declared size, digest,
MIME magic, and exact staged bytes. `Task_execution_context` authorizes an exact
attachment-reference list and exposes sealed paths only after identity/policy
matching. Any future encoder must consume `sealed_attachment_delivery`, preserve
the reference/path ordering, use the matched preflight-validated `size_bytes` as
an allocation bound, and emit no bytes for `Reuse_session_attachments`. That
future work remains unnecessary until the authenticated transport probe passes.

## Web result

Static baseline help and SDK source establish that `--tools` fixes the available
built-in tool set and that an empty tool array becomes `--tools ""`. Cabal now
uses a fixed tool list that excludes `WebSearch` and `WebFetch` for
`Web_disabled`; `--strict-mcp-config` prevents unrelated MCP discovery. User
permission settings cannot add a built-in omitted from the fixed `--tools` set.
This controls Claude's native web tools, not total process egress: separately
authorized Bash commands or explicitly supplied MCP servers can have their own
network access. A host that requires a complete network boundary must provide
one outside the backend process.

Positive web behavior was not authenticated. Cabal therefore rejects
`Web_search` and `Web_search_and_fetch` before config I/O or process spawn, and
the descriptor remains at `Web_disabled` with no web evidence.

## Strict public parsing and diagnostics

The normalized stream parser accepts only exact public structures:

- assistant text and bounded tool identity, never tool arguments;
- mutually consistent canonical lowercase `8-4-4-4-12` session UUIDs from exact
  `system/init` or successful result records; and
- independently valid non-negative usage fields from successful results.

Exit zero becomes task success only when the stream has exactly one terminal
record and it has `type=result`, `subtype=success`, and `is_error=false` with a
documented text or structured result. Missing, subtypeless, malformed, error,
non-terminal, or duplicate result records produce one fixed sanitized failure.
Assistant-only output cannot satisfy native schema execution.

User/input/image records, thinking, tool results, error records, malformed JSON,
unsafe identifiers, paths, and raw fallback text do not become normalized
events or display output. Invocation diagnostics use a separately constructed
redacted argv and a fixed stdin marker; schema, model, session, MCP/settings
paths, prompts, and instructions are omitted.

## Capability outcome

| Capability | Evidence obtained in this phase | Advertised after CBL-07B |
|---|---:|---:|
| Native JSON schema | Exact flag/SDK composition; prior independent E2E evidence unchanged | yes |
| Text stream-JSON input/output | Exact baseline help and parity-matched SDK source/types | yes |
| PNG/JPEG media | Candidate format and offline validators only | no |
| Resume media upload | Offline probe path only | no |
| Session media reuse | Offline no-duplicate-bytes assertion only | no |
| Web-disabled isolation | Exact help/SDK tool semantics and deterministic argv tests | `Web_disabled` only |
| WebSearch | Offline probe path only | no |
| WebSearch + WebFetch | Offline probe path only | no |

No media/web capability metadata changed.

## Deterministic tests

`test/test_claude_code.ml` pins:

- exact text/schema stream argv and stdin shape;
- native schema composition and top-level `$schema` stripping;
- exact read-only and builder tool sets excluding web tools;
- canonical resume validation and the parity SDK's exact flag/value shape;
- redacted diagnostics;
- fail-closed media and positive-web behavior before config or spawn;
- fake-CLI active text, native schema, resume, exact stdin, public callbacks,
  session/cost extraction, MCP cleanup, invalid exit-zero output, timeout, and
  cancellation;
- public assistant/tool/session/usage parsing without duplicate final text; and
- negative privacy fixtures plus the probe's offline self-test.

Re-evaluate media/web only after all content-dependent modes pass against an
integrity-checked exact baseline with an isolated authenticated test profile.
