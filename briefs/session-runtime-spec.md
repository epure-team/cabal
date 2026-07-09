---
slug: session-runtime
date: 2026-07-09
type: feature
status: VALIDATED
mode: full
---

# Client-neutral session runtime for cabal

## Problem / intent

Cabal drives agentic CLIs one-shot (`run_task`: spawn → wait → parse). Two capabilities are missing:

1. **Live sessions** — keep an interactive CLI alive (in its real TUI, where agents behave
   differently than in `--print`) and drive it coarsely, human-attachable.
2. **Client-neutral portability** — own the conversation independently of any one client, so a
   session can be *continued in a different client* and *composed from several sources* by
   injecting a curated composition of past inputs/outputs (filtered, deduped, reordered,
   compacted, …). The goal is continuation via curated injection, **not** faithful replay.

Working-directory state (git) is already client-neutral and shared, so portability only needs to
carry the **conversational thread**, not perfect tool-call fidelity — which makes it tractable.

## Design

**Canonical event model** (`Portable_session`): a client-neutral event
`{ role; text; tool; model; provenance; timestamp; tokens }`. Each backend provides:
- `ingest` : native transcript → portable events (this PR: Claude Code JSONL);
- `render` : portable events → native transcript (this PR: Claude Code JSONL, **conversation-only**,
  no synthetic tool blocks) that the client's own `--resume` loads.

**Composition algebra** (`Session_composition`): pure transforms `event list -> event list`
(filter, dedup by content hash, reorder chronological, merge multi-source, take/drop) run as a
declarative pipeline. Model-driven stages (compact/amplify) are represented as a stage that takes
an injected `summarize` callback — the algebra stays pure; the model call is the caller's.

**Client switch** = `compose(ingest A …) |> render(format B) |> B --resume`. Only clients that
accept a dictated session id are switch *targets*: research confirmed **claude** and **gemini**
(`--session-id <uuid>`); codex/opencode support fork/resume of assigned ids; copilot is lossy.

**Live session** (`Live_session`): tmux-backed driver — `open / send / list / close / target /
capture / has_session`. tmux keeps the real TUI alive and human-attachable (`tmux attach -t`);
multi-line prompts injected via `set-buffer`+`paste-buffer` (robust vs per-line submit). argv
builders are pure and unit-tested; effectful ops wrap `Backend_process`/Eio.

## Requirements

- **FR-001** A portable event models role (user/assistant/system/tool), flattened text, optional
  tool ref (name/input/output summary), model, provenance (source session + client), timestamp,
  and optional token cost. `[@@deriving show, eq, yojson]`.
- **FR-002** `Session_ingest.claude_code` parses Claude Code JSONL content into ordered portable
  events: user/assistant text extracted from string or content blocks; thinking blocks dropped;
  tool_use / tool_result mapped to tool events; malformed lines skipped, never raise.
- **FR-003** `Session_render.claude_code` renders portable events to a Claude-Code-shaped JSONL
  string with a valid `uuid`/`parentUuid` chain, `sessionId`, `timestamp`, `message{role,content}`,
  conversation-only (no synthetic tool_use blocks).
- **FR-004** Round-trip: `ingest (render evs)` preserves each event's role and text (order-stable).
- **FR-005** `Session_composition` provides `filter`, `dedup`, `reorder`, `merge`, `take`, `drop`
  as pure transforms plus a `run : stage list -> event list -> event list` pipeline; `dedup`
  removes events with identical (role, normalized text); `merge` concatenates multiple sources
  preserving provenance.
- **FR-006** A `Compact`/`Amplify` stage carries an injected `summarize : event list -> event list`
  so composition needs no backend dependency and is fully unit-testable with a stub.
- **FR-007** `Live_session` exposes pure argv builders for tmux new-session/send-keys/paste/
  capture/has-session/kill, and effectful `open_/send/capture/close/has_session/list` over Eio;
  `send` uses set-buffer+paste-buffer+Enter for arbitrary multi-line text; `target` returns the
  tmux session name for `tmux attach`.
- **FR-008** No changes to existing public APIs; new modules are additive. Builds under dune,
  `dune runtest` green, `.mli` for every new module.

## Scope boundary (this PR)

**In:** canonical model, composition algebra, Claude Code ingest/render + round-trip, tmux
`Live_session`, unit tests for each, docs note.

**Out (documented follow-up):** ingest/render for codex/gemini/opencode/copilot; model-driven
compaction wired to a real backend; the stateful `cabal-mcp` HTTP server (needs an HTTP stack +
hand-rolled JSON-RPC — no OCaml MCP lib exists); adapter-YAML generalization of transcript
location / session-id flag; end-to-end switch orchestration with live `--resume`.

## GWT scenarios

- Given a real Claude JSONL transcript, When ingested, Then events preserve conversational order
  and drop thinking blocks. (FR-002)
- Given portable events, When rendered then re-ingested, Then roles and texts match. (FR-004)
- Given events from two sources, When merged + deduped, Then duplicates collapse and provenance is
  retained. (FR-005)
- Given a multi-line prompt, When `Live_session.send` builds its commands, Then the body goes via
  paste-buffer and a single Enter submits. (FR-007)

## Risks

- **`render` format drift**: a synthetic transcript must match the client's expected schema, which
  can change between client versions. Mitigation: cover shape with the round-trip test; treat live
  `--resume` acceptance as follow-up integration, not a unit guarantee.
- **tmux tests need tmux**: keep unit tests on the pure argv builders; guard any live tmux test on
  `has_session`/availability so CI without tmux still passes.
