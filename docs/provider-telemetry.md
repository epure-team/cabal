# Failure-path provider metadata

Process failures no longer erase usage reported before exit or timeout. The
existing `task_result` and status/exit semantics remain unchanged; `cost` and
`session_id` stay nullable. `Backend_process.run_task_with` applies its optional
cost parser on every returned terminal status. Parser exceptions become missing
metadata rather than replacing the execution outcome. The non-display stdout
reader retains exact bytes incrementally, including complete reports observed
before a timeout.

`Claude_code.parse_cost_from_stdout` accepts a single terminal JSON object or
JSONL, selecting the last complete `result` event. Cumulative terminal reports
are not summed. Message-only partial usage remains unknown. Observed zero and
cache fields retain the existing parser semantics. This parser and
`Codex_cli.parse_session_id_from_stdout` are additive pure public helpers for
CLI-only consumers; no required record fields or backend protocol change.

Metadata is not proof of complete billing or request/sub-agent accounting.
Missing output, interrupted partial JSON and unsupported provider shapes remain
unknown. Host cancellation without a returned result and abrupt process death
cannot guarantee final metadata. Existing elapsed measurements use a wall
clock, not a monotonic clock. Codex's existing usage aggregation semantics are
unchanged by this patch.

Verification: `dune runtest` passed before and after the change. New
`test/test_provider_usage.ml` fixtures emit a complete usage/session envelope
then exit nonzero, receive SIGTERM or exceed the timeout; available zero/input,
output, cache, session and elapsed survive. Terminal JSONL duplication does not
double-count, and message-only partial usage is not promoted to complete usage.
