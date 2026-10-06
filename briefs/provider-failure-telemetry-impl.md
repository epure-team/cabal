# Provider failure telemetry implementation

Status: COMPLETED; independent review and QA/ship gates pending.

Scope approved by the parent coordinator: preserve existing nullable cost,
session and elapsed metadata on failure, retain stdout before interruption, and
expose terminal Claude JSONL parsing without changing result/backend records.
See `docs/provider-telemetry.md`, the public interfaces and the tests.

Baseline `dune build` and full `dune runtest` passed before edits. Test-first:
nonzero, signal and timeout metadata fixtures failed against the baseline;
terminal JSONL usage failed against the baseline. All five new tests and the
full suite pass after implementation. `git diff --check` passes. No configured
format/lint gate was discovered. Package regeneration's unrelated redundant
Dune constraint was excluded.

Integration verification used the same CWR opam switch for both builds and a
private local install prefix, avoiding global package mutation. One authorized
real read-only Claude haiku invocation captured success metadata. Failure tests
use local synthetic processes; no real credentials/quota were deliberately
failed. No transcript content is included in these artifacts.

Review focus: incremental buffering on interruption, parser exceptions must not
change status, cumulative terminal results must not be summed, API remains
additive. Message-only partial usage, full cancellation capture, monotonic
elapsed and Codex usage semantics are deferred, not silently claimed.

Process deviations: parent research/contract approval was reused rather than
replaying intake/spec; specialist delegation was unavailable because all slots
were occupied. Review is explicitly pending, not self-certified. No merge.

## Independent architecture correction

Root's actual architecture pass found that incremental non-display drains lost
the prior Eio take_all aggregate 128 MiB limit. Restored that limit before each
append, raising the same Eio.Buf_read.Buffer_limit_exceeded at the limit itself.
The private constant is reused for the Eio buffer; no public API was added.
Documented the inherited capture contract in backend_process.mli.

Regression uses a local head /dev/zero process emitting exactly 128 MiB: RED
exit1 without the guard, GREEN exit0 with the guard (six provider_usage cases,
0.328 s). Existing nonzero/signal/timeout metadata cases still pass. Full build
and suites re-run; private Cabal install and CWR integration rebuild/full tests
also pass. No provider call, credential mutation or infrastructure added.
