# Draft PR ship gate — provider-failure-telemetry

Cabal preserves reported usage, session and elapsed metadata on failed/signalled/timed-out executions and parses cumulative terminal Claude JSONL without double counting. The architecture correction in f7e0992c preserves the existing 128 MiB aggregate non-display stdout limit.

Review and QA are GO in the task's recorded Full mode. Actual normalizer, invocation trace and convergence reports are retained. Build and forced full tests pass (826 cases across 50 suites; focused provider usage 6/6); whitespace checks pass. Different-runtime attempts were degraded and discarded, with actual QA breaker checks. Full scope manifests and local hooks are absent and explicitly recorded; no task spec/KB or production request/child coverage is claimed.

Branch: `fix/provider-failure-telemetry`. Contribution repository: `epure-team/cabal`, base `main`. GitHub base fetched and verified as ancestor; no existing PR for this branch was found. Original GitLab origin is preserved; push uses the explicit canonical GitHub URL.

The user authorized upstream draft PR creation. The requested stopping point is an open draft; merge and branch deletion are out of scope. Cabal's trusted same-repository PR must follow its automatic Épure mirror and may not merge independently. CWR adoption requires the Cabal helper/correction contribution.

Only owned source, tests and generic review/QA/ship artifacts are staged. Incidental Cabal package regeneration, private staged install and locally installed review tooling are excluded.

## Draft handoff

Draft PR opened and independently read back: https://github.com/epure-team/cabal/pull/38 (OPEN, draft, base main). Reviewed source correction: f7e0992c; initial review artifact head: 333aa83c. CI and automatic Épure mirror were queued at handoff; they are not claimed passing. No merge was attempted.

Actual pre-push commands: canonical Roster check-review-convergence.js with --static --max-rounds 5 --strikes 2 --timeout 120; check-qa-convergence.js with --max-rounds 5; git diff --check. All exited zero. Both build and dune runtest --force had already passed against the scoped source. Canonical main remained 6ffa08ca96079120d06e0b5ce68780979138ef74; ancestry and absence of duplicate branch PRs were checked before push.

Explicit GitHub-URL git push -u succeeded; gh pr create --draft succeeded; gh pr view verified draft state and expected head. Shipping stops at draft under the user's authorization. Metabolism increment skipped: no harness.json. Advisory cost snapshot skipped: ledger dates lack ISO time bounds; no unbounded account-history query made.
