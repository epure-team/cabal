# Draft PR ship gate — provider-failure-telemetry

Cabal preserves reported usage, session and elapsed metadata on failed/signalled/timed-out executions and parses cumulative terminal Claude JSONL without double counting. The architecture correction in f7e0992c preserves the existing 128 MiB aggregate non-display stdout limit.

Review and QA are GO in the task's recorded Full mode. Actual normalizer, invocation trace and convergence reports are retained. Build and forced full tests pass (826 cases across 50 suites; focused provider usage 6/6); whitespace checks pass. Different-runtime attempts were degraded and discarded, with actual QA breaker checks. Full scope manifests and local hooks are absent and explicitly recorded; no task spec/KB or production request/child coverage is claimed.

Branch: `fix/provider-failure-telemetry`. Contribution repository: `epure-team/cabal`, base `main`. GitHub base fetched and verified as ancestor; no existing PR for this branch was found. Original GitLab origin is preserved; push uses the explicit canonical GitHub URL.

The user authorized upstream draft PR creation. The requested stopping point is an open draft; merge and branch deletion are out of scope. Cabal's trusted same-repository PR must follow its automatic Épure mirror and may not merge independently. CWR adoption requires the Cabal helper/correction contribution.

Only owned source, tests and generic review/QA/ship artifacts are staged. Incidental Cabal package regeneration, private staged install and locally installed review tooling are excluded.
