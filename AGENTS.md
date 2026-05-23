# AGENTS.md for Cabal

Cabal is the Caml Agent Backend Abstraction Library. Keep it usable as a
standalone OCaml library and as the backend abstraction layer vendored under
`epure/libs/cabal`.

## Cabal-specific rules

- Preserve backend and host agnosticism. Cabal may know about backend CLIs and
  their config/process/session-log contracts, but not about any one host
  application's product workflow.
- Do not add dependencies from Cabal source to `epure_lib`, `epure_db`,
  `epure_agents`, `arch_index`, or other Épure monorepo libraries.
- Keep DB state, story orchestration, prompt context policy, and Épure state
  outside Cabal. Host applications own those layers.
- Tests for Cabal live under `libs/cabal/test` in the Épure monorepo, and under
  `test` in the standalone mirror.
- Public APIs must be documented in `.mli` files. Keep private helpers in
  implementation files unless they are intentionally part of the library API.
- Maintain `dune-project`, `cabal.opam`, and CI workflow metadata when changing
  dependencies, supported OCaml versions, or test/build requirements.
- Keep backend adapters data-driven where possible. Prefer extending
  `src/adapters/*.yaml` and shared loader/registry code over duplicating
  per-backend orchestration.
- Cabal mirror sync is dispatch-based: Épure dispatches the standalone Cabal
  workflow on every push to `main`, and Cabal's GitHub Actions run performs the
  protected `main` update from a `libs/cabal` subtree split using the installed
  custom Cabal mirror GitHub App token with Contents and Workflows write
  permissions. Épure only dispatches; do not reintroduce deploy keys or direct
  branch-write pushes from Épure.
- Cabal main mirror updates should not expose manual `workflow_dispatch` ref
  inputs; protected `main` updates must remain repository-dispatch-driven from
  Épure `main` only.
- Direct PRs in `epure-team/cabal` are mirrored automatically into Épure PRs by
  the Cabal-side `pull_request_target` workflow only when the PR is trusted and
  same-repository: `head.repo.full_name == github.repository` and the author
  association is `OWNER`, `MEMBER`, or `COLLABORATOR`, or the initial validation
  step confirms the author has `write`, `maintain`, or `admin` repository
  permission with the read-only Cabal `GITHUB_TOKEN`. This fallback handles
  private org membership that can report `author_association=NONE`. Fork PRs,
  cross-repository PRs, lower permissions, lookup failures, and other untrusted
  PRs must exit successfully before creating an Épure app token, checking out PR
  contents, or writing to Épure; maintainers must move/adopt those changes onto a
  trusted Cabal branch for automatic mirroring. This avoids turning untrusted
  fork contents into same-repository Épure CI runs.
- The Cabal PR sync workflow may copy trusted PR files as data, but must never
  run scripts/tests from the PR branch; Épure PR CI and review are the
  authoritative gate. If a trusted PR produces no `libs/cabal` diff relative to
  Épure `main`, close any stale mirror PR and delete its `cabal-pr-<number>`
  branch instead of leaving a no-op Épure PR open.
- If editing directly in `epure-team/cabal`, do not merge independently except
  for emergency fixes that cannot wait for the Épure PR path. Normal source of
  truth remains `epure/libs/cabal`; direct Cabal PR changes should land through
  the mirrored Épure PR and then flow back to Cabal `main` via mirror sync.

## Capability evidence convention (Story #622 / #628)

- `capabilities.native_json_schema_output = true` MUST be accompanied by
  `native_json_schema_output_evidence = Some _`; setting it to `true` with
  `None` evidence is a CI-enforced contract violation.  The structural
  invariant is enforced by
  `libs/cabal/test/test_demo_622.ml:test_native_json_schema_evidence_required_when_true`,
  which iterates `Backend_registry.all ()` — not a hardcoded list — so every
  backend added in the future is automatically checked.
- All initially shipped backends set `native_json_schema_output = false` with
  `native_json_schema_output_evidence = None`.
- The `capability_evidence` type is defined in `backend_types.mli` (not
  `backend_registry.mli`) to avoid import cycles and to keep it available
  wherever types are referenced.
- When writing new test helpers that directly construct a `Backend_registry.capabilities`
  record literal (instead of using a built-in descriptor), add
  `native_json_schema_output = false; native_json_schema_output_evidence = None`
  to avoid record-field exhaustiveness errors as the type evolves.
