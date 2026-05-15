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
- Cabal sync workflows should not expose manual `workflow_dispatch` ref inputs;
  mirror updates must remain repository-dispatch-driven from Épure `main` only.
- If editing directly in `epure-team/cabal`, ensure accepted changes are
  backported to `epure/libs/cabal`; the normal source of truth is currently the
  Épure monorepo subtree.
