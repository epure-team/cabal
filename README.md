# Cabal

Cabal is the **Caml Agent Backend Abstraction Library**.

It provides backend-agnostic agent execution primitives, project configuration
generation, backend registry metadata, process invocation helpers, adapter
loading, and session log utilities for OCaml projects that need to drive
agentic command-line backends such as Claude Code, Codex, Gemini CLI, Copilot
CLI, and OpenCode.

## Purpose and boundaries

Cabal owns the host-neutral backend layer:

- backend descriptors, capability metadata, and registry lookup;
- backend process execution and version probing;
- project config generation for supported backend CLIs;
- YAML adapter loading;
- normalized backend result/cost/session event types;
- local session-event logging and redaction helpers.

Cabal explicitly does **not** own:

- Épure's SQLite database or any host application's persistent state;
- story, epic, build, review, or product orchestration;
- prompt context policy or knowledge injection rules;
- user/auth/project membership models;
- architecture indexing or quality gates.

Host applications decide what to ask an agent to do, what state to persist, and
which policies to enforce. Cabal only provides the backend abstraction needed to
run the selected CLI safely and consistently.

## Architecture

```text
                host application
       (Épure, tests, or another OCaml app)
                         |
                         v
        +----------------------------------+
        |        Cabal public APIs         |
        | Registry | Completer | Process   |
        | Config   | Session log | Types   |
        +----------------------------------+
             |          |           |
             v          v           v
      +----------+  +---------+  +----------------+
      | adapters |  | config  |  | process runner |
      |  YAML    |  | writer  |  | + redaction    |
      +----------+  +---------+  +----------------+
             \          |           /
              \         v          /
               +------------------+
               | backend CLI tools |
               | claude/codex/... |
               +------------------+
```

The host app calls Cabal's public APIs. Cabal resolves backend metadata,
prepares backend-owned or Cabal-owned configuration, runs the CLI process, and
normalizes results and session log events. Backend CLIs remain external tools;
Cabal does not embed their product logic.

## Layout

- `src/backend_types.*` — shared result, usage, cost, and streaming event types.
- `src/agentic_backend.*` and `src/registry.*` — backend module signature and
  runtime backend registry.
- `src/backend_registry.*` — static backend descriptors and capability metadata.
- `src/backend_completer.*` — construction helpers for task completers and
  validator-safe backend routing.
- `src/backend_process.*` and `src/backend_version.*` — process execution and
  version probing.
- `src/backend_config_gen.*` and `src/backend_config_writer.*` — project config
  ownership, generation, and write safety.
- `src/adapter_loader.*`, `src/yaml_adapter.*`, and `src/adapters/*.yaml` —
  data-driven backend adapter definitions.
- `src/*_cli.*` and `src/claude_code.*` — backend-specific CLI integrations.
- `src/session_event_log.*`, `src/backend_event_redaction.*`,
  `src/session_trimmer.*`, and `src/resource_guardian.*` — observability and
  safety utilities.
- `test/` — Cabal's standalone test suite.

## Build and test

From the standalone Cabal repository:

```bash
opam install . --deps-only --with-test -y
opam exec -- dune build @install
opam exec -- dune runtest
opam lint cabal.opam
```

From the Épure monorepo:

```bash
EPURE_NO_COMMIT_CHECK=1 dune build
EPURE_NO_COMMIT_CHECK=1 dune runtest libs/cabal
opam lint libs/cabal/cabal.opam
```

Épure builds from the monorepo do not require any `opam pin` for Cabal.
Dune sees `libs/cabal` directly as part of the workspace.

For downstream/standalone consumers while Cabal is not yet published, pin it
explicitly from a local checkout:

```bash
opam pin add cabal <path-to-cabal> -y
```

The library remains host-agnostic: callers choose the managed namespace used for
generated files. The default namespace currently preserves historical Epure
runtime artifact ownership and paths for compatibility.

## Sync model

The current source of truth is `libs/cabal` in the Épure monorepo. Épure CI
subtree-splits that directory and direct-pushes the result to
`epure-team/cabal:main`, where the standalone Cabal CI runs against the mirror.

Normal contribution flow for now:

1. change Cabal under `libs/cabal` in Épure;
2. merge through Épure's normal review path;
3. let the mirror-sync workflow update `epure-team/cabal` automatically.

Escape hatch: if a direct PR is accepted in `epure-team/cabal`, backport the
same change to `libs/cabal` in Épure. If the team later decides the standalone
repository should become primary, this flow can be reversed by merging in Cabal
first and backporting to Épure, but that is not the current normal model.
