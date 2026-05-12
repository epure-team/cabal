# Cabal

Cabal is the **Caml Agent Backend Abstraction Library**.

It provides backend-agnostic agent execution primitives, project configuration
generation, backend registry metadata, and session log utilities for OCaml
projects that need to drive agentic command-line backends such as Claude Code,
Codex, Gemini CLI, Copilot CLI, and OpenCode.

This repository treats `libs/cabal` as the monorepo-primary source. It is
intended to be mirrored or subtree-split to `epure-team/cabal` for standalone
distribution as the `cabal` opam package and public OCaml library.

When building Épure from this monorepo before that external package is
published, pin Cabal explicitly from the nested source:

```bash
opam pin add cabal ./libs/cabal -y
```

The library remains host-agnostic: callers choose the managed namespace used for
generated files. The default namespace currently preserves historical Epure
runtime artifact ownership and paths for compatibility.
