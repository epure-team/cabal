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

- any host application's persistent state (SQLite or otherwise);
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
        (BountyNexus, Épure, tests, or
         another OCaml app)
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

## Using Cabal from a host application

Cabal is intentionally minimal — host applications wire backends into the
runtime registry, drive `run_task`, and own everything around it (storage,
prompt policy, retry, UI).

```ocaml
open Cabal

let () =
  (* 1. Load YAML adapters, then register handwritten built-ins when the host
        needs their backend-specific runtime capabilities. *)
  Adapter_loader.register_all () ;
  Registry.register (module Claude_code) ;
  Registry.register (module Gemini_cli) ;
  Registry.register (module Codex_cli) ;
  Registry.register (module Opencode_cli) ;
  Registry.register (module Copilot_cli) ;

  (* 2. Pick a backend by canonical id — the same id used in
        Backend_registry and Backend_config_gen. *)
  let backend =
    match Registry.get "claude-code" with
    | Some b -> b
    | None -> failwith "claude-code backend is not registered"
  in

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* 3. Construct a task. make_task_spec validates the managed namespace
        for you; for type-level enforcement, use Backend_types.validate_namespace. *)
  let spec =
    Backend_types.make_task_spec
      ~prompt:"Summarise the README in one sentence."
      ~instructions:""
      ~working_dir:(Sys.getcwd ())
      ~timeout:60.0
      ~expected_outputs:[Backend_types.Files_changed]
      ()
  in

  (* 4. Run the task. Backends never raise — they always return a
        task_result with a status (Success / Failed _ / Timeout). *)
  let result = Agentic_backend.run_task backend ~sw ~env spec in
  Printf.printf "status=%s files_changed=%d\n"
    (Backend_types.show_result_status result.status)
    (List.length result.files_changed)
```

### Redaction contract for hosts logging backend output

Cabal's `Session_event_log` redacts events **before** writing them. Hosts
that capture raw stdout/stderr from a backend process and log it directly
**must** route the bytes through `Backend_event_redaction.redact_event`
(per-event JSON) or `redact_error_message` (free-form strings) first, or
they bypass Cabal's secret-stripping. The session NDJSON file is created
with mode `0o600` to limit blast radius if redaction is bypassed.

### Adapter trust tiers

`Adapter_loader.register_all` loads YAML adapters in three layers, lowest
priority first:

1. Built-in YAMLs compiled into the library (`src/adapters/*.yaml`).
2. User-global: `~/.cabal/adapters/*.yaml`.
3. Project-local: `.cabal/adapters/*.yaml` (only when `?project_dir` is
   passed).

Project-local adapters override user-global, which override built-ins by
id. Hosts that don't want to honour user-supplied adapters should call
`register_all` without `?project_dir` and validate `$HOME` themselves.

`Adapter_loader.register_all` registers YAML-backed adapters only. It does not
by itself install handwritten backend modules such as `Claude_code`, so hosts
that require backend-specific runtime behavior (native JSON schema enforcement,
read-only flags, resume semantics, config validation, MCP handling, or safety
checks) should register the handwritten built-ins afterward to override the same
backend ids.

### Known limitations

1. `Adapter_loader.register_all` alone executes built-ins through the generic YAML-backed adapter path; hosts that need strict backend-specific behavior (native schema output, read-only semantics, resume, config validation, MCP handling, or safety checks) should register handwritten built-ins after the loader and validate which backend path is active before execution.
2. Provider model-discovery probes may pass credentials in subprocess argv or URL arguments. Avoid running probe-enabled providers with live API keys in shared or untrusted environments until this path is hardened.
3. Config artifact paths are treated as trusted project-relative paths. Hosts should sanitize and validate user-supplied paths before passing artifacts into Cabal.
4. Failed-turn error strings can be logged before full redaction is applied. Avoid embedding credentials in backend error text and prefer redacted logging surfaces.
5. Timeout coverage is still incomplete for all subprocess phases, including spawn and stdin-write edge cases.
6. Additional compatibility and safety regression coverage is still being added, including hardening around adapter/credential flows.

Tracking for ongoing work is available in issue #5.

## Build and test

From the standalone Cabal repository:

```bash
opam install . --deps-only --with-test -y
opam exec -- dune build @install
opam exec -- dune runtest
opam lint cabal.opam
```

### E2E tests (manual, CI-excluded)

Two E2E test binaries are gated by `CABAL_E2E_TESTS=1` and are **not** run
in CI:

| Binary | Purpose |
|---|---|
| [`test/test_demo_627.ml`](test/test_demo_627.ml) | Exercises the enforcer path against all default backends, or an optional filtered backend |
| [`test/test_native_json_schema_backends.ml`](test/test_native_json_schema_backends.ml) | Iterates every backend in the registry with `native_json_schema_output = true`, mirrors host registration by loading YAML adapters and then handwritten built-ins, and exercises the native schema path (Story #628) |

Both are built and run via `@e2e`. With only `CABAL_E2E_TESTS=1`, the alias is
a multi-backend run: `test_demo_627` iterates the default backend set
(`claude-code`, `codex`, `opencode`, `copilot-cli`), while
`test_native_json_schema_backends` iterates every registry backend whose
`native_json_schema_output = true`. `gemini-cli` has a default model and
override env var, but is opt-in via `CABAL_E2E_BACKEND=gemini-cli`.

#### E2E env vars

| Variable | Purpose |
|---|---|
| `CABAL_E2E_TESTS=1` | Enables building and running the E2E binaries |
| `CABAL_E2E_BACKEND` | Optional backend id filter for debugging. Omit it for the default multi-backend run. Comma-separated values are accepted. |
| `CABAL_E2E_MODEL_CLAUDE_CODE` | Optional `claude-code` model override; default is `haiku` |
| `CABAL_E2E_MODEL_CODEX` | Optional `codex` model override; unset means omit `-m` and keep the Codex CLI default model |
| `CABAL_E2E_MODEL_COPILOT_CLI` | Optional `copilot-cli` model override; default is `claude-haiku-4.5` |
| `CABAL_E2E_MODEL_OPENCODE` | Optional `opencode` model override; default is `openai/gpt-5.4-mini` |
| `CABAL_E2E_MODEL_GEMINI_CLI` | Optional `gemini-cli` model override; default is `gemini-3-flash-preview` |

```bash
CABAL_E2E_TESTS=1 dune build @e2e
```

Filtered run with one override:

```bash
CABAL_E2E_TESTS=1 \
  CABAL_E2E_BACKEND=opencode \
  CABAL_E2E_MODEL_OPENCODE=openai/gpt-5.4-mini \
  dune build @e2e
```

`test_native_json_schema_backends` iterates all registry entries with
`native_json_schema_output = true`. It calls `Adapter_loader.register_all ()`
and then registers the handwritten built-ins (`Claude_code`, `Gemini_cli`,
`Codex_cli`, `Opencode_cli`, `Copilot_cli`) to mirror host usage; if a
descriptor-native backend resolves to a runtime backend whose
`Agentic_backend.native_json_schema_output` is false, the test fails closed.
After that runtime-capability check, both E2E binaries skip backends whose CLI
binary is unavailable on `PATH`; authentication failures from installed CLIs are
reported as real E2E failures. Codex intentionally omits a model by default: the
harness invokes `codex exec` without `-m`, letting an already-authenticated
ChatGPT Codex session use its CLI default model. For non-interactive setup,
provide a `CODEX_ACCESS_TOKEN` and bootstrap the CLI before the E2E run:

```bash
printf '%s' "$CODEX_ACCESS_TOKEN" | codex login --with-access-token
```

Version-drift detection is advisory: a warning is emitted when the installed binary version is below
`descriptor.baseline_version`; a debug log is emitted when the installed version
exceeds `tested_at_version`.

If you consume Cabal as a vendored subtree inside a host monorepo, dune sees
the cabal directory directly as part of the workspace — no `opam pin` is
required. Host-side build/test invocations and any host-specific escape
hatches (e.g. commit checks) belong in the host's own documentation, not
here.

For downstream/standalone consumers while Cabal is not yet published, pin it
explicitly from a local checkout:

```bash
opam pin add cabal <path-to-cabal> -y
```

The library remains host-agnostic: callers choose the managed namespace used for
generated files. The default namespace is `cabal` (id `cabal`, display name
`Cabal`, config directory `.cabal/backend-config`); hosts that need a
different namespace must construct one explicitly and pass it through
`Backend_types.make_task_spec` and the `Backend_config_gen.*` setup helpers.

## Sync model

The current source of truth is `libs/cabal` in the Épure monorepo. Épure CI
dispatches Cabal's mirror-sync workflow on every push to Épure `main`. The Cabal
workflow is only triggered by Épure's `repository_dispatch` event and uses a
payload constrained to `source_repository=epure-team/epure`,
`source_ref=refs/heads/main`, and the full 40-character `source_sha`. It checks
out that exact SHA with `CABAL_EPURE_READ_TOKEN`, verifies the payload SHA is
still the latest Épure `main` via the GitHub REST API, subtree-splits
`libs/cabal`, creates an installation token for the Cabal mirror GitHub App, and
pushes the result to `epure-team/cabal:main` with that app token. The standalone
Cabal CI then runs against the mirror. When `libs/cabal` is unchanged, the
subtree split/push step is a no-op.

`epure-team/cabal:main` should remain protected with PRs required for humans.
Install the custom Cabal mirror GitHub App on `epure-team/cabal` with repository
Contents write and Workflows write permissions. Workflows write is required
because the mirrored subtree can update `.github/workflows/*` files in Cabal.
Add that app installation actor as the only bypass actor for the mirror-update
branch ruleset or branch-protection rule. Épure only dispatches the sync event;
it does not hold branch-write or bypass credentials for Cabal.

Direct PRs opened against `epure-team/cabal` are supported as an intake path,
but the final review path still runs through Épure. Cabal's
`sync-pr-to-epure.yml` workflow runs on `pull_request_target` for Cabal PR
open/update/close events, but automatic Épure mirroring is restricted to trusted
same-repository PRs only: `github.event.pull_request.head.repo.full_name` must
equal `github.repository` (`epure-team/cabal`), and the author must either have
`github.event.pull_request.author_association` of `OWNER`, `MEMBER`, or
`COLLABORATOR`, or pass a same-repo permission fallback. The fallback uses the
read-only Cabal `GITHUB_TOKEN` during the initial validation step to query
`GET /repos/{owner}/{repo}/collaborators/{username}/permission` and trusts only
`admin`, `maintain`, or `write`. This handles maintainers whose private org
membership makes GitHub report `author_association=NONE`. Fork PRs,
cross-repository PRs, permission values such as `triage`, `read`, or `none`, and
permission lookup failures exit successfully before creating an Épure app token,
checking out PR contents, or writing to Épure. Maintainers who want automatic
mirroring for untrusted changes must move or adopt them onto a trusted branch in
`epure-team/cabal`. This avoids turning untrusted fork PR contents into
same-repository Épure CI runs.

For trusted open PR events, the workflow checks out the Cabal PR head at the
immutable `github.event.pull_request.head.sha` as data only, checks out
`epure-team/epure@main` with a short-lived GitHub App token, replaces
`epure/libs/cabal` with the Cabal PR tree, and opens or updates an Épure PR from
`cabal-pr-<Cabal PR number>` to `main`. The mirrored Épure PR body records the
Cabal PR URL, source repository/branch/SHA, and notes that Épure PR CI and
review are authoritative. If a trusted open/update event produces no
`libs/cabal` changes relative to Épure `main`, the workflow closes any existing
matching Épure PR with an explanatory comment and deletes the
`cabal-pr-<Cabal PR number>` branch; if there is no existing mirror, it no-ops.

The Cabal PR sync workflow deliberately does **not** execute scripts, tests, or
workflow files from the Cabal PR branch. It only checks out PR contents as data
and copies them with standard archive mechanics while excluding `.git`. The
workflow's default `GITHUB_TOKEN` permissions remain read-only in Cabal
(`contents: read`, `pull-requests: read`); all writes to Épure use a separate
GitHub App installation token scoped to `epure-team/epure` with Contents write
and Pull requests write permissions. If a Cabal PR is closed unmerged, the
matching open Épure PR is closed and its `cabal-pr-<number>` branch is deleted.
If a Cabal PR is merged, the Épure PR is left open for backport and
source-of-truth review instead of being closed automatically.

Required secrets:

- Épure repository: `CABAL_MIRROR_DISPATCH_TOKEN`, with permission to dispatch
  `repository_dispatch` events on `epure-team/cabal`. For a fine-grained token,
  `contents: write` on the Cabal repo is the common minimum; no branch-protection
  bypass is required.
- Cabal repository: `CABAL_EPURE_READ_TOKEN`, with read access to the private
  `epure-team/epure` repository for the Épure-to-Cabal mirror workflow.
- Cabal repository: `CABAL_MIRROR_APP_ID`, the app ID for the custom Cabal mirror
  GitHub App installed on `epure-team/cabal`.
- Cabal repository: `CABAL_MIRROR_APP_PRIVATE_KEY`, the private key for that app;
  the sync-from-Épure workflow exchanges it for a short-lived installation token
  before pushing `main`.
- Cabal repository: `CABAL_TO_EPURE_APP_ID`, the app ID for the GitHub App
  installed on `epure-team/epure` that mirrors Cabal PRs into Épure PR branches.
- Cabal repository: `CABAL_TO_EPURE_APP_PRIVATE_KEY`, the private key for that
  app; the PR sync workflow exchanges it for a short-lived installation token
  before pushing `cabal-pr-<number>` branches or editing Épure PRs.

Normal contribution flow for now:

1. change Cabal under `libs/cabal` in Épure;
2. merge through Épure's normal review path;
3. let the mirror-sync workflow update `epure-team/cabal` automatically.

Direct Cabal PR flow:

1. open or update a trusted same-repository PR in `epure-team/cabal`;
2. let Cabal's PR sync workflow create or update the matching Épure PR;
3. treat the Épure PR as the authoritative CI/review gate;
4. merge through Épure, then let the normal mirror update Cabal `main`.

Fork PRs, cross-repository PRs, and PRs from authors without trusted association
or `write`/`maintain`/`admin` repository permission are not mirrored
automatically. A maintainer must first adopt the change onto a trusted
`epure-team/cabal` branch if it should enter the automatic Épure PR path.

Do not merge direct Cabal PRs independently except for an emergency fix that
cannot wait for the Épure PR path. If that escape hatch is used, backport or
reconcile the change in `libs/cabal` immediately.
