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

## Capability evidence conventions (Story #622 / #628)

- `capabilities.native_json_schema_output = true` MUST be accompanied by
  `native_json_schema_output_evidence = Some _`; setting it to `true` with
  `None` evidence is a CI-enforced contract violation.  The structural
  invariant is enforced by
  `libs/cabal/test/test_demo_622.ml:test_native_json_schema_evidence_required_when_true`,
  which iterates `Backend_registry.all ()` — not a hardcoded list — so every
  backend added in the future is automatically checked.
- Claude Code (`claude-code`, Story #625) and Codex (`codex`, Story #630) have
  `native_json_schema_output = true`. All other built-in backends remain
  `false`.
- The `capability_evidence` type is defined in `backend_types.mli` (not
  `backend_registry.mli`) to avoid import cycles and to keep it available
  wherever types are referenced.
- When writing new test helpers that directly construct a `Backend_registry.capabilities`
  record literal (instead of using a built-in descriptor), add
  `native_json_schema_output = false; native_json_schema_output_evidence = None`
  to avoid record-field exhaustiveness errors as the type evolves.

## Task media/web preflight — CBL-01

- `task_spec.attachments` defaults to `[]`, `task_spec.web_access` defaults to
  `Web_disabled`, and legacy Yojson documents must retain those defaults.
- Attachment paths are workspace-relative. `Task_preflight.validate_inputs`
  opens the workspace first, opens attachments relative to that exact directory
  descriptor, and authorizes the opened attachment descriptor with
  separator-safe containment before reading. Relative and absolute symlinks are
  accepted only when that descriptor resolves to a readable regular file inside
  the opened workspace.
- Attachment size, SHA-256, and magic bytes come from one opened file. SHA-256
  is canonical lowercase hex and computed in process with `digestif`; do not
  replace this with a subprocess or add base64 before a transport needs it.
- Rendered preflight errors must never include raw attachment paths, digests, or
  bytes. Callers provide count/per-file/total limits; Cabal owns no product
  defaults.
- `make_resume_task_spec` intentionally copies attachments and web policy so a
  resumed invocation retains the same caller-approved inputs.
- Positive `media_support` and `web_support` claims require versioned
  `feature_evidence`. `E2e_test` evidence names its reproducible test in
  `notes`; `Manual_probe` stores the exact command in its payload.
- All built-in descriptors remain media-disabled and `Web_disabled` until a
  backend transport and reproducible evidence land together. Native JSON schema
  evidence remains independently mandatory and must not be weakened.
- CBL-01 exposes validation only. Central runtime wiring belongs to CBL-03 and
  backend media/web transports belong to CBL-07.

## Json_schema_validator — Story #623

- `Json_schema_validator.validate` is backed by the `jsonschema` opam package
  (v0.1.0). It supports all keywords defined in JSON Schema drafts 4, 6, 7,
  2019-09, and 2020-12 — including `enum`, `additionalProperties`, `minimum`,
  `maxLength`, `pattern`, `allOf`, `anyOf`, `oneOf`, `if/then/else`, and more.
  Do NOT replace it with a hand-written keyword-subset implementation.
- Default draft is 2020-12 (Decision D-2). Callers requiring a specific draft
  must embed `"$schema"` in the schema document; the package reads that field and
  selects the appropriate draft automatically.
- The module is **pure**: no I/O, no subprocess, no LLM.  The `jsonschema`
  package bundles all meta-schemas; no network access occurs for inline schemas
  (schemas without external `$ref` URLs).
- `Json_schema_validator` is used exclusively by the validate-and-retry path in
  `Json_schema_enforcer`.  The native path (`native_json_schema_output = true`)
  does not call this module.
- The `jsonschema` opam dependency is declared in `libs/cabal/dune-project` and
  `libs/cabal/src/dune`.  When adding `jsonschema` to the root `epure` package,
  add it to `dune-project` under `(package (name epure) ...)` as well.
- Test file: `libs/cabal/test/test_demo_623.ml` — covers AC1 (pure validation),
  AC2 (full keyword coverage proves the package is used), AC3 ($schema draft
  selection).  Test files in `libs/cabal/test/` must include `open Cabal` to
  access library modules without the `Cabal.` prefix.

## Json_schema_enforcer — Story #624

- `Json_schema_enforcer.run_task` wraps `Agentic_backend.run_task` with optional
  validate-and-retry enforcement.  Hard cap: **at most two backend calls** per
  invocation.  No backoff, no configurable budget; callers wanting more attempts
  must call `run_task` again.
- Pass-through: `json_schema = None` → exactly one backend call, result returned
  unchanged as `Ok`.
- Happy path: `json_schema = Some _`, first response passes validation → exactly
  one backend call.
- Session-resume retry path (`capabilities.session_resume = true` and first
  result carries a `session_id`): the session is resumed via
  `Backend_types.make_resume_task_spec`; the retry prompt contains **only** the
  schema block under `## Required output schema` and the compliance instruction
  (original prompt omitted).  Use `Json_schema_enforcer.resume_retry_template`
  as the pinned template constant.
- Fresh-call retry path (otherwise): new invocation whose prompt contains the
  original prompt, then the schema block, then the compliance instruction.  Use
  `Json_schema_enforcer.fresh_retry_template` as the pinned template constant.
- Both attempts fail: returns `Error msg` containing both error strings labelled
  "Attempt 1" and "Attempt 2".  Neither is discarded.
- Non-Success first result (`Failed`/`Timeout`/`Cancelled`): propagated as
  `Ok result` without schema validation or retry.
- `session_id` from the second `task_result` is propagated in the returned
  record when the retry succeeds.
- The two retry prompt template constants (`resume_retry_template`,
  `fresh_retry_template`) are pinned in `json_schema_enforcer.mli` for
  inspection and deterministic testing.
- Unit tests with mock backends: `libs/cabal/test/test_demo_626.ml` — covers all
  AC items above including call-count assertions, prompt-content assertions,
  session-id propagation, and error-message preservation.  Specifically:
  - `Failed`, `Timeout`, and `Cancelled` first-result propagation are each
    tested as separate cases (AC7a/7b/7c) — all three match arms are exercised.
  - Template constant fidelity (AC8/AC9): the actual retry prompt captured from
    the mock is compared byte-for-byte against the exported template constant
    with `{schema}`, `{error}`, and `{original_prompt}` substituted using the
    known schema JSON and the error produced by `Json_schema_validator.validate`
    on the same invalid input.  Divergence between `build_resume_prompt` /
    `build_fresh_prompt` and the exported templates is caught deterministically.

## Native JSON schema wiring — Story #625

- `Json_schema_enforcer.run_task` routes to the **native path** when
  `Agentic_backend.native_json_schema_output backend = true`.  On this path:
  - `task_spec.json_schema` is left intact; the backend's `run_task` wires it to
    the CLI flag (e.g. `--output-schema <schema-json>` for claude-code).
  - `Json_schema_validator` is **NOT** called — no validate-and-retry loop.
  - Any `Failed` result is returned as `Error "native-backend schema rejection: <msg>"` 
    immediately; no second call, no fallback (Decision D-5).
  - `Timeout` / `Cancelled` results are returned as `Ok result` (transport
    failures, not schema rejections).
- **Adding a new `native_json_schema_output = true` backend**: you must also add
  `native_json_schema_output : bool` to the `Agentic_backend.S` module type
  (already done), set it `true` in the backend's `.ml`, wire the schema into the
  CLI via `spec.json_schema` in `build_command` (or equivalent), add a
  `capability_evidence` record to `backend_registry.ml` with `tested_at_version`,
  `json_schema_draft` (e.g. `"2020-12"`), and `test_method` (either
  `Backend_types.E2e_test` or `Backend_types.Manual_probe "<invocation>"`).
  If the backend needs a model override for the manual E2E harness, add its
  default and `CABAL_E2E_MODEL_<BACKEND>` entry to
  `libs/cabal/test/e2e_harness_config.ml`.
- Claude Code uses `--output-schema <inline-JSON>` (JSON Schema draft 2020-12).
  The evidence record is in `backend_registry.ml` under the `claude-code`
  descriptor.
- Unit tests: `libs/cabal/test/test_demo_625.ml` — covers native path routing,
  fail-fast on rejection, pass-through when no schema, session-id propagation,
  and schema presence in the spec passed to the native backend.
- Every inline mock struct that implements `Agentic_backend.S` must declare
  `let native_json_schema_output = false` (or `true` when intentionally testing
  the native path).  Existing tests updated: `test_demo_626.ml`,
  `test_backend.ml`, `test_model_probe.ml`, `test_agent_helpers.ml`,
  `test_pipeline_runtime.ml`, `test_build_flow.ml`.

## E2E tests — Stories #627 and #628

- E2E tests for `Json_schema_enforcer` (Story #627) live in
  `libs/cabal/test/test_demo_627.ml`.  They are **excluded from CI** via
  `(enabled_if (= %{env:CABAL_E2E_TESTS=0} 1))` in the dune stanza — the
  same pattern used by `EPURE_OCAMLLSP_TESTS=1` elsewhere in the repo.  The
  binary is neither built nor executed when `CABAL_E2E_TESTS` is unset.
- `CABAL_E2E_TESTS=1` builds and runs the E2E binaries.  With no other env vars,
  `test_demo_627` is a multi-backend run over the default backend ids in
  `libs/cabal/test/e2e_harness_config.ml` (`claude-code`, `codex`, `opencode`,
  `copilot-cli`).  `CABAL_E2E_BACKEND` is an optional comma-separated filter for
  manual debugging, not a required gate; `gemini-cli` is opt-in through that
  filter.
- The shared `CABAL_E2E_MODEL` contract is removed.  Model overrides are
  backend-specific: `CABAL_E2E_MODEL_CLAUDE_CODE`, `CABAL_E2E_MODEL_CODEX`,
  `CABAL_E2E_MODEL_COPILOT_CLI`, `CABAL_E2E_MODEL_OPENCODE`, and
  `CABAL_E2E_MODEL_GEMINI_CLI`.  Defaults live in
  `e2e_harness_config.ml`: Claude Code uses `haiku`; Codex passes no model and
  keeps the CLI default; Copilot uses `claude-haiku-4.5`; OpenCode uses
  `openai/gpt-5.4-mini`; Gemini uses `gemini-3-flash-preview`.
- The generic native-path E2E test (Story #628) lives in
  `libs/cabal/test/test_native_json_schema_backends.ml`, also gated by
  `CABAL_E2E_TESTS=1`.  It iterates every backend in `Backend_registry.all ()`
  with `native_json_schema_output = true`, mirrors host registration by calling
  `Adapter_loader.register_all ()` and then registering the handwritten built-ins
  (`Claude_code`, `Gemini_cli`, `Codex_cli`, `Opencode_cli`, `Copilot_cli`), and
  fails closed if a descriptor-native backend resolves to a runtime backend whose
  `Agentic_backend.native_json_schema_output` is false.  Only after that runtime
  capability check may it skip a backend whose CLI binary is unavailable on
  `PATH`; installed but unauthenticated CLIs fail as real E2E failures.
  Version-drift detection is advisory: a warning is emitted when the installed
  binary version is below
  `descriptor.baseline_version`; a debug log when the version exceeds
  `evidence.tested_at_version`.
- A `(alias e2e)` rule in `libs/cabal/test/dune` runs **both** binaries
  sequentially; `dune build @e2e` is the discoverable named target.
- The structural CI test for Story #628 lives in
  `libs/cabal/test/test_demo_628.ml` (always compiled, never gated) and asserts:
  (1) `Backend_types.test_method` has `E2e_test` and `Manual_probe of string`
  constructors; (2) `capability_evidence` has `tested_at_version`,
  `json_schema_draft`, and `test_method` fields; (3) every backend with
  `native_json_schema_output = true` in `Backend_registry.all ()` carries
  `native_json_schema_output_evidence = Some _`.
- `Adapter_loader.register_all ()` alone registers YAML-backed adapters and does
  not install handwritten runtime modules or their native capability values. The
  Story #628 E2E and structural guards register handwritten built-ins after the
  loader before calling `Registry.get`, matching host usage without changing
  adapter-loader semantics.
- `libs/cabal/README.md` documents all env vars and links to both test files.
