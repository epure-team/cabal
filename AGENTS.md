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
- Attachment paths are workspace-relative. `Task_preflight.prepare_inputs`
  opens the workspace first, opens attachments relative to that exact directory
  descriptor, and authorizes the opened attachment descriptor with
  separator-safe containment before reading. Relative and absolute symlinks are
  accepted only when that descriptor resolves to a readable regular file inside
  the opened workspace.
- Attachment size, SHA-256, magic bytes, and sealed transport bytes come from one
  opened descriptor/read. SHA-256 is canonical lowercase hex and computed in
  process with `digestif`; do not replace this with a subprocess or add base64.
- Prepared inputs use an unpredictable private `0o700` task directory outside
  the workspace and atomically created `0o600` files with the declared PNG/JPEG
  extension. Never validate and then reopen/copy the caller path. Fail closed on
  platforms/workspace layouts where descriptor-backed staging authorization
  cannot be established.
- Validate descriptor evidence and requested media/web/read-only/resume/schema
  capability consistency before allocating staging or reading attachments.
- `Runtime_dispatch` owns prepared inputs through the complete retry/process
  lifetime and carries their immutable backend/media/web authorization in
  `Task_execution_context.t`. Fresh retries reuse one staged set; resume reuse
  retains references but emits no image paths. A prepared value is one-shot
  across both execute APIs, including attachment-free tasks: only the atomic
  execution owner may call the backend or clean active inputs; switch
  abandonment cleanup may claim only a pending value.
- Cleanup must cover success, failure, timeout, cancellation, fatal exceptions,
  abandoned prepared values, and staging/cleanup failures. Retry cleanup only
  with the fixed central bound, expose sanitized cleanup status on detailed
  executions, and emit fixed diagnostics without paths or exception payloads.
  Cleanup failure must not replace a non-success backend result, structured
  schema failure, or a propagating fatal exception's identity.
- Owner release atomically and permanently revokes transport authorization before
  its first physical deletion attempt. Keep authorization and retryable physical
  cleanup as separate state: cleanup failure must never reactivate either
  `authorized_attachment_paths` or `sealed_attachment_delivery`, and released
  prepared inputs cannot be authorized again.
- Adding `task_execution.cleanup_status` is source-breaking for exhaustive record
  literals/patterns. Prefer `Backend_types.make_task_execution`, whose default is
  `Cleanup_not_required`; pinned literals must add
  `cleanup_status = Backend_types.Cleanup_not_required`. Cleanup-aware consumers
  handle all three constructors; intentionally cleanup-agnostic patterns use `_`.
- Sealed staging protects against caller workspace path replacement and, through
  permissions, accidental/cross-user reads. It does not defend against hostile
  same-UID processes, privileged/system-level access, or a compromised OS.
- Rendered preflight errors must never include raw attachment paths, digests, or
  bytes. Callers provide count/per-file/total limits; Cabal owns no product
  defaults.
- `make_resume_task_spec` intentionally copies attachments and web policy so a
  resumed invocation retains the same caller-approved inputs.
- Positive `media_support` and `web_support` claims require versioned
  `feature_evidence`. `E2e_test` evidence names its reproducible test in
  `notes`; `Manual_probe` stores the exact command in its payload.
- Codex has a positive PNG/JPEG media claim at baseline `0.131.0`. Copilot CLI
  `1.0.54` is media-disabled and its runtime fails before setup/spawn because no
  flag disables user, workspace, plugin, built-in, and account-controlled ODR
  MCP discovery together. Every built-in remains `Web_disabled`.
  The final cached probe produced search without fetch but failed its
  content-dependent official-result assertion; live fetch is not evidence for
  the lower search-only hierarchical level. Native JSON schema evidence remains
  independently mandatory and must not be weakened.
- Codex uploads only centrally sealed absolute paths and never original
  workspace paths. Sensitive low-level Codex calls without matching central
  authorization fail before config I/O/spawn; no-attachment `Web_disabled`
  compatibility may remain. The adapter must consume the generic central sealed
  delivery accessor and must not re-read `Backend_registry` or reconstruct retry
  delivery policy.
- The Codex media/web probe must remain content-dependent: exact color assertions
  for initial/new/resumed images, no `-i` on reuse, search-only cached prompts,
  fetch rejection for cached evidence, and public live web lifecycle evidence.
  Keep subprocesses bounded and all diagnostics fixed/sanitized; argparse errors
  and interruption must emit no usage, traceback, supplied value, or path.
  `--self-test` must exercise every validator and these negative CLI paths.
- Copilot's quarantined candidate transport pins `--prefer-version 1.0.54`,
  uses repeated `--attachment` flags in caller order, and validates the complete
  public `--output-format json
  --stream off` protocol before releasing text/session/usage/tool events or raw
  callbacks. `--allow-all-tools` is required by Copilot prompt mode and is safe
  only while bounded by `--available-tools=view,grep,glob`, explicit
  shell/write/memory/URL denials, no blanket path/URL grants, and isolated
  `COPILOT_HOME`. Because those controls do not disable every MCP discovery
  source, every task remains unsupported and must fail before project config I/O
  or backend spawn. Media, positive web, read-only, resume/reuse, and MCP claims
  remain disabled. The bundled Copilot YAML runtime must stay non-executable.

## Central media/schema E2E — CBL-08 P0

- `test/test_media_web_schema_backends.ml` is authenticated and must stay behind
  `CABAL_E2E_TESTS=1`. Standard CI neither builds nor runs it; the always-on
  `test/test_media_web_schema_e2e_structure.ml` guards selection, fixtures,
  schema/semantic validation, event invariants, credential-free environment
  lookup, and Dune alias/gate wiring.
- Select every descriptor whose media support and evidence are positive and
  valid, then apply the comma-separated `CABAL_E2E_BACKEND` filter. Supply the
  fixture's 2020-12 schema only when native JSON Schema is independently true
  with matching evidence; generic `structured_output` is insufficient. Each
  selected backend gets one central `Task_runtime.start_task` invocation carrying
  both a runtime-generated blue PNG and red JPEG with computed size/SHA-256
  metadata. Codex is currently the only selected media backend and receives
  native schema; quarantined Copilot is not invoked.
  Never call `Agentic_backend.run_task`, `Json_schema_enforcer.run_task`, or a CLI
  directly from this proof.
- Mirror hosts with `Runtime_bootstrap.Hardened_builtins` and fail closed on any
  effective descriptor, independent runtime-capability snapshot, or runtime
  native-schema disagreement. Supply explicit attachment limits and assert
  detailed requested delivery plus `Cleanup_succeeded`.
- Await normalized event delivery and require one last terminal, no post-terminal
  event, exact attempt start/finish agreement, and final public agent output.
  Codex's public JSONL protocol additionally guarantees session and usage events.
  The retained historical Copilot validator requires session, usage, and exactly
  two paired successful `view` events, but it is not executable evidence while
  Copilot is quarantined. A tool event is not mandatory for Codex's image-only
  prompt, but any emitted tool
  lifecycle is valid only after that exact attempt starts and before it finishes,
  and must pair within the attempt by stable id or, only without an id, by name.
  Reject tools on unknown/pending/finished attempts, finish-before-start,
  duplicates, identity mismatches, cross-attempt finishes, and tools left active
  at attempt finish or terminal.
- The schema intentionally permits a small color vocabulary; exact blue/red
  semantic validation is separate so a constant schema-shaped response cannot
  pass. Inspect PNG dimensions and decoded pixels in-process. Pin the no-decoder
  JPEG golden by byte count/SHA-256, independently parse its SOF dimensions, and
  reject corrupted or arbitrarily relabelled fixtures. Live diagnostics are
  fixed and sanitized: never print prompts, raw output, fixture bytes/paths,
  digests, credentials, or session identifiers.
- Skip executable lookup only for genuine `ENOENT`/`ENOTDIR`. Non-executable
  files, permission failures, broken or looping symlinks, all other lookup
  errors, and installed CLIs with failed/malformed version, availability, or
  authentication probes are failures. Reuse `Backend_version.check_gate` for the
  enforced baseline; above-evidence versions get only a fixed advisory.
- A native-schema execution is exactly attempt number 1, `Initial_attempt`, with
  upload delivery, exact attachment references, `Web_disabled`, no local schema
  rejection, and a successful attempt result equal to the final result. Generic
  multi-attempt validation remains capped at two and numbered contiguously from 1.
- Web selection is separate and includes only positive descriptors. Every current
  built-in is `Web_disabled`, so P0 performs no web invocation and does not fail
  for the empty matrix. The complete web E2E matrix remains P1.
- Keep `test_process_group`, descendant cleanup, and other standard process
  ownership tests outside the authenticated gate. Add the CBL-08 binary after the
  existing E2Es in the sequential `@e2e` alias.

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

## Detailed execution telemetry — CBL-05

- `Backend_types.attempt_kind` is the canonical attempt algebra;
  `Task_event.attempt_kind` re-exports it so event and detailed execution kinds
  cannot diverge.
- `Json_schema_enforcer.run_task_detailed` retains every completed backend
  `task_result`; `run_task` must remain only its compatibility projection through
  `Json_schema_enforcer.render_error`. Preserve the exact native error renderer
  and `Attempt 1` / `Attempt 2` labels.
- The hard maximum remains **two backend calls per enforcer invocation**. A
  failed invoked resume is classified structurally as `Resume_failure`; never add
  a third automatic fresh fallback. Callers wanting fresh fallback invoke again.
- Initial and fresh attempts use `Upload_attachments`. Resumed attempts retain
  attachment references/digests and web policy but use
  `Reuse_session_attachments`; `task_spec.attachments` remain workspace-relative
  references, not bytes. Before every call, the exact requested intent must be
  visible through `Task_execution_context.requested_delivery`. This is intent,
  not proof of preflight, content loading, or transport compliance.
- The retry spec is bounded by the remaining time on the existing CBL-04
  absolute deadline, never the original timeout. Cancellation/deadline expiry
  before the retry transition creates neither an attempt nor retry events.
- Aggregate optional cost/token fields independently. Negative/non-finite input
  invalidates only its field; non-negative integer overflow saturates at
  `max_int`, and finite float overflow at `max_float`. Total elapsed is one
  monotonic boundary measurement, including resume classification, not a sum of
  attempt durations; final session/resume selection ignores blank trimmed ids.
- Contain ordinary `is_resume_failure` exceptions as `Transport_failure` with a
  sanitized diagnostic after preserving both results. Cancellation,
  `Out_of_memory`, `Stack_overflow`, and `Sys.Break` propagate. The central
  detailed runtime/dispatch endpoint remains CBL-06 work.
- `Event_delivery_truncated` and event sequence gaps are normal bounded-delivery
  telemetry, not execution failures.

## Rich completer and central detailed dispatch — CBL-06

- `Backend_types.completion_request` is the stable host DTO. Its constructor
  defaults schema/session/max turns to `None`, attachments to `[]`, web to
  `Web_disabled`, and timeout to the existing `max_float`. Keep its shared field
  types/semantics aligned with `task_spec`; completers alone compose the
  system/user prompt and call `make_task_spec`.
- Adding fields to `completion_request` is source-breaking for exhaustive record
  literals/patterns. Treat `make_completion_request` as the canonical
  forward-compatible host construction path and document future evolution there.
- `Backend_completer.make_rich` must use the central `Task_runtime` detailed
  handle. It must not invoke `Agentic_backend` or `Json_schema_enforcer`
  directly. `Runtime_dispatch.prepare` resolves one validated effective entry,
  and that immutable backend snapshot owns every CBL-05 attempt.
- `Runtime_dispatch.run_task_detailed`, `Task_runtime.run_task_detailed`, and
  `Task_runtime.await_detailed` share the legacy handle's CBL-03/04 preflight,
  version policy, availability, absolute deadline, cancellation/process
  ownership, and events. Legacy await/run behavior is a projection of the same
  memoized detailed outcome; preserve its timeout/cancellation and exact error
  rendering semantics.
- Every returned backend result must be committed to detailed progress in the
  same narrow cancellation-protected section as its `Attempt_finished` event.
  Outer timeout/cancellation and sanitized ordinary exceptions retain all such
  attempts, aggregate cost, last nonblank session, and elapsed; never add an
  in-flight attempt. Fatal exception identity and cleanup ordering stay intact.
- Rich completion awaits outcome before event delivery. This ordering keeps
  outcome await safe from the handle's own callback; never await delivery from
  that callback. The returned event collector is bounded to
  `Task_event.max_pending_events`,
  `Task_event.max_pending_observational_events`, and
  `Task_event.max_pending_agent_text_bytes`, while reserving controls plus the
  terminal. `event_trace.omitted_events`, delivery-truncation markers, and
  sequence gaps are normal observability.
- Use `Task_event.Private`'s exhaustive payload classifier and bounded collector
  for rich traces. The exact partition is 192 observations, 62 ordinary
  non-terminal controls, one truncation marker, one terminal, and 64 KiB of
  assistant text; do not duplicate a wildcard-based classifier in completers.
- Rich response text is `agent_text` only. Raw lines are never collected; raw
  stdout/stderr remain only inside the non-serializing CBL-05 in-process detail.
  Keep rich response/error/event types free of generated serialization. The
  legacy successful completer retains its stdout fallback for compatibility.
- Read-only rich/validator invocations set `task_spec.read_only=true` and rely on
  central capability preflight against the resolved effective descriptor before
  availability/spawn. The legacy static validator construction check may remain
  for its fail-fast diagnostic, but it never replaces this central gate.
- `test/test_rich_completer_cwr_compile.ml` is the host-shaped compatibility
  fixture. Cabal must remain independent of CWR/Épure libraries and host
  workflow state.

## Native JSON schema wiring — Story #625

- `Json_schema_enforcer.run_task` routes to the **native path** when
  `Agentic_backend.native_json_schema_output backend = true`.  On this path:
  - `task_spec.json_schema` is left intact; the backend's `run_task` wires it to
    the CLI flag (e.g. `--output-schema <schema-json>` for claude-code).
  - `Json_schema_validator` is **NOT** called — no validate-and-retry loop.
  - Any `Failed` result is returned as
    `Error "native-backend call failed with a schema in force: <msg>"`
    immediately; no second call, no fallback (Decision D-5). The structured
    variant is `Native_backend_failure_with_schema`; it does not assert cause.
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
  `libs/cabal/test/e2e_harness_config.ml` (`claude-code`, `codex`, `opencode`).
  Quarantined Copilot CLI 1.0.54 is excluded. `CABAL_E2E_BACKEND` is an optional
  comma-separated filter for manual debugging, not a required gate;
  `gemini-cli` is opt-in through that filter.
- The shared `CABAL_E2E_MODEL` contract is removed.  Model overrides are
  backend-specific: `CABAL_E2E_MODEL_CLAUDE_CODE`, `CABAL_E2E_MODEL_CODEX`,
  `CABAL_E2E_MODEL_OPENCODE`, and `CABAL_E2E_MODEL_GEMINI_CLI`. Defaults live in
  `e2e_harness_config.ml`: Claude Code uses `haiku`; Codex passes no model and
  keeps the CLI default; OpenCode uses `openai/gpt-5.4-mini`; Gemini uses
  `gemini-3-flash-preview`. Quarantined Copilot has no executable E2E model
  contract.
- The generic native-path E2E test (Story #628) lives in
  `libs/cabal/test/test_native_json_schema_backends.ml`, also gated by
  `CABAL_E2E_TESTS=1`.  It iterates every backend in `Backend_registry.all ()`
  with `native_json_schema_output = true`, mirrors hardened host registration via
  `Runtime_bootstrap.Hardened_builtins`, and
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
  Story #628 E2E and structural guards use `Runtime_bootstrap.Hardened_builtins`
  before calling `Registry.get`; adapter-loader semantics remain unchanged.
- `libs/cabal/README.md` documents all env vars and links to both test files.

## Runtime bootstrap and dispatch — CBL-03

- `Runtime_bootstrap.Hardened_builtins` requires an empty runtime registry,
  ignores HOME/project adapters, retains Pi as embedded YAML, installs the five
  handwritten built-ins, validates all six entries against an independent full
  capability mapping, atomically publishes them with `Enforce_baseline`, and
  leaves model probes off unless explicitly requested with both `sw` and `env`.
- Pi does not transport resume; both its descriptor and YAML runtime report
  `session_resume = false`.
- `Runtime_dispatch.run_task` is the central invocation path. It requires caller
  limits and resolves exactly one `Registry.Validated` entry snapshot at each
  call; it never joins a runtime to `Backend_registry.find`. It runs
  input/capability preflight before any process or availability side effect,
  applies the entry's version policy, checks availability, and passes the same
  backend through schema retries. Raw `Registry.register` entries are rejected
  before side effects. `Enforce_baseline` keeps the missing/unparseable skip
  policy and blocks parseable below-baseline versions; `No_version_gate` skips
  stability comparison. Ordinary exceptions are sanitized; Eio cancellation,
  `Out_of_memory`, `Stack_overflow`, and `Sys.Break` propagate.
- `Backend_completer.make_by_name` and `make_validator_by_name` use central
  dispatch with a private attachment-free/Web-disabled compatibility policy.
  Construction performs no registry lookup or adapter command; dynamic checks
  happen only when the returned completer is invoked.
  `Backend_completer.make`, `Agentic_backend.run_task`, and
  `Json_schema_enforcer.run_task` remain documented low-level bypasses.
- Every Extensible YAML adapter receives a conservative effective descriptor and
  `No_version_gate`; this includes built-in-id overrides. Global → project
  precedence replaces the complete validated entry. YAML neither mutates nor
  inherits built-in/host catalog descriptors.
- `Runtime_entry.origin` is registration metadata: hardened mode maps
  Claude/Codex/Copilot/Gemini/OpenCode to `Handwritten` and Pi to `Yaml`;
  loader entries use `Yaml`, and explicit host entries use `Custom`.
- `Agentic_backend.S` deliberately has no origin field, preserving downstream
  custom module source compatibility. YAML package/config metadata is bounded to
  the latest package per id and cleared by `Registry.clear`.
- Custom backends are registered additively as a validated descriptor/runtime
  pair through `Runtime_bootstrap.register_custom`; their explicit descriptor is
  the full host capability attestation and `Enforce_baseline` is the safe default.
  Collisions replace nothing.
