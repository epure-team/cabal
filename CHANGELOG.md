# Changelog

All notable changes to Cabal are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project does not yet emit semantic version tags — entries here are grouped
by the date a change merged to `main`.

## Unreleased

### Added
- **Central media + schema E2E proof (CBL-08 P0).** A new authenticated,
  `CABAL_E2E_TESTS=1`-gated harness selects evidence-backed media descriptors and
  exercises Codex PNG/JPEG upload plus native JSON Schema in one hardened
  `Task_runtime` invocation. Runtime-generated 64×64 fixtures carry computed
  size/SHA-256 metadata; a strict schema plus an exact semantic assertion makes
  the response image-dependent. The proof checks the validated descriptor/runtime
  binding, enforced baseline, explicit limits, sealed upload intent, exactly one
  successful initial native attempt numbered 1, successful cleanup, public
  normalized output, session/usage protocol events, exactly one terminal, and
  attempt/tool lifecycle pairing by attempt and stable identity, with tool events
  confined between the exact attempt's start and finish. It selects only
  positive-media descriptors with valid native draft 2020-12 evidence, skips
  genuine absent binaries only, and fails other lookup or installed
  probe/authentication errors. Credential-free PNG pixel inspection and fixed
  JPEG golden/dimension provenance reject corruption and arbitrary color labels.
  All live diagnostics remain fixed and sanitized. Web selection is
  positive-capability only; with every current backend `Web_disabled`, P0 performs
  no web call and leaves the full web matrix to P1. An always-on structural suite
  proves fixture, schema, selection, event, environment, and Dune-gating contracts
  without credentials; existing process cleanup tests remain outside the E2E gate.
- **Sealed Codex media transport and web investigation (CBL-07A).** Codex 0.131.0
  now carries evidence-backed PNG/JPEG upload capability. Its web adapter modes
  remain implemented, but the descriptor and independent runtime snapshot stay
  `Web_disabled`: the final authenticated cached probe proved search without
  fetch yet repeatedly failed its content-dependent official-result assertion,
  and live fetch does not imply the lower search-only hierarchical level. Central
  preflight opens each workspace-relative attachment once and streams the same
  bytes through size, digest, magic, and a private task-scoped transport copy;
  Codex receives only those sealed absolute paths. One sealed set survives fresh
  schema retries, resume reuse sends no duplicate image argv, and cleanup covers
  success, backend failure/timeout, cancellation, fatal exceptions, abandoned
  prepared calls, staging failure, and retryable cleanup failure. Prepared values
  are atomically one-shot across both execute APIs; capability gates precede
  staging, active execution cannot race switch abandonment, and bounded cleanup
  exposes sanitized detailed status without replacing non-success, structured
  schema-failure, or fatal semantics. Release permanently revokes sealed
  transport authorization before deletion, including when physical cleanup
  fails and is retried. Sensitive low-level Codex calls without
  matching immutable central authorization fail before config I/O or process
  spawn. The bounded, public-only authenticated probe validates exact PNG/JPEG
  colors, a new resumed upload, and no-upload session recall. Its offline
  validators additionally pin cached search-only behavior, reject
  search-plus-fetch evidence, and exercise fixed CLI diagnostics for invalid
  arguments, malformed records, timeouts, and interruption.
- **Source-breaking detailed cleanup status.** `Backend_types.task_execution`
  gained `cleanup_status`. Prefer `Backend_types.make_task_execution`, which
  defaults it to `Cleanup_not_required`; downstream direct record literals must
  add `cleanup_status = Backend_types.Cleanup_not_required`. Cleanup-aware
  consumers must handle `Cleanup_not_required`, `Cleanup_succeeded`, and
  `Cleanup_failed`; patterns that intentionally ignore cleanup should use `_`.
- **Stable rich completer and central detailed dispatch (CBL-06).**
  `Backend_types.completion_request` and its constructor expose system/user
  prompts, schema, resume session, attachments, web policy, timeout, and maximum
  turns without requiring hosts to construct `task_spec`. `Backend_completer`
  re-exports the DTO and adds `rich_completer` / `make_rich`, returning
  normalized text plus complete CBL-05 execution or a structured central versus
  execution failure. `Runtime_dispatch` and `Task_runtime` now expose detailed
  run/await endpoints on the same CBL-03/04 validated, cancellable, absolute-
  deadline handle; legacy run/completer APIs project that memoized detailed
  outcome with their exact historical strings/status behavior. Rich calls await
  event delivery and return a monotonic normalized trace bounded to 256 entries,
  192 observations, and 64 KiB of assistant text, with a saturating collector-
  omission count and a dedicated terminal/control reserve. Raw lines are never
  collected and rich text never falls back to raw stdout. Effective-descriptor
  preflight remains authoritative for media, web, session, and read-only policy;
  one prepared backend snapshot owns all attempts. A host/CWR-shaped compile
  fixture consumes the full DTO and structured outcome without adding host
  dependencies. Outer retry deadline/cancellation now retains every atomically
  committed backend result, aggregate cost, last nonblank session, and elapsed;
  sanitized ordinary execution exceptions retain the same partial execution,
  while fatal exceptions still propagate after cleanup. Rich trace collection
  reuses `Task_event`'s exhaustive classifier and shared post-delivery bound of
  192 observations, 62 ordinary controls, one merged truncation marker, one
  terminal, and 64 KiB of text. The canonical request constructor avoids the
  source break exhaustive record literals incur when the DTO gains fields.
- **Detailed schema-attempt execution telemetry.**
  `Json_schema_enforcer.run_task_detailed` returns ordered initial/fresh/resumed
  attempts with complete results, uniquely named monotonic call timing,
  validator errors, and backend-visible requested attachment/web delivery
  intent. Executions aggregate every optional cost/token field independently
  with invalid-input isolation and saturating overflow, measure total wall time
  through resume classification, and select the last nonblank trimmed session
  explicitly. Native backend failure with a schema in force, double validation
  failure, transport failure, and recognized resume failure are structurally classified
  while retaining all completed attempts. `run_task` is now an exact
  compatibility projection through the public error renderer. The existing
  two-call hard cap is unchanged: a failed invoked resume never triggers an
  automatic third fresh fallback; callers start a new invocation to resend
  attachments. Retries preserve web/attachment references and are bounded by
  the remaining shared absolute deadline. Ordinary resume-classifier exceptions
  are contained as transport failures with sanitized diagnostics; cancellation
  and fatal runtime exceptions propagate. Central detailed runtime/dispatch
  wiring is now provided by the CBL-06 central detailed dispatch path.
- **Cancellable task runtime and normalized lifecycle events.**
  `Task_runtime` now exposes isolated Eio-backed handles with idempotent
  cancellation and repeatable/concurrent await. `Runtime_dispatch.run_task`
  forwards through the same handle state machine. One absolute monotonic
  `task_spec.timeout` covers resolution, preflight, version/availability checks,
  schema retries, process execution, parsing, and finalization; retries cannot
  reset the budget, invalid negative/NaN values fail before backend calls, and
  the legacy `max_float` default remains unbounded. `Task_event` provides strict
  task-local sequence/attempt numbers, start-relative timestamps, process and
  retry lifecycle, public output/session/usage events, and exactly one terminal
  event. Each backend invocation now has paired `Attempt_started` /
  `Attempt_finished` events. One asynchronous task-local callback drain preserves
  sequence order without blocking backend execution or handle outcomes;
  `Task_runtime.await_event_delivery` explicitly waits for terminal callback
  completion. Its pending queue is capped at 256 events (192 observations plus a
  64-slot lifecycle/marker/terminal reserve), 64 KiB of assistant text, and
  16 KiB per text delta. Backpressure omissions are summarized without raw data
  by `Event_delivery_truncated`; one bounded marker accumulates counts while
  pending, and fully omitted events leave delivered sequence gaps. Callback
  exceptions are isolated with sanitized diagnostics, while raw lines and
  reasoning remain confined to `on_raw_line`. Process cleanup/reaping and
  task-scoped Codex schema-file cleanup complete before terminal enqueue.
- **Validated runtime bootstrap and central dispatch.**
  `Runtime_bootstrap` now offers compatibility-preserving `Extensible` loading
  and an atomic `Hardened_builtins` profile that ignores user/project adapters,
  keeps model probes opt-in, installs the five handwritten implementations plus
  embedded Pi, and validates descriptor/runtime consistency before commit.
   `Runtime_dispatch.run_task` requires caller-provided attachment limits,
   resolves one immutable validated entry at every invocation, and rejects raw
   runtime-only overrides before side effects rather than lending them catalog
   claims. Input/capability preflight precedes version or availability work;
   ordinary exceptions are sanitized, cancellation is normalized after cleanup,
   and fatal exceptions propagate through handle await. Hardened entries bind
   independent full capability snapshots,
   approved descriptors, explicit origins, and `Enforce_baseline`. Extensible
   YAML entries bind conservative effective descriptors and `No_version_gate`
   with global → project whole-entry precedence, including built-in-id overrides.
   Custom backends can be added as validated descriptor/runtime pairs through
   `Runtime_bootstrap.register_custom`; provenance no longer adds a required
   field to `Agentic_backend.S`.
- **Per-backend `models` enumeration** (non-breaking, additive).
  Every adapter implementing `Agentic_backend.S` now exposes a
  `models : string list` member listing the model ids it accepts via its
  CLI's `--model` (or equivalent) flag. The data is surfaced two ways:
  `Agentic_backend.models : t -> string list` for callers that already
  hold a backend handle, and `Registry.list_models : string -> string
  list option` for callers that only know the backend id. `Yaml_adapter`
  config gains a `models : string list` field populated from a top-level
  `models:` sequence in the YAML file; missing or malformed sequences
  fall back to `[]` (the documented "let the adapter pick" sentinel).
  Existing call sites that pass `~model:None` to
  `Backend_types.make_task_spec` continue to work unchanged.

  The same surface now has a dynamic-probe layer on top: `Agentic_backend.S`
  gained an optional `models_probe : (sw:_ -> env:_ -> (string list,
  string) result) option`. When `Adapter_loader.register_all` is called
  with `~sw` and `~env`, each registered backend's probe is invoked once
  under exception protection; a non-empty `Ok` result replaces the static
  list for that backend, while any `Error _`, exception, or `Ok []` cleanly
  falls back to the static declaration. The resolved view is exposed via
  `Registry.resolved_models : string -> (string list *
  Registry.models_source) option`, with `models_source = Probe | Static |
  Hybrid` documenting the origin (`Hybrid` is reserved and not yet
  emitted). `Registry.list_models` keeps its existing return type and is
  now `Option.map fst @@ resolved_models`. Among the built-in adapters
  only `opencode` ships a probe today (parsing `opencode models`);
  `claude-code`, `codex`, `gemini-cli`, and `copilot-cli` set
  `models_probe = None` until their upstream CLIs expose a non-interactive
  listing command.
- **`Backend_types.task_result.agent_text`** (non-breaking, additive).
  Adapters now populate a new `agent_text : string` field with the agent's
  final reply, extracted from each CLI's native output format
  (claude-code JSON envelope, codex JSONL, gemini stream-json, opencode JSON
  events, copilot plain text). `stdout` continues to carry the raw bytes
  produced by the CLI so callers that need backend-specific post-processing
  retain access. Host applications consuming the response text should switch
  from `Yojson.Safe.from_string result.stdout` (or equivalent CLI-specific
  parsing) to `result.agent_text` so they stop having to know which CLI ran.
  `Backend_types.make_task_result` gains a `?agent_text:string` optional
  argument that defaults to `""`, so existing call sites continue to compile
  unchanged. `Backend_completer.make` now prefers `agent_text` over `stdout`
  for the completion text (with a `stdout` fallback for backends that have
  not yet populated `agent_text`).

### Changed
- **`Agentic_backend.S.run_task` gained `?context` (breaking for backend module
  implementers).** Downstream modules must change
  `let run_task ~sw ~env ?on_raw_line spec = ...` to
  `let run_task ~sw ~env ?context ?on_raw_line spec = ...`, using
  `?context:_` when lifecycle events are not consumed. Existing call sites using
  the first-class `Agentic_backend.run_task` wrapper remain source-compatible.
  `test/test_custom_backend_compile.ml` pins both sides of this contract.
- **Adapter override directory renamed from `.epure/` to `.cabal/`** (breaking).
  `Adapter_loader.register_all` now reads user-global overrides from
  `~/.cabal/adapters/*.yaml` and project-local overrides from
  `<project_dir>/.cabal/adapters/*.yaml`. Host applications that previously
  dropped YAML overrides under `~/.epure/adapters/` or
  `<project>/.epure/adapters/` must move them to the new locations. Built-in
  adapters compiled into the library are unaffected. The rename makes the
  override path host-neutral now that cabal is consumed beyond Épure.
- **Default `managed_namespace` is now host-neutral** (breaking).
  `Backend_types.default_managed_namespace` is now `{ id = "cabal";
  display_name = "Cabal"; config_dir = ".cabal/backend-config" }` instead
  of `{ id = "epure"; display_name = "Epure"; config_dir =
  ".epure/backend-config" }`. Effects on freshly written artifacts:
  attribution lines change to `Generated by Cabal — do not edit manually`,
  managed comment markers become `cabal-managed` / `cabal-hash`, host-owned
  config files land under `.cabal/backend-config/`, sidecar files use the
  `.cabal-meta.json` suffix, and `--force` backups use the `.cabal-backup`
  suffix. Existing on-disk files with legacy `epure-*` markers and
  attribution remain readable: `Backend_config_writer` recognises the
  legacy namespace as a fallback and migrates legacy JSON metadata keys
  (`_epure_attribution`, `_epure-managed`, `_epure-hash`) to the current
  namespace's headers on next write. Hosts that depended on the old default
  must construct an explicit `Backend_types.managed_namespace` and thread
  it through `make_task_spec` / `setup_project_config_with_options` /
  `generate_all_with_options`.
- **Cabal-internal environment variables renamed** to drop the `EPURE_`
  prefix. `CABAL_BACKEND_FAST` / `CABAL_BACKEND_SMART` are the new
  `Backend_tier.init` lookups; `CABAL_VALIDATOR_PARALLEL_MAX_MEM`
  configures `Resource_guardian`'s memory threshold; and
  `CABAL_MOCK_AGENT_FIXTURES` is the new `Mock_agent` fixtures path. The
  old `EPURE_*` names continue to work as deprecated aliases when their
  `CABAL_*` counterpart is unset, so existing host-side wrappers are not
  broken — but new code and CI should set the `CABAL_*` names.
- **Standalone-repo `README.md` no longer documents
  `EPURE_NO_COMMIT_CHECK=1 dune ...`** invocations, which are an
  Épure-monorepo-specific escape hatch and belong in the monorepo's own
  docs. Architecture diagram and ownership boundaries reworded so the host
  is `BountyNexus, Épure, tests, or another OCaml app`, not "Épure" by
  default.
- **`.mli` docstrings generalised**: `Backend_config_gen.mli`,
  `Backend_config_writer.mli`, `Backend_types.mli`, `Mock_agent.mli`, and
  `Session_event_log` module-level docs now describe behaviour as "the
  host application does X" rather than "Épure does X" when the behaviour
  is host-neutral. The `Epure_owned` ownership constructor is retained
  under its current name for source compatibility, but its docstring
  clarifies that it denotes a host-owned artifact under the active managed
  namespace's `config_dir`. User-facing strings written into generated
  Copilot CLI instructions and OpenCode comments now read "host
  application" instead of "Epure".

### Security
- **Structured public-output parsing now fails closed.** Claude, Codex, Gemini,
  OpenCode, and Pi normalized paths accept only documented successful assistant
  or tool records. Error, reasoning, user, unknown, and malformed records stay
  raw-only. Extensible YAML overrides of those built-in ids use the same strict
  parser and cannot promote raw fallback text.
- **Session NDJSON files now created with mode `0o600`** (previously `0o640`,
  group-readable). Post-redaction backend events could leak to any user in the
  file owner's group on shared/CI machines.
- **Redaction policy hardened.** The `sensitive_fields` set now covers
  `environment`, `env`, `env_vars`, `oauth_token` / `oauth` / `jwt` / `bearer`,
  `cookie` / `set_cookie` / `session`, `connection_string` / `dsn`,
  `signature`, `client_secret`, and AWS / GCP credential field names.
  Pattern-based fallbacks now redact URLs containing embedded
  `user:password@` credentials and JWT-shaped tokens in any field.
- **`url` and `error` removed from `safe_string_fields`.** Both legitimately
  carry credentials (postgres://user:pw@host) or echo them in error
  messages; the field-name allowlist was masking those leaks.

### Fixed
- **Build break:** the `mli` at `resource_guardian.mli:127` and three sites in
  `backend_process.ml` used the eio-1.x parameterised resource types
  (`_ Eio.Time.clock`, `_ Eio.Flow.source`, `_ Eio.Flow.sink`). The library
  targets eio 0.11 where these are zero-arity. Drop the `_` and pin eio to
  `>= 0.11 & < 1.0` in `dune-project`.
- **File-descriptor leak** in `Backend_config_writer.read_file_opt`: the
  channel was only closed on the success path. Now uses `Fun.protect`.
- **Race condition** in `Resource_guardian` PID set: `register_pid` /
  `unregister_pid` mutated a `mutable int list` without synchronisation. Now
  stored in `int list Atomic.t` with a CAS-retry helper.
- **Adapter id drift:** bundled YAML adapters at
  `src/adapters/{gemini,copilot}.yaml` registered as `gemini` / `copilot`,
  while the static descriptors, the hand-written `*_cli.ml` modules, and
  `backend_config_gen` all use the canonical `gemini-cli` / `copilot-cli`.
  `Registry.get "gemini-cli"` returned `None`. Both YAMLs renamed.

### Added
- **Version / availability probes are now timed.**
  `Backend_process.capture_version_output` and `Backend_process.check_available`
  take `?timeout_seconds` (default 5.0) and return promptly on a hung binary
  instead of freezing registry initialisation indefinitely.
- **`Resource_guardian.registered_pids`** read-only accessor for diagnostics.
- **`Backend_types.validated_namespace`** — opt-in `private managed_namespace`
  wrapper. `validate_namespace : managed_namespace -> (validated_namespace,
  string) result` is the only constructor; hosts can plumb the type through
  their own artifact paths to make namespace validation a compile-time
  obligation.
- **`Backend_json_helpers`** module sharing the `json_string_map` codec
  previously duplicated in three `*_cli.ml` adapters.
- **`Backend_config_cleanup`** module: text-cleanup helpers
  (`remove_dangling_commas_before_closers`, `strip_managed_mcp_block`)
  extracted from `Backend_config_writer`.
- **CI matrix** now covers `ubuntu-latest` + `macos-latest` on OCaml 5.3.0,
  plus a `ubuntu-latest` leg on 5.2.0. Adds `dune build @doc` to every leg
  and a separate coverage job uploading a `bisect_ppx` HTML report.
- **Opam metadata:** `dev-repo`, `doc`, `tags` fields populated via
  `dune-project`. `cabal.opam` regenerated.

### Diagnostics
- `Session_event_log.write_raw_event` and `read_events` now emit
  `Diagnostics.warn` on unparseable lines instead of silently dropping them.
- `Adapter_loader.env_mappings_field` now emits one
  `Diagnostics.warn` per non-string env value, tagged with the YAML source.

### Tests added
- `test/test_session_event_log.ml` — file mode, dir mode, diagnostics
  surfacing.
- `test/test_backend_event_redaction.ml` — 18 cases covering sensitive
  fields, value patterns, and nested-container redaction.
- `test/test_backend_config_writer.ml` — managed-header round-trip,
  idempotent writes, hash-mismatch refusal, `force` backup, invalid-namespace
  refusal.
- `test/test_validated_namespace.ml` — smart-constructor acceptance and
  rejection cases.
- `test/test_resource_guardian.ml` — `Domain`-based concurrency tests for
  register/unregister.
- `test/test_backend_process.ml` extended with version-probe timeout
  assertions.
- `test/test_adapter_loader.ml` extended with top-level / env-mapping
  negatives and a diagnostic-capture test.
- `test/test_backend_registry.ml` extended with two AC5
  static-vs-runtime consistency property tests.

### Deferred
- **ocamlformat enforcement** — adopting stock ocamlformat would reformat
  every existing `.ml`/`.mli`. The current style is consistent but
  hand-formatted; a sweeping reformat is out of scope for a bug-fix PR and
  is better tracked as its own opt-in.
- **Full split of `Backend_config_writer`** — only the cleanup helpers were
  extracted; further decomposition of sidecar IO / write policy would change
  the public surface and is left for a follow-up.
- **`maintenance_intent`** opam field — requires `(lang dune 3.18)`; the
  project pins 3.13.
