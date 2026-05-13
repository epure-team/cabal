# Changelog

All notable changes to Cabal are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project does not yet emit semantic version tags — entries here are grouped
by the date a change merged to `main`.

## Unreleased

### Changed
- **Adapter override directory renamed from `.epure/` to `.cabal/`** (breaking).
  `Adapter_loader.register_all` now reads user-global overrides from
  `~/.cabal/adapters/*.yaml` and project-local overrides from
  `<project_dir>/.cabal/adapters/*.yaml`. Host applications that previously
  dropped YAML overrides under `~/.epure/adapters/` or
  `<project>/.epure/adapters/` must move them to the new locations. Built-in
  adapters compiled into the library are unaffected. The rename makes the
  override path host-neutral now that cabal is consumed beyond Épure.

### Security
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
