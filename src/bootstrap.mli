(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** The single entry point that makes agentic backends available.

    Every seam that later calls {!Backend_completer.make_by_name} or
    {!Registry.get} must call {!ensure} first, and must call nothing else.
    Hand-rolling a list of [Registry.register (module …)] calls is the defect
    this module exists to remove: before it, fifteen seams registered fifteen
    different sets, so [mock-agent] was a validator at five of them and
    rejected at the other ten, and seven commands could not see any
    YAML adapter at all.

    {1 Ordering contract}

    {!ensure} registers the six native OCaml backends {b first}, in
    declaration order, and only then loads YAML adapters via
    {!Adapter_loader.register_all}. Two consequences are deliberate and are
    pinned by [libs/cabal/test/test_bootstrap.ml]:

    - A bundled YAML adapter never displaces a native backend of the same id.
      {!Adapter_loader} skips a {e builtin} adapter whose id is already
      registered, so no [Replacing existing backend] warning is emitted for
      the five ids both layers define. A user-global or project-local adapter
      still overrides a native, and still warns — a deliberate override
      should be visible.
    - {!Registry.list} therefore returns natives before YAML-only adapters.
      That order is a contract, not an accident: {!Lead.derive_backend_from_model}
      picks the {e first} backend declaring a given model, so a native wins a
      model tie-break {b by design}. *)

(** [ensure ?project_dir ()] registers the six native backends
    ([claude-code], [gemini-cli], [codex], [opencode], [copilot-cli],
    [mock-agent]) and then every YAML adapter reachable from [project_dir].

    Memoised on [project_dir]: calling it again with the same argument is a
    no-op — no re-registration, no warning, no adapter directory re-read.
    Calling it with a {e different} [project_dir] re-runs, so the new root's
    [.cabal/adapters] is picked up.

    Omitting [project_dir] skips only the project-local adapter directory;
    builtin and user-global adapters are still loaded.

    {pre}
    (none)

    {post}
    Every native backend id resolves via [Registry.get], and
    [Backend_registry.supports_read_only "mock-agent"] is [true].

    {violators}
    Registry.clear — empties the registry without clearing this module's
    memo, so a subsequent [ensure] would be a no-op over an empty registry.
    Call {!reset} alongside it.

    {violates}
    Adapter_loader.register_all — [ensure] registers natives before it runs,
    which is what makes its builtin skip-on-collision branch fire.
*)
val ensure : ?project_dir:string -> unit -> unit

(** [reset ()] forgets the memo so the next {!ensure} re-registers.

    {b Test-only.} Production code has no reason to call it: [ensure] is
    already idempotent, and re-running registration cannot make the registry
    more correct. It exists because {!Registry.clear} empties the registry
    without this module noticing, so a test that clears and then calls
    {!ensure} would otherwise be left with nothing registered. Exposing it —
    rather than making tests hand-roll their own registration again — is the
    point.

    {pre}
    (none)

    {post}
    The next {!ensure} with any [project_dir] performs registration.

    {violators}
    (none)

    {violates}
    Bootstrap.ensure — invalidates its memo, so the next call does work.
*)
val reset : unit -> unit
