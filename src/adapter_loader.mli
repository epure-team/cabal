(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Loads YAML-defined agentic backends from files and embedded strings.

    This module is responsible for discovering, parsing, validating, and
    registering YAML-configured backends.  At startup, {!register_all} loads
    built-in YAML configs (embedded at compile time) followed by any
    user/project-local overrides.

    See {!Yaml_adapter} for the runtime backend implementation. *)

(** A parsed YAML adapter configuration before full validation.
    Fields mirror {!Yaml_adapter.config} but all are optional to capture
    partial inputs gracefully. *)
type yaml_adapter_config = Yaml_adapter.config

(** [validate config] checks that required fields ([name],
    [invocation_command], [template_set]) are non-empty.
    Returns [Error field_name] for the first missing/empty field found.

    {pre}
    (none)

    {post}
    Returns [Ok ()] if all required fields are present and non-empty, or
    [Error field_name] identifying the first missing or empty required field.

    {violators}
    (none)

    {violates}
    (none)
*)
val validate : yaml_adapter_config -> (unit, string) result

(** [load_string ~source s] parses a YAML string [s] into a
    {!yaml_adapter_config}.  [source] is recorded in the [source] field
    (e.g., ["builtin"], a file path, or ["user-global"]).
    Returns [Error msg] if the YAML is malformed or required fields are absent.

    {pre}
    [source] should be a non-empty string identifying the origin of [s] for
    error reporting.

    {post}
    Returns [Ok config] with the parsed adapter configuration on success, or
    [Error msg] describing why the YAML could not be parsed or validated.

    {violators}
    (none)

    {violates}
    (none)
*)
val load_string :
  source:string -> string -> (yaml_adapter_config, string) result

(** [load_dir dir] reads every [*.yaml] file in [dir] and parses each one.
    Returns a list of [(filename, result)] pairs — successful parses alongside
    errors so the caller can report individual failures without aborting.

    {pre}
    [dir] must be a readable directory path; a non-existent directory returns
    an empty list.

    {post}
    Returns a list of [(filename, Ok config)] for successfully parsed files
    and [(filename, Error msg)] for files that failed to parse or validate;
    the list order follows directory iteration order.

    {violators}
    (none)

    {violates}
    (none)
*)
val load_dir : string -> (string * (yaml_adapter_config, string) result) list

(** [embedded_backends ()] constructs the compiled-in YAML runtime candidates
    without reading [HOME], project directories, or invoking model probes. The
    function does not mutate {!Registry}; callers may validate the complete
    candidate set before committing it.

    @return [Error _] if any embedded adapter cannot be parsed or validated. *)
val embedded_backends : unit -> (Agentic_backend.t list, string) result

(** [resolve_registered_model_probes ~sw ~env ()] explicitly runs the existing
    protected model-probe layer for every currently registered backend and
    publishes probe/static resolution in {!Registry}. Probe errors and
    exceptions retain the backend's static model list. *)
val resolve_registered_model_probes :
  sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> unit -> unit

(** [register_all ()] loads and registers backends in priority order:

    1. Built-in YAML configs embedded at compile time ([source = "builtin"])
    2. User-global overrides: [~/.cabal/adapters/*.yaml]
    3. Project-local overrides: [.cabal/adapters/*.yaml] (highest priority)

    Later registrations for the same [name] replace earlier loader-owned pairs.
    For each non-built-in YAML id, the loader first installs or updates a
    conservative descriptor (no media, web, native-schema, resume, or read-only
    claims) and only then installs the runtime, so descriptor ownership failures
    cannot leave a runtime-only partial pair. Global → project precedence applies
    to both runtime and loader-owned descriptor metadata.

    Built-in descriptors remain immutable. User/project YAML overrides of a
    built-in must match its binary identity and runtime-represented capabilities;
    mismatches are rejected while the previous runtime remains installed.
    Errors from individual files are reported but do not abort other files.

    When [~sw] and [~env] are both supplied, every registered backend that
    declares a [models_probe] is invoked under exception protection and the
    result is published into the registry as the resolved model view (see
    {!Registry.resolved_models}).  Backends whose probe returns [Error _],
    raises, or returns [Ok []] fall back to their static {!Agentic_backend.models}
    list.  Without [~sw]/[~env] the probe layer is skipped and every backend
    surfaces with its static list tagged {!Registry.Static}.

    {pre}
    Should be called once at startup before any backend lookups are performed.
    [project_dir], if provided, must be a valid host project root.

    {post}
    Populates the {!Registry} with all successfully loaded backends. Files
    that fail to parse or validate are reported to stderr; the rest are
    registered. Later calls overwrite earlier registrations for the same name.

    {violators}
    (none)

    {violates}
    (none)
*)
val register_all :
  ?project_dir:string ->
  ?sw:Eio.Switch.t ->
  ?env:Eio_unix.Stdenv.base ->
  unit ->
  unit
