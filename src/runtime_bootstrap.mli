(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Validated runtime registry bootstrap.

    [Extensible] preserves {!Adapter_loader.register_all} precedence and probe
    behavior. [Hardened_builtins] ignores user/project adapters, stages only the
    six approved embedded ids, replaces the five handwritten implementations,
    validates each against an independent full runtime-capability mapping,
    central execution policy, and approved static descriptor, then atomically
    publishes the complete set. Copilot is published with a typed quarantine
    because complete MCP isolation is unproven.

    Registry state is startup state for one OCaml domain. Bootstrap and custom
    registration must finish before concurrent task execution. *)

(** Runtime composition profile. *)
type profile =
  | Extensible  (** Embedded, then user-global, then project-local YAML. *)
  | Hardened_builtins
      (** Approved embedded built-ins only, with handwritten replacements. *)

(** Invalid optional-argument combination. *)
type option_error =
  | Incomplete_eio_context
      (** Exactly one of [sw] and [env] was supplied. *)
  | Probe_context_required
      (** [probe_models=true] requires both [sw] and [env]. *)
  | Extensible_probe_override_conflict
      (** Extensible mode received [sw]/[env] but explicitly disabled probes,
          which would change [Adapter_loader.register_all] semantics. *)

(** Structural runtime/descriptor inconsistency. *)
type validation_error = Runtime_entry.validation_error =
  | Invalid_runtime_id
  | Runtime_id_mismatch
  | Invalid_runtime_display_name
  | Invalid_descriptor_display_name
  | Invalid_descriptor_binary_name
  | Invalid_descriptor_baseline_version
  | Descriptor_evidence_invalid of Task_preflight.error
  | Runtime_capabilities_mismatch
  | Session_resume_mismatch
  | Native_json_schema_output_mismatch

(** Bootstrap or additive-registration failure. *)
type error =
  | Invalid_options of option_error
  | Hardened_registry_not_empty
  | Embedded_adapter_invalid
  | Hardened_builtin_set_invalid
  | Descriptor_missing
  | Candidate_invalid of validation_error
  | Custom_runtime_id_collision
  | Custom_descriptor_id_collision

(** [render_validation_error error] returns a sanitized structural diagnostic.
    It never includes adapter file paths, task paths, digests, or bytes. *)
val render_validation_error : validation_error -> string

(** [render_error error] returns a sanitized bootstrap diagnostic. *)
val render_error : error -> string

(** [valid_runtime_id value] checks the canonical, side-effect-free backend-id
    syntax accepted by bootstrap and by-name routing. *)
val valid_runtime_id : string -> bool

(** [validate_backend ~descriptor ~backend] delegates to {!Runtime_entry.create}
    and discards the resulting token. For source compatibility, it treats
    [descriptor.capabilities] as an explicit caller attestation. It performs no
    I/O or registry mutation. *)
val validate_backend :
  descriptor:Backend_registry.descriptor ->
  backend:Agentic_backend.t ->
  (unit, validation_error) result

(** [validate_backend_capabilities] additionally requires an independently
    supplied full runtime capability snapshot and checks exact equality with the
    effective descriptor. *)
val validate_backend_capabilities :
  runtime_capabilities:Backend_registry.capabilities ->
  descriptor:Backend_registry.descriptor ->
  backend:Agentic_backend.t ->
  (unit, validation_error) result

(** [register_runtime ?project_dir ?sw ?env ?probe_models ~profile ()]
    composes and registers a runtime.

    [Extensible] calls {!Adapter_loader.register_all} with embedded → global →
    project precedence. Every YAML runtime, including a built-in-id override,
    binds a conservative effective descriptor and explicit
    {!Runtime_entry.No_version_gate} policy without mutating or inheriting the
    static descriptor catalog. Supplying [sw] and [env] preserves its current
    automatic probe behavior; explicitly disabling probes in that combination
    is rejected rather than silently changing semantics.

    [Hardened_builtins] requires an empty {!Registry}, never reads [HOME] or
    [project_dir], and disables model probes unless [probe_models=true] is
    explicitly paired with both [sw] and [env]. Static version metadata is
    validated, but no CLI installation, authentication, or network access is
    required when probes are disabled. Approved entries use
    {!Runtime_entry.Enforce_baseline}. All are dispatch-enabled except Copilot,
    whose {!Runtime_entry.Dispatch_quarantined} policy prevents every preflight,
    version, availability, configuration, and task side effect. Any pre-commit
    failure leaves runtime and descriptor registries unchanged. *)
val register_runtime :
  ?project_dir:string ->
  ?sw:Eio.Switch.t ->
  ?env:Eio_unix.Stdenv.base ->
  ?probe_models:bool ->
  profile:profile ->
  unit ->
  (unit, error) result

(** [register_custom ~descriptor ~backend] additively registers one custom
    runtime/descriptor pair. The explicit host descriptor is also its complete
    runtime-capability attestation; entry construction validates all structure,
    evidence, and module-represented booleans before mutation. Custom entries use
    the safe {!Runtime_entry.Enforce_baseline} default. Existing runtime ids and
    built-in/additive descriptor ids are collisions and are never replaced.

    Registration is an explicit startup operation on single-domain registry
    state; it must not race with other registry mutations. The catalog is added
    only after token construction; committing that already validated immutable
    token to {!Registry} is a no-fail whole-entry mutation. *)
val register_custom :
  descriptor:Backend_registry.descriptor ->
  backend:Agentic_backend.t ->
  (unit, error) result
