(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Purely validated runtime registration entries. *)

(** Provenance assigned by the trusted registration path. *)
type implementation_origin =
  | Handwritten  (** Cabal's approved backend-specific implementation. *)
  | Yaml  (** The generic {!Yaml_adapter} implementation. *)
  | Custom  (** An explicitly registered host or test implementation. *)

(** Installed-version policy bound to one effective runtime descriptor. *)
type version_policy =
  | Enforce_baseline
      (** Probe and enforce [effective_descriptor.baseline_version]. *)
  | No_version_gate
      (** Skip stability comparison. Used for generic YAML adapters whose
          upstream version formats are not part of Cabal's stable contract. *)

(** Pure entry-construction failure. *)
type validation_error =
  | Invalid_runtime_id
  | Runtime_id_mismatch
  | Invalid_runtime_display_name
  | Invalid_descriptor_display_name
  | Invalid_descriptor_binary_name
  | Invalid_descriptor_baseline_version
  | Descriptor_evidence_invalid of Task_preflight.error
  | Runtime_capabilities_mismatch
      (** The trusted full runtime capability snapshot differs from the
          effective descriptor. *)
  | Session_resume_mismatch
  | Native_json_schema_output_mismatch

(** One immutable dispatch snapshot. The private record can only be constructed
    by {!create}, while callers may inspect its validated metadata. *)
type t = private {
  backend : Agentic_backend.t;
  effective_descriptor : Backend_registry.descriptor;
  runtime_capabilities : Backend_registry.capabilities;
  origin : implementation_origin;
  version_policy : version_policy;
}

(** [render_validation_error error] returns a sanitized structural diagnostic. *)
val render_validation_error : validation_error -> string

(** [valid_runtime_id value] checks the canonical backend-id syntax without
    performing I/O. *)
val valid_runtime_id : string -> bool

(** [create ~backend ~descriptor ~runtime_capabilities ~origin ~version_policy]
    validates all structural fields and evidence, exact equality between the
    effective descriptor and the independently supplied full runtime capability
    snapshot, and the two capability booleans represented directly by
    {!Agentic_backend.S}. It performs no I/O or registry mutation. *)
val create :
  backend:Agentic_backend.t ->
  descriptor:Backend_registry.descriptor ->
  runtime_capabilities:Backend_registry.capabilities ->
  origin:implementation_origin ->
  version_policy:version_policy ->
  (t, validation_error) result
