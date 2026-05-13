(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Generic project-owned backend configuration artifact writer.

    This module owns only backend-agnostic concerns: artifact metadata,
    namespace-driven attribution markers, managed hash checks, strict-JSON
    sidecar metadata, atomic writes, backups, and setup result aggregation.
    Backend modules own the actual backend-native config templates and JSON
    records. *)

(** Ownership classification of a generated config artifact. *)
type ownership =
  | Epure_owned
      (** Host-owned artifact (constructor name retained for source
          compatibility).  File lives under the managed namespace's
          [config_dir] (default [.cabal/backend-config/]); passed explicitly
          to the backend at invocation. *)
  | Backend_project
      (** File lives at the backend's required fixed project path and is
          protected against silent overwrites. *)

(** A generated backend configuration artifact. *)
type artifact = {
  backend_id : string;  (** Backend identifier. *)
  ownership : ownership;  (** Ownership/write policy. *)
  managed_namespace : Backend_types.managed_namespace;
      (** Namespace controlling managed markers and suffixes. *)
  project_relative_path : string;  (** Target path under project root. *)
  content : string;  (** Full artifact content. *)
}

(** Result of writing an artifact to disk. *)
type write_result =
  | Written of string
  | Already_current
  | Refused_hash_mismatch of string
  | Backed_up_and_written of {path : string; backup_path : string}
  | Skipped_user_content of string
  | Invalid_managed_namespace of string

(** Per-artifact result produced by [setup_artifacts]. *)
type artifact_write_outcome = {
  artifact : artifact;  (** Artifact whose write was attempted. *)
  project_path : string;  (** Absolute project path for the artifact. *)
  result : write_result;  (** Result returned by [write_artifact]. *)
}

(** Result of setting up a backend's project config artifacts. *)
type setup_result = {
  project_config_path : string option;
  write_outcome : write_result option;
  write_outcomes : artifact_write_outcome list;
}

(** Attribution text used by backend-specific providers in managed headers. *)
val attribution_text : string

(** [attribution_text_for namespace] returns the namespace-specific generated
    file attribution line used in managed headers.

    {pre}
    [namespace.display_name] is non-empty.

    {post}
    Returns a deterministic attribution line containing [namespace.display_name].

    {violators}
    (none)

    {violates}
    (none) *)
val attribution_text_for : Backend_types.managed_namespace -> string

(** Managed marker used by backend-specific providers in managed headers. *)
val managed_marker : string

(** [managed_marker_for namespace] returns the namespace-specific managed
    marker, such as ["cabal-managed"] for the default namespace.

    {pre}
    [namespace.id] has already passed managed namespace validation.

    {post}
    Returns [namespace.id ^ "-managed"].

    {violators}
    (none)

    {violates}
    (none) *)
val managed_marker_for : Backend_types.managed_namespace -> string

(** [hash_marker_for namespace] returns the namespace-specific managed hash
    marker, such as ["cabal-hash"] for the default namespace.

    {pre}
    [namespace.id] has already passed managed namespace validation.

    {post}
    Returns [namespace.id ^ "-hash"].

    {violators}
    (none)

    {violates}
    (none) *)
val hash_marker_for : Backend_types.managed_namespace -> string

(** Supported comment syntaxes for generic managed headers. *)
type comment_style = Slash | Hash | Html

(** [with_epure_header style body] prepends attribution and managed marker
    comments without a hash.  Use for host-owned artifacts that are freely
    regenerated.  Function name retained for source compatibility; the
    output uses the active namespace's markers, not literal Épure tags. *)
val with_epure_header :
  ?managed_namespace:Backend_types.managed_namespace ->
  comment_style ->
  string ->
  string

(** [with_managed_header style ~backend_id body] prepends attribution,
    managed marker, and body hash comments.  Use for backend-project artifacts
    that can carry comments. *)
val with_managed_header :
  ?managed_namespace:Backend_types.managed_namespace ->
  comment_style ->
  backend_id:string ->
  string ->
  string

(** [is_managed_content content] detects inline managed markers (current
    namespace plus the legacy [epure-managed] alias for migration). *)
val is_managed_content :
  ?managed_namespace:Backend_types.managed_namespace -> string -> bool

(** [extract_hash content] extracts an inline managed hash if present. *)
val extract_hash :
  ?managed_namespace:Backend_types.managed_namespace -> string -> string option

(** [managed_body_hash ~backend_id content] computes the generic managed body
    hash after removing inline managed metadata and managed MCP blocks.  The
    [backend_id] label is retained for compatibility. *)
val managed_body_hash :
  ?managed_namespace:Backend_types.managed_namespace ->
  backend_id:string ->
  string ->
  string

(** [write_artifact ~project_dir ~force artifact] writes [artifact] using the
    generic ownership policy.

    {b Path-traversal contract:} [artifact.project_relative_path] is appended
    to [project_dir] verbatim. Cabal does not normalise [..] segments. The
    caller (host application) is responsible for ensuring the path stays
    within [project_dir]; passing a value containing [..] or an absolute
    path will silently write outside the project tree. Use
    {!Backend_types.validate_namespace} for the [managed_namespace] half of
    the contract — that side is enforced here and returns
    [Invalid_managed_namespace _]. *)
val write_artifact :
  project_dir:string -> force:bool -> artifact -> write_result

(** [write_result_was_applied result] is [true] when [result] means the target
    file contains the generated artifact content after setup. *)
val write_result_was_applied : write_result -> bool

(** [setup_artifacts ~project_dir ~force artifacts] writes [artifacts] in
    order and returns both the primary artifact's backward-compatible outcome
    and the per-artifact outcomes for all generated files. *)
val setup_artifacts :
  project_dir:string -> force:bool -> artifact list -> setup_result

(** [precedence_warning_for ~backend_id ~write_outcome] returns a non-fatal
    warning when registry metadata says the backend cannot guarantee project
    config precedence over user-global config. *)
val precedence_warning_for :
  backend_id:string -> write_outcome:write_result option -> string option
