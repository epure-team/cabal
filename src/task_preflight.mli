(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Host-neutral validation for task inputs and backend capabilities.

    This module performs no backend invocation or network access. Rendered
    errors intentionally omit attachment paths and file contents. *)

(** Caller-provided attachment limits. Zero-valued limits are valid and may be
    used to disable attachments. *)
type limits = {
  max_attachments : int;  (** Maximum attachment count. *)
  max_file_size_bytes : int;  (** Maximum size of any one attachment. *)
  max_total_size_bytes : int;  (** Maximum aggregate attachment size. *)
}

(** Limit field identifying an invalid negative value. *)
type limit_name = Max_attachments | Max_file_size_bytes | Max_total_size_bytes

(** Typed input-validation failure. Attachment identifiers may be carried for
    programmatic remediation, but {!render_error} never includes them, paths,
    digests, or file bytes. *)
type input_error =
  | Negative_limit of limit_name
  | Incoherent_limits
  | Too_many_attachments of { maximum : int; actual : int }
  | Empty_attachment_id
  | Duplicate_attachment_id of string
  | Absolute_attachment_path of string
  | Workspace_unavailable
  | Attachment_missing of string
  | Attachment_outside_workspace of string
  | Attachment_not_regular of string
  | Attachment_unreadable of string
  | Attachment_changed_during_validation of string
  | Attachment_staging_failed
  | Attachment_cleanup_failed
  | Attachment_size_mismatch of {
      attachment_id : string;
      declared : int;
      actual : int;
    }
  | Attachment_too_large of {
      attachment_id : string;
      maximum : int;
      actual : int;
    }
  | Total_size_too_large of { maximum : int; actual : int }
  | Malformed_sha256 of string
  | Digest_mismatch of string
  | Media_type_mismatch of {
      attachment_id : string;
      media_type : Backend_types.media_type;
    }

(** Typed capability-validation failure. Proof-invariant failures describe an
    invalid descriptor; unsupported-feature failures describe a task that must
    be rerouted or reduced. *)
type capability_error =
  | Native_json_schema_support_without_evidence
  | Native_json_schema_evidence_without_support
  | Invalid_native_json_schema_evidence
  | Invalid_native_json_schema_evidence_version
  | Media_support_without_evidence
  | Web_support_without_evidence
  | Invalid_media_support_evidence
  | Invalid_web_support_evidence
  | Invalid_media_support_evidence_version
  | Invalid_web_support_evidence_version
  | Unsupported_media_type of Backend_types.media_type
  | Unsupported_web_access of {
      requested : Backend_types.web_access;
      maximum : Backend_types.web_access;
    }
  | Mcp_unsupported
  | Read_only_unsupported
  | Session_resume_unsupported

(** Preflight failure category. *)
type error = Input of input_error | Capability of capability_error

(** Opaque task-scoped transport copy of validated attachment bytes. *)
type prepared_inputs

(** [render_error error] returns an actionable diagnostic without embedding
    attachment paths, identifiers, digests, or file bytes.

    {b Preconditions.} None.

    {b Postconditions.} Returns a non-empty sanitized diagnostic suitable for
    logs or events.

    {b Violators.} None.

    {b Violates.} None. *)
val render_error : error -> string

(** [validate_inputs ~limits spec] is the compatibility validator for attachment
    metadata and files in [spec.working_dir]. The workspace is opened first, and
    relative attachment paths are opened relative to that exact directory
    descriptor. Relative and absolute symlinks are accepted only when the opened
    attachment descriptor resolves to a readable regular file inside the
    resolved opened workspace. No content is read before this descriptor-based
    authorization. Size limits are enforced while streaming; mutation checks,
    observed size, SHA-256, and PNG/JPEG magic bytes all use that same opened
    file descriptor. The opened workspace and attachment paths are re-resolved
    after streaming and must remain exactly the authorized, separator-safely
    contained paths. The exact streamed bytes are sealed into private transport
    files and immediately removed before this compatibility call returns. Call
    {!prepare_inputs} when those same validated bytes must survive through
    backend execution. Platforms that cannot resolve opened descriptor paths
    fail closed.

    {b Preconditions.} [limits] is caller-owned policy; no library defaults are
    implied.

    {b Postconditions.} Returns [Ok ()] exactly when limits, identifiers, paths,
    metadata, digests, and media signatures pass validation. Returns a typed
    [Input _] failure before any backend is invoked.

    {b Violators.} None.

    {b Violates.} None. *)
val validate_inputs :
  limits:limits -> Backend_types.task_spec -> (unit, error) result

(** [prepare_inputs ~limits spec] validates inputs and, for every attachment,
    streams the exact bytes read for size, digest, and magic validation into a
    private task-scoped transport file outside the opened workspace. Staged
    paths are opaque to hosts and retain the declared PNG/JPEG extension.

    The caller owns the returned value and must call {!release_inputs} after the
    complete backend/retry lifetime. *)
val prepare_inputs :
  limits:limits -> Backend_types.task_spec -> (prepared_inputs, error) result

(** [release_inputs prepared] atomically and permanently revokes transport
    access before its first deletion attempt, then idempotently removes every
    sealed file and its private directory. Cleanup failures are returned as
    sanitized typed errors and physical deletion may be retried by calling this
    function again; a failed deletion never reauthorizes transport access. *)
val release_inputs : prepared_inputs -> (unit, error) result

(** [validate_descriptor descriptor] validates capability-evidence invariants
    without inspecting a task or performing I/O. Positive native-schema, media,
    and web claims require complete, reproducible evidence whose
    [tested_at_version] is an exact [major.minor.patch] value; native-schema
    evidence is rejected when the corresponding capability is disabled.

    {b Preconditions.} None.

    {b Postconditions.} Returns [Ok ()] exactly when the descriptor's evidence
    relationships and payloads are structurally valid.

    {b Violators.} None.

    {b Violates.} None. *)
val validate_descriptor : Backend_registry.descriptor -> (unit, error) result

(** [validate_capabilities ~descriptor spec] verifies descriptor proof
    invariants and checks that [descriptor] supports every requested media type,
    the requested web level, MCP configuration mode, read-only mode, and session
    resume. [Mcp_none] rejects any nonempty [spec.mcp_servers] before input I/O.
    A non-native [spec.json_schema] is accepted because [Json_schema_enforcer]
    provides the validate-and-retry fallback.

    {b Preconditions.} [descriptor] is the descriptor selected for the
    prospective invocation. Descriptor evidence is checked through
    {!validate_descriptor} before requested capabilities are evaluated.

    {b Postconditions.} Returns [Ok ()] when proof invariants and requested
    capabilities are satisfied, otherwise a typed [Capability _] failure.
    Performs no I/O.

    {b Violators.} None.

    {b Violates.} None. *)
val validate_capabilities :
  descriptor:Backend_registry.descriptor ->
  Backend_types.task_spec ->
  (unit, error) result

(** Internal transport bridge. Hosts should treat staged paths as sensitive and
    must not log or serialize them. *)
module Private : sig
  (** Deterministic failure-injection seam for staging tests. Production code
      must use {!prepare_inputs}. *)
  val prepare_inputs_with_hooks :
    ?on_staging_directory:(string -> unit) ->
    ?on_staged_file:(string -> Unix.file_descr -> unit) ->
    ?on_cleanup_attempt:(unit -> unit) ->
    limits:limits ->
    Backend_types.task_spec ->
    (prepared_inputs, error) result

  val staged_attachments :
    prepared_inputs -> (Backend_types.media_attachment * string) list

  val staging_directory : prepared_inputs -> string option
  (** [active prepared] is false permanently once release has started,
      independently of whether physical cleanup succeeds or is retried. *)
  val active : prepared_inputs -> bool
end
