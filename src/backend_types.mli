(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Core types for agentic backend abstraction.

    These types define the contract between Épure's orchestrator and
    agentic clients (Claude Code, Copilot, Codex, etc.). See DESIGN.md
    Section 5.1 "Agentic Execution Model" and Section 6 "Agentic Backend
    Layer". *)

(** {1 Duration} *)

(** Duration in seconds. *)
type duration = float [@@deriving yojson]

(** Convert a float number of seconds into a [duration].

    {pre}
    (none)

    {post}
    Returns [f] as a [duration] value; this is a pure identity conversion.

    {violators}
    (none)

    {violates}
    (none) *)
val duration_of_seconds : float -> duration

(** Extract the underlying seconds from a [duration].

    {pre}
    (none)

    {post}
    Returns the float value representing the number of seconds in [d]; this
    is a pure identity conversion.

    {violators}
    (none)

    {violates}
    (none) *)
val duration_to_seconds : duration -> float

(** Pretty-print a [duration] to a formatter.

    {pre}
    (none)

    {post}
    Writes a human-readable representation of the duration to [fmt].

    {violators}
    (none)

    {violates}
    (none) *)
val pp_duration : Format.formatter -> duration -> unit

(** Convert a [duration] to a human-readable string.

    {pre}
    (none)

    {post}
    Returns a string representation of the duration (e.g. ["5.00s"]).

    {violators}
    (none)

    {violates}
    (none) *)
val show_duration : duration -> string

(** {1 MCP Configuration} *)

(** MCP server configuration for an agentic invocation. *)
type mcp_server_config = {
  name : string;  (** Server name chosen by the host (e.g., "myhost-mcp") *)
  command : string;  (** Command to spawn the server *)
  args : string list;  (** Command arguments *)
  env : (string * string) list;  (** Environment variables *)
}
[@@deriving show, eq, yojson]

(** Namespace for backend artifacts generated and managed by the host. *)
type managed_namespace = {
  id : string;  (** Stable lowercase identifier used in markers/suffixes. *)
  display_name : string;  (** Human-readable name used in attribution text. *)
  config_dir : string;  (** Project-relative directory for owned configs. *)
}
[@@deriving show, eq, yojson]

(** Default host-neutral namespace: id [cabal], display name [Cabal], config
    directory [.cabal/backend-config]. Host applications may construct their
    own {!managed_namespace} and pass it through {!make_task_spec} to override
    these defaults. *)
val default_managed_namespace : managed_namespace

(** [validate_managed_namespace namespace] validates that namespace-controlled
    fields are safe for markers, suffixes, and project-relative paths.

    {pre}
    (none)

    {post}
    Returns [Ok ()] only when [id] matches [[a-z0-9_-]+], [display_name] is
    non-empty, and [config_dir] is non-empty, relative, and contains no empty,
    [.] or [..] path segments.  Returns [Error msg] without normalising unsafe
    input.

    {violators}
    (none)

    {violates}
    (none) *)
val validate_managed_namespace : managed_namespace -> (unit, string) result

(** Validated wrapper around a {!managed_namespace}.

    [validated_namespace] is a private alias for [managed_namespace] — values
    can only be produced by {!validate_namespace}, and existing record syntax
    cannot construct one directly. Coerce back with [(v :> managed_namespace)]
    for interop with code that still takes the unwrapped form.

    Host applications that want to make namespace validation a type-level
    obligation (rather than a runtime check at the boundary) should plumb
    [validated_namespace] through their own artifact-creating code paths. *)
type validated_namespace = private managed_namespace

(** [validate_namespace ns] returns [Ok v] when [ns] passes
    {!validate_managed_namespace}, else propagates the same [Error msg].

    {pre}
    (none)

    {post}
    The returned [validated_namespace] is observationally equal to [ns];
    use the [:>] coercion to recover a [managed_namespace] view.

    {violators}
    (none)

    {violates}
    (none) *)
val validate_namespace :
  managed_namespace -> (validated_namespace, string) result

(** Host-provided association between a file extension and LSP language id. *)
type lsp_file_association = {
  extension : string;  (** File extension, including the leading dot. *)
  language_id : string;  (** Language id expected by the backend/LSP client. *)
}
[@@deriving show, eq, yojson]

(** Host-provided LSP server configuration for backend-native rendering. *)
type lsp_server_config = {
  name : string;  (** Backend-native server key/display name. *)
  command : string;  (** LSP executable command. *)
  args : string list;  (** Command arguments. *)
  file_associations : lsp_file_association list;
      (** File associations supplied by the host layer. *)
}
[@@deriving show, eq, yojson]

(** {1 Capability Evidence} *)

(** Verification method for a backend capability evidence record.

    Two constructors:
    - [E2e_test] — the capability was verified by a reproducible end-to-end
      test. The corresponding evidence record identifies that test.
    - [Manual_probe of string] — the capability was verified by a manual CLI
      probe.  The string payload MUST document the exact invocation used
      (e.g. [claude --version && printf '...' | claude --output-schema ...])
      so the evidence is reproducible and reviewable in PRs. *)
type test_method =
  | E2e_test
      (** Verified by a reproducible end-to-end test identified by the
          corresponding evidence record. *)
  | Manual_probe of string
      (** Verified by a manual CLI probe; the string documents the exact
          invocation. *)
[@@deriving show, eq, yojson]

(** Versioned evidence for a media or web capability claim.

    Positive feature claims use [Some evidence]. For [E2e_test], [notes]
    identifies the reproducible test. For [Manual_probe command], [command]
    records the exact invocation. [evidence_url] may point to supporting
    upstream documentation. *)
type feature_evidence = {
  tested_at_version : string;
      (** Backend binary version at which the feature was verified. *)
  test_method : test_method;  (** Reproducible verification method. *)
  evidence_url : string option;
      (** Optional public documentation or evidence URL. *)
  notes : string;  (** Audit notes, including the E2E test reference. *)
}
[@@deriving show, eq, yojson]

(** Evidence record documenting the basis for a backend capability claim.

    Required as [Some _] whenever
    [capabilities.native_json_schema_output = true].  Carries exactly three
    fields (Story #628 decision D-8):

    - [tested_at_version] — the backend binary version at which the capability
      was last verified.  Used for drift detection: the installed version is
      compared against [descriptor.baseline_version] (lower bound) and
      [tested_at_version] (upper bound).
    - [json_schema_draft] — the JSON Schema draft the native CLI accepted
      (e.g. ["2020-12"]).  Callers using the native path are responsible for
      supplying a conforming schema.
    - [test_method] — how the capability was verified; prevents bare-string
      guessing and creates a reviewable paper trail. For native JSON schema
      evidence, [E2e_test] specifically denotes
      [test_native_json_schema_backends] with [CABAL_E2E_TESTS=1];
      [Manual_probe command] retains the exact invocation.

    Defined here (not in [Backend_registry]) so it is available without a
    dependency cycle. *)
type capability_evidence = {
  tested_at_version : string;
      (** Backend binary version at which the capability was last verified. *)
  json_schema_draft : string;
      (** JSON Schema draft accepted by the backend's native CLI
          (e.g. ["2020-12"]). *)
  test_method : test_method;
      (** How the capability was verified — [E2e_test] or
          [Manual_probe of invocation_string]. *)
}

(** {1 Task Specification} *)

(** Supported media encodings for task attachments. *)
type media_type = Png | Jpeg [@@deriving show, eq, yojson]

(** Caller-declared metadata for one workspace-relative media file.

    The file itself is not serialized into the task. Hosts should invoke through
    [Runtime_dispatch], which validates metadata/workspace confinement and seals
    the exact authorized bytes with [Task_preflight.prepare_inputs]. *)
type media_attachment = {
  id : string;  (** Opaque, non-empty identifier unique within the task. *)
  path : string;  (** Workspace-relative file path. *)
  media_type : media_type;  (** Declared media encoding. *)
  sha256 : string;  (** Canonical lowercase SHA-256 digest. *)
  size_bytes : int;  (** Declared file size in bytes. *)
}
[@@deriving show, eq, yojson]

(** Maximum web access requested for a task. *)
type web_access =
  | Web_disabled  (** No web access requested. *)
  | Web_search  (** Search is allowed, but fetching result pages is not. *)
  | Web_search_and_fetch  (** Search and result-page fetching are allowed. *)
[@@deriving show, eq, yojson]

(** Stable host-facing completion request.

    This DTO is the completion-oriented subset of {!task_spec}; its field types
    and meanings are identical to the corresponding task fields. The completer
    owns prompt composition, working-directory/backend configuration, expected
    outputs, and read-only policy, so a host need not construct a [task_spec].
    Attachment values remain workspace-relative references and are validated by
    central preflight before execution.

    Adding a field to an OCaml record is source-breaking for callers that build
    exhaustive record literals or patterns. Hosts should therefore treat
    {!make_completion_request} as the canonical forward-compatible construction
    path and reserve direct literals for code intentionally pinned to this exact
    record version. *)
type completion_request = {
  system_prompt : string;
      (** System instructions. Omitted from the composed backend prompt when
          [resume_session_id] is present because the resumed session already
          carries them. *)
  prompt : string;  (** User request for this completion turn. *)
  json_schema : Yojson.Safe.t option;
      (** Optional inline schema, with the same enforcement semantics as
          [task_spec.json_schema]. *)
  resume_session_id : string option;
      (** Optional backend session to resume. *)
  attachments : media_attachment list;
      (** Workspace-relative media references; defaults to [[]]. *)
  web_access : web_access;
      (** Requested web policy; defaults to [Web_disabled]. *)
  timeout : duration;
      (** One absolute whole-task budget; defaults to the legacy [max_float]. *)
  max_turns : int option;  (** Optional backend agent-turn limit. *)
}

(** Expected output specification. *)
type output_spec =
  | Files_changed  (** Expect file modifications (detected via git diff) *)
  | Structured_report  (** Expect report via MCP report/submit tool *)
[@@deriving show, eq, yojson]

(** Task specification for an agentic invocation.

    This is a self-contained work order that the agentic client interprets
    and executes. It is not a raw LLM prompt. *)
type task_spec = {
  prompt : string;
      (** Role-specific prompt authored by the host application. Describes
          what to do. *)
  instructions : string;
      (** Project-specific instructions: conventions, constraints, rules. *)
  mcp_servers : mcp_server_config list;
      (** MCP servers to connect (host-provided MCP servers + project tools). *)
  lsp_servers : lsp_server_config list; [@default []]
      (** Host-provided LSP server definitions for backends that can render
          project LSP config. *)
  working_dir : string;  (** Target project directory. *)
  timeout : duration;  (** Maximum execution time (circuit breaker). *)
  expected_outputs : output_spec list;
      (** What the host application expects back. *)
  attachments : media_attachment list; [@default []]
      (** Workspace-relative media inputs. Legacy JSON defaults to [[]]. *)
  web_access : web_access; [@default Web_disabled]
      (** Requested web policy. Legacy JSON defaults to [Web_disabled]. *)
  managed_namespace : managed_namespace; [@default default_managed_namespace]
      (** Namespace for managed config markers, sidecars, temp files, and owned
          backend config directories. *)
  model : string option; [@default None]
      (** Optional model to use (e.g., "opus", "sonnet", "haiku"). Backend-specific. *)
  resume_session_id : string option; [@default None]
      (** Optional CLI session ID to resume (e.g., Claude Code --resume). *)
  max_turns : int option; [@default None]
      (** Optional maximum agentic turns. Maps to --max-turns in Claude Code. *)
  read_only : bool; [@default false]
      (** If true, restrict the agent to read-only operations: no shell
          execution, no file writes.  Maps to [--sandbox read-only] in codex
          and [--disallowedTools "Bash Write Edit NotebookEdit"] in claude-code.
          Use for validator roles (Reviewer, Critic, Architect). *)
  json_schema : Yojson.Safe.t option; [@default None]
      (** Optional inline JSON Schema document (as a parsed JSON value).
          When [Some schema], the enforcer validates the backend's response
          against [schema] and retries on failure.  When [None], the task
          runs without schema enforcement (pass-through). *)
}
[@@deriving show, eq, yojson]

(** {1 Task Result} *)

(** Result status for a completed task. *)
type result_status =
  | Success  (** Task completed successfully *)
  | Failed of string  (** Task failed with error message *)
  | Timeout  (** Task exceeded timeout *)
  | Cancelled  (** Task was cancelled *)
[@@deriving show, eq, yojson]

(** Structured report submitted via MCP report/submit tool.

    This is the structured feedback from an agentic client. The exact
    schema depends on the agent role. *)
type structured_report = {
  verdict : string option;
      (** Overall verdict (e.g., "approved", "rejected", "needs_changes"). *)
  issues : string list;  (** List of issues found (for Reviewer/Critic). *)
  questions : string list;  (** Clarifying questions (for Analyst). *)
  suggestions : string list;  (** Improvement suggestions (for Architect). *)
  raw_json : Yojson.Safe.t option;
      (** Raw JSON if additional structured data was provided. *)
}
[@@deriving show, eq, yojson]

(** Cost information if available from the backend. *)
type cost = {
  tokens_input : int option;  (** Input tokens consumed *)
  tokens_output : int option;  (** Output tokens generated *)
  cost_usd : float option;  (** Estimated cost in USD *)
  cache_creation_input_tokens : int option; [@default None]
      (** Prompt cache creation tokens (Claude Code). *)
  cache_read_input_tokens : int option; [@default None]
      (** Prompt cache read tokens (Claude Code). *)
}
[@@deriving show, eq, yojson]

(** Result of an agentic task invocation. *)
type task_result = {
  status : result_status;  (** Completion status. *)
  files_changed : string list;  (** Paths of modified files (from git diff). *)
  report : structured_report option;
      (** Structured report if submitted via MCP. *)
  elapsed : duration;  (** Wall-clock time for the invocation. *)
  cost : cost option;  (** Cost information if available. *)
  stdout : string;
      (** Raw captured stdout from the client process, byte-for-byte as
          produced by the underlying CLI (JSON envelope, JSONL stream, plain
          text, etc.).  Useful for debugging and backend-specific
          post-processing.  Host applications that just want the agent's
          response text should prefer {!agent_text}. *)
  agent_text : string; [@default ""]
      (** Normalised final agent message text, extracted by the adapter from
          its CLI's output format.  Empty string when no agent message was
          produced or extraction failed.  This is the host-neutral surface for
          the agent's response: hosts read this without having to know which
          agentic CLI ran or how to parse its stdout. *)
  stderr : string;  (** Captured stderr from the client. *)
  exit_code : int;  (** Exit code from the client process. *)
  session_id : string option; [@default None]
      (** CLI session ID for resume support (e.g., Claude Code session UUID). *)
}
[@@deriving show, eq, yojson]

(** {1 Detailed Task Execution} *)

(** Stable kind of one backend call within a detailed execution.

    This algebra is shared with {!Task_event.attempt_kind}: [Initial_attempt]
    is the caller's invocation, [Fresh_attempt] is a schema retry using a new
    backend invocation, and [Resumed_attempt] is a schema retry of an existing
    backend session. *)
type attempt_kind = Initial_attempt | Fresh_attempt | Resumed_attempt

(** Media handling selected for one invoked backend attempt.

    [Upload_attachments] requests that a future media-aware transport resolve
    the workspace-relative references and send their bytes. It applies to initial
    and fresh attempts. [Reuse_session_attachments] requests no second upload
    because a resumed session is expected to retain them; the attachment
    metadata remains available as reference/digest telemetry. These constructors
    describe intent, not proof of transport behavior or preflight approval. *)
type attachment_delivery = Upload_attachments | Reuse_session_attachments

(** Requested input-delivery policy for one attempt.

    This record contains attachment metadata, including workspace-relative
    paths and digests, copied from the task specification for in-process
    host/transport inspection. It neither performs preflight/content loading nor
    proves that a transport honored the request. Cabal does not put it into
    normalized task events or rendered execution errors. *)
type attempt_delivery = {
  attachment_references : media_attachment list;
      (** Requested attachment references/digests, never attachment bytes. *)
  attachment_delivery : attachment_delivery;
      (** Whether a transport uploads from the references or reuses session
          media. *)
  web_access_policy : web_access;
      (** Web policy preserved from the caller's task. *)
}

(** Complete telemetry for one actually invoked backend call.

    [number] is 1-based and attempts are stored in invocation order.
    [result] is retained without projection, including its own backend-reported
    elapsed time, cost, session and output fields. [attempt_elapsed] separately
    measures the backend call at the enforcer boundary using the monotonic clock.
    [schema_validation_error] is [Some message] only when this successful
    transport result was actually validated and rejected by the validator. *)
type task_attempt = {
  number : int;  (** 1-based invocation number. *)
  kind : attempt_kind;  (** Initial, fresh retry, or resumed retry. *)
  result : task_result;  (** Complete unmodified backend result. *)
  attempt_elapsed : duration;
      (** Monotonic elapsed time around this backend call. *)
  schema_validation_error : string option;
      (** Validator error for this result, when validation was applicable and
          failed. *)
  delivery : attempt_delivery;
      (** Requested media/web intent exposed to the backend before this call. *)
}

(** Sanitized central sealed-input cleanup telemetry. [Cleanup_not_required]
    means no sealed artifact needed removal (for example, direct use of
    {!Json_schema_enforcer} or an attachment-free central invocation).
    [Cleanup_succeeded] and [Cleanup_failed] report the bounded central cleanup
    outcome without paths or exception details. *)
type cleanup_status =
  | Cleanup_not_required
  | Cleanup_succeeded
  | Cleanup_failed

(** Detailed result of one schema-enforcer invocation.

    [attempts] contains every completed backend call in order. [total_elapsed]
    is measured once around the complete enforcer invocation and is not a sum of
    backend- or attempt-reported durations. [total_cost] aggregates each optional
    cost/token field independently across attempts. [final_session_id] is the
    last non-empty session id returned by any completed attempt. *)
type task_execution = {
  final_result : task_result;
      (** Final projected result. On retry success this is the complete
          successful retry result. *)
  attempts : task_attempt list;  (** Ordered completed backend calls. *)
  total_elapsed : duration;  (** Monotonic wall-clock time for the enforcer. *)
  total_cost : cost option;  (** Field-wise aggregate over attempt costs. *)
  final_session_id : string option;
      (** Last non-empty session id across [attempts]. *)
  cleanup_status : cleanup_status;
      (** Final sanitized cleanup status for central prepared inputs. *)
}

(** Classified failure of the invoked corrective attempt.

    [Schema_validation_failure] is a second validator rejection.
    [Transport_failure] is a non-success backend result. [Resume_failure] is a
    non-success result recognized by the backend as rejection/failure of the
    requested session resume. The complete result remains in the enclosing
    {!task_execution}. *)
type retry_failure =
  | Schema_validation_failure of string
  | Transport_failure of result_status
  | Resume_failure of result_status

(** Structured schema-enforcement failure retaining complete execution data.

    [Native_backend_failure_with_schema] is fail-fast after one native-schema
    backend call. It intentionally does not claim that the backend classified
    the failure as a schema rejection. [Schema_retry_failed] retains the first
    validator error separately from the classified second failure. When the
    second failure is
    [Schema_validation_failure error], both validator strings are therefore
    independently inspectable, while both complete results remain in
    [execution.attempts]. *)
type task_execution_error =
  | Native_backend_failure_with_schema of {
      execution : task_execution;
      message : string;
    }
      (** One native-schema backend call returned [Failed message]. *)
  | Schema_retry_failed of {
      execution : task_execution;
          (** Complete telemetry for both invoked attempts. *)
      attempt_1_validation_error : string;
          (** Validator rejection from the initial attempt. *)
      attempt_2_failure : retry_failure;
          (** Classified failure from the corrective attempt. *)
    }

(** Opaque task request wrapper carrying caller-owned context through backend
    execution. The backend library treats [ctxt] as an uninterpreted value. *)
type 'ctxt task_request = {
  spec : task_spec;  (** Backend task specification to execute. *)
  ctxt : 'ctxt;  (** Caller-owned context passed through unchanged. *)
}

(** Opaque task response wrapper pairing a backend result with caller-owned
    context. The context is not serialized or inspected by the backend. *)
type 'ctxt task_response = {
  result : task_result;  (** Backend execution result. *)
  ctxt : 'ctxt;  (** The same caller-owned context supplied in the request. *)
}

(** {1 Constructors} *)

(** [make_completion_request ~system_prompt ~prompt ()] constructs the stable
    completion DTO. [json_schema], [resume_session_id], and [max_turns] default
    to [None], [attachments] to [[]], [web_access] to [Web_disabled], and
    [timeout] to [max_float], preserving the existing completer/task defaults.
    The constructor performs no I/O or capability checks; those belong to
    central dispatch when the request is submitted. This constructor is the
    canonical host construction path: future optional request fields can be
    added as optional labelled arguments without forcing exhaustive record
    literals to change. *)
val make_completion_request :
  system_prompt:string ->
  prompt:string ->
  ?json_schema:Yojson.Safe.t ->
  ?resume_session_id:string ->
  ?attachments:media_attachment list ->
  ?web_access:web_access ->
  ?timeout:duration ->
  ?max_turns:int ->
  unit ->
  completion_request

(** [make_task_spec ~prompt ~working_dir ()] creates a task specification
    with sensible defaults. The legacy default timeout is [max_float], treated
    by {!Task_runtime} as effectively unbounded.
    Default expected_outputs is [[Files_changed; Structured_report]].

    {pre}
    [prompt] and [working_dir] must be non-empty strings.

    {post}
    Returns a [task_spec] with [prompt] and [working_dir] set and all
    optional fields at their documented defaults, including no attachments and
    [Web_disabled].

    {violators}
    (none)

    {violates}
    (none) *)
val make_task_spec :
  prompt:string ->
  ?instructions:string ->
  ?mcp_servers:mcp_server_config list ->
  ?lsp_servers:lsp_server_config list ->
  working_dir:string ->
  ?timeout:duration ->
  ?expected_outputs:output_spec list ->
  ?attachments:media_attachment list ->
  ?web_access:web_access ->
  ?model:string ->
  ?resume_session_id:string ->
  ?max_turns:int ->
  ?managed_namespace:managed_namespace ->
  ?read_only:bool ->
  ?json_schema:Yojson.Safe.t ->
  unit ->
  task_spec

(** [make_resume_task_spec ~base ~resume_session_id ()] creates a minimal,
    role-neutral task specification for resuming an interrupted backend
    session. The returned spec copies runtime fields from [base] (including
    [working_dir], [timeout], [expected_outputs], [mcp_servers], [model],
    [max_turns], [read_only], [attachments], and [web_access]), replaces the
    prompt with the generic resume instruction, clears [instructions], and sets
    [resume_session_id]. Attachments and web policy are deliberately copied so
    a resumed invocation retains the same caller-approved inputs.

    {pre}
    [resume_session_id] identifies a backend session compatible with
    [base.working_dir].

    {post}
    Returns a [task_spec] that resumes the same task without role-specific
    wording, file preambles, or inline-context claims.

    {violators}
    (none)

    {violates}
    (none) *)
val make_resume_task_spec :
  base:task_spec -> resume_session_id:string -> unit -> task_spec

(** [make_mcp_server_config ~name ~command ()] creates an MCP server config.

    {pre}
    [name] and [command] must be non-empty strings.

    {post}
    Returns an [mcp_server_config] with [name] and [command] set and [args]
    and [env] defaulting to empty lists.

    {violators}
    (none)

    {violates}
    (none) *)
val make_mcp_server_config :
  name:string ->
  command:string ->
  ?args:string list ->
  ?env:(string * string) list ->
  unit ->
  mcp_server_config

(** An empty structured report with all fields as None/empty.

    {pre}
    (none)

    {post}
    Returns a [structured_report] with [verdict = None], all list fields
    empty, and [raw_json = None].

    {violators}
    (none)

    {violates}
    (none) *)
val empty_report : structured_report

(** An empty cost record with all fields as None.

    {pre}
    (none)

    {post}
    Returns a [cost] record with every field set to [None].

    {violators}
    (none)

    {violates}
    (none) *)
val empty_cost : cost

(** [aggregate_costs costs] aggregates optional backend cost records field by
    field. Unknown values are ignored only for their own field: known values in
    that field are summed, while a field for which every attempt is unknown
    remains [None]. The result is [None] only when every input cost record is
    [None]; a present all-unknown record produces [Some empty_cost]. A negative
    integer or float, NaN, or infinite float invalidates only its own field and
    yields [None] for that field. Non-negative integer overflow saturates at
    [max_int], and finite float overflow saturates at [max_float]. Currency and
    token units are inherited unchanged from {!type:cost}. The helper does not
    mutate input records or fabricate values for all-unknown fields. *)
val aggregate_costs : cost option list -> cost option

(** [make_task_result ~status ()] creates a task result with sensible defaults.

    {pre}
    (none)

    {post}
    Returns a [task_result] with [status] set and all optional fields at
    their documented defaults (empty lists, [None] for optional fields,
    [0] for [exit_code]).

    {violators}
    (none)

    {violates}
    (none) *)
val make_task_result :
  status:result_status ->
  ?files_changed:string list ->
  ?report:structured_report ->
  ?elapsed:duration ->
  ?cost:cost ->
  ?stdout:string ->
  ?agent_text:string ->
  ?stderr:string ->
  ?exit_code:int ->
  ?session_id:string ->
  unit ->
  task_result

(** [make_task_request ~spec ~ctxt] creates an opaque task request wrapper.

    {pre}
    (none)

    {post}
    Returns a request containing [spec] and [ctxt] unchanged.

    {violators}
    (none)

    {violates}
    (none) *)
val make_task_request : spec:task_spec -> ctxt:'ctxt -> 'ctxt task_request

(** [make_task_response ~result ~ctxt ()] creates an opaque task response
    wrapper.

    {pre}
    (none)

    {post}
    Returns a response containing [result] and [ctxt] unchanged.

    {violators}
    (none)

    {violates}
    (none) *)
val make_task_response :
  result:task_result -> ctxt:'ctxt -> unit -> 'ctxt task_response
