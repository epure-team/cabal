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

(** Evidence record documenting the basis for a backend capability claim.

    Required as [Some _] whenever
    [capabilities.native_json_schema_output = true].  Carries version
    boundaries for drift detection and a link to upstream evidence.  Defined
    here so it is available to [Backend_registry] without a dependency cycle. *)
type capability_evidence = {
  baseline_version : string;
      (** Minimum binary version from which the capability is supported. *)
  tested_at_version : string;
      (** Version at which the capability was last manually verified. *)
  evidence_url : string;
      (** Upstream documentation or changelog link supporting the claim. *)
  notes : string;
      (** Human-readable summary of the evidence. *)
}

(** {1 Task Specification} *)

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

(** [make_task_spec ~prompt ~working_dir ()] creates a task specification
    with sensible defaults. Default timeout is 300 seconds (5 minutes).
    Default expected_outputs is [[Files_changed; Structured_report]].

    {pre}
    [prompt] and [working_dir] must be non-empty strings.

    {post}
    Returns a [task_spec] with [prompt] and [working_dir] set and all
    optional fields at their documented defaults.

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
    [max_turns], and [read_only]), replaces the prompt with the generic resume
    instruction, clears [instructions], and sets [resume_session_id].

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
