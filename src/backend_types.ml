(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Core types for agentic backend abstraction. *)

(* Duration *)

type duration = float [@@deriving eq, yojson]

let duration_of_seconds s = s

let duration_to_seconds d = d

let pp_duration fmt d = Format.fprintf fmt "%.2fs" d

let show_duration d = Format.asprintf "%a" pp_duration d

(* MCP Configuration *)

type mcp_server_config = {
  name : string;
  command : string;
  args : string list;
  env : (string * string) list;
}
[@@deriving show, eq, yojson]

(* Managed namespace for generated backend-owned artifacts. *)

type managed_namespace = {
  id : string;
  display_name : string;
  config_dir : string;
}
[@@deriving show, eq, yojson]

let default_managed_namespace =
  {id = "cabal"; display_name = "Cabal"; config_dir = ".cabal/backend-config"}

let is_valid_namespace_id id =
  let len = String.length id in
  let valid_char = function
    | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  len > 0 && String.for_all valid_char id

let split_path_segments path = String.split_on_char '/' path

let validate_config_dir config_dir =
  if String.trim config_dir = "" then
    Error "invalid managed namespace: config_dir must be non-empty"
  else if not (Filename.is_relative config_dir) then
    Error "invalid managed namespace: config_dir must be relative"
  else
    let segments = split_path_segments config_dir in
    if
      List.exists
        (fun segment -> segment = "" || segment = "." || segment = "..")
        segments
    then
      Error
        "invalid managed namespace: config_dir must not contain empty, '.', or \
         '..' path segments"
    else Ok ()

let validate_managed_namespace (namespace : managed_namespace) =
  if not (is_valid_namespace_id namespace.id) then
    Error
      "invalid managed namespace: id must match [a-z0-9_-]+ and be non-empty"
  else if String.trim namespace.display_name = "" then
    Error "invalid managed namespace: display_name must be non-empty"
  else validate_config_dir namespace.config_dir

type validated_namespace = managed_namespace

let validate_namespace (ns : managed_namespace) :
    (validated_namespace, string) result =
  match validate_managed_namespace ns with Ok () -> Ok ns | Error e -> Error e

(* Host-provided LSP configuration. *)

type lsp_file_association = {extension : string; language_id : string}
[@@deriving show, eq, yojson]

type lsp_server_config = {
  name : string;
  command : string;
  args : string list;
  file_associations : lsp_file_association list;
}
[@@deriving show, eq, yojson]

(* Capability Evidence *)

type test_method = E2e_test | Manual_probe of string

type capability_evidence = {
  tested_at_version : string;
  json_schema_draft : string;
  test_method : test_method;
}

(* Task Specification *)

type media_type = Png | Jpeg [@@deriving show, eq, yojson]

type media_attachment = {
  id : string;
  path : string;
  media_type : media_type;
  sha256 : string;
  size_bytes : int;
}
[@@deriving show, eq, yojson]

type web_access = Web_disabled | Web_search | Web_search_and_fetch
[@@deriving show, eq, yojson]

type output_spec = Files_changed | Structured_report
[@@deriving show, eq, yojson]

type task_spec = {
  prompt : string;
  instructions : string;
  mcp_servers : mcp_server_config list;
  lsp_servers : lsp_server_config list; [@default []]
  working_dir : string;
  timeout : duration;
  expected_outputs : output_spec list;
  attachments : media_attachment list; [@default []]
  web_access : web_access; [@default Web_disabled]
  managed_namespace : managed_namespace; [@default default_managed_namespace]
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
          Use for validator roles (Reviewer, Critic, Architect) that must
          analyse diffs without being able to compile or patch code. *)
  json_schema : Yojson.Safe.t option; [@default None]
      (** Optional inline JSON Schema document (as a parsed JSON value).
          When [Some schema], the enforcer validates the backend's response
          against [schema] and retries on failure.  When [None], the task
          runs without schema enforcement (pass-through). *)
}
[@@deriving show, eq, yojson]

(* Task Result *)

type result_status = Success | Failed of string | Timeout | Cancelled
[@@deriving show, eq, yojson]

type structured_report = {
  verdict : string option;
  issues : string list;
  questions : string list;
  suggestions : string list;
  raw_json : Yojson.Safe.t option;
}
[@@deriving show, eq, yojson]

type cost = {
  tokens_input : int option;
  tokens_output : int option;
  cost_usd : float option;
  cache_creation_input_tokens : int option; [@default None]
  cache_read_input_tokens : int option; [@default None]
}
[@@deriving show, eq, yojson]

type task_result = {
  status : result_status;
  files_changed : string list;
  report : structured_report option;
  elapsed : duration;
  cost : cost option;
  stdout : string;  (** Raw captured stdout from the client (unparsed). *)
  agent_text : string; [@default ""]
      (** Normalised final agent message text, extracted by the adapter from
          its CLI's output format.  Empty string when no agent message was
          produced or extraction failed.  Hosts should prefer this field over
          [stdout] when they need the agent's response text; [stdout] remains
          available for debugging or backend-specific post-processing. *)
  stderr : string;
  exit_code : int;
  session_id : string option; [@default None]
      (** CLI session ID for resume support (e.g., Claude Code session UUID). *)
}
[@@deriving show, eq, yojson]

type 'ctxt task_request = {spec : task_spec; ctxt : 'ctxt}

type 'ctxt task_response = {result : task_result; ctxt : 'ctxt}

(* Constructors *)

let make_task_spec ~prompt ?(instructions = "") ?(mcp_servers = [])
    ?(lsp_servers = []) ~working_dir ?(timeout = 300.0)
    ?(expected_outputs = [Files_changed; Structured_report]) ?(attachments = [])
    ?(web_access = Web_disabled) ?model ?resume_session_id ?max_turns
    ?(managed_namespace = default_managed_namespace) ?(read_only = false)
    ?json_schema () =
  (match validate_managed_namespace managed_namespace with
  | Ok () -> ()
  | Error msg -> invalid_arg msg) ;
  {
    prompt;
    instructions;
    mcp_servers;
    lsp_servers;
    working_dir;
    timeout;
    expected_outputs;
    attachments;
    web_access;
    managed_namespace;
    model;
    resume_session_id;
    max_turns;
    read_only;
    json_schema;
  }

let generic_resume_prompt =
  "Your previous turn was interrupted unexpectedly. Resume the same task from \
   the current session state. Do not restart from the beginning."

let make_resume_task_spec ~base ~resume_session_id () =
  {
    base with
    prompt = generic_resume_prompt;
    instructions = "";
    resume_session_id = Some resume_session_id;
    attachments = base.attachments;
    web_access = base.web_access;
  }

let make_mcp_server_config ~name ~command ?(args = []) ?(env = []) () =
  {name; command; args; env}

let empty_report =
  {
    verdict = None;
    issues = [];
    questions = [];
    suggestions = [];
    raw_json = None;
  }

let empty_cost =
  {
    tokens_input = None;
    tokens_output = None;
    cost_usd = None;
    cache_creation_input_tokens = None;
    cache_read_input_tokens = None;
  }

let make_task_result ~status ?(files_changed = []) ?report ?(elapsed = 0.0)
    ?cost ?(stdout = "") ?(agent_text = "") ?(stderr = "") ?(exit_code = 0)
    ?session_id () =
  {
    status;
    files_changed;
    report;
    elapsed;
    cost;
    stdout;
    agent_text;
    stderr;
    exit_code;
    session_id;
  }

let make_task_request ~spec ~ctxt = {spec; ctxt}

let make_task_response ~result ~ctxt () = {result; ctxt}
