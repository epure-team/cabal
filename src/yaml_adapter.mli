(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** YAML-configured agentic backend.

    A yaml adapter implements {!Agentic_backend.S} for any CLI tool configured
    via a YAML file.  It runs [invocation_command] with the assembled prompt
    passed on stdin and returns raw stdout as the response.

    See [adapter_loader] for bulk loading from directories. *)

(** Configuration for a YAML-defined adapter. *)
type config = {
  name : string;  (** Unique backend ID (e.g., ["gemini"]). *)
  display_name : string;  (** Human-readable name (e.g., ["Gemini CLI"]). *)
  invocation_command : string;
      (** Space-separated command to invoke (e.g., ["gemini --yolo"]). *)
  template_set : string;
      (** Template set used for prompt assembly (e.g., ["default"]). *)
  env_mappings : (string * string) list;
      (** Extra environment variables to set before running the command.
          Currently recorded but not yet applied by the backend. *)
  timeout_seconds : float;  (** Maximum wall-clock seconds before SIGTERM. *)
  source : string;
      (** Origin: ["builtin"], user-global path, or project-local path. *)
  models : string list;
      (** Model identifiers declared selectable by this adapter.  An empty list
          means "let the adapter pick its default"; populated lists feed UI
          dropdowns that need to enumerate models per backend. *)
}

(** [make_backend config] creates an {!Agentic_backend.t} from a YAML config.

    - [available] runs [first_word_of_invocation_command --version] and
      returns [true] if the exit code is 0.
    - [run_task] invokes [invocation_command] with the full prompt
      (prompt + instructions) written to stdin and returns raw stdout.

    {pre}
    (none)

    {post}
    Returns an [Agentic_backend.t] wrapping the CLI tool described by [config].

    {violators}
    (none)

    {violates}
    (none) *)
val make_backend : config -> Agentic_backend.t

(** [config_of backend] returns the YAML config if [backend] was created via
    {!make_backend}, or [None] for native OCaml backends.

    {pre}
    (none)

    {post}
    Returns [Some config] if [backend] was created from a YAML config, [None] for native backends.

    {violators}
    (none)

    {violates}
    (none) *)
val config_of : Agentic_backend.t -> config option

(** [parse_pi_json_events stdout] extracts the assistant's answer from Pi's
    NDJSON [--mode json] stream, keeping text blocks only so a caller never
    receives the reasoning or event stream.

    Exposed for testing: it parses untrusted model output, and a silent parse
    failure here is indistinguishable from a model that answered with nothing.

    {pre}
    [stdout] is arbitrary text. Non-JSON lines and unexpected shapes are
    tolerated, not an error.

    {post}
    Returns the concatenation of every terminal assistant message's text
    blocks, newline-separated across messages. Returns [""] when no such block
    is present -- including when the stream is malformed. Callers must treat
    [""] as absence of an answer; this function does not distinguish it from a
    genuinely empty one.

    {violators}
    (none)

    {violates}
    (none) *)
val parse_pi_json_events : string -> string

(** [parse_pi_session_id stdout] returns the identifier from Pi's session
    header, so an orchestrator can attest the association between its own run
    ledger and Pi's independently stored transcript.

    {pre}
    [stdout] is arbitrary text.

    {post}
    Returns [Some id] for the first [{"type":"session","id":...}] record, or
    [None] when absent or malformed.

    {violators}
    (none)

    {violates}
    (none) *)
val parse_pi_session_id : string -> string option

(** [pi_stream_ended_without_finish_reason result] recognises the transient
    Ollama transport defect that Pi surfaces as a hard failure.

    {pre}
    [result] is a completed [task_result].

    {post}
    True only for a [Failed] result whose output mentions the defect. Any other
    status is false, so a successful run is never retried. The match is on
    output text, so it is sensitive to upstream wording.

    {violators}
    (none)

    {violates}
    (none) *)
val pi_stream_ended_without_finish_reason : Backend_types.task_result -> bool
