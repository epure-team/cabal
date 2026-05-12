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
