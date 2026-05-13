(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Two-tier backend system for fast/cheap vs smart/expensive model selection.

    Usage tiers:
    - Fast: Talos chat, quick checks, mechanical validation (e.g., gpt-4o-mini, haiku)
    - Smart: Deep reasoning, strategist personas, architecture review (e.g., opus, o3)

    Tier specs are of the form "backend:model" (e.g., "claude-code:haiku", "codex:gpt-4o-mini").
    If no model is specified, the backend's default is used. *)

(** A tier specification: backend name and optional model. *)
type tier_spec = {backend_name : string; model : string option}

(** Parse a tier spec from a string like "claude-code:haiku" or "codex". *)
let parse_tier_spec s =
  match String.split_on_char ':' s with
  | [backend] -> {backend_name = backend; model = None}
  | [backend; model] -> {backend_name = backend; model = Some model}
  | _ -> {backend_name = s; model = None}

(** Convert tier spec back to string. *)
let tier_spec_to_string spec =
  match spec.model with
  | None -> spec.backend_name
  | Some m -> Printf.sprintf "%s:%s" spec.backend_name m

(** Default tier configurations. *)
let default_fast = {backend_name = "claude-code"; model = Some "haiku"}

let default_smart = {backend_name = "claude-code"; model = Some "sonnet"}

(** Model name mapping: converts abstract tier names to backend-specific models.
    This allows using "fast" or "smart" as model names that get translated. *)
let map_model_for_backend ~backend_name ~model =
  match (backend_name, model) with
  (* Claude Code model aliases *)
  | "claude-code", Some "fast" -> Some "haiku"
  | "claude-code", Some "smart" -> Some "sonnet"
  | "claude-code", Some "deep" -> Some "opus"
  (* Codex/OpenAI model aliases *)
  | "codex", Some "fast" -> Some "gpt-5.1-codex-mini"
  | "codex", Some "smart" -> Some "gpt-5.3-codex"
  | "codex", Some "deep" -> Some "gpt-5.1-codex-max"
  (* Gemini model aliases *)
  | "gemini-cli", Some "fast" -> Some "gemini-1.5-flash"
  | "gemini-cli", Some "smart" -> Some "gemini-1.5-pro"
  (* Pass through other models unchanged *)
  | _, m -> m

(** Get the effective model for a tier spec. *)
let effective_model spec =
  map_model_for_backend ~backend_name:spec.backend_name ~model:spec.model

(** Global tier configuration, set at startup. *)
let fast_tier = ref default_fast

let smart_tier = ref default_smart

(** Initialize tiers from environment or command-line args.
    Call this once at startup. *)
let init ?fast ?smart () =
  let getenv_first names =
    List.find_map (fun n -> Sys.getenv_opt n) names
  in
  (match fast with
  | Some s -> fast_tier := parse_tier_spec s
  | None -> (
      match getenv_first ["CABAL_BACKEND_FAST"; "EPURE_BACKEND_FAST"] with
      | Some s -> fast_tier := parse_tier_spec s
      | None -> ())) ;
  match smart with
  | Some s -> smart_tier := parse_tier_spec s
  | None -> (
      match getenv_first ["CABAL_BACKEND_SMART"; "EPURE_BACKEND_SMART"] with
      | Some s -> smart_tier := parse_tier_spec s
      | None -> ())

(** Get the current fast tier spec. *)
let get_fast () = !fast_tier

(** Get the current smart tier spec. *)
let get_smart () = !smart_tier

(** Convenience: get fast tier backend name. *)
let fast_backend () = !fast_tier.backend_name

(** Convenience: get fast tier model. *)
let fast_model () = effective_model !fast_tier

(** Convenience: get smart tier backend name. *)
let smart_backend () = !smart_tier.backend_name

(** Convenience: get smart tier model. *)
let smart_model () = effective_model !smart_tier
