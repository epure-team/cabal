(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Shared JSON helpers for backend CLI adapters.

    These tiny helpers were re-implemented identically in
    [copilot_cli.ml], [gemini_cli.ml], and [opencode_cli.ml]. The actual
    MCP/LSP record shapes remain backend-specific (different CLIs require
    different fields), but the JSON encoding of [(string * string) list]
    as a flat object — used everywhere for [env] mappings — is genuinely
    shared. *)

(** Convenience alias for a flat string→string map (e.g., process env). *)
type json_string_map = (string * string) list

(** [json_string_map_to_yojson m] encodes [m] as a Yojson assoc whose
    every value is a JSON string. *)
val json_string_map_to_yojson : json_string_map -> Yojson.Safe.t

(** [json_string_map_of_yojson j] decodes a JSON object whose every value
    is a string. Returns [Error msg] on the first non-string value found
    (msg includes the offending key) or when [j] is not a JSON object. *)
val json_string_map_of_yojson :
  Yojson.Safe.t -> (json_string_map, string) result
