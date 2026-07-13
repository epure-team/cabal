(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Stand-in for the real {!Cabal_mcp_impl}, selected by mcp/dune's
    [(select ...)] form when [mcp-kit] isn't available in this workspace
    (e.g. the standalone cabal build/CI). *)

let main () =
  prerr_endline
    "cabal-mcp: built without mcp-kit (not available in this workspace); \
     rebuild with mcp-kit installed, e.g. in the Epure monorepo, to run the \
     MCP server." ;
  exit 1
