(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for [Backend_registry] — Stories #476, #629, #630, #631.

    This module is a test executable.  It has no public API.

    All test functions are internal to the executable and are not exposed
    through this interface.

    {b Coverage:}
    - AC1 (Story #476): All 5 built-in backends have a descriptor.
    - AC2 (Story #476): Exact baseline versions match the reference table.
    - AC3 (Story #476): All required capability flags are present on each
      descriptor.
    - AC4 (Story #476): [backend_supports_file_reading] is routed through
      the registry.
    - AC5 (Story #476): Static-vs-runtime consistency property.
    - AC6 (Epic #95): Native JSON schema output per-backend pinning and
      evidence checks for Stories #629 (claude-code AC2(a) retrofit),
      #630 (codex AC2(b) documented non-support), and #631 (opencode
      AC2(b) documented non-support).  Includes QG-1 structural integrity
      check (NFR-S1) and NFR-U2 scope-isolation check.

    {pre}
    (none — test executable, no preconditions on callers)

    {post}
    (none — test executable, no postconditions on callers)

    {violators}
    (none)

    {violates}
    (none) *)
