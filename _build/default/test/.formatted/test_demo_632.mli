(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #632 — Native JSON schema wiring for gemini-cli (AC2(b)).

    This module is a test executable.  It has no public API.

    All test functions are internal to the executable and are not exposed
    through this interface.

    {b Coverage:}
    - AC1: Investigation note exists at the canonical path, cites
           baseline_version 0.38.2, records at least one source URL and an
           accessed-on date (NFR-U3).
    - AC2(b): gemini-cli descriptor has [native_json_schema_output = false]
              (NFR-R1 pinning test).  At baseline [0.38.2], gemini-cli exposes
              no CLI surface for forwarding [generationConfig] or
              [response_schema] into the Gemini API invocation.
    - QG-1: Structural integrity — every built-in backend with
            [native_json_schema_output = true] carries a non-[None]
            [capability_evidence] record (NFR-S1).
    - NFR-U2: Scope isolation — claude-code retains [true]; codex, opencode,
              and copilot-cli retain [false].

    {pre}
    (none — test executable, no preconditions on callers)

    {post}
    (none — test executable, no postconditions on callers)

    {violators}
    (none)

    {violates}
    (none) *)
