(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Story #633 — Native JSON schema wiring for copilot-cli (AC2(b)).

    This module is a test executable.  It has no public API.

    All test functions are internal to the executable and are not exposed
    through this interface.

    {b Coverage:}
    - AC1: Investigation note exists at the canonical path, cites
           baseline_version 1.0.54, records at least one source URL and an
           accessed-on date (NFR-U3).
    - AC2(b): copilot-cli descriptor has [native_json_schema_output = false]
              (NFR-R1 pinning test).  At baseline [1.0.54], copilot-cli exposes
              no CLI surface for forwarding a JSON schema into the GitHub
              Copilot API invocation — no [--json-schema], [--response-format],
              or equivalent flag is documented.  Per D-15, hint-style
              enforcement does not qualify for AC2(a).
    - AC2(b): copilot-cli [capability_evidence = None].
    - QG-1: Structural integrity — every built-in backend with
            [native_json_schema_output = true] carries a non-[None]
            [capability_evidence] record (NFR-S1).
    - NFR-U2: Scope isolation — claude-code retains [true]; codex, opencode,
              and gemini-cli retain [false].

    {pre}
    (none — test executable, no preconditions on callers)

    {post}
    (none — test executable, no postconditions on callers)

    {violators}
    (none)

    {violates}
    (none) *)
