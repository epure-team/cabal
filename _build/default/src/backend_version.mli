(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Backend version parsing, comparison, and baseline gate — Story #477.

    Pure module: no I/O, no subprocess calls.  Suitable for unit testing
    without a live backend binary.

    Baseline versions are read from {!Backend_registry} descriptors so there
    is a single source of truth for each backend's required minimum version. *)

(** {1 Types} *)

(** A parsed semantic version.  [prerelease] holds the hyphen-separated
    pre-release identifier (e.g. ["rc.1"], ["alpha"]) when present, or [None]
    for stable releases and build-metadata-only strings. *)
type semver = {
  major : int;
  minor : int;
  patch : int;
  prerelease : string option;
}

(** {1 Parsing} *)

(** [parse_from_output s] scans [s] for the first occurrence of the pattern
    [N.N.N] (three dot-separated non-negative integers) and returns it as a
    [semver].

    The scan is intentionally loose: it ignores any prefix or suffix, making
    it robust to version strings like ["claude 2.1.117"], ["gemini-cli/0.38.2
    linux/amd64"], [" v1.14.20"], or [" 0.122.0-beta"].

    {pre}
    (none)

    {post}
    Returns [Ok v] when a valid [N.N.N] triplet is found; [Error _] otherwise.

    {violators}
    (none)

    {violates}
    (none) *)
val parse_from_output : string -> (semver, string) result

(** [of_string s] parses [s] as an exact ["major.minor.patch"] version string.
    Accepts an optional leading ["v"] prefix (e.g. ["v2.1.117"]).

    {pre}
    (none)

    {post}
    Equivalent to [parse_from_output s].  Provided as a convenience alias for
    contexts where the input is already a clean version string such as
    {!Backend_registry.descriptor.baseline_version}.

    {violators}
    (none)

    {violates}
    (none) *)
val of_string : string -> (semver, string) result

(** [is_prerelease v] returns [true] when [v.prerelease] is [Some _], i.e.\ the
    version string contained a hyphen-separated pre-release identifier such as
    [-alpha], [-beta], [-rc.1], or [-dev].  Build-metadata-only suffixes
    ([+...]) do not count as prerelease.

    This is the single predicate that both the version gate and tests must use;
    no caller may re-implement the detection via its own regex or string check.

    {pre}
    (none)

    {post}
    [is_prerelease v = (v.prerelease <> None)].

    {violators}
    (none)

    {violates}
    (none) *)
val is_prerelease : semver -> bool

(** {1 Comparison} *)

(** [compare a b] compares two semantic versions lexicographically by
    (major, minor, patch).  Returns a negative integer if [a < b], zero if
    [a = b], and a positive integer if [a > b].

    {pre}
    (none)

    {post}
    Satisfies the standard comparison contract: antisymmetric, transitive,
    and total.

    {violators}
    (none)

    {violates}
    (none) *)
val compare : semver -> semver -> int

(** {1 Baseline Gate} *)

(** [check_gate ~descriptor ~installed] compares [installed] against the
    baseline version recorded in [descriptor].  Returns [Ok ()] when
    [installed >= baseline]; returns [Error msg] with an actionable message
    when [installed < baseline].

    The error message names the backend, the installed version, the required
    baseline, and the [--force-backend] override flag so the user knows
    exactly what to do.

    In addition to the below-baseline check, [check_gate] rejects any
    [installed] version for which {!is_prerelease} returns [true], even if
    [installed] is numerically at or above the baseline.  The error message
    for a prerelease rejection includes the prerelease version string, the
    stable baseline, and the [--force-backend] override hint.

    {pre}
    [descriptor.baseline_version] must be a valid ["N.N.N"] string (guaranteed
    for all built-in descriptors in {!Backend_registry}).

    {post}
    Returns [Ok ()] iff [compare installed baseline >= 0] AND
    [not (is_prerelease installed)].

    {violators}
    (none)

    {violates}
    (none) *)
val check_gate :
  descriptor:Backend_registry.descriptor ->
  installed:semver ->
  (unit, string) result
