(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type semver = {
  major : int;
  minor : int;
  patch : int;
  prerelease : string option;
}

(* Scan [s] from position [i] for a run of ASCII digits.
   Returns [Some (value, next_pos)] or [None] if no digit at [i]. *)
let parse_digits s i =
  let n = String.length s in
  if i >= n then None
  else
    let c = Char.code s.[i] in
    if c < 48 || c > 57 then None
    else begin
      let j = ref i in
      while !j < n && Char.code s.[!j] >= 48 && Char.code s.[!j] <= 57 do
        incr j
      done ;
      Option.map
        (fun value -> (value, !j))
        (int_of_string_opt (String.sub s i (!j - i)))
    end

(* Extract the prerelease identifier after position [pos] in [s].
   Expects a '-' at [pos].  Reads until whitespace, '+', or end-of-string.
   Returns [None] when there is no '-' or the identifier is empty. *)
let extract_prerelease s pos =
  let n = String.length s in
  if pos >= n || s.[pos] <> '-' then None
  else begin
    let i = ref (pos + 1) in
    while
      !i < n
      && s.[!i] <> ' '
      && s.[!i] <> '\t'
      && s.[!i] <> '\n'
      && s.[!i] <> '\r'
      && s.[!i] <> '+'
    do
      incr i
    done ;
    if !i = pos + 1 then None else Some (String.sub s (pos + 1) (!i - pos - 1))
  end

(* Find the first N.N.N triplet in [s].  Scan one character at a time so
   we tolerate any prefix (e.g. "claude ", "v", "gemini-cli/").
   After the patch digits, a '-' prefix starts a prerelease identifier; a
   '+' prefix (no '-') indicates build metadata only (not prerelease). *)
let parse_from_output s =
  let n = String.length s in
  let rec scan i =
    if i >= n then Error "no version string found in output"
    else
      match parse_digits s i with
      | None -> scan (i + 1)
      | Some (major, j) ->
          if j < n && s.[j] = '.' then
            match parse_digits s (j + 1) with
            | None -> scan (i + 1)
            | Some (minor, k) ->
                if k < n && s.[k] = '.' then
                  match parse_digits s (k + 1) with
                  | None -> scan (i + 1)
                  | Some (patch, p) ->
                      let prerelease = extract_prerelease s p in
                      Ok {major; minor; patch; prerelease}
                else scan (i + 1)
          else scan (i + 1)
  in
  scan 0

let of_string s = parse_from_output s

let is_valid_version_string value =
  let valid_component component =
    component <> ""
    && String.for_all (function '0' .. '9' -> true | _ -> false) component
    && Option.is_some (int_of_string_opt component)
  in
  match String.split_on_char '.' value with
  | [major; minor; patch] ->
      valid_component major && valid_component minor && valid_component patch
  | _ -> false

let compare a b =
  let c = Int.compare a.major b.major in
  if c <> 0 then c
  else
    let c = Int.compare a.minor b.minor in
    if c <> 0 then c else Int.compare a.patch b.patch

let is_prerelease v = v.prerelease <> None

let semver_to_string v =
  match v.prerelease with
  | None -> Printf.sprintf "%d.%d.%d" v.major v.minor v.patch
  | Some pre -> Printf.sprintf "%d.%d.%d-%s" v.major v.minor v.patch pre

let check_gate ~descriptor ~installed =
  match of_string descriptor.Backend_registry.baseline_version with
  | Error e ->
      Error
        (Printf.sprintf
           "could not parse baseline version for %s: %s"
           descriptor.Backend_registry.display_name
           e)
  | Ok baseline ->
      if compare installed baseline < 0 then
        Error
          (Printf.sprintf
             "%s %s is below the required baseline %s. Upgrade to %s or later, \
              or bypass the check with --force-backend (unsupported versions \
              may behave unexpectedly)."
             descriptor.Backend_registry.display_name
             (semver_to_string installed)
             descriptor.Backend_registry.baseline_version
             descriptor.Backend_registry.baseline_version)
      else if is_prerelease installed then
        Error
          (Printf.sprintf
             "%s %s is a prerelease version. The stable baseline requires %s \
              or later. Prerelease versions may behave unexpectedly; upgrade \
              to a stable release or bypass the check with --force-backend."
             descriptor.Backend_registry.display_name
             (semver_to_string installed)
             descriptor.Backend_registry.baseline_version)
      else Ok ()
