(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Yaml_adapter

let ( let* ) = Result.bind

type yaml_adapter_config = config

(* --- Bundled built-in YAML strings ---------------------------------------- *)

module Builtin = struct
  let claude_code = [%blob "adapters/claude-code.yaml"]

  let gemini = [%blob "adapters/gemini.yaml"]

  let copilot = [%blob "adapters/copilot.yaml"]

  let codex = [%blob "adapters/codex.yaml"]

  let opencode = [%blob "adapters/opencode.yaml"]

  let all =
    [
      ("claude-code", claude_code);
      ("gemini", gemini);
      ("copilot", copilot);
      ("codex", codex);
      ("opencode", opencode);
    ]
end

(* --- YAML parsing helpers -------------------------------------------------- *)

let string_field obj key =
  match List.assoc_opt key obj with
  | Some (`String s) -> Ok s
  | Some _ -> Error (Printf.sprintf "field '%s' is not a string" key)
  | None -> Ok ""

let float_field_opt obj key default =
  match List.assoc_opt key obj with
  | Some (`Float f) -> f
  | Some (`String s) -> ( try float_of_string s with _ -> default)
  | _ -> default

(** [env_mappings_field ~source obj key] reads an "env"-style sub-mapping.
    Non-string values are dropped (env vars must be strings on a process
    boundary) but each drop emits a [Diagnostics.warn] tagged with the
    YAML [source] and the offending field, so misconfigured adapters
    don't fail mysteriously at backend invocation time. *)
let env_mappings_field ~source obj key =
  match List.assoc_opt key obj with
  | Some (`O pairs) ->
    List.filter_map
      (fun (k, v) ->
        match v with
        | `String s -> Some (k, s)
        | other ->
          let kind =
            match other with
            | `Bool _ -> "bool"
            | `Float _ -> "number"
            | `O _ -> "mapping"
            | `A _ -> "sequence"
            | `Null -> "null"
            | `String _ -> "string"
          in
          Diagnostics.warn
            "[adapter_loader] %s: ignoring env mapping %S — expected \
             string, got %s"
            source
            k
            kind ;
          None)
      pairs
  | _ -> []

(* --- Validate ------------------------------------------------------------- *)

let validate (cfg : config) =
  if String.length (String.trim cfg.name) = 0 then Error "name"
  else if String.length (String.trim cfg.invocation_command) = 0 then
    Error "invocation_command"
  else if String.length (String.trim cfg.template_set) = 0 then
    Error "template_set"
  else Ok ()

(* --- Load from string ----------------------------------------------------- *)

let load_string ~source s =
  match Yaml.of_string s with
  | Error (`Msg msg) -> Error (Printf.sprintf "YAML parse error: %s" msg)
  | Ok (`O obj) -> (
      let* name = string_field obj "name" in
      let* display_name = string_field obj "display_name" in
      let* invocation_command = string_field obj "invocation_command" in
      let* template_set = string_field obj "template_set" in
      let env_mappings = env_mappings_field ~source obj "env" in
      let timeout_seconds = float_field_opt obj "timeout_seconds" 300.0 in
      let cfg =
        {
          name;
          display_name =
            (if String.length display_name = 0 then name else display_name);
          invocation_command;
          template_set;
          env_mappings;
          timeout_seconds;
          source;
        }
      in
      match validate cfg with
      | Ok () -> Ok cfg
      | Error field -> Error (Printf.sprintf "missing required field: %s" field)
      )
  | Ok _ -> Error "YAML must be a mapping at the top level"

(* --- Load from directory --------------------------------------------------- *)

let read_file_opt path =
  if Sys.file_exists path && not (Sys.is_directory path) then
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let n = in_channel_length ic in
          let buf = Bytes.create n in
          really_input ic buf 0 n ;
          Some (Bytes.to_string buf))
    with _ -> None
  else None

let load_dir dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    let entries = try Array.to_list (Sys.readdir dir) with _ -> [] in
    let yaml_files =
      List.filter
        (fun name ->
          let len = String.length name in
          len > 5
          && String.sub name (len - 5) 5 = ".yaml"
          && not (Sys.is_directory (Filename.concat dir name)))
        entries
    in
    List.map
      (fun filename ->
        let path = Filename.concat dir filename in
        let result =
          match read_file_opt path with
          | None -> Error (Printf.sprintf "cannot read file: %s" path)
          | Some content -> load_string ~source:path content
        in
        (filename, result))
      yaml_files

(* --- Register all ---------------------------------------------------------- *)

let register_all ?project_dir () =
  (* 1. Built-in YAML configs — lowest priority *)
  List.iter
    (fun (_name, content) ->
      match load_string ~source:"builtin" content with
      | Ok cfg ->
          let backend = Yaml_adapter.make_backend cfg in
          Registry.register backend
      | Error msg ->
          Diagnostics.warn
            "[adapter_loader] builtin adapter parse error: %s"
            msg)
    Builtin.all ;
  (* 2. User-global: ~/.cabal/adapters/*.yaml *)
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  if home <> "" then begin
    let global_dir =
      Filename.concat home (Filename.concat ".cabal" "adapters")
    in
    List.iter
      (fun (filename, result) ->
        match result with
        | Ok cfg ->
            let backend = Yaml_adapter.make_backend cfg in
            Registry.register backend
        | Error msg -> Diagnostics.warn "[adapter_loader] %s: %s" filename msg)
      (load_dir global_dir)
  end ;
  (* 3. Project-local: .cabal/adapters/*.yaml — highest priority *)
  match project_dir with
  | Some pd ->
      let local_dir =
        Filename.concat pd (Filename.concat ".cabal" "adapters")
      in
      List.iter
        (fun (filename, result) ->
          match result with
          | Ok cfg ->
              let backend = Yaml_adapter.make_backend cfg in
              Registry.register backend
          | Error msg -> Diagnostics.warn "[adapter_loader] %s: %s" filename msg)
        (load_dir local_dir)
  | None -> ()
