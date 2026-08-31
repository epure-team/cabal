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
  let pi = [%blob "adapters/pi.yaml"]

  let all =
    [
      ("claude-code", claude_code);
      ("gemini", gemini);
      ("copilot", copilot);
      ("codex", codex);
      ("opencode", opencode);
      ("pi", pi);
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

(** [string_list_field ~source obj key] reads an array of strings.  Non-string
    entries are dropped with a [Diagnostics.warn] so a stray bool/number in
    [models:] doesn't break adapter registration.  Returns [[]] when [key] is
    absent or not a sequence. *)
let string_list_field ~source obj key =
  match List.assoc_opt key obj with
  | Some (`A items) ->
      List.filter_map
        (fun item ->
          match item with
          | `String s -> Some s
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
                "[adapter_loader] %s: ignoring %s entry — expected string, got \
                 %s"
                source
                key
                kind ;
              None)
        items
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
      let models = string_list_field ~source obj "models" in
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
          models;
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

let embedded_backends () =
  let rec load loaded = function
    | [] -> Ok (List.rev loaded)
    | (_name, content) :: rest -> (
        match load_string ~source:"builtin" content with
        | Error message -> Error message
        | Ok config -> load (Yaml_adapter.make_backend config :: loaded) rest)
  in
  load [] Builtin.all

(* --- Descriptor/runtime pair registration -------------------------------- *)

let nonempty_text value =
  String.trim value <> ""
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x20 && code <> 0x7f)
       value

let valid_runtime_id value =
  nonempty_text value
  && value = String.trim value
  && String.length value <= 128
  && String.for_all
       (function
         | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' -> true
         | _ -> false)
       value

let conservative_descriptor cfg binary_name : Backend_registry.descriptor =
  {
    id = cfg.name;
    display_name = cfg.display_name;
    binary_name;
    baseline_version = "0.0.0";
    capabilities =
      {
        structured_output = false;
        streaming_output = false;
        session_resume = false;
        mcp_support = Backend_registry.Mcp_none;
        read_only_support = false;
        project_config_surface = Backend_registry.Config_none;
        precedence_confidence = Backend_registry.Low;
        generated_lsp_config = false;
        file_reading = false;
        media_support = {media_types = []; evidence = None};
        web_support = {maximum = Backend_types.Web_disabled; evidence = None};
        native_json_schema_output = false;
        native_json_schema_output_evidence = None;
      };
  }

let builtin_descriptor id =
  List.find_opt
    (fun descriptor -> descriptor.Backend_registry.id = id)
    (Backend_registry.all ())

let validate_builtin_override cfg descriptor binary_name =
  if binary_name <> descriptor.Backend_registry.binary_name then
    Error "binary identity differs from the immutable built-in descriptor"
  else if descriptor.capabilities.session_resume then
    Error "generic YAML cannot satisfy built-in session-resume capability"
  else if descriptor.capabilities.native_json_schema_output then
    Error "generic YAML cannot satisfy built-in native-schema capability"
  else Ok ()

let register_external_config cfg =
  let* binary_name =
    match Yaml_adapter.binary_name cfg with
    | Some binary_name -> Ok binary_name
    | None -> Error "invocation command has no safe binary identity"
  in
  if not (valid_runtime_id cfg.name) then
    Error "adapter id is structurally invalid"
  else if not (nonempty_text cfg.display_name) then
    Error "adapter display name is structurally invalid"
  else
    match builtin_descriptor cfg.name with
    | Some descriptor ->
        let* () = validate_builtin_override cfg descriptor binary_name in
        Registry.register_from_adapter_loader (Yaml_adapter.make_backend cfg) ;
        Ok ()
    | None ->
        let descriptor = conservative_descriptor cfg binary_name in
        let* () =
          match Backend_registry.upsert_yaml_descriptor descriptor with
          | Ok () -> Ok ()
          | Error Backend_registry.Immutable_builtin_descriptor ->
              Error "built-in descriptor is immutable"
          | Error Backend_registry.Descriptor_not_owned_by_yaml_loader ->
              Error "descriptor id is owned by the host"
        in
        Registry.register_from_adapter_loader (Yaml_adapter.make_backend cfg) ;
        Ok ()

let register_external_result filename = function
  | Error message -> Diagnostics.warn "[adapter_loader] %s: %s" filename message
  | Ok cfg -> (
      match register_external_config cfg with
      | Ok () -> ()
      | Error message ->
          Diagnostics.warn "[adapter_loader] %s: %s" filename message)

(* --- Probe runner --------------------------------------------------------- *)

(** [run_probe ~sw ~env backend] invokes the backend's [models_probe] (if any)
    under exception protection and returns [Some (Probe, models)] when the
    probe returned a non-empty list.  Any other outcome — [None] probe,
    [Error _], an exception, or [Ok []] — yields [None] and the caller falls
    back to the static list. *)
let run_probe ~sw ~env backend =
  match Agentic_backend.models_probe backend with
  | None -> None
  | Some probe -> (
      let id = Agentic_backend.id backend in
      match probe ~sw ~env with
      | exception e ->
          Diagnostics.warn
            "[adapter_loader] %s models_probe raised: %s"
            id
            (Printexc.to_string e) ;
          None
      | Error msg ->
          Diagnostics.warn "[adapter_loader] %s models_probe error: %s" id msg ;
          None
      | Ok [] ->
          Diagnostics.warn
            "[adapter_loader] %s models_probe returned empty list; falling \
             back to static"
            id ;
          None
      | Ok models -> Some models)

(** [resolve_probes_for_registered ~sw ~env ()] walks every registered
    backend and, for each one with a [models_probe], publishes the
    probe-resolved view into the registry side table.  Backends without a
    probe (or whose probe falls back) keep the [Static] view that
    [Registry.register] seeded. *)
let resolve_probes_for_registered ~sw ~env () =
  List.iter
    (fun backend ->
      let id = Agentic_backend.id backend in
      match run_probe ~sw ~env backend with
      | Some models -> Registry.set_resolved_models id (models, Registry.Probe)
      | None ->
          Registry.set_resolved_models
            id
            (Agentic_backend.models backend, Registry.Static))
    (Registry.list ())

let resolve_registered_model_probes = resolve_probes_for_registered

(* --- Register all ---------------------------------------------------------- *)

let register_all ?project_dir ?sw ?env () =
  (* 1. Built-in YAML configs — lowest priority *)
  List.iter
    (fun (_name, content) ->
      match load_string ~source:"builtin" content with
      | Ok cfg ->
          let backend = Yaml_adapter.make_backend cfg in
          Registry.register_from_adapter_loader backend
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
      (fun (filename, result) -> register_external_result filename result)
      (load_dir global_dir)
  end ;
  (* 3. Project-local: .cabal/adapters/*.yaml — highest priority *)
  (match project_dir with
  | Some pd ->
      let local_dir =
        Filename.concat pd (Filename.concat ".cabal" "adapters")
      in
      List.iter
        (fun (filename, result) -> register_external_result filename result)
        (load_dir local_dir)
  | None -> ()) ;
  (* 4. Probe layer — when an Eio environment is available, ask each
     backend's [models_probe] for the live model list and cache the
     outcome in the registry.  Without [~sw]/[~env] the static seed from
     [Registry.register] remains in place. *)
  match (sw, env) with
  | Some sw, Some env -> resolve_probes_for_registered ~sw ~env ()
  | _ -> ()
