(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type metadata = string * string [@@deriving eq]

type window = {citation_id : string; index : int; total : int; content : string}
[@@deriving eq]

type resource = {
  id : string;
  title : string;
  kind : string;
  metadata : metadata list;
  windows : window list;
}
[@@deriving eq]

type limits = {
  max_window_chars : int;
  max_metadata_value_chars : int;
  max_rendered_chars : int;
}
[@@deriving eq]

type workspace = {
  label : string;
  authorization_boundary : string;
  resources : resource list;
  limits : limits;
}
[@@deriving eq]

type completion_prompt = {system_prompt : string; prompt : string}
[@@deriving eq]

type resource_descriptor = {
  descriptor_id : string;
  descriptor_title : string;
  descriptor_kind : string;
  descriptor_metadata : metadata list;
}
[@@deriving eq]

type read_limits = {
  read_max_window_chars : int;
  read_overlap_chars : int;
  max_windows_per_resource : int;
}
[@@deriving eq]

type read_window_request = {
  resource_id : string;
  window_index : int;
  max_chars : int;
  overlap_chars : int;
}
[@@deriving eq]

type read_window_result = {
  citation_id : string;
  content : string;
  has_more : bool;
}
[@@deriving eq]

let default_limits =
  {
    max_window_chars = 18_000;
    max_metadata_value_chars = 256;
    max_rendered_chars = 56_000;
  }

let default_read_limits =
  {
    read_max_window_chars = 18_000;
    read_overlap_chars = 1_000;
    max_windows_per_resource = 64;
  }

let default_authorization_boundary =
  "Resources are supplied by the host application after its own authorization \
   checks. Cabal does not enforce authorization."

let bounded_prefix ~limit text =
  if limit <= 0 then "" else String.sub text 0 (min limit (String.length text))

let bounded_reader_error = function
  | "" -> "reader_failed"
  | msg ->
      let normalized =
        msg |> String.lowercase_ascii
        |> String.map (function
          | ('a' .. 'z' | '0' .. '9' | '_' | '-') as c -> c
          | _ -> '_')
      in
      bounded_prefix ~limit:64 normalized

let make_window ~citation_id ~index ~total ~content =
  {citation_id; index; total; content}

let make_resource ~id ~title ~kind ?(metadata = []) ~windows () =
  {id; title; kind; metadata; windows}

let make_resource_descriptor ~id ~title ~kind ?(metadata = []) () =
  {
    descriptor_id = id;
    descriptor_title = title;
    descriptor_kind = kind;
    descriptor_metadata = metadata;
  }

let is_blank text = String.trim text = ""

let validate_token ~field value =
  if is_blank value then Error (field ^ " must be non-empty") else Ok ()

let validate_limits limits =
  if limits.max_window_chars <= 0 then Error "max_window_chars must be positive"
  else if limits.max_metadata_value_chars <= 0 then
    Error "max_metadata_value_chars must be positive"
  else if limits.max_rendered_chars <= 0 then
    Error "max_rendered_chars must be positive"
  else Ok ()

let validate_read_limits limits =
  if limits.read_max_window_chars <= 0 then
    Error "read_max_window_chars must be positive"
  else if limits.read_overlap_chars < 0 then
    Error "read_overlap_chars must be non-negative"
  else if limits.read_overlap_chars >= limits.read_max_window_chars then
    Error "read_overlap_chars must be smaller than read_max_window_chars"
  else if limits.max_windows_per_resource <= 0 then
    Error "max_windows_per_resource must be positive"
  else Ok ()

let validate_metadata limits (key, value) =
  match validate_token ~field:"metadata key" key with
  | Error _ as err -> err
  | Ok () ->
      if String.length value > limits.max_metadata_value_chars then
        Error ("metadata value too long for key " ^ key)
      else Ok ()

let rec validate_all f = function
  | [] -> Ok ()
  | item :: rest -> (
      match f item with Ok () -> validate_all f rest | Error _ as err -> err)

let validate_descriptor limits descriptor =
  match validate_token ~field:"resource id" descriptor.descriptor_id with
  | Error _ as err -> err
  | Ok () -> (
      match
        validate_token ~field:"resource title" descriptor.descriptor_title
      with
      | Error _ as err -> err
      | Ok () -> (
          match
            validate_token ~field:"resource kind" descriptor.descriptor_kind
          with
          | Error _ as err -> err
          | Ok () ->
              validate_all
                (validate_metadata limits)
                descriptor.descriptor_metadata))

let validate_window _limits (window : window) =
  match validate_token ~field:"window citation_id" window.citation_id with
  | Error _ as err -> err
  | Ok () ->
      if window.index <= 0 then Error "window index must be positive"
      else if window.total <= 0 then Error "window total must be positive"
      else if window.index > window.total then
        Error "window index must be <= total"
      else Ok ()

let validate_resource limits resource =
  match validate_token ~field:"resource id" resource.id with
  | Error _ as err -> err
  | Ok () -> (
      match validate_token ~field:"resource title" resource.title with
      | Error _ as err -> err
      | Ok () -> (
          match validate_token ~field:"resource kind" resource.kind with
          | Error _ as err -> err
          | Ok () -> (
              match
                validate_all (validate_metadata limits) resource.metadata
              with
              | Error _ as err -> err
              | Ok () -> validate_all (validate_window limits) resource.windows)
          ))

let make ?(authorization_boundary = default_authorization_boundary)
    ?(limits = default_limits) ~label ~resources () =
  match validate_limits limits with
  | Error _ as err -> err
  | Ok () -> (
      match validate_token ~field:"workspace label" label with
      | Error _ as err -> err
      | Ok () -> (
          match
            validate_token
              ~field:"authorization boundary"
              authorization_boundary
          with
          | Error _ as err -> err
          | Ok () -> (
              match validate_all (validate_resource limits) resources with
              | Error _ as err -> err
              | Ok () -> Ok {label; authorization_boundary; resources; limits}))
      )

let split_text_windows ~limit ~overlap text =
  let len = String.length text in
  if len = 0 then []
  else
    let limit = max 1 limit in
    let overlap = max 0 (min overlap (limit - 1)) in
    if len <= limit then [text]
    else
      let rec loop acc start =
        if start >= len then List.rev acc
        else
          let stop = min len (start + limit) in
          let part = String.sub text start (stop - start) in
          if stop >= len then List.rev (part :: acc)
          else loop (part :: acc) (stop - overlap)
      in
      loop [] 0

let collect_resource_windows ?(limits = default_read_limits) ~read_window
    descriptor =
  match validate_read_limits limits with
  | Error _ as err -> err
  | Ok () -> (
      match validate_descriptor default_limits descriptor with
      | Error _ as err -> err
      | Ok () -> (
          let rec loop acc index =
            if index > limits.max_windows_per_resource then
              Error "workspace_window_limit_exceeded"
            else
              let request =
                {
                  resource_id = descriptor.descriptor_id;
                  window_index = index;
                  max_chars = limits.read_max_window_chars;
                  overlap_chars = limits.read_overlap_chars;
                }
              in
              match read_window request with
              | Error msg ->
                  Error ("workspace_reader_error:" ^ bounded_reader_error msg)
              | Ok result -> (
                  match
                    validate_token
                      ~field:"window citation_id"
                      result.citation_id
                  with
                  | Error _ as err -> err
                  | Ok () ->
                      if
                        String.length result.content
                        > limits.read_max_window_chars
                      then Error "workspace_reader_window_too_large"
                      else
                        let window =
                          make_window
                            ~citation_id:result.citation_id
                            ~index
                            ~total:index
                            ~content:result.content
                        in
                        if result.has_more then loop (window :: acc) (index + 1)
                        else
                          let windows = List.rev (window :: acc) in
                          let total = List.length windows in
                          Ok
                            (List.map
                               (fun window -> {window with total})
                               windows))
          in
          match loop [] 1 with
          | Error _ as err -> err
          | Ok windows ->
              Ok
                (make_resource
                   ~id:descriptor.descriptor_id
                   ~title:descriptor.descriptor_title
                   ~kind:descriptor.descriptor_kind
                   ~metadata:descriptor.descriptor_metadata
                   ~windows
                   ())))

let collect_workspace ?authorization_boundary ?(limits = default_limits)
    ?(read_limits = default_read_limits) ~label ~descriptors ~read_window () =
  let rec loop acc = function
    | [] ->
        make ?authorization_boundary ~limits ~label ~resources:(List.rev acc) ()
    | descriptor :: rest -> (
        match
          collect_resource_windows ~limits:read_limits ~read_window descriptor
        with
        | Error _ as err -> err
        | Ok resource -> loop (resource :: acc) rest)
  in
  loop [] descriptors

let render_metadata limits metadata =
  metadata
  |> List.map (fun (key, value) ->
      Printf.sprintf
        "  - %s: %s"
        key
        (bounded_prefix ~limit:limits.max_metadata_value_chars value))
  |> String.concat "\n"

let render_manifest workspace =
  let resources =
    match workspace.resources with
    | [] -> "- No resources supplied."
    | resources ->
        resources
        |> List.map (fun resource ->
            let metadata = render_metadata workspace.limits resource.metadata in
            let metadata =
              if metadata = "" then "" else "\n  metadata:\n" ^ metadata
            in
            Printf.sprintf
              "- [%s] %s (%s), windows=%d%s"
              resource.id
              resource.title
              resource.kind
              (List.length resource.windows)
              metadata)
        |> String.concat "\n"
  in
  Printf.sprintf
    "# Virtual workspace: %s\n\
     Authorization boundary: %s\n\
     Resource manifest:\n\
     %s"
    workspace.label
    workspace.authorization_boundary
    resources

let render_window limits resource window =
  Printf.sprintf
    "### Resource [%s] %s — window %d/%d — citation %s\n%s"
    resource.id
    resource.title
    window.index
    window.total
    window.citation_id
    (bounded_prefix ~limit:limits.max_window_chars window.content)

let render_resource limits resource =
  match resource.windows with
  | [] ->
      Printf.sprintf
        "## Resource [%s] %s\n(no windows supplied)"
        resource.id
        resource.title
  | windows ->
      windows
      |> List.map (render_window limits resource)
      |> String.concat "\n\n"

let render_context workspace =
  let rendered =
    Printf.sprintf
      "%s\n\n%s"
      (render_manifest workspace)
      (workspace.resources
      |> List.map (render_resource workspace.limits)
      |> String.concat "\n\n")
  in
  bounded_prefix ~limit:workspace.limits.max_rendered_chars rendered

let augment_prompt workspace ~prompt =
  Printf.sprintf "%s\n\n# Task\n%s" (render_context workspace) prompt

let workspace_system_instructions =
  "A virtual workspace is supplied in the user prompt. Treat every resource as \
   host-authorized context, but do not assume the agent enforces \
   authorization. Cite workspace citation ids when using resource facts. If \
   the supplied workspace lacks the information needed for the task, say so \
   instead of inventing facts."

let prepare_completion workspace ~system_prompt ~prompt =
  {
    system_prompt =
      Printf.sprintf "%s\n\n%s" system_prompt workspace_system_instructions;
    prompt = augment_prompt workspace ~prompt;
  }
