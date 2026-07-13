(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let read_window_from_authorized_texts authorized_texts request =
  match List.assoc_opt request.Cabal.Virtual_workspace.resource_id authorized_texts with
  | None -> Error "resource_not_found"
  | Some text ->
    let windows =
      Cabal.Virtual_workspace.split_text_windows
        ~limit:request.max_chars
        ~overlap:request.overlap_chars
        text
      |> Array.of_list
    in
    let index = request.window_index - 1 in
    if index < 0 || index >= Array.length windows then Error "window_not_found"
    else
      Ok
        Cabal.Virtual_workspace.
          {
            citation_id =
              Printf.sprintf "%s#window-%d" request.resource_id request.window_index;
            content = windows.(index);
            has_more = request.window_index < Array.length windows;
          }

let build_prompt () =
  let authorized_texts =
    [
      ( "resource-a",
        "Synthetic authorized resource text. The host application performed \
         authorization before this list was built." );
    ]
  in
  let descriptor =
    Cabal.Virtual_workspace.make_resource_descriptor
      ~id:"resource-a"
      ~title:"Synthetic resource"
      ~kind:"example"
      ~metadata:[("source", "host")]
      ()
  in
  let read_window = read_window_from_authorized_texts authorized_texts in
  match
    Cabal.Virtual_workspace.collect_workspace
      ~label:"example workspace"
      ~descriptors:[descriptor]
      ~read_window
      ()
  with
  | Error msg -> Error msg
  | Ok workspace ->
    Ok
      (Cabal.Virtual_workspace.prepare_completion workspace
         ~system_prompt:"Answer with citations."
         ~prompt:"Summarize the supplied resources.")
