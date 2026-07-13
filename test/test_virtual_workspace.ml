(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let contains haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let check_error name = function
  | Ok _ -> Alcotest.fail (name ^ ": expected Error")
  | Error _ -> ()

let test_render_manifest_excludes_window_content () =
  let open Cabal.Virtual_workspace in
  let resource =
    make_resource ~id:"doc-1" ~title:"Board minutes" ~kind:"document"
      ~metadata:[("source", "meeting"); ("classification", "internal")]
      ~windows:
        [ make_window ~citation_id:"doc-1#w1" ~index:1 ~total:1
            ~content:"Sensitive window text" ]
      ()
  in
  match make ~label:"review" ~resources:[resource] () with
  | Error msg -> Alcotest.fail msg
  | Ok workspace ->
    let manifest = render_manifest workspace in
    Alcotest.(check bool) "has id" true (contains manifest "doc-1");
    Alcotest.(check bool) "has boundary" true
      (contains manifest "Cabal does not enforce authorization");
    Alcotest.(check bool) "does not include content" false
      (contains manifest "Sensitive window text")

let test_render_context_bounds_window_and_total () =
  let open Cabal.Virtual_workspace in
  let limits =
    {max_window_chars = 5; max_metadata_value_chars = 20; max_rendered_chars = 1_000}
  in
  let resource =
    make_resource ~id:"r1" ~title:"Transcript" ~kind:"transcript"
      ~windows:
        [make_window ~citation_id:"r1#w1" ~index:1 ~total:1 ~content:"abcdefghij"]
      ()
  in
  match make ~limits ~label:"summary" ~resources:[resource] () with
  | Error msg -> Alcotest.fail msg
  | Ok workspace ->
    let rendered = render_context workspace in
    Alcotest.(check bool) "window is bounded" true (contains rendered "abcde");
    Alcotest.(check bool) "tail removed" false (contains rendered "fghij");
    Alcotest.(check bool) "total bounded" true (String.length rendered <= 1_000)

let test_validation_rejects_invalid_structural_values () =
  let open Cabal.Virtual_workspace in
  check_error "empty label" (make ~label:" " ~resources:[] ());
  check_error "bad limits"
    (make ~limits:{default_limits with max_window_chars = 0} ~label:"x"
       ~resources:[] ());
  let bad_window =
    make_window ~citation_id:"w1" ~index:2 ~total:1 ~content:"text"
  in
  let bad_resource =
    make_resource ~id:"r1" ~title:"Title" ~kind:"document"
      ~windows:[bad_window] ()
  in
  check_error "bad window" (make ~label:"x" ~resources:[bad_resource] ())

let test_split_text_windows_bounds_overlap () =
  let open Cabal.Virtual_workspace in
  Alcotest.(check (list string)) "empty" []
    (split_text_windows ~limit:4 ~overlap:1 "");
  Alcotest.(check (list string)) "short" ["abc"]
    (split_text_windows ~limit:4 ~overlap:1 "abc");
  Alcotest.(check (list string)) "overlap"
    ["abcd"; "defg"; "ghij"]
    (split_text_windows ~limit:4 ~overlap:1 "abcdefghij");
  Alcotest.(check (list string)) "normalizes bad limit"
    ["a"; "b"; "c"]
    (split_text_windows ~limit:0 ~overlap:99 "abc")

let test_collect_resource_windows_reads_until_done () =
  let open Cabal.Virtual_workspace in
  let calls = ref [] in
  let read_window request =
    calls := request :: !calls;
    match request.window_index with
    | 1 -> Ok {citation_id = "r1#w1"; content = "alpha"; has_more = true}
    | 2 -> Ok {citation_id = "r1#w2"; content = "beta"; has_more = false}
    | _ -> Alcotest.fail "unexpected extra reader call"
  in
  let descriptor =
    make_resource_descriptor ~id:"r1" ~title:"Resource" ~kind:"document" ()
  in
  match collect_resource_windows ~read_window descriptor with
  | Error msg -> Alcotest.fail msg
  | Ok (resource : resource) ->
    Alcotest.(check string) "id" "r1" resource.id;
    Alcotest.(check int) "windows" 2 (List.length resource.windows);
    Alcotest.(check int) "first total" 2 (List.hd resource.windows).total;
    Alcotest.(check (list int)) "call order" [1; 2]
      (!calls |> List.rev |> List.map (fun r -> r.window_index))

let test_collect_resource_windows_errors_at_hard_limit () =
  let open Cabal.Virtual_workspace in
  let calls = ref 0 in
  let read_window request =
    incr calls;
    Ok
      {
        citation_id = Printf.sprintf "r1#w%d" request.window_index;
        content = "window";
        has_more = true;
      }
  in
  let limits =
    {read_max_window_chars = 6; read_overlap_chars = 1; max_windows_per_resource = 3}
  in
  let descriptor =
    make_resource_descriptor ~id:"r1" ~title:"Resource" ~kind:"document" ()
  in
  match collect_resource_windows ~limits ~read_window descriptor with
  | Ok _ -> Alcotest.fail "expected hard-limit error"
  | Error msg ->
    Alcotest.(check string) "bounded error" "workspace_window_limit_exceeded" msg;
    Alcotest.(check int) "calls" 3 !calls

let test_collect_resource_windows_rejects_overlong_content () =
  let open Cabal.Virtual_workspace in
  let read_window _request =
    Ok {citation_id = "r1#w1"; content = "too-long"; has_more = false}
  in
  let limits =
    {read_max_window_chars = 3; read_overlap_chars = 1; max_windows_per_resource = 3}
  in
  let descriptor =
    make_resource_descriptor ~id:"r1" ~title:"Resource" ~kind:"document" ()
  in
  match collect_resource_windows ~limits ~read_window descriptor with
  | Ok _ -> Alcotest.fail "expected overlong-content error"
  | Error msg ->
    Alcotest.(check string) "bounded error" "workspace_reader_window_too_large" msg

let test_collect_workspace_propagates_reader_error () =
  let open Cabal.Virtual_workspace in
  let read_window _request = Error "HOST_FREEFORM_FAILURE_PAYLOAD" in
  let descriptor =
    make_resource_descriptor ~id:"r1" ~title:"Resource" ~kind:"document" ()
  in
  match collect_workspace ~label:"workspace" ~descriptors:[descriptor] ~read_window () with
  | Ok _ -> Alcotest.fail "expected reader failure"
  | Error msg ->
    Alcotest.(check bool) "has code" true
      (contains msg "workspace_reader_error:");
    Alcotest.(check bool) "does not expose raw text" false
      (contains msg "HOST_FREEFORM_FAILURE_PAYLOAD")

let test_augment_prompt_keeps_task_after_workspace () =
  let open Cabal.Virtual_workspace in
  let resource =
    make_resource ~id:"r1" ~title:"Evidence" ~kind:"resource"
      ~windows:
        [make_window ~citation_id:"c1" ~index:1 ~total:1 ~content:"secret-window-text"]
      ()
  in
  match make ~label:"analysis" ~resources:[resource] () with
  | Error msg -> Alcotest.fail msg
  | Ok workspace ->
    let prompt = augment_prompt workspace ~prompt:"Produce a synthesis." in
    Alcotest.(check bool) "has context" true
      (contains prompt "# Virtual workspace: analysis");
    Alcotest.(check bool) "has task" true
      (contains prompt "# Task\nProduce a synthesis.")

let test_prepare_completion_adds_workspace_discipline () =
  let open Cabal.Virtual_workspace in
  let resource =
    make_resource ~id:"r1" ~title:"Evidence" ~kind:"resource"
      ~windows:
        [make_window ~citation_id:"c1" ~index:1 ~total:1 ~content:"secret-window-text"]
      ()
  in
  match make ~label:"analysis" ~resources:[resource] () with
  | Error msg -> Alcotest.fail msg
  | Ok workspace ->
    let prepared =
      prepare_completion workspace ~system_prompt:"Be concise."
        ~prompt:"Produce a synthesis."
    in
    Alcotest.(check bool) "keeps caller system" true
      (contains prepared.system_prompt "Be concise.");
    Alcotest.(check bool) "adds citation discipline" true
      (contains prepared.system_prompt "Cite workspace citation ids");
    Alcotest.(check bool) "keeps content out of system" false
      (contains prepared.system_prompt "secret-window-text");
    Alcotest.(check bool) "has context" true
      (contains prepared.prompt "# Virtual workspace: analysis");
    Alcotest.(check bool) "has task" true
      (contains prepared.prompt "# Task\nProduce a synthesis.")

let test_backend_completer_workspace_wrapper_prepares_prompts () =
  let open Cabal.Virtual_workspace in
  let resource =
    make_resource ~id:"r1" ~title:"Evidence" ~kind:"resource"
      ~windows:
        [make_window ~citation_id:"c1" ~index:1 ~total:1 ~content:"secret-window-text"]
      ()
  in
  match make ~label:"analysis" ~resources:[resource] () with
  | Error msg -> Alcotest.fail msg
  | Ok workspace ->
    let captured_system = ref "" in
    let captured_prompt = ref "" in
    let captured_resume = ref None in
    let fake_completer ~system_prompt ~prompt ~json_schema:_ ~resume_session_id =
      captured_system := system_prompt;
      captured_prompt := prompt;
      captured_resume := resume_session_id;
      Ok {Cabal.Backend_completer.text = "ok"; backend_session_id = Some "s1"}
    in
    match
      Cabal.Backend_completer.complete_with_workspace fake_completer ~workspace
        ~system_prompt:"Be precise." ~prompt:"Answer." ~json_schema:None
        ~resume_session_id:(Some "s0")
    with
    | Error msg -> Alcotest.fail msg
    | Ok result ->
      Alcotest.(check string) "result" "ok" result.text;
      Alcotest.(check (option string)) "resume preserved" (Some "s0") !captured_resume;
      Alcotest.(check bool) "system prepared" true
        (contains !captured_system "Cite workspace citation ids");
      Alcotest.(check bool) "prompt prepared" true
        (contains !captured_prompt "# Virtual workspace: analysis")

let test_cabal_workspace_stays_domain_neutral () =
  let candidates =
    [
      "../src/virtual_workspace.ml";
      "src/virtual_workspace.ml";
      "src/cabal/src/virtual_workspace.ml";
    ]
  in
  let file =
    match List.find_opt Sys.file_exists candidates with
    | Some path -> path
    | None -> Alcotest.fail "virtual_workspace.ml source not found"
  in
  let content =
    let ic = open_in file in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  List.iter
    (fun forbidden ->
      Alcotest.(check bool) ("excludes " ^ forbidden) false
        (contains content forbidden))
    [
      "Vigil";
      "PSE";
      "CSE";
      "MEETING_TEAM";
      "user_space_access";
      "deriving show";
      "show_window";
      "show_resource";
      "show_workspace";
      "read_window: string";
    ]

let read_file_if_exists path =
  if Sys.file_exists path then
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> Some (really_input_string ic (in_channel_length ic)))
  else None

let rec find_upward depth relative =
  if depth < 0 then None
  else
    let path = String.concat "/" (List.init depth (fun _ -> "..") @ [relative]) in
    if Sys.file_exists path then Some path else find_upward (depth - 1) relative

let test_cabal_workspace_docs_are_upstream_neutral () =
  let doc_relatives =
    [
      "src/cabal/docs/virtual-workspace.md";
      "docs/virtual-workspace.md";
    ]
  in
  let example_relatives =
    [
      "src/cabal/examples/virtual_workspace_host.ml";
      "examples/virtual_workspace_host.ml";
    ]
  in
  let find_contents relatives =
    relatives
    |> List.filter_map (fun relative -> find_upward 8 relative)
    |> List.sort_uniq String.compare
    |> List.filter_map (fun path ->
      read_file_if_exists path |> Option.map (fun content -> path, content))
  in
  let doc_contents = find_contents doc_relatives in
  let example_contents = find_contents example_relatives in
  let contents =
    doc_contents @ example_contents
  in
  Alcotest.(check bool) "doc found" true (doc_contents <> []);
  Alcotest.(check bool) "example found" true (example_contents <> []);
  List.iter
    (fun (_path, content) ->
      List.iter
        (fun forbidden ->
          Alcotest.(check bool) ("docs exclude " ^ forbidden) false
            (contains content forbidden))
        ["Vigil"; "PSE"; "CSE"; "MEETING_TEAM"; "user_space_access"])
    contents;
  let joined =
    contents |> List.map snd |> String.concat "\n"
  in
  Alcotest.(check bool) "host owns authorization" true
    (contains joined "The host owns authorization");
  Alcotest.(check bool) "cabal does not fetch" true
    (contains joined "Cabal does not fetch resources")

let () =
  Alcotest.run "cabal-virtual-workspace"
    [
      ( "render",
        [
          ("manifest_excludes_window_content", `Quick, test_render_manifest_excludes_window_content);
          ("context_bounds", `Quick, test_render_context_bounds_window_and_total);
          ("augment_prompt", `Quick, test_augment_prompt_keeps_task_after_workspace);
          ("prepare_completion", `Quick, test_prepare_completion_adds_workspace_discipline);
          ("complete_with_workspace", `Quick, test_backend_completer_workspace_wrapper_prepares_prompts);
        ] );
      ( "validation",
        [("invalid_values", `Quick, test_validation_rejects_invalid_structural_values)] );
      ( "reader",
        [
          ("split_text_windows", `Quick, test_split_text_windows_bounds_overlap);
          ("collect_until_done", `Quick, test_collect_resource_windows_reads_until_done);
          ("hard_limit", `Quick, test_collect_resource_windows_errors_at_hard_limit);
          ("overlong_content", `Quick, test_collect_resource_windows_rejects_overlong_content);
          ("reader_error", `Quick, test_collect_workspace_propagates_reader_error);
        ] );
      ( "upstreamability",
        [
          ("domain_neutral", `Quick, test_cabal_workspace_stays_domain_neutral);
          ("docs_neutral", `Quick, test_cabal_workspace_docs_are_upstream_neutral);
        ] );
    ]
