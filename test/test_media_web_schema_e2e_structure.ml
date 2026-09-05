(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Always-on structural guards for the credential-gated CBL-08 E2E proof. *)

open Cabal

let contains text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else loop (offset + 1)
  in
  needle_length = 0 || loop 0

let index_of text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop offset =
    if offset + needle_length > text_length then None
    else if String.sub text offset needle_length = needle then Some offset
    else loop (offset + 1)
  in
  if needle_length = 0 then Some 0 else loop 0

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let read_test_file name =
  if Sys.file_exists name then read_file name
  else read_file (Filename.concat "test" name)

let replace_byte bytes offset value =
  let copy = Bytes.of_string bytes in
  Bytes.set copy offset value ;
  Bytes.unsafe_to_string copy

let test_capability_driven_selection () =
  let descriptors = Backend_registry.all () in
  Alcotest.(check (list string))
    "quarantined Copilot is excluded from default executable E2Es"
    ["claude-code"; "codex"; "opencode"]
    E2e_harness_config.all_backend_ids ;
  let media =
    E2e_harness_config.media_schema_descriptors ~descriptors ()
  in
  Alcotest.(check (list string))
    "all positive media transports"
    ["codex"]
    (E2e_harness_config.media_descriptors ~descriptors ()
    |> List.map (fun (d : Backend_registry.descriptor) -> d.id)) ;
  Alcotest.(check (list string))
    "current positive media/schema matrix"
    ["codex"]
    (List.map (fun (d : Backend_registry.descriptor) -> d.id) media) ;
  let codex =
    match media with
    | [descriptor] -> descriptor
    | _ -> Alcotest.fail "expected exactly one P0 media/schema descriptor"
  in
  Alcotest.(check bool)
    "Codex P0 includes PNG"
    true
    (List.mem Backend_types.Png
       codex.capabilities.media_support.media_types) ;
  Alcotest.(check bool)
    "Codex P0 includes JPEG"
    true
    (List.mem Backend_types.Jpeg
       codex.capabilities.media_support.media_types) ;
  Alcotest.(check bool)
    "Codex P0 uses native schema"
    true
    codex.capabilities.native_json_schema_output ;
  let copilot =
    match Backend_registry.find "copilot-cli" with
    | Some descriptor -> descriptor
    | None -> Alcotest.fail "Copilot descriptor is missing"
  in
  Alcotest.(check bool)
    "Copilot media remains disabled without complete MCP isolation"
    true
    (copilot.capabilities.media_support.media_types = []
    && copilot.capabilities.media_support.evidence = None
    && not copilot.capabilities.native_json_schema_output) ;
  let structured_without_native_schema =
    {
      codex with
      id = "structured-without-native-schema";
      capabilities =
        {
          codex.capabilities with
          structured_output = true;
          native_json_schema_output = false;
          native_json_schema_output_evidence = None;
        };
    }
  in
  Alcotest.(check (list string))
    "generic structured output is not native schema support"
    []
    (E2e_harness_config.media_schema_descriptors
       ~descriptors:[structured_without_native_schema]
       ()
    |> List.map (fun (d : Backend_registry.descriptor) -> d.id)) ;
  let native_without_generic_structured =
    {
      codex with
      id = "native-without-generic-structured";
      capabilities = {codex.capabilities with structured_output = false};
    }
  in
  Alcotest.(check (list string))
    "native schema eligibility does not use generic structured output"
    ["native-without-generic-structured"]
    (E2e_harness_config.media_schema_descriptors
       ~descriptors:[native_without_generic_structured]
       ()
    |> List.map (fun (d : Backend_registry.descriptor) -> d.id)) ;
  let native_without_evidence =
    {
      codex with
      id = "native-without-evidence";
      capabilities =
        {codex.capabilities with native_json_schema_output_evidence = None};
    }
  in
  Alcotest.(check (list string))
    "native schema without evidence is excluded"
    []
    (E2e_harness_config.media_schema_descriptors
       ~descriptors:[native_without_evidence]
       ()
    |> List.map (fun (d : Backend_registry.descriptor) -> d.id)) ;
  let evidence =
    match codex.capabilities.native_json_schema_output_evidence with
    | Some evidence -> evidence
    | None -> Alcotest.fail "Codex native schema evidence is missing"
  in
  let incompatible_draft =
    {
      codex with
      id = "native-with-incompatible-draft";
      capabilities =
        {
          codex.capabilities with
          native_json_schema_output_evidence =
            Some {evidence with json_schema_draft = "7"};
        };
    }
  in
  Alcotest.(check (list string))
    "native schema evidence must match the fixture draft"
    []
    (E2e_harness_config.media_schema_descriptors
       ~descriptors:[incompatible_draft]
       ()
    |> List.map (fun (d : Backend_registry.descriptor) -> d.id)) ;
  let malformed_evidence =
    {
      codex with
      id = "native-with-malformed-evidence";
      capabilities =
        {
          codex.capabilities with
          native_json_schema_output_evidence =
            Some {evidence with tested_at_version = "not-a-version"};
        };
    }
  in
  Alcotest.(check (list string))
    "malformed native evidence is excluded"
    []
    (E2e_harness_config.media_schema_descriptors
       ~descriptors:[malformed_evidence]
       ()
    |> List.map (fun (d : Backend_registry.descriptor) -> d.id)) ;
  let installed_floor =
    match Backend_version.of_string "0.131.0" with
    | Ok version -> version
    | Error _ -> Alcotest.fail "test baseline is invalid"
  in
  let descriptor_baseline =
    match Backend_version.of_string codex.baseline_version with
    | Ok version -> version
    | Error _ -> Alcotest.fail "Codex descriptor baseline is invalid"
  in
  Alcotest.(check bool)
    "Codex media baseline is at least 0.131.0"
    true
    (Backend_version.compare descriptor_baseline installed_floor >= 0) ;
  Alcotest.(check int)
    "no current positive web descriptor"
    0
    (List.length (E2e_harness_config.web_descriptors ~descriptors ()))

let test_filter_and_model_env_are_credential_free () =
  let reads = ref [] in
  let getenv name =
    reads := name :: !reads ;
    match name with
    | "CABAL_E2E_BACKEND" -> Some " codex, claude-code "
    | "CABAL_E2E_MODEL_CODEX" -> None
    | _ -> None
  in
  let selected =
    E2e_harness_config.selected_media_schema_descriptors ~getenv
      ~descriptors:(Backend_registry.all ()) ()
  in
  ignore (E2e_harness_config.model_for_backend ~getenv "codex") ;
  Alcotest.(check (list string))
    "filter retains only capability-positive media/schema backends"
    ["codex"]
    (List.map (fun (d : Backend_registry.descriptor) -> d.id) selected) ;
  List.iter
    (fun name ->
      Alcotest.(check bool)
        "harness configuration never reads credential variables"
        true
        (name = "CABAL_E2E_BACKEND" || name = "CABAL_E2E_MODEL_CODEX"))
    !reads

let test_runtime_binding_mismatch_fails_closed () =
  Registry.clear () ;
  Fun.protect
    ~finally:Registry.clear
    (fun () ->
      match
        Runtime_bootstrap.register_runtime
          ~profile:Runtime_bootstrap.Hardened_builtins
          ()
      with
      | Error _ -> Alcotest.fail "credential-free hardened bootstrap failed"
      | Ok () -> (
          match (Backend_registry.find "codex", Registry.find_entry "codex") with
          | Some descriptor, Some (Registry.Validated entry) ->
              Alcotest.(check bool)
                "matching descriptor/runtime binding"
                true
                (E2e_harness_config.runtime_binding_matches_descriptor descriptor
                   entry) ;
              let mismatched =
                {
                  descriptor with
                  capabilities =
                    {
                      descriptor.capabilities with
                      native_json_schema_output = false;
                      native_json_schema_output_evidence = None;
                    };
                }
              in
              Alcotest.(check bool)
                "native descriptor/runtime mismatch fails closed"
                false
                (E2e_harness_config.runtime_binding_matches_descriptor mismatched
                   entry)
          | _ -> Alcotest.fail "hardened Codex runtime binding is unavailable"))

let test_binary_lookup_and_version_probe_tri_state () =
  let directory = Filename.temp_dir "cabal-cbl08-lookup-" "" in
  Fun.protect
    ~finally:(fun () ->
      Sys.readdir directory
      |> Array.iter (fun name -> Unix.unlink (Filename.concat directory name)) ;
      Unix.rmdir directory)
    (fun () ->
      let write name mode =
        let path = Filename.concat directory name in
        let channel = open_out_bin path in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () -> output_string channel "#!/bin/sh\nexit 0\n") ;
        Unix.chmod path mode ;
        path
      in
      ignore (write "present" 0o700) ;
      ignore (write "not-executable" 0o600) ;
      let path_component_file = write "path-component-file" 0o600 in
      Unix.symlink "missing-target" (Filename.concat directory "dangling") ;
      Unix.symlink "loop" (Filename.concat directory "loop") ;
      let getenv = function "PATH" -> Some directory | _ -> None in
      let check_lookup label expected binary =
        Alcotest.(check bool)
          label true
          (E2e_harness_config.lookup_executable ~getenv binary = expected)
      in
      check_lookup "executable file is present"
        E2e_harness_config.Executable_present "present" ;
      check_lookup "ENOENT is genuinely absent"
        E2e_harness_config.Executable_absent "absent" ;
      Alcotest.(check bool)
        "ENOTDIR path component is genuinely absent"
        true
        (E2e_harness_config.lookup_executable
           ~getenv:(function
             | "PATH" -> Some path_component_file
             | _ -> None)
           "child"
        = E2e_harness_config.Executable_absent) ;
      check_lookup "non-executable file is a lookup failure"
        E2e_harness_config.Executable_lookup_failed "not-executable" ;
      check_lookup "dangling symlink is a lookup failure"
        E2e_harness_config.Executable_lookup_failed "dangling" ;
      check_lookup "symlink loop is a lookup failure"
        E2e_harness_config.Executable_lookup_failed "loop" ;
      Alcotest.(check bool)
        "missing PATH is a lookup failure"
        true
        (E2e_harness_config.lookup_executable ~getenv:(fun _ -> None) "present"
        = E2e_harness_config.Executable_lookup_failed) ;
      Alcotest.(check bool)
        "ENOENT is skippable" true
        (E2e_harness_config.lookup_error_is_absent Unix.ENOENT) ;
      Alcotest.(check bool)
        "ENOTDIR is skippable" true
        (E2e_harness_config.lookup_error_is_absent Unix.ENOTDIR) ;
      Alcotest.(check bool)
        "permission denial is not skippable" false
        (E2e_harness_config.lookup_error_is_absent Unix.EACCES) ;
      Alcotest.(check bool)
        "lookup loop is not skippable" false
        (E2e_harness_config.lookup_error_is_absent Unix.ELOOP) ;
      let descriptor =
        match Backend_registry.find "codex" with
        | Some descriptor -> {descriptor with binary_name = "present"}
        | None -> Alcotest.fail "Codex descriptor is unavailable"
      in
      let expect_probe_failure label expected capture =
        Alcotest.(check bool)
          label true
          (E2e_harness_config.probe_version ~capture descriptor = expected)
      in
      let capture result command =
        Alcotest.(check (list string))
          "version probe uses the present descriptor binary"
          ["present"; "--version"] command ;
        result
      in
      expect_probe_failure "present binary with failed version process"
        E2e_harness_config.Version_probe_failed
        (capture (Error "sanitized")) ;
      expect_probe_failure "present binary with malformed version output"
        E2e_harness_config.Version_output_malformed
        (capture (Ok "not-a-version")) ;
      expect_probe_failure "present binary below baseline"
        E2e_harness_config.Version_gate_rejected
        (capture (Ok "codex-cli 0.1.0")) ;
      match
        E2e_harness_config.probe_version
          ~capture:(capture (Ok "codex-cli 0.131.0"))
          descriptor
      with
      | E2e_harness_config.Version_supported _ -> ()
      | _ -> Alcotest.fail "baseline version probe was rejected")

let test_fixture_schema_and_semantic_marker () =
  let fixtures = Media_web_schema_fixture.all in
  Alcotest.(check int) "one PNG and one JPEG fixture" 2 (List.length fixtures) ;
  List.iter
    (fun (fixture : Media_web_schema_fixture.t) ->
      Alcotest.(check bool)
        "fixture is tiny"
        true
        (String.length fixture.bytes > 0 && String.length fixture.bytes < 20_000) ;
      Alcotest.(check string)
        "fixture digest matches generated bytes"
        fixture.attachment.sha256
        Digestif.SHA256.(to_hex (digest_string fixture.bytes)) ;
      Alcotest.(check int)
        "fixture size metadata matches generated bytes"
        fixture.attachment.size_bytes
        (String.length fixture.bytes) ;
      Alcotest.(check bool)
        "fixture path remains workspace-relative"
        true
        (Filename.is_relative fixture.attachment.path))
    fixtures ;
  let png =
    List.find
      (fun fixture -> fixture.Media_web_schema_fixture.attachment.media_type = Png)
      fixtures
  in
  let jpeg =
    List.find
      (fun fixture -> fixture.Media_web_schema_fixture.attachment.media_type = Jpeg)
      fixtures
  in
  Alcotest.(check string)
    "PNG magic"
    "\x89PNG\r\n\x1a\n"
    (String.sub png.bytes 0 8) ;
  Alcotest.(check string)
    "JPEG start marker"
    "\xff\xd8"
    (String.sub jpeg.bytes 0 2) ;
  Alcotest.(check string)
    "JPEG end marker"
    "\xff\xd9"
    (String.sub jpeg.bytes (String.length jpeg.bytes - 2) 2) ;
  let check_inspected label expected_color
      (fixture : Media_web_schema_fixture.t) =
    match
      Media_web_schema_fixture.inspect_image fixture.attachment.media_type
        fixture.bytes
    with
    | Error _ -> Alcotest.fail (label ^ " fixture inspection failed")
    | Ok semantics ->
        Alcotest.(check int) (label ^ " width") 64 semantics.width ;
        Alcotest.(check int) (label ^ " height") 64 semantics.height ;
        Alcotest.(check string)
          (label ^ " independently inspected color")
          expected_color semantics.dominant_color
  in
  check_inspected "PNG" "blue" png ;
  check_inspected "JPEG" "red" jpeg ;
  Alcotest.(check string)
    "JPEG bytes match the fixed golden digest"
    "86bf3e5ac9402d1e210db8199d7fb4ea42e567cdf8097e2d18d527d0d77ae1e4"
    jpeg.attachment.sha256 ;
  Alcotest.(check int)
    "JPEG bytes match the fixed golden size"
    336 jpeg.attachment.size_bytes ;
  List.iter
    (fun fixture ->
      match Media_web_schema_fixture.validate_fixture_semantics fixture with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "fixture semantic provenance was rejected")
    fixtures ;
  let corrupted_png =
    replace_byte png.bytes (String.length png.bytes - 16) '\x01'
  in
  (match Media_web_schema_fixture.inspect_image Png corrupted_png with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "corrupted PNG passed independent inspection") ;
  let corrupted_jpeg = replace_byte jpeg.bytes 24 '\x01' in
  (match Media_web_schema_fixture.inspect_image Jpeg corrupted_jpeg with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "corrupted JPEG passed golden provenance") ;
  let red_png_bytes = Media_web_schema_fixture.solid_png 225 20 20 in
  let red_png_attachment =
    {
      png.attachment with
      sha256 = Digestif.SHA256.(to_hex (digest_string red_png_bytes));
      size_bytes = String.length red_png_bytes;
    }
  in
  let wrong_png =
    {png with attachment = red_png_attachment; bytes = red_png_bytes}
  in
  (match Media_web_schema_fixture.validate_fixture_semantics wrong_png with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "wrong-color PNG matched blue fixture provenance") ;
  let wrong_jpeg =
    {
      jpeg with
      semantics = {jpeg.semantics with dominant_color = "blue"};
    }
  in
  (match Media_web_schema_fixture.validate_fixture_semantics wrong_jpeg with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "arbitrary JPEG color assignment was accepted") ;
  let plain_prompt =
    Media_web_schema_fixture.prompt_without_native_schema fixtures
  in
  List.iter
    (fun field ->
      Alcotest.(check bool)
        ("plain media prompt names " ^ field) true
        (contains plain_prompt field))
    ["png_dominant_color"; "jpeg_dominant_color"] ;
  List.iter
    (fun answer ->
      Alcotest.(check bool)
        "plain media prompt does not disclose the expected answer" false
        (contains plain_prompt answer))
    ["\"blue\""; "\"red\""] ;
  let expected = Media_web_schema_fixture.expected_document_text fixtures in
  (match
     Json_schema_validator.validate
       ~schema:(Media_web_schema_fixture.schema fixtures)
       ~document:expected
   with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "expected image-derived document violates schema") ;
  (match Media_web_schema_fixture.validate_response fixtures expected with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "expected image-derived response was rejected") ;
  (match
     Media_web_schema_fixture.validate_response fixtures
       ("```json\n" ^ expected ^ "\n```")
   with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "public JSON code fence was not normalized") ;
  let constant_but_wrong =
    {|{"png_dominant_color":"blue","jpeg_dominant_color":"blue"}|}
  in
  (match Media_web_schema_fixture.validate_response fixtures constant_but_wrong with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "constant schema compliance bypassed image semantics")

let test_generated_fixtures_pass_central_input_validation () =
  let working_dir = Filename.temp_dir "cabal-cbl08-structure-" "" in
  Unix.chmod working_dir 0o700 ;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (fixture : Media_web_schema_fixture.t) ->
          let path = Filename.concat working_dir fixture.attachment.path in
          if Sys.file_exists path then Sys.remove path)
        Media_web_schema_fixture.all ;
      Unix.rmdir working_dir)
    (fun () ->
      let attachments =
        Media_web_schema_fixture.materialize ~working_dir
          Media_web_schema_fixture.all
      in
      let spec =
        Backend_types.make_task_spec
          ~prompt:"structural fixture validation"
          ~working_dir
          ~attachments
          ~web_access:Web_disabled
          ~json_schema:
            (Media_web_schema_fixture.schema Media_web_schema_fixture.all)
          ()
      in
      let limits : Task_preflight.limits =
        {
          max_attachments = 2;
          max_file_size_bytes = 20_000;
          max_total_size_bytes = 40_000;
        }
      in
      match Task_preflight.validate_inputs ~limits spec with
      | Ok () -> ()
      | Error _ ->
          Alcotest.fail "generated fixtures failed central input validation")

let result_cost : Backend_types.cost =
  {
    tokens_input = Some 1;
    tokens_output = Some 1;
    cost_usd = None;
    cache_creation_input_tokens = None;
    cache_read_input_tokens = None;
  }

let event seq attempt payload : Task_event.t =
  {seq; attempt; timestamp = Float.of_int seq; payload}

let expect_invalid_tool_lifecycle ?(attempt_numbers = [1]) label events =
  match
    Media_web_schema_e2e_support.validate_tool_lifecycle ~attempt_numbers events
  with
  | Error Media_web_schema_e2e_support.Tool_lifecycle_disagreement -> ()
  | Error _ -> Alcotest.fail (label ^ " produced the wrong trace error")
  | Ok _ -> Alcotest.fail (label ^ " tool lifecycle was accepted")

let test_tool_lifecycle_identity_pairing () =
  let started ?id seq attempt name =
    event seq attempt (Task_event.Tool_started {id; name})
  in
  let finished ?id ?name seq attempt =
    event seq attempt (Task_event.Tool_finished {id; name})
  in
  let bounded events =
    event 0 1 (Attempt_started Initial_attempt)
    :: events @ [event 99 1 (Attempt_finished Attempt_succeeded)]
  in
  let valid =
    bounded
      [
        started ~id:"stable-a" 1 1 "read";
        finished ~id:"stable-a" ~name:"renamed-but-id-is-stable" 2 1;
        started 3 1 "fallback-name";
        finished ~name:"fallback-name" 4 1;
      ]
  in
  (match
     Media_web_schema_e2e_support.validate_tool_lifecycle ~attempt_numbers:[1]
       valid
   with
   | Ok ["read"; "fallback-name"] -> ()
  | Ok _ -> Alcotest.fail "valid tool lifecycle returned the wrong start count"
  | Error _ -> Alcotest.fail "valid stable-id/name fallback pairing was rejected") ;
  expect_invalid_tool_lifecycle "mismatched stable id"
    (bounded
       [
         started ~id:"stable-a" 1 1 "read";
         finished ~id:"stable-b" ~name:"read" 2 1;
       ]) ;
  expect_invalid_tool_lifecycle "mismatched fallback name"
    (bounded [started 1 1 "read"; finished ~name:"write" 2 1]) ;
  expect_invalid_tool_lifecycle "finish before start"
    (bounded [finished ~id:"stable-a" ~name:"read" 1 1]) ;
  expect_invalid_tool_lifecycle ~attempt_numbers:[1; 2] "cross-attempt finish"
    [
      event 0 1 (Attempt_started Initial_attempt);
      event 1 2 (Attempt_started Fresh_attempt);
      started ~id:"stable-a" 2 1 "read";
      finished ~id:"stable-a" ~name:"read" 3 2;
    ] ;
  expect_invalid_tool_lifecycle "duplicate active start"
    (bounded
       [started ~id:"stable-a" 1 1 "read"; started ~id:"stable-a" 2 1 "read"]) ;
  expect_invalid_tool_lifecycle "dangling active tool at terminal"
    [
      event 0 1 (Attempt_started Initial_attempt);
      started ~id:"stable-a" 1 1 "read";
      event 2 1 (Terminal Succeeded);
    ]

let test_attempt_numbering_and_native_contract () =
  let attachments =
    List.map
      (fun fixture -> fixture.Media_web_schema_fixture.attachment)
      Media_web_schema_fixture.all
  in
  let success =
    Backend_types.make_task_result ~status:Success ~agent_text:"{}" ()
  in
  let failed =
    Backend_types.make_task_result ~status:(Failed "sanitized") ~agent_text:"" ()
  in
  let delivery attachment_delivery attachment_references =
    Backend_types.
      {
        attachment_references;
        attachment_delivery;
        web_access_policy = Web_disabled;
      }
  in
  let make_attempt ?(schema_validation_error = None) number kind result delivery =
    Backend_types.
      {
        number;
        kind;
        result;
        attempt_elapsed = 1.0;
        schema_validation_error;
        delivery;
      }
  in
  let upload = delivery Upload_attachments attachments in
  let reuse = delivery Reuse_session_attachments attachments in
  let execution final_result attempts =
    Backend_types.make_task_execution ~final_result ~attempts
      ~cleanup_status:Cleanup_succeeded ()
  in
  let native_attempt = make_attempt 1 Initial_attempt success upload in
  let valid_native = execution success [native_attempt] in
  Alcotest.(check bool)
    "exact native initial attempt"
    true
    (Media_web_schema_e2e_support.valid_attempts ~native:true ~attachments
       valid_native) ;
  let reject_native label attempt final_result =
    Alcotest.(check bool)
      label false
      (Media_web_schema_e2e_support.valid_attempts ~native:true ~attachments
         (execution final_result [attempt]))
  in
  reject_native "native attempt number starts at one"
    {native_attempt with number = 2}
    success ;
  reject_native "native attempt is initial"
    {native_attempt with kind = Fresh_attempt}
    success ;
  reject_native "native attempt uploads attachments"
    {native_attempt with delivery = reuse}
    success ;
  reject_native "native attachment references match"
    {
      native_attempt with
      delivery = delivery Upload_attachments [];
    }
    success ;
  reject_native "native attempt has no local schema rejection"
    {native_attempt with schema_validation_error = Some "sanitized"}
    success ;
  reject_native "native attempt result is final result" native_attempt
    {success with agent_text = "different"} ;
  reject_native "native attempt status succeeded"
    {native_attempt with result = failed}
    failed ;
  let first = make_attempt 1 Initial_attempt failed upload in
  let second = make_attempt 2 Fresh_attempt success upload in
  Alcotest.(check bool)
    "generic retry attempts are contiguous from one"
    true
    (Media_web_schema_e2e_support.valid_attempts ~native:false ~attachments
       (execution success [first; second])) ;
  Alcotest.(check bool)
    "generic retry attempt gap is rejected"
    false
    (Media_web_schema_e2e_support.valid_attempts ~native:false ~attachments
       (execution success [first; {second with number = 3}])) ;
  Alcotest.(check bool)
    "generic retry attempt reordering is rejected"
    false
    (Media_web_schema_e2e_support.valid_attempts ~native:false ~attachments
       (execution success [{first with number = 2}; {second with number = 1}]))

let test_event_trace_contract () =
  let text = {|{"png_dominant_color":"blue","jpeg_dominant_color":"red"}|} in
  let result =
    Backend_types.make_task_result ~status:Success ~agent_text:text
      ~session_id:"opaque-session" ~cost:result_cost ()
  in
  let attempt : Backend_types.task_attempt =
    {
      number = 1;
      kind = Initial_attempt;
      result;
      attempt_elapsed = 1.0;
      schema_validation_error = None;
      delivery =
        {
          attachment_references = [];
          attachment_delivery = Upload_attachments;
          web_access_policy = Web_disabled;
        };
    }
  in
  let execution =
    Backend_types.make_task_execution ~final_result:result ~attempts:[attempt]
      ~cleanup_status:Cleanup_succeeded ()
  in
  let events =
    [
      event 0 0 Task_started;
      event 1 1 (Attempt_started Initial_attempt);
      event 2 1 (Session_id "opaque-session");
      event 3 1 (Agent_text_delta text);
      event 4 1 (Token_usage result_cost);
      event 5 1 (Attempt_finished Attempt_succeeded);
      event 6 1 (Terminal Succeeded);
    ]
  in
  let requirements =
    Media_web_schema_e2e_support.protocol_requirements_for_backend "codex"
  in
  (match
     Media_web_schema_e2e_support.validate_event_trace ~requirements execution
       events
   with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "valid normalized event trace was rejected") ;
  let post_terminal = events @ [event 7 1 (Agent_text_delta "forbidden")] in
  (match
     Media_web_schema_e2e_support.validate_event_trace ~requirements execution
       post_terminal
   with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "post-terminal event was accepted") ;
  let missing_finish =
    List.filter
      (fun event ->
        match event.Task_event.payload with Attempt_finished _ -> false | _ -> true)
      events
  in
  match
    Media_web_schema_e2e_support.validate_event_trace ~requirements execution
      missing_finish
  with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "attempt start/finish disagreement was accepted"

(* Investigation-only regression for historical Copilot observations. It is not
   positive capability evidence: quarantined Copilot remains excluded from the
   CBL-08 descriptor selection and executable E2E run. *)
let test_historical_copilot_observation_requires_two_view_lifecycles () =
  let text = {|{"png_dominant_color":"blue","jpeg_dominant_color":"red"}|} in
  let result =
    Backend_types.make_task_result ~status:Success ~agent_text:text
      ~session_id:"session" ~cost:result_cost ()
  in
  let attempt : Backend_types.task_attempt =
    {
      number = 1;
      kind = Initial_attempt;
      result;
      attempt_elapsed = 1.0;
      schema_validation_error = None;
      delivery =
        {
          attachment_references = [];
          attachment_delivery = Upload_attachments;
          web_access_policy = Web_disabled;
        };
    }
  in
  let execution =
    Backend_types.make_task_execution ~final_result:result ~attempts:[attempt]
      ~cleanup_status:Cleanup_succeeded ()
  in
  let requirements =
    Media_web_schema_e2e_support.protocol_requirements_for_backend "copilot-cli"
  in
  let tool seq id name payload =
    event seq 1
      (payload
         (Task_event.{id = Some id; name}))
  in
  let started seq id name = tool seq id name (fun tool -> Tool_started tool) in
  let finished seq id name =
    event seq 1 (Tool_finished {id = Some id; name = Some name})
  in
  let trace tools =
    [event 0 0 Task_started; event 1 1 (Attempt_started Initial_attempt)]
    @ tools
    @ [
        event 20 1 (Session_id "session");
        event 21 1 (Agent_text_delta text);
        event 22 1 (Token_usage result_cost);
        event 23 1 (Attempt_finished Attempt_succeeded);
        event 24 1 (Terminal Succeeded);
      ]
  in
  let expect_ok events =
    match
      Media_web_schema_e2e_support.validate_event_trace ~requirements execution
        events
    with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "two exact view lifecycles were rejected"
  in
  let expect_rejected label events =
    match
      Media_web_schema_e2e_support.validate_event_trace ~requirements execution
        events
    with
    | Error _ -> ()
    | Ok () ->
        Alcotest.fail
          (label ^ " satisfied the historical Copilot observation contract")
  in
  expect_ok
    (trace
       [
         started 2 "view-1" "view";
         finished 3 "view-1" "view";
         started 4 "view-2" "view";
         finished 5 "view-2" "view";
       ]) ;
  expect_rejected "one view"
    (trace [started 2 "view-1" "view"; finished 3 "view-1" "view"]) ;
  expect_rejected "grep and glob"
    (trace
       [
         started 2 "grep-1" "grep";
         finished 3 "grep-1" "grep";
         started 4 "glob-1" "glob";
         finished 5 "glob-1" "glob";
       ]) ;
  expect_rejected "unexpected extra tool"
    (trace
       [
         started 2 "view-1" "view";
         finished 3 "view-1" "view";
         started 4 "view-2" "view";
         finished 5 "view-2" "view";
         started 6 "glob-1" "glob";
         finished 7 "glob-1" "glob";
       ])

let test_tool_events_respect_attempt_boundaries () =
  let text = {|{"result":"ok"}|} in
  let result = Backend_types.make_task_result ~status:Success ~agent_text:text () in
  let attempt : Backend_types.task_attempt =
    {
      number = 1;
      kind = Initial_attempt;
      result;
      attempt_elapsed = 1.0;
      schema_validation_error = None;
      delivery =
        {
          attachment_references = [];
          attachment_delivery = Upload_attachments;
          web_access_policy = Web_disabled;
        };
    }
  in
  let execution =
    Backend_types.make_task_execution ~final_result:result ~attempts:[attempt]
      ~cleanup_status:Cleanup_succeeded ()
  in
  let requirements =
    Media_web_schema_e2e_support.protocol_requirements_for_backend "generic"
  in
  let started seq attempt =
    event seq attempt (Task_event.Tool_started {id = Some "tool-a"; name = "read"})
  in
  let finished seq attempt =
    event seq attempt
      (Task_event.Tool_finished {id = Some "tool-a"; name = Some "read"})
  in
  let expect_rejected label events =
    match
      Media_web_schema_e2e_support.validate_event_trace ~requirements execution
        events
    with
    | Error Media_web_schema_e2e_support.Tool_lifecycle_disagreement -> ()
    | Error _ -> Alcotest.fail (label ^ " produced the wrong trace error")
    | Ok () -> Alcotest.fail (label ^ " tool events were accepted")
  in
  expect_rejected "tool before attempt start"
    [
      event 0 0 Task_started;
      started 1 1;
      finished 2 1;
      event 3 1 (Attempt_started Initial_attempt);
      event 4 1 (Agent_text_delta text);
      event 5 1 (Attempt_finished Attempt_succeeded);
      event 6 1 (Terminal Succeeded);
    ] ;
  expect_rejected "tool after attempt finish"
    [
      event 0 0 Task_started;
      event 1 1 (Attempt_started Initial_attempt);
      event 2 1 (Agent_text_delta text);
      event 3 1 (Attempt_finished Attempt_succeeded);
      started 4 1;
      finished 5 1;
      event 6 1 (Terminal Succeeded);
    ] ;
  expect_rejected "tool on nonexistent attempt"
    [
      event 0 0 Task_started;
      event 1 1 (Attempt_started Initial_attempt);
      started 2 2;
      finished 3 2;
      event 4 1 (Agent_text_delta text);
      event 5 1 (Attempt_finished Attempt_succeeded);
      event 6 1 (Terminal Succeeded);
    ]

let test_multi_attempt_tool_trace () =
  let first_result =
    Backend_types.make_task_result ~status:Success ~agent_text:"first" ()
  in
  let final_text = {|{"result":"final"}|} in
  let final_result =
    Backend_types.make_task_result ~status:Success ~agent_text:final_text ()
  in
  let delivery : Backend_types.attempt_delivery =
    {
      attachment_references = [];
      attachment_delivery = Upload_attachments;
      web_access_policy = Web_disabled;
    }
  in
  let first : Backend_types.task_attempt =
    {
      number = 1;
      kind = Initial_attempt;
      result = first_result;
      attempt_elapsed = 1.0;
      schema_validation_error = Some "sanitized";
      delivery;
    }
  in
  let second : Backend_types.task_attempt =
    {
      number = 2;
      kind = Fresh_attempt;
      result = final_result;
      attempt_elapsed = 1.0;
      schema_validation_error = None;
      delivery;
    }
  in
  let execution =
    Backend_types.make_task_execution ~final_result ~attempts:[first; second]
      ~cleanup_status:Cleanup_succeeded ()
  in
  Alcotest.(check bool)
    "generic two-attempt helper accepts contiguous attempts"
    true
    (Media_web_schema_e2e_support.valid_attempts ~native:false ~attachments:[]
       execution) ;
  let tool_started seq attempt =
    event seq attempt
      (Task_event.Tool_started {id = Some "reused-id"; name = "read"})
  in
  let tool_finished seq attempt =
    event seq attempt
      (Task_event.Tool_finished {id = Some "reused-id"; name = Some "read"})
  in
  let events =
    [
      event 0 0 Task_started;
      event 1 1 (Attempt_started Initial_attempt);
      tool_started 2 1;
      tool_finished 3 1;
      event 4 1 (Agent_text_delta "first");
      event 5 1 (Attempt_finished Attempt_succeeded);
      event 6 2 (Attempt_started Fresh_attempt);
      tool_started 7 2;
      tool_finished 8 2;
      event 9 2 (Agent_text_delta final_text);
      event 10 2 (Attempt_finished Attempt_succeeded);
      event 11 2 (Terminal Succeeded);
    ]
  in
  let requirements =
    Media_web_schema_e2e_support.protocol_requirements_for_backend "generic"
  in
  match
    Media_web_schema_e2e_support.validate_event_trace ~requirements execution
      events
  with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "valid two-attempt tool trace was rejected"

let test_e2e_binary_is_credential_gated_and_sequential () =
  let dune = read_test_file "dune" in
  let source = read_test_file "test_media_web_schema_backends.ml" in
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        ("dune contains " ^ needle)
        true
        (contains dune needle))
    [
      "(name test_media_web_schema_backends)";
      "(= %{env:CABAL_E2E_TESTS=0} 1)";
      "(alias e2e-cbl08)";
      "test_media_web_schema_backends.exe";
      "(run ./test_media_web_schema_backends.exe)";
    ] ;
  let alias_stanza =
    match index_of dune "(rule\n (alias e2e)\n" with
    | None -> Alcotest.fail "missing named E2E alias"
    | Some offset -> String.sub dune offset (String.length dune - offset)
  in
  let require_alias_index needle =
    match index_of alias_stanza needle with
    | Some index -> index
    | None -> Alcotest.fail ("missing dune E2E entry: " ^ needle)
  in
  let enforcer = require_alias_index "(run ./test_demo_627.exe)" in
  let native = require_alias_index "(run ./test_native_json_schema_backends.exe)" in
  let media =
    require_alias_index "(run ./test_media_web_schema_backends.exe)"
  in
  Alcotest.(check bool)
    "@e2e runs existing prerequisites before media/schema"
    true
    (enforcer < native && native < media) ;
  let binary_stanza =
    match index_of dune "(name test_media_web_schema_backends)" with
    | None -> Alcotest.fail "missing media/schema E2E stanza"
    | Some offset ->
        let remainder = String.sub dune offset (String.length dune - offset) in
        let length =
          match index_of remainder "\n\n(test\n" with
          | Some length -> length
          | None -> String.length remainder
        in
        String.sub remainder 0 length
  in
  Alcotest.(check bool)
    "the media/schema binary itself is CABAL_E2E_TESTS-gated"
    true
    (contains binary_stanza
       "(enabled_if\n  (= %{env:CABAL_E2E_TESTS=0} 1))") ;
  Alcotest.(check bool)
    "the named E2E alias is inert unless CABAL_E2E_TESTS=1"
    true
    (contains alias_stanza
       "(enabled_if\n  (= %{env:CABAL_E2E_TESTS=0} 1))") ;
  Alcotest.(check bool)
    "live proof uses hardened bootstrap"
    true
    (contains source "Runtime_bootstrap.Hardened_builtins") ;
  Alcotest.(check bool)
    "live proof uses the central task runtime"
    true
    (contains source "Task_runtime.start_task") ;
  Alcotest.(check bool)
    "live schema inclusion uses evidence and compatible draft" true
    (contains source "E2e_harness_config.valid_native_schema_descriptor descriptor") ;
  Alcotest.(check bool)
    "live proof never invokes the low-level backend"
    false
    (contains source "Agentic_backend.run_task") ;
  Alcotest.(check bool)
    "live proof never invokes the low-level schema enforcer"
    false
    (contains source "Json_schema_enforcer.run_task") ;
  Alcotest.(check bool)
    "live proof has no credential environment contract"
    false
    (contains source "ACCESS_TOKEN" || contains source "API_KEY")

let () =
  Alcotest.run
    "CBL-08 structural E2E guards"
    [
      ( "selection",
        [
          Alcotest.test_case "capability-driven media/web matrix" `Quick
            test_capability_driven_selection;
          Alcotest.test_case "filter/model env are credential-free" `Quick
            test_filter_and_model_env_are_credential_free;
          Alcotest.test_case "runtime mismatch fails closed" `Quick
            test_runtime_binding_mismatch_fails_closed;
          Alcotest.test_case "binary lookup and version tri-state" `Quick
            test_binary_lookup_and_version_probe_tri_state;
        ] );
      ( "fixtures and schema",
        [
          Alcotest.test_case "deterministic media-derived response" `Quick
            test_fixture_schema_and_semantic_marker;
          Alcotest.test_case "generated bytes pass central preflight" `Quick
            test_generated_fixtures_pass_central_input_validation;
        ] );
      ( "events",
        [
          Alcotest.test_case "terminal and attempts are consistent" `Quick
            test_event_trace_contract;
          Alcotest.test_case
            "historical Copilot observation requires exactly two view lifecycles"
            `Quick
            test_historical_copilot_observation_requires_two_view_lifecycles;
          Alcotest.test_case "tools stay within attempt boundaries" `Quick
            test_tool_events_respect_attempt_boundaries;
          Alcotest.test_case "multi-attempt tools remain isolated" `Quick
            test_multi_attempt_tool_trace;
          Alcotest.test_case "tool identity lifecycle is paired" `Quick
            test_tool_lifecycle_identity_pairing;
          Alcotest.test_case "attempt numbering and native contract" `Quick
            test_attempt_numbering_and_native_contract;
        ] );
      ( "gating",
        [
          Alcotest.test_case "authenticated binary stays outside standard CI"
            `Quick test_e2e_binary_is_credential_gated_and_sequential;
        ] );
    ]
