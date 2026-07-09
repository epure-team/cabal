open Cabal
open Portable_session

let test_make_event () =
  let e = make_event User "hi" in
  Alcotest.(check bool) "role" true (e.role = User);
  Alcotest.(check string) "text" "hi" e.text;
  Alcotest.(check bool) "provenance default" true (e.provenance = empty_provenance);
  Alcotest.(check bool) "tool default none" true (e.tool = None);
  Alcotest.(check bool) "model default none" true (e.model = None)

let test_normalized_text () =
  let e = make_event User "  a\t b\n c  " in
  Alcotest.(check string) "collapsed+trimmed" "a b c" (normalized_text e);
  Alcotest.(check string) "empty" "" (normalized_text (make_event User "   "))

let test_yojson_roundtrip () =
  let e =
    make_event ~model:"opus" ~timestamp:"2026-01-01T00:00:00Z"
      ~tool:{ name = "Bash"; input_summary = "ls"; output_summary = "out" }
      Assistant "x"
  in
  match event_of_yojson (event_to_yojson e) with
  | Ok e' -> Alcotest.(check bool) "roundtrip" true (equal_event e e')
  | Error m -> Alcotest.fail m

let () =
  Alcotest.run "portable_session"
    [
      ( "model",
        [
          Alcotest.test_case "make_event defaults" `Quick test_make_event;
          Alcotest.test_case "normalized_text" `Quick test_normalized_text;
          Alcotest.test_case "yojson roundtrip" `Quick test_yojson_roundtrip;
        ] );
    ]
