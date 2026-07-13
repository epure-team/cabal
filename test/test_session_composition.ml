(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Cabal
open Portable_session
open Session_composition

let ev ?ts role text = make_event ?timestamp:ts role text
let texts evs = List.map (fun e -> e.text) evs

let test_dedup () =
  (* "a" and "a " normalize equal for the same role → collapse to one;
     Assistant "a" is a distinct (role, text) key; "b" is kept. *)
  let evs = [ ev User "a"; ev User "a "; ev Assistant "a"; ev User "b" ] in
  let out = dedup evs in
  Alcotest.(check int) "count" 3 (List.length out);
  Alcotest.(check (list string)) "order preserved" [ "a"; "a"; "b" ] (texts out)

let test_reorder () =
  let evs =
    [
      ev ~ts:"2026-01-03" User "c";
      ev ~ts:"2026-01-01" User "a";
      ev User "none";
      ev ~ts:"2026-01-02" User "b";
    ]
  in
  Alcotest.(check (list string))
    "chronological, untimed first" [ "none"; "a"; "b"; "c" ]
    (texts (reorder evs))

let test_merge () =
  let a = [ ev User "a1"; ev User "a2" ] and b = [ ev User "b1" ] in
  let out = merge [ a; b ] in
  Alcotest.(check (list string)) "concatenated" [ "a1"; "a2"; "b1" ] (texts out)

let test_take_drop () =
  let evs = [ ev User "1"; ev User "2"; ev User "3" ] in
  Alcotest.(check int) "take 2" 2 (List.length (take 2 evs));
  Alcotest.(check int) "drop 1" 2 (List.length (drop 1 evs));
  Alcotest.(check int) "take negative clamps to 0" 0 (List.length (take (-5) evs));
  Alcotest.(check int) "drop overflow clamps to 0" 0 (List.length (drop 99 evs))

let test_run_pipeline () =
  let evs = [ ev User "keep"; ev Assistant "drop me"; ev User "keep" ] in
  let stages =
    [
      Filter (fun e -> e.role = User);
      Dedup;
      Compact (fun l -> l @ [ ev System "summary" ]);
    ]
  in
  let out = run stages evs in
  (* filter → 2 user "keep"; dedup → 1; compact appends a system event → 2. *)
  Alcotest.(check int) "count" 2 (List.length out);
  Alcotest.(check bool) "compaction applied" true
    (List.exists (fun e -> e.role = System) out)

let test_run_identity () =
  let evs = [ ev User "x" ] in
  Alcotest.(check (list string)) "empty pipeline is identity" [ "x" ]
    (texts (run [] evs))

let () =
  Alcotest.run "session_composition"
    [
      ( "transforms",
        [
          Alcotest.test_case "dedup" `Quick test_dedup;
          Alcotest.test_case "reorder" `Quick test_reorder;
          Alcotest.test_case "merge" `Quick test_merge;
          Alcotest.test_case "take/drop" `Quick test_take_drop;
        ] );
      ( "pipeline",
        [
          Alcotest.test_case "run with stages" `Quick test_run_pipeline;
          Alcotest.test_case "run identity" `Quick test_run_identity;
        ] );
    ]
