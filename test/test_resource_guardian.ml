(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Resource_guardian pure logic (no /proc dependency). *)

module G = Cabal.Resource_guardian

let test_create_defaults () =
  let g = G.create G.default_config in
  let s = G.current_stats g in
  Alcotest.(check (float 0.01)) "initial memory 0%" 0.0 s.memory_percent ;
  Alcotest.(check (float 0.01)) "initial cpu 0%" 0.0 s.cpu_percent ;
  let h = G.history g in
  Alcotest.(check int) "empty memory history" 0 (List.length h.memory) ;
  Alcotest.(check int) "empty cpu history" 0 (List.length h.cpu)

let test_memory_pressure_below_threshold () =
  let g = G.create {G.default_config with warn_percent = 80} in
  Alcotest.(check bool) "no pressure at 0%" false (G.memory_pressure g)

let test_register_unregister_pid () =
  let g = G.create G.default_config in
  (* Register two PIDs, unregister one — no crash *)
  G.register_pid g 1234 ;
  G.register_pid g 5678 ;
  G.unregister_pid g 1234 ;
  (* Unregister non-existent PID — no crash *)
  G.unregister_pid g 9999 ;
  (* Basic sanity: the guardian is still usable *)
  Alcotest.(check bool) "still works after pid ops" false (G.memory_pressure g)

(** Spawn [n] domains, each registering its index as a PID. After all join,
    the registered list must contain exactly [n] distinct pids.

    Without synchronisation, two domains can both read the same old list
    head and one of their inserts gets lost. *)
let test_concurrent_register_does_not_lose_pids () =
  let g = G.create G.default_config in
  let n = 200 in
  let doms =
    Array.init n (fun i -> Domain.spawn (fun () -> G.register_pid g i))
  in
  Array.iter Domain.join doms ;
  let pids = G.registered_pids g in
  let len = List.length pids in
  Alcotest.(check int) "all concurrent registrations survived" n len ;
  let sorted = List.sort compare pids in
  let expected = List.init n (fun i -> i) in
  Alcotest.(check (list int))
    "every pid is present exactly once"
    expected
    sorted

let test_concurrent_unregister_does_not_leave_phantoms () =
  let g = G.create G.default_config in
  let n = 200 in
  for i = 0 to n - 1 do
    G.register_pid g i
  done ;
  let doms =
    Array.init n (fun i -> Domain.spawn (fun () -> G.unregister_pid g i))
  in
  Array.iter Domain.join doms ;
  Alcotest.(check int)
    "no phantoms after concurrent unregister"
    0
    (List.length (G.registered_pids g))

let () =
  Alcotest.run
    "Resource_guardian"
    [
      ( "core",
        [
          ("create with defaults", `Quick, test_create_defaults);
          ( "no pressure below threshold",
            `Quick,
            test_memory_pressure_below_threshold );
          ("register/unregister PIDs", `Quick, test_register_unregister_pid);
        ] );
      ( "concurrency",
        [
          ( "concurrent register does not lose pids",
            `Quick,
            test_concurrent_register_does_not_lose_pids );
          ( "concurrent unregister leaves no phantoms",
            `Quick,
            test_concurrent_unregister_does_not_leave_phantoms );
        ] );
    ]
