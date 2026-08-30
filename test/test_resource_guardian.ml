(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Resource_guardian pure logic (no /proc dependency). *)

module G = Cabal.Resource_guardian

let () =
  Process_test_helper.install_launcher () ;
  Process_test_helper.run_if_requested ()

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

let fresh_marker () =
  let path = Filename.temp_file "cabal-resource-guardian-" ".marker" in
  Sys.remove path ;
  path

let wait_for_file ~clock path =
  let deadline = Eio.Time.now clock +. 2.0 in
  let rec loop () =
    if Sys.file_exists path then ()
    else if Eio.Time.now clock >= deadline then
      Alcotest.failf "timed out waiting for %s" path
    else begin
      Eio.Time.sleep clock 0.02 ;
      loop ()
    end
  in
  loop ()

let wait_for_pid_exit ~clock pid =
  let deadline = Eio.Time.now clock +. 4.0 in
  let rec loop () =
    try
      Unix.kill pid 0 ;
      if Eio.Time.now clock >= deadline then
        Alcotest.failf "target PID %d remained after guardian cleanup" pid
      else begin
        Eio.Time.sleep clock 0.02 ;
        loop ()
      end
    with
    | Unix.Unix_error (Unix.ESRCH, _, _) -> ()
    | Unix.Unix_error (error, _, _) ->
        Alcotest.failf
          "could not probe target PID %d: %s"
          pid
          (Unix.error_message error)
  in
  loop ()

let marker_timestamp path =
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      match float_of_string_opt (String.trim (input_line channel)) with
      | Some timestamp -> timestamp
      | None -> Alcotest.failf "invalid timestamp in %s" path)

let test_guardian_terminates_owned_targets_concurrently () =
  let helper = Unix.realpath Sys.executable_name in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let first_marker = fresh_marker () in
  let second_marker = fresh_marker () in
  let spawn marker =
    Cabal.Process_group.spawn
      ~sw
      ~clock
      ~mgr:(Eio.Stdenv.process_mgr env)
      [helper; "--process-descendant-helper"; "timestamped-child"; marker]
  in
  let first = spawn first_marker in
  let second = spawn second_marker in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun target ->
          try Cabal.Process_group.terminate ~grace_seconds:0.1 ~clock target
          with _ -> ())
        [first; second] ;
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ first_marker;
          first_marker ^ ".pid";
          first_marker ^ ".terminated";
          second_marker;
          second_marker ^ ".pid";
          second_marker ^ ".terminated";
        ])
    (fun () ->
      wait_for_file ~clock first_marker ;
      wait_for_file ~clock second_marker ;
      let guardian =
        G.create {warn_percent = 0; kill_percent = 0; poll_interval_s = 60.0}
      in
      G.register_target guardian first ;
      G.register_target guardian second ;
      let started = Eio.Time.now clock in
      let started_wall = Unix.gettimeofday () in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          try
            G.run guardian ~clock ;
            `Stop_daemon
          with Eio.Cancel.Cancelled _ -> `Stop_daemon) ;
      wait_for_file ~clock (first_marker ^ ".terminated") ;
      wait_for_file ~clock (second_marker ^ ".terminated") ;
      let first_terminated = marker_timestamp (first_marker ^ ".terminated") in
      let second_terminated = marker_timestamp (second_marker ^ ".terminated") in
      Alcotest.(check bool)
        "both owned targets receive TERM before one grace window"
        true
        ( first_terminated -. started_wall < 0.75
          && second_terminated -. started_wall < 0.75 );
      Alcotest.(check bool)
        "owned target TERM delivery is concurrent"
        true
        (abs_float (first_terminated -. second_terminated) < 0.25) ;
      wait_for_pid_exit ~clock (Cabal.Process_group.pid first) ;
      wait_for_pid_exit ~clock (Cabal.Process_group.pid second) ;
      Alcotest.(check bool)
        "owned target cleanup takes one grace window"
        true
        (Eio.Time.now clock -. started < 3.5) ;
      G.unregister_target guardian first ;
      G.unregister_target guardian second ;
      Alcotest.(check (list int))
        "reaped targets are removed before any later guardian action"
        []
        (List.map Cabal.Process_group.pid (G.registered_targets guardian)))

let test_target_registration_is_atomic_and_idempotent () =
  let helper = Unix.realpath Sys.executable_name in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let target =
    Cabal.Process_group.spawn
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~mgr:(Eio.Stdenv.process_mgr env)
      [helper; "--process-descendant-helper"; "sleep"]
  in
  Fun.protect
    ~finally:(fun () ->
      try
        Cabal.Process_group.terminate
          ~grace_seconds:0.1
          ~clock:(Eio.Stdenv.clock env)
          target
      with _ -> ())
    (fun () ->
      let guardian = G.create G.default_config in
      G.register_target guardian target ;
      let domains =
        Array.init 200 (fun _ ->
            Domain.spawn (fun () -> G.register_target guardian target))
      in
      Array.iter Domain.join domains ;
      Alcotest.(check int)
        "one owned target after concurrent duplicate registration"
        1
        (List.length (G.registered_targets guardian)) ;
      G.unregister_target guardian target ;
      Alcotest.(check int)
        "target removal is visible"
        0
        (List.length (G.registered_targets guardian)))

let test_distinct_targets_are_not_conflated_by_identity () =
  let helper = Unix.realpath Sys.executable_name in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spawn () =
    Cabal.Process_group.spawn
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~mgr:(Eio.Stdenv.process_mgr env)
      [helper; "--process-descendant-helper"; "sleep"]
  in
  let first = spawn () in
  let second = spawn () in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun target ->
          try
            Cabal.Process_group.terminate
              ~grace_seconds:0.01
              ~clock:(Eio.Stdenv.clock env)
              target
          with _ -> ())
        [first; second])
    (fun () ->
      let guardian = G.create G.default_config in
      G.register_target guardian first ;
      G.register_target guardian second ;
      G.unregister_target guardian first ;
      match G.registered_targets guardian with
      | [remaining] ->
          Alcotest.(check bool)
            "unregistering one owned handle keeps the other"
            true
            (remaining == second)
      | _ ->
          Alcotest.fail "distinct targets were conflated in guardian registry")

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
          ( "process targets register atomically",
            `Quick,
            test_target_registration_is_atomic_and_idempotent );
           ( "distinct process targets retain separate identities",
             `Quick,
             test_distinct_targets_are_not_conflated_by_identity );
           ( "owned target cleanup is concurrent",
             `Quick,
             test_guardian_terminates_owned_targets_concurrently );
        ] );
    ]
