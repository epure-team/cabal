(* Session-name confinement for the MCP-exposed tmux runtime.

   tmux reads a [-t] argument as a TARGET SPEC, not as a name: it matches by
   prefix, by fnmatch, by session id ([$0]) and by window/pane syntax. Measured
   against tmux 3.7b on an isolated socket, with a live session "victimsecrets":

     capture-pane -t victim   -> exit 0, pane contents returned
     kill-session -t victim   -> exit 0, session killed
     has-session  -t 'victim*'-> exit 0

   Since these argv are built from a name a model supplies over MCP, an
   unchecked name reaches sessions it does not name. Three guards now stand
   between the two, and each is pinned below:

     1. [Live_session.is_valid_name] rejects at the MCP boundary
     2. every [-t] is pinned to exact matching with the [=] prefix
     3. the server runs on its own socket, so the operator's tmux is unreachable

   The point of testing all three is that any one of them alone would let the
   others rot silently. *)

let check_name (name, expected) =
  Alcotest.(check bool)
    (Printf.sprintf "is_valid_name %S" name)
    expected
    (Cabal.Live_session.is_valid_name name)

let test_rejected_names () =
  List.iter check_name
    [
      (* the shapes tmux would read as a target spec *)
      ("$0", false);
      ("%1", false);
      ("a:b", false);
      ("a.b", false);
      ("..", false);
      ("victim*", false);
      ("a b", false);
      ("évil", false);
      (* degenerate *)
      ("", false);
      (String.make 65 'a', false);
      (* legitimate *)
      ("human", true);
      ("cabal-mcp_1", true);
      (String.make 64 'a', true);
    ]

(* A prefix of a live session name is the exact case that was exploited: the
   name itself is well-formed, so validation alone cannot stop it. This is what
   the [=] prefix is for. *)
let test_prefix_of_live_session_is_a_valid_name () =
  Alcotest.(check bool)
    "\"human\" is a well-formed name even though it prefixes \"humanshell\""
    true
    (Cabal.Live_session.is_valid_name "human")

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i =
    i + nn <= nh && (String.sub hay i nn = needle || go (i + 1))
  in
  nn = 0 || go 0

let argv_str argv = String.concat " " argv

(* Guard 2: every argv that carries a [-t] must carry it as [=name]. A plain
   [-t name] here is the vulnerability, so this test fails closed on it. *)
let test_targets_are_exact () =
  let cases =
    [
      ("capture", Cabal.Live_session.capture_argv ~name:"human");
      ("has_session", Cabal.Live_session.has_session_argv ~name:"human");
      ("kill_session", Cabal.Live_session.kill_session_argv ~name:"human");
      ("enter", Cabal.Live_session.enter_argv ~name:"human");
      ("paste_buffer", Cabal.Live_session.paste_buffer_argv ~name:"human");
    ]
  in
  List.iter
    (fun (label, argv) ->
      let rec target_is_exact = function
        | "-t" :: v :: _ -> String.length v > 0 && v.[0] = '='
        | _ :: rest -> target_is_exact rest
        | [] -> Alcotest.failf "%s: no -t argument at all" label
      in
      Alcotest.(check bool)
        (Printf.sprintf "%s pins -t to exact match" label)
        true
        (target_is_exact argv))
    cases

(* Guard 3: the socket. Without [-L], this shares the operator's tmux server. *)
let test_dedicated_socket () =
  List.iter
    (fun (label, argv) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s runs on a dedicated socket" label)
        true
        (contains (argv_str argv) "-L cabal-mcp"))
    [
      ("capture", Cabal.Live_session.capture_argv ~name:"human");
      ("has_session", Cabal.Live_session.has_session_argv ~name:"human");
      ("kill_session", Cabal.Live_session.kill_session_argv ~name:"human");
      ("list_sessions", Cabal.Live_session.list_sessions_argv ());
      ( "new_session",
        Cabal.Live_session.new_session_argv ~name:"human" "sh" );
    ]

(* Session CREATION takes a real name, not a target, so [-s] must NOT be
   prefixed — otherwise the created session is literally called "=human". *)
let test_creation_name_is_not_prefixed () =
  let argv = Cabal.Live_session.new_session_argv ~name:"human" "sh" in
  let rec name_after_s = function
    | "-s" :: v :: _ -> v
    | _ :: rest -> name_after_s rest
    | [] -> Alcotest.fail "new_session_argv has no -s"
  in
  Alcotest.(check string) "-s carries the bare name" "human" (name_after_s argv)

let () =
  Alcotest.run
    "Live_session names"
    [
      ( "validation",
        [
          Alcotest.test_case "rejected and accepted names" `Quick
            test_rejected_names;
          Alcotest.test_case "a prefix of a live session is well-formed" `Quick
            test_prefix_of_live_session_is_a_valid_name;
        ] );
      ( "argv confinement",
        [
          Alcotest.test_case "every -t is an exact target" `Quick
            test_targets_are_exact;
          Alcotest.test_case "every invocation uses the dedicated socket"
            `Quick test_dedicated_socket;
          Alcotest.test_case "-s is not prefixed" `Quick
            test_creation_name_is_not_prefixed;
        ] );
    ]
