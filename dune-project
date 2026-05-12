(lang dune 3.13)
(name cabal)
(generate_opam_files true)

(package
 (name cabal)
 (synopsis "Caml Agent Backend Abstraction Library")
 (description
  "Backend-agnostic agent execution primitives, configuration generation, and session logging for OCaml projects.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.13))
  yojson
  (eio (and (>= 0.11) (< 1.0)))
  (eio_posix (and (>= 0.11) (< 1.0)))
  (eio_main (and :with-test (>= 0.11) (< 1.0)))
  yaml
  ppx_deriving
  ppx_deriving_yojson
  ppx_blob
  (alcotest :with-test)
  (conf-git :with-test)
  (bisect_ppx :with-test))
 (license MIT)
 (authors "Epure Team")
 (maintainers "Epure Team")
 (homepage "https://github.com/epure-team/cabal")
 (source (github epure-team/cabal))
 (bug_reports "https://github.com/epure-team/cabal/issues")
 (tags ("agentic" "backend" "abstraction" "cli" "llm"))
 (documentation "https://epure-team.github.io/cabal/"))
