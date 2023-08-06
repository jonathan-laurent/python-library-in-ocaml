(lang dune 3.3)

(generate_opam_files true)

(authors "Jonathan Laurent")
(maintainers "jonathan.laurent@cs.cmu.edu")

(package
 (name python_library_in_ocaml)
 (synopsis "Utilities to write high-quality Python libraries in OCaml")
 (description "")
 (depends
  (pyml (>= 20220905))
  (dedent (>= 0.16.0))
  (metaquot (>= 0.5.2))
  (ppxlib (>= 0.30.0))
  (base (>= 0.16.2))
  (ppx_sexp_conv (>= 0.16.0))
  (ppx_let (>= 0.16.0))
 ))