(lang dune 3.10)
(using dune_site 0.1)

(name python-libgen)
(generate_opam_files true)

(authors "Jonathan Laurent")
(maintainers "jonathan.laurent@cs.cmu.edu")

(package
 (name python-libgen)
 (sites (share templates))
 (source (github "jonathan-laurent/python-libgen"))
 (synopsis "Utilities to write high-quality Python libraries in OCaml")
 (description "")
 (depends
  (base (>= 0.16.2))
  (dedent (>= 0.16.0))
  (pyml (>= 20220905))
  (ppxlib (>= 0.30.0))
  (metaquot (>= 0.5.2))
  (dune-site (>= 3.10.0))
  (ppx_python (>= 0.16.0))
  (ppx_deriving (>= 5.2.1))
  (ppx_sexp_conv (>= 0.16.0))
 ))