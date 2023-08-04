(lang dune 3.3)

(generate_opam_files true)

(authors "Jonathan Laurent")
(maintainers "jonathan.laurent@cs.cmu.edu")

(package
 (name python_library_in_ocaml)
 (allow_empty) ; the project contains no library, just a *.so
 (synopsis "Utilities to write high-quality Python libraries in OCaml")
 (description "")
 (depends
  (pyml (>= 20220905))
  (fmt (>= 0.9))
 ))