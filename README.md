# Writing Python Libraries in OCaml

This library allows generating high-quality Python bindings of OCaml libraries, with type annotations.

## Installation

```sh
opam pin add -y python_library_in_ocaml https://github.com/jonathan-laurent/python-library-in-ocaml.git
```

## Building the example

To build the example:

```sh
dune build
dune install python_library_in_ocaml
cd example
pip install -e .
python -m mylib.test
```