# Writing Python Libraries in OCaml

This library allows generating high-quality Python bindings of OCaml libraries, with type annotations.

## Installation

```sh
opam pin add -y python-libgen https://github.com/jonathan-laurent/python-libgen.git
```

## Building the example

To build the example:

```sh
dune build
dune install python-libgen
cd example
pip install -e .
python -m mylib.test
```