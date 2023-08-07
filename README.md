# Writing High-Quality Python Libraries in OCaml

To build the example:

```sh
dune build
dune install python_library_in_ocaml
cd example
pip install -e .
python -m mylib.test
```

## Missing Features

- [ ] Support polymorphic types
- [ ] Support exporting constants
- [ ] Support optional and keyword-only arguments
- [ ] Compile `unit -> int` function to `() -> int` in Python
- [ ] Add utilities to build a pyobject and export it manually
- [ ] Concise variant encoding in `ppx_python`
- [ ] Functional callbacks and arrow types