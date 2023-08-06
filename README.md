# Writing High-Quality Python Libraries in OCaml

To build the example:

```
dune install python_library_in_ocaml
cd example
pip install -e .
```

## Missing Features

- [ ] Support optional and keyword-only arguments
- [ ] Compile `unit -> int` function to `() -> int` in Python