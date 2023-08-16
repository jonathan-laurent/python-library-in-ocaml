## TODO

- [ ] Global module docstring
- [ ] Compile `unit -> int` function to `() -> int` in Python
- [ ] Implement to_python for arrow types
- [ ] Add tests for arrow types
- [ ] Support arguments like `int * (string -> string)`
- [x] Python DSL to clean up pretty printing and optimize generated code
- [x] Generate *.pyi stub for clarity
- [x] Do not destroy and rebuild identically
- [x] Do not export opaque type declarations
- [x] Add docstrings in stub.py
- [x] Support ppx_import
- [x] Support exporting constants
- [x] Support optional and keyword-only arguments
- [x] Add utilities to build a pyobject and export it manually
- [x] Functional callbacks and arrow types
- [o] Concise variant encoding in `ppx_python`