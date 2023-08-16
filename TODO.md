## Planned Features

- [x] Python DSL to clean up pretty printing and optimize generated code
- [x] Generate *.pyi stub for clarity
- [x] Do not destroy and rebuild identically
- [x] Do not export opaque type declarations
- [x] Add docstrings in stub.py
- [x] Support ppx_import
- [x] Support exporting constants
- [ ] Global module docstring
- [x] Support optional and keyword-only arguments
- [ ] Compile `unit -> int` function to `() -> int` in Python
- [x] Add utilities to build a pyobject and export it manually
- [o] Concise variant encoding in `ppx_python`
- [x] Functional callbacks and arrow types
- [ ] Implement to_python for arrow types