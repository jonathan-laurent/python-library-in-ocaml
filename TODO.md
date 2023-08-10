## Planned Features

- [ ] Python DSL to clean up pretty printing and optimize generated code
- [ ] Generate *.pyi stub for clarity
- [ ] Do not destroy and rebuild identically
- [x] Do not export opaque type declarations
- [x] Add docstrings in stub.py
- [x] Support ppx_import
- [ ] Support exporting constants
- [ ] Support optional and keyword-only arguments
- [ ] Compile `unit -> int` function to `() -> int` in Python
- [x] Add utilities to build a pyobject and export it manually
- [ ] Concise variant encoding in `ppx_python`
- [ ] Functional callbacks and arrow types