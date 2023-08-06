# Autogenerated by python_library_in_ocaml.
# type: ignore

from ctypes import RTLD_LOCAL, PyDLL, c_char_p

from importlib.resources import files, as_file

DLL_NAME = "{LIB_NAME}_ocaml.so"

dll_resource = files("{LIB_NAME}.bin").joinpath(DLL_NAME)
with as_file(dll_resource) as dll_file:
    dll = PyDLL(dll_file, RTLD_LOCAL)

argv_t = c_char_p * 3
argv = argv_t(DLL_NAME.encode("utf-8"), b"register", None)
dll.caml_startup(argv)

import {LIB_NAME}_ocaml