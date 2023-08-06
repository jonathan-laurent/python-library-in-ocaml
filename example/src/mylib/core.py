from ctypes import RTLD_LOCAL, PyDLL, c_char_p

from importlib.resources import files, as_file

DLL_NAME = "mylib_ocaml.so"

dll_resource = files("mylib.bin").joinpath(DLL_NAME)
with as_file(dll_resource) as dll_file:
    dll = PyDLL(dll_file, RTLD_LOCAL)

argv_t = c_char_p * 2
argv = argv_t(DLL_NAME.encode("utf-8"), None)
dll.caml_startup(argv)

import mylib_ocaml  # type: ignore

def fact(n):
    return mylib_ocaml.fact(n)

print(fact(10))