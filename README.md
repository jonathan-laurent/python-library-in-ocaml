# Writing High-Quality Python Libraries in OCaml

Representation of types:

We add a `[%python_type: (int * string)]` command, which compiles to `Py_Tuple [Py_Atomic Py_Int; Py_Atomic Py_String]`.

and also `type foo = ... [@@deriving python_type]` generates a `python_typedef_for_foo` function.
