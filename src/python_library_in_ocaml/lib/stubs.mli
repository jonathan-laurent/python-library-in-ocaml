open Repr

val generate_py_stub :
     lib_name:string
  -> generated:string
  -> types:python_type_declaration list
  -> values:python_value list
  -> string

val generate_pyi_stub :
  types:python_type_declaration list -> values:python_value list -> string
