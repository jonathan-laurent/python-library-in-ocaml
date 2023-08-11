type settings = { use_dataclasses : bool }

val generate_py_stub :
  settings:settings ->
  lib_name:string ->
  generated:string ->
  types:Repr.type_declaration list ->
  values:Py.Object.t Repr.value list ->
  string
