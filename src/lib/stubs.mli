type settings = { use_dataclasses : bool }

val generate_py_stub :
  interface_only:bool ->
  settings:settings ->
  lib_name:string ->
  generated:string ->
  types:Repr.type_declaration list ->
  values:Repr.value list ->
  string
