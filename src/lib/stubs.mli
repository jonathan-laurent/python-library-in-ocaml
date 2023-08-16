(** Generate Python stubs from type and value representations. *)

type settings = { use_dataclasses : bool }
(* Stub generation settings. If [use_dataclasses] is set to [true], then a more
   idiomatic encoding of types is used that uses dataclasses. Otherwise, the
   [ppx_python] encoding is used (fastest option). *)

val generate_py_stub :
  interface_only:bool ->
  settings:settings ->
  lib_name:string ->
  generated:string ->
  types:Repr.type_declaration list ->
  values:Repr.value list ->
  string
