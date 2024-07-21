(** Utilities derive type and value representations and write ppx rewriters that
    leverage them. *)

val register_type_declaration_deriver :
  name:string ->
  (loc:Location.t ->
  Python_libgen.Repr.type_declaration ->
  Ppxlib.structure_item list) ->
  unit

val register_value_declaration_expander :
  name:string ->
  (loc:Location.t ->
  rec_flag:Ppxlib.rec_flag ->
  name:string ->
  args:(string * Python_libgen.Repr.arg_kind * Ppxlib.core_type) list ->
  ret:Ppxlib.core_type ->
  signature:Python_libgen.Repr.value_signature ->
  expr:Ppxlib.expression ->
  Ppxlib.structure_item) ->
  unit
