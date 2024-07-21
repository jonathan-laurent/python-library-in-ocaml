(** Translate runtime values of type representations into their OCaml AST
    representation.

    This module could be autogenerated in the future. *)

open Python_libgen.Repr

val atomic_type_ast : loc:Location.t -> atomic_type -> Ppxlib.expression
val type_expr_ast : loc:Location.t -> type_expr -> Ppxlib.expression
val variant_args_ast : loc:Location.t -> variant_args -> Ppxlib.expression
val type_definition_ast : loc:Location.t -> type_definition -> Ppxlib.expression

val type_declaration_ast :
  loc:Location.t -> type_declaration -> Ppxlib.expression

val value_signature_ast : loc:Location.t -> value_signature -> Ppxlib.expression
