open Python_libgen.Repr

val atomic_type_ast : loc:Location.t -> atomic_type -> Parsetree.expression

val type_expr_ast : loc:Location.t -> type_expr -> Parsetree.expression

val variant_args_ast : loc:Location.t -> variant_args -> Parsetree.expression

val type_definition_ast :
  loc:Location.t -> type_definition -> Parsetree.expression

val type_declaration_ast :
  loc:Location.t -> type_declaration -> Parsetree.expression

val value_signature_ast :
  loc:Location.t -> value_signature -> Parsetree.expression
