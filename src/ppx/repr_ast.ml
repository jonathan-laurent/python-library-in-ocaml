open Python_libgen.Repr
open Ppxlib.Ast_builder.Default

let atomic_type_ast ~loc = function
  | Bool -> [%expr Python_libgen.Repr.Bool]
  | Int -> [%expr Python_libgen.Repr.Int]
  | Float -> [%expr Python_libgen.Repr.Float]
  | String -> [%expr Python_libgen.Repr.String]
  | Unit -> [%expr Python_libgen.Repr.Unit]
  | Custom s -> [%expr Python_libgen.Repr.Custom [%e estring ~loc s]]

let rec type_expr_ast ~loc = function
  | Tvar s -> [%expr Python_libgen.Repr.Tvar [%e estring ~loc s]]
  | App (t, args) ->
      [%expr
        Python_libgen.Repr.App
          ( [%e atomic_type_ast ~loc t],
            [%e elist ~loc (List.map (type_expr_ast ~loc) args)] )]
  | Tuple args ->
      [%expr
        Python_libgen.Repr.Tuple
          [%e elist ~loc (List.map (type_expr_ast ~loc) args)]]
  | List arg -> [%expr Python_libgen.Repr.List [%e type_expr_ast ~loc arg]]
  | Array arg -> [%expr Python_libgen.Repr.Array [%e type_expr_ast ~loc arg]]
  | Option arg -> [%expr Python_libgen.Repr.Option [%e type_expr_ast ~loc arg]]

let variant_args_ast ~loc = function
  | Anonymous ts ->
      [%expr
        Python_libgen.Repr.Anonymous
          [%e elist ~loc (List.map (type_expr_ast ~loc) ts)]]
  | Labeled ts ->
      let field (l, t) =
        [%expr [%e estring ~loc l], [%e type_expr_ast ~loc t]]
      in
      [%expr Python_libgen.Repr.Labeled [%e elist ~loc (List.map field ts)]]

let type_definition_ast ~loc = function
  | Alias t -> [%expr Python_libgen.Repr.Alias [%e type_expr_ast ~loc t]]
  | Record fields ->
      let field (l, t) =
        [%expr [%e estring ~loc l], [%e type_expr_ast ~loc t]]
      in
      [%expr Python_libgen.Repr.Record [%e elist ~loc (List.map field fields)]]
  | Enum ctors ->
      [%expr
        Python_libgen.Repr.Enum [%e elist ~loc (List.map (estring ~loc) ctors)]]
  | Variant ctors ->
      let ctor (s, args) =
        [%expr [%e estring ~loc s], [%e variant_args_ast ~loc args]]
      in
      [%expr Python_libgen.Repr.Variant [%e elist ~loc (List.map ctor ctors)]]

let type_declaration_ast ~loc { type_name; type_vars; definition } =
  [%expr
    Python_libgen.Repr.
      {
        type_name = [%e estring ~loc type_name];
        type_vars = [%e elist ~loc (List.map (estring ~loc) type_vars)];
        definition = [%e type_definition_ast ~loc definition];
      }]

let value_signature_ast ~loc = function
  | Constant t -> [%expr Python_libgen.Repr.Constant [%e type_expr_ast ~loc t]]
  | Function { args; ret } ->
      let arg (s, t) = [%expr [%e estring ~loc s], [%e type_expr_ast ~loc t]] in
      [%expr
        Python_libgen.Repr.Function
          {
            args = [%e elist ~loc (List.map arg args)];
            ret = [%e type_expr_ast ~loc ret];
          }]
