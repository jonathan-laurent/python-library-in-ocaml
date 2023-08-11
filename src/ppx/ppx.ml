open Base
open Ppxlib
open Ast_builder.Default
module Repr = Python_libgen.Repr

module Renaming = struct
  let snake_case_to_capitalized_camel_case s =
    String.split ~on:'_' s |> List.map ~f:String.capitalize |> String.concat

  let convert_type_name = snake_case_to_capitalized_camel_case
  let convert_constructor_name = snake_case_to_capitalized_camel_case
  let convert_type_var = snake_case_to_capitalized_camel_case

  open Python_libgen.Repr

  let rename_atomic_type = function
    | Custom s -> Custom (convert_type_name s)
    | x -> x

  let rec rename_type_expr = function
    | Var s -> Var (convert_type_var s)
    | App (a, ts) -> App (rename_atomic_type a, List.map ~f:rename_type_expr ts)
    | Tuple ts -> Tuple (List.map ~f:rename_type_expr ts)
    | List t -> List (rename_type_expr t)
    | Array t -> Array (rename_type_expr t)
    | Option t -> Option (rename_type_expr t)

  let rename_variant_args = function
    | Anonymous ts -> Anonymous (List.map ~f:rename_type_expr ts)
    | Labeled fs ->
        Labeled (List.map ~f:(fun (l, t) -> (l, rename_type_expr t)) fs)

  let rename_type_definition = function
    | Record fs ->
        Record (List.map ~f:(fun (l, t) -> (l, rename_type_expr t)) fs)
    | Variant vs ->
        Variant
          (List.map
             ~f:(fun (l, a) ->
               (convert_constructor_name l, rename_variant_args a))
             vs)
    | Alias t -> Alias (rename_type_expr t)
    | Enum vs -> Enum (List.map ~f:convert_constructor_name vs)

  let rename_type_declaration { type_name; type_vars; definition } =
    {
      type_name = convert_type_name type_name;
      type_vars = List.map ~f:convert_type_var type_vars;
      definition = rename_type_definition definition;
    }

  let rename_value_signature = function
    | Constant t -> Constant (rename_type_expr t)
    | Function { args; ret } ->
        Function
          {
            args = List.map ~f:(fun (s, t) -> (s, rename_type_expr t)) args;
            ret = rename_type_expr ret;
          }

  let _rename_value { convert; name; signature } =
    { convert; name; signature = rename_value_signature signature }
end

module Python_docstring = struct
  let expand ~ctxt name docstring =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let docstring = Dedent.string docstring in
    [%stri
      let () =
        Python_libgen.register_python_docstring ~name:[%e estring ~loc name]
          ~docstring:[%e estring ~loc docstring]]

  let extension =
    Extension.V3.declare "python_docstring" Extension.Context.structure_item
      Ast_pattern.(
        pstr
          (pstr_value drop
             (value_binding ~pat:(ppat_var __) ~expr:(estring __) ^:: nil)
          ^:: nil))
      expand
end

let python_type_export ~loc declaration =
  let declaration = Renaming.rename_type_declaration declaration in
  [
    [%stri
      let () =
        Python_libgen.register_python_type
          [%e Repr_ast.type_declaration_ast ~loc declaration]];
  ]

let make_python_function ~loc ~args ~ret ~name =
  (* Py.Callable.of_function ~name:... ~docstring:...
     (fun args ->
       let arg0 = [%of_python] args.(0) in
       ... in
       [%python_of: ...] (f arg0...)) *)
  let n = List.length args in
  let lident s = Loc.make ~loc (Longident.Lident s) in
  assert (n > 0);
  let body =
    let bindings =
      List.mapi args ~f:(fun i (arg_name, arg_type) ->
          value_binding ~loc
            ~pat:(ppat_var ~loc (Loc.make ~loc arg_name))
            ~expr:[%expr [%of_python: [%t arg_type]] args.([%e eint ~loc i])])
    and expr =
      [%expr
        [%python_of: [%t ret]]
          [%e
            pexp_apply ~loc
              (pexp_ident ~loc (lident name))
              (List.map args ~f:(fun (arg_name, _) ->
                   (Nolabel, pexp_ident ~loc (lident arg_name))))]]
    in
    pexp_let ~loc Nonrecursive bindings expr
  in
  [%expr
    Py.Callable.of_function ~name:[%e estring ~loc name] ~docstring:""
      (fun args ->
        if Array.length args <> [%e eint ~loc n] then
          failwith "incorrect number of arguments";
        [%e body])]

let value_binding_no_warn ~loc ~pat ~expr =
  {
    pvb_pat = pat;
    pvb_expr = expr;
    pvb_loc = loc;
    pvb_attributes =
      [
        attribute ~loc ~name:(Loc.make ~loc "warning")
          ~payload:(PStr [%str "-32"]);
      ];
  }

let python_export ~loc ~rec_flag ~name ~args ~ret ~signature ~expr =
  let signature = Renaming.rename_value_signature signature in
  match args with
  | [] -> Location.raise_errorf ~loc "Constant exportation not implemented"
  | _ ->
      let pyobject = make_python_function ~loc ~args ~ret ~name in
      let name_pat = ppat_var ~loc (Loc.make ~loc name) in
      let name_longident = Loc.make ~loc (Longident.Lident name) in
      (* let myfun =
         let rec myfun x y = ... in
         let () = Python_libgen.(register_python_value {
           convert=...; name=...; doc=...; signature=...}) in
         myfun *)
      pstr_value ~loc Nonrecursive
        [
          value_binding_no_warn ~loc ~pat:name_pat
            ~expr:
              (pexp_let ~loc rec_flag
                 [ value_binding ~loc ~pat:name_pat ~expr ]
                 [%expr
                   let () =
                     Python_libgen.(
                       register_python_value
                         {
                           convert = (fun () -> [%e pyobject]);
                           name = [%e estring ~loc name];
                           signature =
                             [%e Repr_ast.value_signature_ast ~loc signature];
                         })
                   in
                   [%e pexp_ident ~loc name_longident]]);
        ]

let () =
  Repr_rewriter.register_type_declaration_deriver ~name:"python_export_type"
    python_type_export;
  Repr_rewriter.register_value_declaration_expander ~name:"python_export"
    python_export;
  let rule = Context_free.Rule.extension Python_docstring.extension in
  Driver.register_transformation ~rules:[ rule ] "python_docstring"
