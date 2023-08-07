open Base
open Ppxlib
open Ast_builder.Default

let concise_variants = false

(* Naming conventions *)
module Names = struct
  let snake_case_to_capitalized_camel_case s =
    String.split ~on:'_' s |> List.map ~f:String.capitalize |> String.concat

  let convert_atomic_type_name = snake_case_to_capitalized_camel_case

  let convert_type_var = snake_case_to_capitalized_camel_case
end

(* Utilities for building [Python_library_in_ocaml.Repr] expressions *)
module Repr = struct
  let atomic ~loc = function
    | "int" ->
        [%expr Python_library_in_ocaml.(Py_Atomic Py_Int)]
    | "float" ->
        [%expr Python_library_in_ocaml.(Py_Atomic Py_Float)]
    | "string" ->
        [%expr Python_library_in_ocaml.(Py_Atomic Py_String)]
    | "bool" ->
        [%expr Python_library_in_ocaml.(Py_Atomic Py_Bool)]
    | "unit" ->
        [%expr Python_library_in_ocaml.(Py_Atomic Py_None)]
    | s ->
        let custom = estring ~loc (Names.convert_atomic_type_name s) in
        [%expr Python_library_in_ocaml.(Py_Atomic (Py_Custom [%e custom]))]

  let none ~loc = [%expr Python_library_in_ocaml.(Py_Atomic Py_None)]

  let type_var ~loc name =
    let name = Names.convert_type_var name in
    [%expr Python_library_in_ocaml.Py_Var [%e estring ~loc name]]

  let type_apply ~loc ctor exprs =
    let ctor = Names.convert_type_var ctor in
    [%expr
      Python_library_in_ocaml.(
        Py_Apply (Py_Custom [%e estring ~loc ctor], [%e elist ~loc exprs]) )]

  let union ~loc asts =
    [%expr Python_library_in_ocaml.Py_Union [%e elist ~loc asts]]

  let tuple ~loc args =
    [%expr Python_library_in_ocaml.Py_Tuple [%e elist ~loc args]]

  let list ~loc arg = [%expr Python_library_in_ocaml.Py_List [%e arg]]

  let option ~loc arg = union ~loc [arg; none ~loc]

  let tagged_tuple ~loc tag arg =
    [%expr
      Python_library_in_ocaml.(
        Py_Tuple [Py_Literal [%e estring ~loc tag]; [%e arg]] )]

  let py_constant ~loc tyrep =
    [%expr Python_library_in_ocaml.Py_Constant [%e tyrep]]

  let py_function ~loc ~args ~ret =
    let args =
      List.map args ~f:(fun (s, a) -> pexp_tuple ~loc [estring ~loc s; a])
    in
    [%expr
      Python_library_in_ocaml.Py_Function
        {args= [%e elist ~loc args]; ret= [%e ret]}]

  let alias ~loc def = [%expr Python_library_in_ocaml.Py_Alias [%e def]]

  let typed_dict ~loc fields =
    let fields =
      List.map fields ~f:(fun (s, f) -> pexp_tuple ~loc [estring ~loc s; f])
    in
    [%expr Python_library_in_ocaml.Py_TypedDict [%e elist ~loc fields]]

  let type_decl ~loc ~name ~vars def =
    let name = estring ~loc (Names.convert_atomic_type_name name) in
    let vars =
      elist ~loc
        (List.map vars ~f:(fun v -> estring ~loc (Names.convert_type_var v)))
    in
    [%expr
      Python_library_in_ocaml.
        {type_name= [%e name]; type_vars= [%e vars]; definition= [%e def]}]

  let alias_decl ~loc ~name ~vars def =
    type_decl ~loc ~name ~vars (alias ~loc def)

  let record_decl ~loc ~name ~vars fields =
    type_decl ~loc ~name ~vars (typed_dict ~loc fields)
end

(* Misc utilities *)
module Utils = struct
  let longident_as_string ~loc = function
    | Longident.Lident s ->
        s
    | _ ->
        Location.raise_errorf ~loc "no qualified identifier is allowed"
end

open Repr
open Utils

(* Translating between OCaml and Python types *)
module Python_type = struct
  let rec generate {ptyp_desc; ptyp_loc= loc; _} =
    match ptyp_desc with
    | Ptyp_var v ->
        type_var ~loc v
    | Ptyp_tuple args ->
        tuple ~loc (List.map args ~f:generate)
    | Ptyp_constr (t, args) -> (
        let t = longident_as_string ~loc t.txt in
        match (t, args) with
        | _, [] ->
            atomic ~loc t
        | "option", [arg] ->
            option ~loc (generate arg)
        | "list", [arg] ->
            list ~loc (generate arg)
        | ctor, args ->
            type_apply ~loc ctor (List.map ~f:generate args) )
    | _ ->
        Location.raise_errorf ~loc "unsupported type"
end

(* Code for registering type declarations *)
module Python_export_type = struct
  let constructor_declaration ~loc ctor =
    match ctor with
    | {pcd_name; pcd_vars= []; pcd_args= Pcstr_tuple ts; _} -> (
        let name = pcd_name.txt in
        let args = List.map ts ~f:Python_type.generate in
        match args with
        | [] when concise_variants ->
            estring ~loc name
        | [] when not concise_variants ->
            tagged_tuple ~loc name (none ~loc)
        | [t] when concise_variants ->
            tagged_tuple ~loc name t
        | _ ->
            tagged_tuple ~loc name (tuple ~loc args) )
    | _ ->
        Location.raise_errorf ~loc "unsupported variant type"

  let as_type_var {ptyp_desc; _} =
    match ptyp_desc with Ptyp_var s -> s | _ -> assert false

  let type_declaration ~loc
      {ptype_name; ptype_kind; ptype_manifest; ptype_params; _} =
    let name : string = ptype_name.txt in
    let vars = List.map ptype_params ~f:(fun (t, _) -> as_type_var t) in
    let type_expr =
      match (ptype_kind, ptype_manifest) with
      | Ptype_abstract, Some def ->
          alias_decl ~loc ~name ~vars (Python_type.generate def)
      | Ptype_variant ctors, _ ->
          let args = List.map ctors ~f:(constructor_declaration ~loc) in
          alias_decl ~loc ~name ~vars (union ~loc args)
      | Ptype_record fields, _ ->
          let args =
            List.map fields ~f:(fun f ->
                (f.pld_name.txt, Python_type.generate f.pld_type) )
          in
          record_decl ~loc ~name ~vars args
      | _ ->
          Location.raise_errorf ~loc "unhandled construct"
    in
    [ [%stri
        let () = Python_library_in_ocaml.register_python_type [%e type_expr]] ]

  let types_declaration ~ctxt
      ((_rec_flag, type_declarations) : rec_flag * type_declaration list) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.concat_map type_declarations ~f:(type_declaration ~loc)
end

(* Register docstrings *)
module Python_docstring = struct
  let expand ~ctxt name docstring =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let docstring = Dedent.string docstring in
    [%stri
      let () =
        Python_library_in_ocaml.register_python_docstring
          ~name:[%e estring ~loc name] ~docstring:[%e estring ~loc docstring]]

  let extension =
    Extension.V3.declare "python_docstring" Extension.Context.structure_item
      Ast_pattern.(
        pstr
          ( pstr_value drop
              (value_binding ~pat:(ppat_var __) ~expr:(estring __) ^:: nil)
          ^:: nil ) )
      expand
end

(* Exporting Python values *)
module Python_export = struct
  let rec extract_fun_type {pexp_desc; _} =
    let open Option.Let_syntax in
    match pexp_desc with
    | Pexp_fun
        ( _
        , _
        , { ppat_desc=
              Ppat_constraint ({ppat_desc= Ppat_var arg_name; _}, arg_type)
          ; _ }
        , expr ) ->
        let%bind args, ret = extract_fun_type expr in
        Some ((arg_name.txt, arg_type) :: args, ret)
    | Pexp_constraint (_, ret_type) ->
        Some ([], ret_type)
    | _ ->
        None

  let value_signature ~loc ~args ~ret =
    match args with
    | [] ->
        Repr.py_constant ~loc (Python_type.generate ret)
    | args ->
        let args =
          List.map args ~f:(fun (s, t) -> (s, Python_type.generate t))
        in
        Repr.py_function ~loc ~args ~ret:(Python_type.generate ret)

  let make_pyobject ~loc ~args ~ret ~name =
    (* Py.Callable.of_function ~name:... ~docstring:...
       (fun args ->
         let arg0 = [%of_python] args.(0) in
         ... in
         [%python_of: ...] (f arg0...)) *)
    let n = List.length args in
    let lident s = Loc.make ~loc (Longident.Lident s) in
    assert (n > 0) ;
    let body =
      let bindings =
        List.mapi args ~f:(fun i (arg_name, arg_type) ->
            value_binding ~loc
              ~pat:(ppat_var ~loc (Loc.make ~loc arg_name))
              ~expr:[%expr [%of_python: [%t arg_type]] args.([%e eint ~loc i])] )
      and expr =
        [%expr
          [%python_of: [%t ret]]
            [%e
              pexp_apply ~loc
                (pexp_ident ~loc (lident name))
                (List.map args ~f:(fun (arg_name, _) ->
                     (Nolabel, pexp_ident ~loc (lident arg_name)) ) )]]
      in
      pexp_let ~loc Nonrecursive bindings expr
    in
    [%expr
      Py.Callable.of_function ~name:[%e estring ~loc name] ~docstring:""
        (fun args ->
          if Array.length args <> [%e eint ~loc n] then
            failwith "incorrect number of arguments" ;
          [%e body] )]

  let value_binding_no_warn ~loc ~pat ~expr =
    { pvb_pat= pat
    ; pvb_expr= expr
    ; pvb_loc= loc
    ; pvb_attributes=
        [ attribute ~loc ~name:(Loc.make ~loc "warning")
            ~payload:(PStr [%str "-32"]) ] }

  let expand ~ctxt rec_flag name expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match extract_fun_type expr with
    | None ->
        Location.raise_errorf ~loc "invalid function definition format"
    | Some (args, ret) ->
        let doc = estring ~loc "" in
        let signature = value_signature ~loc ~args ~ret in
        let pyobject = make_pyobject ~loc ~args ~ret ~name in
        let name_pat = ppat_var ~loc (Loc.make ~loc name) in
        let name_longident = Loc.make ~loc (Longident.Lident name) in
        (* let myfun =
            let rec myfun x y = ... in
            let () = Python_library_in_ocaml.(register_python_value {
              pyobject=...; name=...; doc=...; signature=...}) in
            myfun *)
        pstr_value ~loc Nonrecursive
          [ value_binding_no_warn ~loc ~pat:name_pat
              ~expr:
                (pexp_let ~loc rec_flag
                   [value_binding ~loc ~pat:name_pat ~expr]
                   [%expr
                     let () =
                       Python_library_in_ocaml.(
                         register_python_value
                           { pyobject= (fun () -> [%e pyobject])
                           ; name= [%e estring ~loc name]
                           ; doc= [%e doc]
                           ; signature= [%e signature] } )
                     in
                     [%e pexp_ident ~loc name_longident]] ) ]

  let extension =
    Extension.V3.declare "python_export" Extension.Context.structure_item
      Ast_pattern.(
        pstr
          ( pstr_value __ (value_binding ~pat:(ppat_var __) ~expr:__ ^:: nil)
          ^:: nil ) )
      expand
end

let () =
  let rule = Context_free.Rule.extension Python_export.extension in
  Driver.register_transformation ~rules:[rule] "python_export"

let () =
  let rule = Context_free.Rule.extension Python_docstring.extension in
  Driver.register_transformation ~rules:[rule] "python_docstring"

let deriver =
  Deriving.add
    ~str_type_decl:
      (Deriving.Generator.V2.make_noarg Python_export_type.types_declaration)
    "python_export_type"
