open Ppxlib
module Repr = Python_libgen.Repr

let ignore_longident_prefix ~loc = function
  | Longident.Lident s | Longident.Ldot (_, s) -> s
  | Longident.Lapply _ ->
      Location.raise_errorf ~loc "Longident.Lapply is not allowed"

let rec type_expr { ptyp_desc; ptyp_loc = loc; _ } =
  match ptyp_desc with
  | Ptyp_var v -> Repr.Tvar v
  | Ptyp_tuple args -> Repr.Tuple (List.map type_expr args)
  | Ptyp_constr (t, args) -> (
      let t = ignore_longident_prefix ~loc t.txt in
      match (t, args) with
      | "int", [] -> Repr.(App (Int, []))
      | "float", [] -> Repr.(App (Float, []))
      | "string", [] -> Repr.(App (String, []))
      | "bool", [] -> Repr.(App (Bool, []))
      | "unit", [] -> Repr.(App (Unit, []))
      | s, [] -> Repr.(App (Custom s, []))
      | "option", [ arg ] -> Repr.Option (type_expr arg)
      | "list", [ arg ] -> Repr.List (type_expr arg)
      | "array", [ arg ] -> Repr.Array (type_expr arg)
      | ctor, args -> Repr.(App (Custom ctor, List.map type_expr args)))
  | _ -> Location.raise_errorf ~loc "unsupported type"

module Type_declaration = struct
  let constructor_declaration ~loc { pcd_name; pcd_vars; pcd_args; _ } =
    if List.length pcd_vars > 0 then
      Location.raise_errorf ~loc "GADTs are not supported";
    let variant_args =
      match pcd_args with
      | Pcstr_tuple ts -> Repr.Anonymous (List.map (fun t -> type_expr t) ts)
      | Pcstr_record records ->
          Repr.Labeled
            (List.map (fun r -> (r.pld_name.txt, type_expr r.pld_type)) records)
    in
    (pcd_name.txt, variant_args)

  let as_type_var { ptyp_desc; _ } =
    match ptyp_desc with Ptyp_var s -> s | _ -> assert false

  let make_enum_or_variant ctors =
    let is_enum =
      List.for_all
        (fun (_, a) -> match a with Repr.Anonymous [] -> true | _ -> false)
        ctors
    in
    if is_enum then Repr.Enum (List.map fst ctors) else Repr.Variant ctors

  let type_declaration ~loc
      { ptype_name; ptype_kind; ptype_manifest; ptype_params; _ } =
    let vars = List.map (fun (t, _) -> as_type_var t) ptype_params in
    let definition =
      match (ptype_kind, ptype_manifest) with
      | Ptype_abstract, Some def -> Repr.Alias (type_expr def)
      | Ptype_variant ctors, _ ->
          make_enum_or_variant (List.map (constructor_declaration ~loc) ctors)
      | Ptype_record fields, _ ->
          Repr.Record
            (List.map (fun f -> (f.pld_name.txt, type_expr f.pld_type)) fields)
      | _ -> Location.raise_errorf ~loc "unhandled construct"
    in
    [ Repr.{ type_name = ptype_name.txt; type_vars = vars; definition } ]

  let is_opaque_alias decl =
    match (decl.ptype_kind, decl.ptype_manifest) with
    | Ptype_abstract, Some { ptyp_attributes; _ }
      when ptyp_attributes
           |> List.exists (fun a -> String.equal a.attr_name.txt "opaque") ->
        true
    | _ -> false

  let type_declarations ~loc declarations =
    List.concat_map
      (fun d -> if is_opaque_alias d then [] else type_declaration ~loc d)
      declarations
end

module Value_declaration_expander = struct
  let rec extract_fun_type { pexp_desc; _ } =
    let ( let* ) = Option.bind in
    match pexp_desc with
    | Pexp_fun
        ( _,
          _,
          {
            ppat_desc =
              Ppat_constraint ({ ppat_desc = Ppat_var arg_name; _ }, arg_type);
            _;
          },
          expr ) ->
        let* args, ret = extract_fun_type expr in
        Some ((arg_name.txt, arg_type) :: args, ret)
    | Pexp_constraint (_, ret_type) -> Some ([], ret_type)
    | _ -> None

  let expand ~loc expr =
    match extract_fun_type expr with
    | None -> Location.raise_errorf ~loc "invalid function definition format"
    | Some (args, ret) -> (args, ret)

  let pattern =
    Ast_pattern.(
      pstr
        (pstr_value __ (value_binding ~pat:(ppat_var __) ~expr:__ ^:: nil)
        ^:: nil))

  let value_signature ~args ~ret =
    match args with
    | [] -> Repr.Constant (type_expr ret)
    | args ->
        Repr.Function
          {
            args = List.map (fun (s, t) -> (s, type_expr t)) args;
            ret = type_expr ret;
          }

  let extension ~name f =
    Extension.V3.declare name Extension.Context.structure_item pattern
      (fun ~ctxt rec_flag name expr ->
        let loc = Expansion_context.Extension.extension_point_loc ctxt in
        let args, ret = expand ~loc expr in
        let signature = value_signature ~args ~ret in
        f ~loc ~rec_flag ~name ~args ~ret ~signature ~expr)
end

let register_type_declaration_deriver ~name f =
  ignore
  @@ Deriving.add
       ~str_type_decl:
         (Deriving.Generator.V2.make_noarg
            (fun ~ctxt (_rec_flag, declarations) ->
              let loc = Expansion_context.Deriver.derived_item_loc ctxt in
              let reprs =
                Type_declaration.type_declarations ~loc declarations
              in
              List.concat_map (f ~loc) reprs))
       name

let register_value_declaration_expander ~name f =
  let rule =
    Context_free.Rule.extension (Value_declaration_expander.extension ~name f)
  in
  Driver.register_transformation ~rules:[ rule ] name
