open Base
open Ppxlib
open Ast_builder.Default

let concise_variants = false

(** Naming conventions *)
module Names = struct
  let python_typedef_for t = "python_typedef_for_" ^ t

  let snake_case_to_capitalized_camel_case s =
    String.split ~on:'_' s |> List.map ~f:String.capitalize |> String.concat

  let convert_atomic_type_name = snake_case_to_capitalized_camel_case
end

(* Utilities for building [Python_library_in_ocaml] expressions *)
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

  let alias_decl ~loc aliased def =
    let aliased = estring ~loc (Names.convert_atomic_type_name aliased) in
    [%expr Python_library_in_ocaml.Py_Alias ([%e aliased], [%e def])]

  let record_decl ~loc fields =
    let fields =
      List.map fields ~f:(fun (s, f) -> pexp_tuple ~loc [estring ~loc s; f])
    in
    [%expr Python_library_in_ocaml.Py_TypedDict [%e elist ~loc fields]]
end

(** Misc utilities  *)
module Utils = struct
  let longident_as_string ~loc = function
    | Longident.Lident s ->
        s
    | _ ->
        Location.raise_errorf ~loc "no qualified identifier is allowed"
end

open Repr
open Names
open Utils

(* Code for the [%python_type: ...] constructs *)
module Type_for = struct
  let rec generate {ptyp_desc; ptyp_loc= loc; _} =
    match ptyp_desc with
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
        | _ ->
            Location.raise_errorf ~loc "unsupported type" )
    | _ ->
        Location.raise_errorf ~loc "unsupported type"
end

(* Code for compiling [@@deriving python_type] in structures *)
module Typedef_for_structure = struct
  let constructor_declaration ~loc ctor =
    match ctor with
    | {pcd_name; pcd_vars= []; pcd_args= Pcstr_tuple ts; _} -> (
        let name = pcd_name.txt in
        let args = List.map ts ~f:Type_for.generate in
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

  let type_declaration ~loc {ptype_name; ptype_kind; ptype_manifest; _} =
    let name : string = ptype_name.txt in
    let type_expr =
      match (ptype_kind, ptype_manifest) with
      | Ptype_abstract, Some def ->
          alias_decl ~loc name (Type_for.generate def)
      | Ptype_variant ctors, _ ->
          let args = List.map ctors ~f:(constructor_declaration ~loc) in
          alias_decl ~loc name (union ~loc args)
      | Ptype_record fields, _ ->
          let args =
            List.map fields ~f:(fun f ->
                (f.pld_name.txt, Type_for.generate f.pld_type) )
          in
          record_decl ~loc args
      | _ ->
          Location.raise_errorf ~loc "unhandled construct"
    in
    let binding = pvar ~loc (python_typedef_for name) in
    [[%stri let [%p binding] = [%e type_expr]]]

  let types_declaration ~ctxt
      ((_rec_flag, type_declarations) : rec_flag * type_declaration list) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.concat_map type_declarations ~f:(type_declaration ~loc)
end

module Typedef_for_signature = struct
  let type_declaration ~loc {ptype_name; _} =
    let name = python_typedef_for ptype_name.txt in
    [ psig_value ~loc
        (value_description ~loc ~name:(Loc.make ~loc name)
           ~type_:[%type: Python_library_in_ocaml.python_type_def] ~prim:[] ) ]

  let types_declaration ~ctxt
      ((_rec_flag, type_declarations) : rec_flag * type_declaration list) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.concat_map type_declarations ~f:(type_declaration ~loc)
end

(* Exporting Python values *)
module Py_export = struct end

let deriver =
  Deriving.add
    ~str_type_decl:
      (Deriving.Generator.V2.make_noarg Typedef_for_structure.types_declaration)
    ~sig_type_decl:
      (Deriving.Generator.V2.make_noarg Typedef_for_signature.types_declaration)
    "python_type"
