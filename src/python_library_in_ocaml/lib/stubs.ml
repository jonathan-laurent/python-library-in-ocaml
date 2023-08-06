open Base
open Repr
open Stdio

let templates_locations = Sites.Sites.templates

let lookup_template filename =
  List.find_map
    ~f:(fun dir ->
      let filename' = Stdlib.Filename.concat dir filename in
      if Stdlib.Sys.file_exists filename' then Some filename' else None )
    templates_locations
  |> Option.value_exn ~message:("Not found: templates/" ^ filename)

type imports =
  {mutable literal: bool; mutable type_alias: bool; mutable typed_dict: bool}

let show_atomic_type ~quote = function
  | Py_Bool ->
      "bool"
  | Py_Int ->
      "int"
  | Py_Float ->
      "float"
  | Py_String ->
      "str"
  | Py_None ->
      "None"
  | Py_Custom s ->
      if quote then "\"" ^ s ^ "\"" else s

let show_type_expr ~imports t =
  let rec aux = function
    | Py_Atomic a ->
        show_atomic_type ~quote:true a
    | Py_Tuple ts ->
        "tuple[" ^ String.concat ~sep:", " (List.map ts ~f:aux) ^ "]"
    | Py_List t ->
        "list[" ^ aux t ^ "]"
    | Py_Literal s ->
        imports.literal <- true ;
        "Literal[\"" ^ s ^ "\"]"
    | Py_Union ts ->
        assert (not (List.is_empty ts)) ;
        String.concat ~sep:" | " (List.map ts ~f:aux)
  in
  aux t

let show_type_declaration ~imports td =
  match td.definition with
  | Py_Alias t ->
      imports.type_alias <- true ;
      Printf.sprintf "%s: TypeAlias = %s" td.type_name
        (show_type_expr ~imports t)
  | Py_TypedDict fields ->
      imports.typed_dict <- true ;
      let header =
        Printf.sprintf "class %s(TypedDict, total=True):" td.type_name
      in
      let fields =
        List.map fields ~f:(fun (name, t) ->
            Printf.sprintf "    %s: %s" name (show_type_expr ~imports t) )
      in
      String.concat ~sep:"\n" (header :: fields)

let indent s =
  String.split_lines s
  |> List.map ~f:(fun s -> "    " ^ s)
  |> String.concat_lines
  |> fun s -> String.drop_suffix s 1 (* remove trailing \n *)

let show_value_type_declaration ~imports v =
  match v.signature with
  | Py_Constant t ->
      Printf.sprintf "%s: %s = ..." v.name (show_type_expr ~imports t)
  | Py_Function {args; ret} ->
      let args =
        String.concat ~sep:", "
          (List.map args ~f:(fun (a, t) ->
               Printf.sprintf "%s: %s" a (show_type_expr ~imports t) ) )
      in
      let header =
        Printf.sprintf "def %s(%s) -> %s:" v.name args
          (show_type_expr ~imports ret)
      in
      let docstring =
        match Register.registered_python_docstring v.name with
        | None ->
            []
        | Some doc ->
            [indent ("\"\"\"\n" ^ doc ^ "\n\"\"\"")]
      in
      String.concat ~sep:"\n" ([header] @ docstring @ ["    ..."])

let type_py_stub td = td.type_name ^ " = ..."

let value_py_stub ~generated v =
  let mod_ = Create_module.internal_module ~generated in
  match v.signature with
  | Py_Constant _ ->
      Printf.sprintf "%s = %s.%s" v.name mod_ v.name
  | Py_Function {args; ret= _} ->
      let args = String.concat ~sep:", " (List.map args ~f:(fun (a, _) -> a)) in
      let header = Printf.sprintf "def %s(%s):" v.name args in
      let body = Printf.sprintf "    return %s.%s(%s)" mod_ v.name args in
      String.concat ~sep:"\n" [header; body]

let generate_py_stub ~lib_name ~generated ~types ~values =
  let prelude =
    In_channel.read_all (lookup_template "stub.py")
    |> Base.String.substr_replace_all ~pattern:"{LIB_NAME}" ~with_:lib_name
    |> Base.String.substr_replace_all ~pattern:"{GENERATED}" ~with_:generated
    |> Base.String.substr_replace_all ~pattern:"{GENERATED_BY_PYML}"
         ~with_:(Create_module.internal_module ~generated)
  in
  let types_section =
    String.concat ~sep:"\n" (List.map types ~f:type_py_stub)
  in
  let values_section =
    String.concat ~sep:"\n\n" (List.map values ~f:(value_py_stub ~generated))
  in
  String.concat ~sep:"\n\n" [prelude; types_section; values_section] ^ "\n"

let show_imports ~imports =
  let to_import = Queue.create () in
  if imports.type_alias then Queue.enqueue to_import "TypeAlias" ;
  if imports.typed_dict then Queue.enqueue to_import "TypedDict" ;
  if imports.literal then Queue.enqueue to_import "Literal" ;
  let to_import = Queue.to_list to_import in
  if List.is_empty to_import then []
  else ["from typing import " ^ String.concat ~sep:", " to_import]

let generate_pyi_stub ~types ~values =
  let imports = {literal= false; type_alias= false; typed_dict= false} in
  let prelude = In_channel.read_all (lookup_template "stub.pyi") in
  let types_section = List.map types ~f:(show_type_declaration ~imports) in
  let val_section = List.map values ~f:(show_value_type_declaration ~imports) in
  let imports = show_imports ~imports in
  String.concat ~sep:"\n\n" ([prelude] @ imports @ types_section @ val_section)
  ^ "\n"
