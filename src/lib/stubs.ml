type settings = { use_dataclasses : bool }

[@@@warning "-32"]

let templates_locations = Sites.Sites.templates

let lookup_template filename =
  List.find_map
    (fun dir ->
      let filename' = Filename.concat dir filename in
      if Sys.file_exists filename' then Some filename' else None)
    templates_locations
  |> Base.Option.value_exn ~message:("Not found: templates/" ^ filename)

module type Encoding = sig
  val compile_type_declaration : Repr.type_declaration -> Pydsl.item list
  val compile_value_declaration : 'a Repr.value -> Pydsl.item list
end

module type Params = sig
  val generated_module : string
end

(* The default encoding simply follows the ppx_python conventions. It is
   simple and fast but not very idiomatic in Python. *)
module Default_encoding (P : Params) : Encoding = struct
  open Repr
  open Pydsl

  let dump_labels = function
    | Repr.Anonymous ts -> ts
    | Labeled lts -> List.map snd lts

  let compile_type_declaration
      { type_name = name; type_vars = vars; definition } =
    match definition with
    | Alias t -> [ Declare_type { name; vars; def = Simple t } ]
    | Record fields -> [ Declare_typed_dict { name; vars; fields } ]
    | Enum cases ->
        let cases = List.map (fun s -> (s, [])) cases in
        [ Declare_type { name; vars; def = Tagged_union cases } ]
    | Variant cases ->
        let cases = List.map (fun (s, c) -> (s, dump_labels c)) cases in
        [ Declare_type { name; vars; def = Tagged_union cases } ]

  let compile_value_declaration { name; signature; _ } =
    match signature with
    | Repr.Constant _ -> assert false
    | Repr.Function { args; ret } ->
        let docstring = Register.registered_python_docstring name in
        let call_args = List.map (fun (a, _) -> Lvalue (Var a)) args in
        let internals =
          Create_module.internal_module ~generated:P.generated_module
        in
        let body = [ Return (Call (Field (Var internals, name), call_args)) ] in
        [ Declare_fun { name; args; ret; docstring; body } ]
end

module Dataclasses_encoding (P : Params) : Encoding = struct
  open Repr
  open Pydsl

  let conv_generic conv_name =
    let open Pydsl in
    let rec aux ~env lval t =
      match t with
      | Repr.Var v -> Call (Var (conv_name v), [ Lvalue lval ])
      | App (ctor, ts) -> (
          match ctor with
          | Int | Bool | Float | String | Unit -> Lvalue lval
          | Custom u ->
              Call
                ( Var (conv_name u),
                  Lvalue lval :: List.map (fun t -> Lambda (aux ~env Arg t)) ts
                ))
      | Tuple ts ->
          Tuple (List.mapi (fun i t -> aux ~env (Index (lval, i)) t) ts)
      | List t | Array t -> Comprehension (Lvalue lval, aux ~env Arg t)
      | Option t -> Case_not_none (Lvalue lval, aux ~env Arg t)
    in
    aux

  let t_of_ocaml t = "_" ^ t ^ "_of_ocaml"
  let ocaml_of_t t = "_" ^ "ocaml_of_" ^ t
  let of_ocaml = conv_generic t_of_ocaml
  let ocaml_of = conv_generic ocaml_of_t

  let dataclass_encoding args =
    match args with
    | Repr.Labeled fields -> fields
    | Repr.Anonymous [] -> []
    | Repr.Anonymous [ x ] -> [ ("arg", x) ]
    | Repr.Anonymous xs -> [ ("args", Repr.Tuple xs) ]

  let dump_labels = function
    | Repr.Anonymous ts -> ts
    | Labeled lts -> List.map snd lts

  let compile_type_declaration
      { type_name = name; type_vars = vars; definition } =
    match definition with
    | Alias t -> [ Declare_type { name; vars; def = Simple t } ]
    | Record fields -> [ Declare_typed_dict { name; vars; fields } ]
    | Enum cases ->
        let cases = List.map (fun s -> (s, [])) cases in
        [ Declare_type { name; vars; def = Tagged_union cases } ]
    | Variant cases ->
        let cases = List.map (fun (s, c) -> (s, dump_labels c)) cases in
        [ Declare_type { name; vars; def = Tagged_union cases } ]

  let compile_value_declaration { name; signature; _ } =
    match signature with
    | Repr.Constant _ -> assert false
    | Repr.Function { args; ret } ->
        let docstring = Register.registered_python_docstring name in
        let call_args = List.map (fun (a, _) -> Lvalue (Var a)) args in
        let internals =
          Create_module.internal_module ~generated:P.generated_module
        in
        let body = [ Return (Call (Field (Var internals, name), call_args)) ] in
        [ Declare_fun { name; args; ret; docstring; body } ]
end

let generate_py_stub ~settings ~lib_name ~generated ~types ~values =
  let prelude =
    Stdio.In_channel.read_all (lookup_template "stub.py")
    |> Base.String.substr_replace_all ~pattern:"{LIB_NAME}" ~with_:lib_name
    |> Base.String.substr_replace_all ~pattern:"{GENERATED}" ~with_:generated
    |> Base.String.substr_replace_all ~pattern:"{GENERATED_BY_PYML}"
         ~with_:(Create_module.internal_module ~generated)
  in
  let (module E : Encoding) =
    let (module Params : Params) =
      (module struct
        let generated_module = generated
      end)
    in
    if settings.use_dataclasses then (module Dataclasses_encoding (Params))
    else (module Default_encoding (Params))
  in
  let stub =
    List.concat_map E.compile_type_declaration types
    @ List.concat_map E.compile_value_declaration values
  in
  String.concat "\n\n"
    ([ prelude ] @ Pydsl.generate_imports stub @ [ Pydsl.show_stub stub ])
