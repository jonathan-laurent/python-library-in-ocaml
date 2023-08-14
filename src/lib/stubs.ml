type settings = { use_dataclasses : bool }

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
        let call_args = List.map (fun (a, _, _) -> Lvalue (Var a)) args in
        let internals =
          Create_module.internal_module ~generated:P.generated_module
        in
        let body = [ Return (Call (Field (Var internals, name), call_args)) ] in
        [ Declare_typed_fun { name; args; ret; docstring; body } ]
end

module Dataclasses_encoding (P : Params) : Encoding = struct
  open Repr
  open Pydsl

  let conv_generic conv_name =
    let open Pydsl in
    let rec aux lval t =
      match t with
      | Repr.Tvar v -> Call (Var (conv_name v), [ Lvalue lval ])
      | App (ctor, ts) -> (
          match ctor with
          | Int | Bool | Float | String | Unit -> Lvalue lval
          | Custom u ->
              Call
                ( Var (conv_name u),
                  Lvalue lval :: List.map (fun t -> Lambda (aux Arg t)) ts ))
      | Tuple ts ->
          Create_tuple (List.mapi (fun i t -> aux (Index (lval, i)) t) ts)
      | List t | Array t -> Comprehension (Lvalue lval, aux Arg t)
      | Option t -> Case_not_none (Lvalue lval, aux lval t)
    in
    aux

  let t_of_ocaml t = "_" ^ t ^ "_of_ocaml"
  let ocaml_of_t t = "_" ^ "ocaml_of_" ^ t
  let of_ocaml = conv_generic t_of_ocaml
  let ocaml_of = conv_generic ocaml_of_t
  let ith_arg i = "arg" ^ string_of_int (i + 1)

  let dataclass_encoding args =
    match args with
    | Repr.Labeled fields -> fields
    | Repr.Anonymous [] -> []
    | Repr.Anonymous [ x ] -> [ ("arg", x) ]
    | Repr.Anonymous xs -> List.mapi (fun i a -> (ith_arg i, a)) xs

  let variant_union ~vars cases =
    Union
      (List.map
         (fun (s, _) -> App (Custom s, List.map (fun v -> Repr.Tvar v) vars))
         cases)

  let make_conv conv ~name ~vars f =
    Declare_fun
      {
        name = conv name;
        args = "x" :: List.map conv vars;
        body = [ Return (f (Var "x")) ];
      }

  let ocaml_of_dataclass x fields =
    let init (f, t) = (f, ocaml_of (Field (x, f)) t) in
    Create_dict (List.map init fields)

  let dataclass_of_ocaml name x fields =
    let init (f, t) = (f, of_ocaml (Str_index (x, f)) t) in
    Create_dataclass (name, List.map init fields)

  let ocaml_of_variant x ctor = function
    | Repr.Anonymous [] -> Create_tuple [ String_constant ctor; None_constant ]
    | Repr.Anonymous [ t ] ->
        Create_tuple
          [
            String_constant ctor; Create_tuple [ ocaml_of (Field (x, "arg")) t ];
          ]
    | Repr.Anonymous args ->
        let ith i t = ocaml_of (Field (x, ith_arg i)) t in
        Create_tuple [ String_constant ctor; Create_tuple (List.mapi ith args) ]
    | Repr.Labeled fields ->
        Create_tuple [ String_constant ctor; ocaml_of_dataclass x fields ]

  let variant_of_ocaml name x = function
    | Repr.Anonymous [] -> Create_dataclass (name, [])
    | Repr.Anonymous [ t ] ->
        Create_dataclass
          (name, [ ("arg", of_ocaml (Index (Index (x, 1), 0)) t) ])
    | Repr.Anonymous args ->
        Create_dataclass
          ( name,
            List.mapi
              (fun i t -> (ith_arg i, of_ocaml (Index (Index (x, 1), i)) t))
              args )
    | Repr.Labeled fields -> dataclass_of_ocaml name (Index (x, 1)) fields

  let compile_type_declaration
      { type_name = name; type_vars = vars; definition } =
    match definition with
    | Alias t ->
        [
          Declare_type { name; vars; def = Simple t };
          make_conv ocaml_of_t ~name ~vars (fun x -> ocaml_of x t);
          make_conv t_of_ocaml ~name ~vars (fun x -> of_ocaml x t);
        ]
    | Record fields ->
        [
          Declare_dataclass { name; vars; fields };
          make_conv ocaml_of_t ~name ~vars (fun x ->
              ocaml_of_dataclass x fields);
          make_conv t_of_ocaml ~name ~vars (fun x ->
              dataclass_of_ocaml name x fields);
        ]
    | Enum cases ->
        [
          Declare_enum { name; cases };
          make_conv ocaml_of_t ~name ~vars (fun x -> Enum_value x);
          make_conv t_of_ocaml ~name ~vars (fun x ->
              let case s = (s, Lvalue (Field (Var name, s))) in
              Str_cases (x, List.map case cases));
        ]
    | Variant cases ->
        let children =
          List.map
            (fun (s, c) ->
              let fields = dataclass_encoding c in
              Declare_dataclass { name = s; vars; fields })
            cases
        and union = Declare_type { name; vars; def = variant_union ~vars cases }
        and conversions =
          [
            make_conv ocaml_of_t ~name ~vars (fun x ->
                let case (s, args) = (s, ocaml_of_variant x s args) in
                Type_cases (x, List.map case cases));
            make_conv t_of_ocaml ~name ~vars (fun x ->
                let case (s, args) = (s, variant_of_ocaml s x args) in
                Str_cases (Index (x, 0), List.map case cases));
          ]
        in
        children @ [ union ] @ conversions

  let ret_var = "_ret"

  let compile_value_declaration { name; signature; _ } =
    match signature with
    | Repr.Constant _ -> assert false
    | Repr.Function { args; ret } ->
        let docstring = Register.registered_python_docstring name in
        let call_args = List.map (fun (a, _, t) -> ocaml_of (Var a) t) args in
        let internals =
          Create_module.internal_module ~generated:P.generated_module
        in
        let body =
          [
            Assign (ret_var, Call (Field (Var internals, name), call_args));
            Return (of_ocaml (Var ret_var) ret);
          ]
        in
        [ Declare_typed_fun { name; args; ret; docstring; body } ]
end

let generate_py_stub ~interface_only ~settings ~lib_name ~generated ~types
    ~values =
  let prelude =
    let template = if interface_only then "stub.pyi" else "stub.py" in
    Stdio.In_channel.read_all (lookup_template template)
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
  let stub = if interface_only then Pydsl.interface_only stub else stub in
  String.concat "\n\n"
    ([ prelude ]
    @ Pydsl.generate_imports stub
    @ [ Pydsl.(show_stub (add_quote_hints stub)) ])
