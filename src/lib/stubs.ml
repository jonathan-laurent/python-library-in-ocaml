open Base

type settings = {use_dataclasses: bool}

let templates_locations = Sites.Sites.templates

let lookup_template filename =
  List.find_map
    ~f:(fun dir ->
      let filename' = Stdlib.Filename.concat dir filename in
      if Stdlib.Sys.file_exists filename' then Some filename' else None )
    templates_locations
  |> Option.value_exn ~message:("Not found: templates/" ^ filename)

type imports =
  | Literal
  | TypeAlias
  | TypedDict
  | TypeVar
  | Generic
  | Dataclass
  | Enum
[@@deriving eq]

let imported_source = function
  | Literal ->
      ("typing", "Literal")
  | TypeAlias ->
      ("typing", "TypeAlias")
  | TypedDict ->
      ("typing", "TypedDict")
  | TypeVar ->
      ("typing", "TypeVar")
  | Generic ->
      ("typing", "Generic")
  | Dataclass ->
      ("dataclasses", "dataclass")
  | Enum ->
      ("enum", "Enum")

module Env = struct
  type t = {imports: imports Queue.t; type_vars: string Queue.t}

  let create () = {imports= Queue.create (); type_vars= Queue.create ()}

  let ensure_imported env import =
    if not (Queue.mem ~equal:equal_imports env.imports import) then
      Queue.enqueue env.imports import

  let add_typevar env var =
    ensure_imported env TypeVar ;
    if not (Queue.mem ~equal:String.equal env.type_vars var) then
      Queue.enqueue env.type_vars var
end

let add_quotes s = "\"" ^ s ^ "\""

let quote_opt ~quote s = if quote then add_quotes s else s

let show_atomic_type ~quote =
  let open Repr in
  function
  | Bool ->
      "bool"
  | Int ->
      "int"
  | Float ->
      "float"
  | String ->
      "str"
  | Unit ->
      "None"
  | Custom s ->
      quote_opt ~quote s

let show_type_expr ~env ~quote t =
  let open Repr in
  let rec aux ~quote = function
    | Var s ->
        Env.add_typevar env s ; s
    | App (ctor, ts) ->
        quote_opt ~quote
          ( show_atomic_type ~quote:false ctor
          ^ "["
          ^ String.concat ~sep:", " (List.map ts ~f:(aux ~quote:false))
          ^ "]" )
    | Atomic a ->
        show_atomic_type ~quote a
    | Tuple ts ->
        "tuple[" ^ String.concat ~sep:", " (List.map ts ~f:(aux ~quote)) ^ "]"
    | List t | Array t ->
        "list[" ^ aux ~quote t ^ "]"
    | Option t ->
        aux ~quote t ^ " | None"
  in
  aux ~quote t

let show_type_alias ~env ~name alias =
  Env.ensure_imported env TypeAlias ;
  Printf.sprintf "%s: TypeAlias = %s" name alias

let show_typed_dict_record_declaration ~env ~name ~vars fields =
  Env.ensure_imported env TypedDict ;
  let header =
    if List.is_empty vars then
      Printf.sprintf "class %s(TypedDict, total=True):" name
    else (
      Env.ensure_imported env Generic ;
      Printf.sprintf "class %s(TypedDict, Generic[%s], total=True):" name
        (String.concat ~sep:", " vars) )
  in
  let fields =
    List.map fields ~f:(fun (name, t) ->
        Printf.sprintf "    %s: %s" name (show_type_expr ~env ~quote:true t) )
  in
  String.concat ~sep:"\n" (header :: fields)

let show_dataclass_record_declaration ~env ~name ~vars fields =
  Env.ensure_imported env Dataclass ;
  let header =
    let gen =
      if List.is_empty vars then ""
      else Printf.sprintf "(Generic[%s])" (String.concat ~sep:", " vars)
    in
    Printf.sprintf "@dataclass\nclass %s%s:" name gen
  in
  let fields =
    if List.is_empty fields then [Printf.sprintf "    pass"]
    else
      List.map fields ~f:(fun (name, t) ->
          Printf.sprintf "    %s: %s" name (show_type_expr ~env ~quote:true t) )
  in
  String.concat ~sep:"\n" (header :: fields)

let show_enum_declaration ~env ~name ~vars cases =
  assert (List.is_empty vars) ;
  Env.ensure_imported env Enum ;
  let header = Printf.sprintf "class %s(Enum):" name in
  let fields =
    List.map cases ~f:(fun s -> Printf.sprintf "    %s = \"%s\"" s s)
  in
  String.concat ~sep:"\n" (header :: fields)

let show_dataclass_variant_declaration ~env ~name ~vars cases =
  Env.ensure_imported env Dataclass ;
  let kids =
    List.map cases ~f:(fun (ctor, args) ->
        let fields =
          match args with
          | Repr.Labeled fields ->
              fields
          | Repr.Anonymous [] ->
              []
          | Repr.Anonymous [x] ->
              [("arg", x)]
          | Repr.Anonymous xs ->
              [("args", Repr.Tuple xs)]
        in
        show_dataclass_record_declaration ~env ~name:ctor ~vars fields )
  in
  let vars_s = String.concat ~sep:", " vars in
  let vars_s = if String.is_empty vars_s then "" else "[" ^ vars_s ^ "]" in
  let union =
    show_type_alias ~env ~name
      (String.concat ~sep:" | "
         (List.map
            ~f:(fun (ctor, _) -> Printf.sprintf "%s%s" ctor vars_s)
            cases ) )
  in
  String.concat ~sep:"\n\n" (kids @ [union])

let dump_labels =
  let open Repr in
  function Anonymous ts -> ts | Labeled lts -> List.map ~f:snd lts

let show_union_variant_declaration ~env ~name ~vars:_ cases =
  Env.ensure_imported env Literal ;
  let cases =
    List.map
      ~f:(fun (ctor, args) ->
        let args = dump_labels args in
        let payload =
          match args with
          | [] ->
              "None"
          | _ ->
              "tuple["
              ^ String.concat ~sep:", "
                  (List.map ~f:(show_type_expr ~env ~quote:true) args)
              ^ "]"
        in
        Printf.sprintf "tuple[Literal[\"%s\"], %s]" ctor payload )
      cases
  in
  show_type_alias ~env ~name (String.concat ~sep:" | " cases)

let show_type_declaration ~settings ~env ~quote td =
  let open Repr in
  let name = td.type_name and vars = td.type_vars in
  match td.definition with
  | Alias t ->
      show_type_alias ~env ~name (show_type_expr ~env ~quote t)
  | Record fields ->
      if settings.use_dataclasses then
        show_dataclass_record_declaration ~env ~name ~vars fields
      else show_typed_dict_record_declaration ~env ~name ~vars fields
  | Variant cases ->
      if settings.use_dataclasses then
        show_dataclass_variant_declaration ~env ~name ~vars cases
      else show_union_variant_declaration ~env ~name ~vars cases
  | Enum cases ->
      if settings.use_dataclasses then
        show_enum_declaration ~env ~name ~vars cases
      else
        let cases = List.map cases ~f:(fun name -> (name, Anonymous [])) in
        show_union_variant_declaration ~env ~name ~vars cases

let indent s =
  String.split_lines s
  |> List.map ~f:(fun s -> "    " ^ s)
  |> String.concat_lines
  |> fun s -> String.drop_suffix s 1
(* remove trailing \n *)

let show_value_declaration ~env ~quote ~generated v =
  let open Repr in
  let mod_ = Create_module.internal_module ~generated in
  match v.signature with
  | Constant t ->
      Printf.sprintf "%s: %s = ..." v.name (show_type_expr ~env ~quote t)
  | Function {args; ret} ->
      let args_untyped =
        String.concat ~sep:", " (List.map args ~f:(fun (a, _) -> a))
      in
      let args_typed =
        String.concat ~sep:", "
          (List.map args ~f:(fun (a, t) ->
               Printf.sprintf "%s: %s" a (show_type_expr ~env ~quote t) ) )
      in
      let header =
        Printf.sprintf "def %s(%s) -> %s:" v.name args_typed
          (show_type_expr ~env ~quote ret)
      in
      let docstring =
        match Register.registered_python_docstring v.name with
        | None ->
            []
        | Some doc ->
            [indent ("\"\"\"\n" ^ doc ^ "\n\"\"\"")]
      in
      let body =
        Printf.sprintf "    return %s.%s(%s)" mod_ v.name args_untyped
      in
      String.concat ~sep:"\n" ([header] @ docstring @ [body])

let show_imports ~env =
  let import_stmts =
    env.Env.imports |> Queue.to_list
    |> List.map ~f:imported_source
    |> List.sort_and_group ~compare:(fun (m, _) (m', _) -> String.compare m m')
    |> List.map ~f:(fun grouped ->
           let m = fst (List.hd_exn grouped) in
           let args = List.map ~f:snd grouped |> String.concat ~sep:", " in
           Printf.sprintf "from %s import %s" m args )
  in
  let type_vars_defs =
    Queue.to_list env.Env.type_vars
    |> List.dedup_and_sort ~compare:String.compare
    |> List.map ~f:(fun v -> Printf.sprintf "%s = TypeVar(\"%s\")" v v)
  in
  import_stmts @ type_vars_defs

let generate_py_stub ~settings ~lib_name ~generated ~types ~values =
  let prelude =
    Stdio.In_channel.read_all (lookup_template "stub.py")
    |> Base.String.substr_replace_all ~pattern:"{LIB_NAME}" ~with_:lib_name
    |> Base.String.substr_replace_all ~pattern:"{GENERATED}" ~with_:generated
    |> Base.String.substr_replace_all ~pattern:"{GENERATED_BY_PYML}"
         ~with_:(Create_module.internal_module ~generated)
  in
  let env = Env.create () in
  let types_section =
    List.map types ~f:(show_type_declaration ~settings ~env ~quote:true)
  in
  let values_section =
    List.map values ~f:(show_value_declaration ~env ~quote:false ~generated)
  in
  let imports = show_imports ~env in
  String.concat ~sep:"\n\n"
    ([prelude] @ imports @ types_section @ values_section @ ["\n"])
