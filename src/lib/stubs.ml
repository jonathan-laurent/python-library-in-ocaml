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

type imports = Literal | TypeAlias | TypedDict | Generic | Dataclass | Enum
[@@deriving eq]

module Env = struct
  type t = {imports: imports Queue.t; type_vars: string Queue.t}

  let create () = {imports= Queue.create (); type_vars= Queue.create ()}

  let ensure_imported env import =
    if not (Queue.mem ~equal:equal_imports env.imports import) then
      Queue.enqueue env.imports import

  let add_typevar env var =
    if not (Queue.mem ~equal:String.equal env.type_vars var) then
      Queue.enqueue env.type_vars var
end

let concat sep strs = String.concat ~sep strs

let fmt = Printf.sprintf

let imported_source = function
  | Literal ->
      ("typing", "Literal")
  | TypeAlias ->
      ("typing", "TypeAlias")
  | TypedDict ->
      ("typing", "TypedDict")
  | Generic ->
      ("typing", "Generic")
  | Dataclass ->
      ("dataclasses", "dataclass")
  | Enum ->
      ("enum", "Enum")

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

let show_dataclass_record_declaration ~env:_ ~name:_ ~vars:_ _fields =
  assert false

let show_enum_declaration ~env:_ ~name:_ ~vars:_ _cases = assert false

let show_dataclass_variant_declaration ~env:_ ~name:_ ~vars:_ _cases =
  assert false

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

(* let show_value_declaration ~quote ~imports v =
   let open Repr in
   match v.signature with
   | Constant t ->
       Printf.sprintf "%s: %s = ..." v.name (show_type_expr ~quote ~imports t)
   | Py_Function {args; ret} ->
       let args =
         String.concat ~sep:", "
           (List.map args ~f:(fun (a, t) ->
                Printf.sprintf "%s: %s" a (show_type_expr ~quote ~imports t) ) )
       in
       let header =
         Printf.sprintf "def %s(%s) -> %s:" v.name args
           (show_type_expr ~quote ~imports ret)
       in
       let docstring =
         match Register.registered_python_docstring v.name with
         | None ->
             []
         | Some doc ->
             [indent ("\"\"\"\n" ^ doc ^ "\n\"\"\"")]
       in
       String.concat ~sep:"\n" ([header] @ docstring @ ["    ..."]) *)
