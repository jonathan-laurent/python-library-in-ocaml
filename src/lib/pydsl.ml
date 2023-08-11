(* The [Arg] expression binds the innermost variable that is introduced
   by either [Lambda] or [Comprehension]. *)

let arg_var = "_x"
let quote_char = ':'

type lvalue =
  | Arg
  | Var of string
  | Field of lvalue * string
  | Str_index of lvalue * string
  | Index of lvalue * int

and expr =
  | None_constant
  | String_constant of string
  | Lvalue of lvalue
  | Call of lvalue * expr list
  | Create_tuple of expr list
  | Lambda of expr (* lambda _x: #1 *)
  | Case_not_none of expr * expr (* #2 if #1 is not None else None *)
  | Comprehension of expr * expr (* [#2 for _x in #1] *)
  | Dataclass_of_dict of string * expr
  | Dict_of_dataclass of expr
  | Create_dataclass of string * (string * expr) list
  | Create_dict of (string * expr) list
  | Enum_value of lvalue
  | Str_cases of lvalue * (string * expr) list
  | Type_cases of lvalue * (string * expr) list

and instr = Assign of string * expr | Return of expr
and block = instr list

and atomic_type = Repr.atomic_type =
  | Bool
  | Int
  | Float
  | String
  | Unit
  | Custom of string

and type_expr = Repr.type_expr =
  | Tvar of string
  | App of atomic_type * type_expr list
  | Tuple of type_expr list
  | List of type_expr
  | Array of type_expr
  | Option of type_expr

and alias_def =
  | Simple of type_expr
  | Union of type_expr list
  | Tagged_union of (string * type_expr list) list

and item =
  | Declare_enum of { name : string; cases : string list }
  | Declare_dataclass of {
      name : string;
      vars : string list;
      fields : (string * type_expr) list;
    }
  | Declare_typed_dict of {
      name : string;
      vars : string list;
      fields : (string * type_expr) list;
    }
  | Declare_type of { name : string; vars : string list; def : alias_def }
  | Declare_typed_fun of {
      name : string;
      args : (string * type_expr) list;
      docstring : string option;
      ret : type_expr;
      body : block;
    }
  | Declare_fun of { name : string; args : string list; body : block }

and stub = item list [@@deriving visitors { variety = "map" }]

let fmt = Printf.sprintf
let concat sep = Base.String.concat ~sep
let indent s = "    " ^ s
let add_quotes s = "\"" ^ s ^ "\""

type quoting_hint = Quote | No_quote

let extract_quoting_hint s =
  if s.[0] = quote_char then (String.sub s 1 (String.length s - 1), Quote)
  else (s, No_quote)

let show_atomic_type =
  let open Repr in
  function
  | Bool -> ("bool", No_quote)
  | Int -> ("int", No_quote)
  | Float -> ("float", No_quote)
  | String -> ("str", No_quote)
  | Unit -> ("None", No_quote)
  | Custom s -> extract_quoting_hint s

let rec show_type ~quote t =
  let open Repr in
  match t with
  | Tvar s -> s
  | App (ctor, ts) ->
      let ctor, hint = show_atomic_type ctor in
      let res =
        if ts = [] then ctor
        else
          fmt "%s[%s]" ctor (concat ", " (List.map (show_type ~quote:false) ts))
      in
      if quote && hint = Quote then add_quotes res else res
  | Tuple ts -> "tuple[" ^ concat ", " (List.map (show_type ~quote) ts) ^ "]"
  | List t | Array t -> "list[" ^ show_type ~quote t ^ "]"
  | Option t -> show_type ~quote t ^ " | None"

let rec show_lvalue = function
  | Arg -> arg_var
  | Var s -> s
  | Field (l, f) -> fmt "%s.%s" (show_lvalue l) f
  | Index (l, i) -> fmt "%s[%d]" (show_lvalue l) i
  | Str_index (l, s) -> fmt "%s[%s]" (show_lvalue l) (add_quotes s)

let rec show_if_chain = function
  | [] -> "NotImplemented"
  | (c, e) :: cases -> fmt "%s if %s else %s" e c (show_if_chain cases)

let rec show_expr = function
  | None_constant -> "None"
  | String_constant s -> add_quotes s
  | Lvalue v -> show_lvalue v
  | Call (f, args) ->
      fmt "%s(%s)" (show_lvalue f) (concat ", " (List.map show_expr args))
  | Lambda body -> fmt "(lambda %s: %s)" arg_var (show_expr body)
  | Create_tuple [] -> "()"
  | Create_tuple [ arg ] -> fmt "(%s,)" (show_expr arg)
  | Create_tuple args -> fmt "(%s)" (concat ", " (List.map show_expr args))
  | Case_not_none (e1, e2) ->
      fmt "%s if %s is not None else None" (show_expr e2) (show_expr e1)
  | Comprehension (l, e) ->
      fmt "[%s for %s in %s]" (show_expr e) arg_var (show_expr l)
  | Dataclass_of_dict (d, e) -> fmt "%s(**(%s))" d (show_expr e)
  | Dict_of_dataclass e -> fmt "(%s).__dict__" (show_expr e)
  | Create_dataclass (d, []) -> fmt "%s()" d
  | Create_dataclass (d, args) ->
      fmt "%s(%s)" d
        (concat ", "
           (List.map (fun (c, e) -> fmt "%s=%s" c (show_expr e)) args))
  | Create_dict args ->
      fmt "{%s}"
        (concat ", "
           (List.map
              (fun (c, e) -> fmt "%s: %s" (add_quotes c) (show_expr e))
              args))
  | Enum_value l -> fmt "%s.value" (show_lvalue l)
  | Str_cases (arg, cases) ->
      show_if_chain
        (List.map
           (fun (s, e) ->
             (fmt "%s == %s" (show_lvalue arg) (add_quotes s), show_expr e))
           cases)
  | Type_cases (arg, cases) ->
      show_if_chain
        (List.map
           (fun (s, e) ->
             (fmt "isinstance(%s, %s)" (show_lvalue arg) s, show_expr e))
           cases)

let show_instr = function
  | Assign (s, e) -> [ fmt "%s = %s" s (show_expr e) ]
  | Return e -> [ fmt "return %s" (show_expr e) ]

let show_block b = List.concat_map show_instr b

let show_fields fields =
  List.map
    (fun (s, t) -> indent (fmt "%s: %s" s (show_type ~quote:true t)))
    fields

let class_decl name args =
  let parents =
    match args with [] -> "" | _ -> "(" ^ concat ", " args ^ ")"
  in
  fmt "class %s%s:" name parents

let generic = function
  | [] -> []
  | vars -> [ fmt "Generic[%s]" (concat ", " vars) ]

let make_union = function
  | [ arg ] -> arg
  | args -> fmt "Union[%s]" (concat ", " args)

let show_alias_def = function
  | Simple t -> show_type ~quote:true t
  | Union ts -> make_union (List.map (show_type ~quote:true) ts)
  | Tagged_union ctors ->
      let aux (tag, ts) =
        fmt "tuple[Literal[\"%s\"], %s]" tag
          (show_type ~quote:true
             (if ts = [] then Repr.(App (Unit, [])) else Repr.Tuple ts))
      in
      make_union (List.map aux ctors)

let indent_docstring s =
  Base.String.split_lines s
  |> List.map (fun s -> "    " ^ s)
  |> Base.String.concat_lines
  |> fun s -> Base.String.drop_suffix s 1

let show_item_lines = function
  | Declare_enum { name; cases } ->
      [ class_decl name [ "Enum" ] ]
      @ List.map (fun f -> indent (fmt "%s = \"%s\"" f f)) cases
  | Declare_dataclass { name; vars; fields } ->
      [ "@dataclass"; class_decl name (generic vars) ] @ show_fields fields
  | Declare_typed_dict { name; vars; fields } ->
      [ class_decl name ([ "TypedDict" ] @ generic vars @ [ "total=True" ]) ]
      @ show_fields fields
  | Declare_type { name; def; _ } ->
      [ fmt "%s: TypeAlias = %s" name (show_alias_def def) ]
  | Declare_fun { name; args; body } ->
      fmt "def %s(%s):" name (concat ", " args)
      :: List.map indent (show_block body)
  | Declare_typed_fun { name; docstring; args; ret; body } ->
      let docstring =
        match docstring with
        | None -> []
        | Some doc -> [ indent_docstring ("\"\"\"\n" ^ doc ^ "\n\"\"\"") ]
      in
      [
        fmt "def %s(%s) -> %s:" name
          (concat ", "
          @@ List.map
               (fun (s, t) -> fmt "%s: %s" s (show_type ~quote:false t))
               args)
          (show_type ~quote:false ret);
      ]
      @ docstring
      @ List.map indent (show_block body)

let show_item i = concat "\n" (show_item_lines i)
let show_stub s = concat "\n\n" (List.map show_item s)

let add_quote_hints stub =
  let defined = Hashtbl.create 100 in
  let add_defined s = Hashtbl.add defined s () in
  let is_defined s = Hashtbl.mem defined s in
  let visitor =
    object
      inherit [_] map as super

      method! visit_Custom () s =
        if is_defined s then Custom s else Custom (String.make 1 quote_char ^ s)

      method! visit_Declare_enum () name cases =
        let res = super#visit_Declare_enum () name cases in
        add_defined name;
        res

      method! visit_Declare_dataclass () name vars fields =
        let res = super#visit_Declare_dataclass () name vars fields in
        add_defined name;
        res

      method! visit_Declare_typed_dict () name vars def =
        let res = super#visit_Declare_typed_dict () name vars def in
        add_defined name;
        res

      method! visit_Declare_type () name vars def =
        let res = super#visit_Declare_type () name vars def in
        add_defined name;
        res
    end
  in
  visitor#visit_stub () stub

let generate_imports stub =
  let module Queue = Base.Queue in
  let imports = Queue.create () in
  let vars = Queue.create () in
  let add imp = Queue.enqueue imports imp in
  let add_vars ?(import_generic = false) vs =
    List.iter (Queue.enqueue vars) vs;
    if List.length vs > 0 then (
      if import_generic then add ("typing", "Generic");
      add ("typing", "TypeVar"))
  in
  let process_alias_def = function
    | Tagged_union _ ->
        add ("typing", "Literal");
        add ("typing", "Union")
    | Union _ -> add ("typing", "Union")
    | _ -> ()
  in
  let process_item = function
    | Declare_enum _ -> add ("enum", "Enum")
    | Declare_dataclass { vars; _ } ->
        add ("dataclasses", "dataclass");
        add_vars ~import_generic:true vars
    | Declare_typed_dict { vars; _ } ->
        add ("typing", "TypedDict");
        add_vars ~import_generic:true vars
    | Declare_type { vars; def; _ } ->
        add ("typing", "TypeAlias");
        add_vars vars;
        process_alias_def def
    | Declare_fun _ | Declare_typed_fun _ -> ()
  in
  List.iter process_item stub;
  let import_stmts =
    imports |> Queue.to_list
    |> Base.List.dedup_and_sort ~compare:[%ord: string * string]
    |> Base.List.sort_and_group ~compare:(fun (m, _) (m', _) ->
           String.compare m m')
    |> List.map (fun grouped ->
           let m = fst (List.hd grouped) in
           let args = concat ", " (List.map snd grouped) in
           fmt "from %s import %s" m args)
  in
  let type_vars_defs =
    Queue.to_list vars
    |> Base.List.dedup_and_sort ~compare:String.compare
    |> List.map (fun v -> Printf.sprintf "%s = TypeVar(\"%s\")" v v)
  in
  import_stmts @ type_vars_defs
