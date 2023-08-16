let arg_var = "_x" (* canonical argument name used for binders *)
let quote_char = ':'

type lvalue =
  | Var of string
  | Field of lvalue * string
  | Str_index of lvalue * string
  | Index of lvalue * int

and shape_annot = Same_shape of lvalue

and expr =
  | Ellipsis_expr  (** ... *)
  | None_constant
  | String_constant of string
  | Lvalue of lvalue
  | Call of lvalue * expr list
  | Create_tuple of expr list * shape_annot option
  | Lambda of string * expr
  | Lambda_multi of string list * expr
  | Case_not_none of { tested : expr; expr : expr }
      (** <expr> if <tested> is not None else None *)
  | Comprehension of { var : string; list : expr; expr : expr }
      (** <expr> for <var> in <list> *)
  | Dataclass_of_dict of string * lvalue
  | Dict_of_dataclass of lvalue
  | Create_dataclass of string * (string * expr) list * shape_annot option
  | Create_dict of (string * expr) list * shape_annot option
  | Enum_value of lvalue
  | Str_cases of lvalue * (string * expr) list
  | Type_cases of lvalue * (string * expr) list
  | Let_in of { var : string; assigned : expr; expr : expr }

and instr = Assign of string * expr | Return of expr | Ellipsis
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
  | Callable of type_expr list * type_expr

and arg_kind = Repr.arg_kind = Positional | Keyword | Optional

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
      args : (string * arg_kind * type_expr) list;
      docstring : string option;
      ret : type_expr;
      body : block;
    }
  | Declare_typed_constant of {
      name : string;
      const_type : type_expr;
      const_def : expr;
    }
  | Declare_fun of { name : string; args : string list; body : block }

and stub = item list
[@@deriving eq, visitors { variety = "map" }, visitors { variety = "iter" }]

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
  | Callable (args, ret) ->
      fmt "Callable[[%s], %s]"
        (concat ", " (List.map (show_type ~quote) args))
        (show_type ~quote ret)

let rec show_lvalue = function
  | Var s -> s
  | Field (l, f) -> fmt "%s.%s" (show_lvalue l) f
  | Index (l, i) -> fmt "%s[%d]" (show_lvalue l) i
  | Str_index (l, s) -> fmt "%s[%s]" (show_lvalue l) (add_quotes s)

let rec show_if_chain = function
  | [] -> "NotImplemented"
  | (c, e) :: cases -> fmt "%s if %s else %s" e c (show_if_chain cases)

let rec show_expr = function
  | Ellipsis_expr -> "..."
  | None_constant -> "None"
  | String_constant s -> add_quotes s
  | Lvalue v -> show_lvalue v
  | Call (f, args) ->
      fmt "%s(%s)" (show_lvalue f) (concat ", " (List.map show_expr args))
  | Lambda (v, body) -> fmt "(lambda %s: %s)" v (show_expr body)
  | Lambda_multi ([], e) -> fmt "(lambda: %s)" (show_expr e)
  | Lambda_multi (vs, e) -> fmt "(lambda %s: %s)" (concat ", " vs) (show_expr e)
  | Create_tuple ([], _) -> "()"
  | Create_tuple ([ arg ], _) -> fmt "(%s,)" (show_expr arg)
  | Create_tuple (args, _) -> fmt "(%s)" (concat ", " (List.map show_expr args))
  | Case_not_none { tested; expr } ->
      fmt "%s if %s is not None else None" (show_expr expr) (show_expr tested)
  | Comprehension { var; list; expr } ->
      fmt "[%s for %s in %s]" (show_expr expr) var (show_expr list)
  | Dataclass_of_dict (d, e) -> fmt "%s(**%s)" d (show_lvalue e)
  | Dict_of_dataclass e -> fmt "%s.__dict__" (show_lvalue e)
  | Create_dataclass (d, [], _) -> fmt "%s()" d
  | Create_dataclass (d, args, _) ->
      fmt "%s(%s)" d
        (concat ", "
           (List.map (fun (c, e) -> fmt "%s=%s" c (show_expr e)) args))
  | Create_dict (args, _) ->
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
  | Let_in { var; assigned; expr } ->
      fmt "(lambda %s: %s)(%s)" var (show_expr expr) (show_expr assigned)

let show_instr = function
  | Assign (s, e) -> [ fmt "%s = %s" s (show_expr e) ]
  | Return e -> [ fmt "return %s" (show_expr e) ]
  | Ellipsis -> [ "..." ]

let show_block b = List.concat_map show_instr b

let show_fields = function
  | [] -> [ indent "pass" ]
  | fields ->
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
  | Declare_typed_constant { name; const_type; const_def } ->
      [
        fmt "%s: %s = %s" name
          (show_type ~quote:false const_type)
          (show_expr const_def);
      ]
  | Declare_typed_fun { name; docstring; args; ret; body } ->
      let docstring =
        match docstring with
        | None -> []
        | Some doc -> [ indent_docstring ("\"\"\"\n" ^ doc ^ "\n\"\"\"") ]
      in
      let kwd_args_processed = ref false in
      let show_arg (s, k, t) =
        let t = show_type ~quote:false t in
        let maybe_sep () =
          if !kwd_args_processed then ""
          else (
            kwd_args_processed := true;
            "*, ")
        in
        let base = fmt "%s: %s" s t in
        match k with
        | Positional -> base
        | Keyword -> maybe_sep () ^ base
        | Optional -> maybe_sep () ^ base ^ " = None"
      in

      [
        fmt "def %s(%s) -> %s:" name
          (concat ", " @@ List.map show_arg args)
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
    | Declare_fun _ | Declare_typed_fun _ | Declare_typed_constant _ -> ()
  in
  let visitor =
    object
      inherit [_] iter as super

      method! visit_item () x =
        process_item x;
        super#visit_item () x

      method! visit_Callable () args ret =
        add ("typing", "Callable");
        super#visit_Callable () args ret
    end
  in
  visitor#visit_stub () stub;
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

let interface_only stub =
  List.filter_map
    (function
      | ( Declare_enum _ | Declare_dataclass _ | Declare_typed_dict _
        | Declare_type _ ) as it ->
          Some it
      | Declare_fun _ -> None
      | Declare_typed_fun { name; args; docstring; ret; body = _ } ->
          Some
            (Declare_typed_fun
               { name; args; docstring; ret; body = [ Ellipsis ] })
      | Declare_typed_constant { name; const_type; const_def = _ } ->
          Some
            (Declare_typed_constant
               { name; const_type; const_def = Ellipsis_expr }))
    stub

let simplify_expr = function
  | Create_tuple (args, Some (Same_shape v))
    when Base.List.for_alli args ~f:(fun i a ->
             equal_expr a (Lvalue (Index (v, i)))) ->
      Lvalue v
  | Lambda (s, Call (f, [ Lvalue (Var s') ])) when String.equal s s' -> Lvalue f
  | Lambda_multi (vs, Call (f, es))
    when [%eq: expr list] es (List.map (fun v -> Lvalue (Var v)) vs) ->
      Lvalue f
  | Case_not_none { tested; expr } when equal_expr tested expr -> expr
  | Comprehension { list; var; expr = Lvalue (Var var') }
    when String.equal var var' ->
      list
  | Create_dataclass (d, args, Some (Same_shape v))
    when Base.List.for_all args ~f:(fun (s, e) ->
             equal_expr e (Lvalue (Str_index (v, s)))) ->
      Dataclass_of_dict (d, v)
  | Create_dict (args, Some (Same_shape v))
    when Base.List.for_all args ~f:(fun (s, e) ->
             equal_expr e (Lvalue (Field (v, s)))) ->
      Dict_of_dataclass v
  | Let_in { var; assigned; expr } when equal_expr assigned (Lvalue (Var var))
    ->
      expr
  | Let_in { var; assigned; expr } when equal_expr expr (Lvalue (Var var)) ->
      assigned
  | e -> e

let rec simplify_block = function
  | [ Assign (s, e); Return (Lvalue (Var s')) ] when String.equal s s' ->
      [ Return e ]
  | [] -> []
  | i :: is -> i :: simplify_block is

let optimize_stub stub =
  let visitor =
    object
      inherit [_] map as super
      method! visit_block () s = simplify_block (super#visit_block () s)
      method! visit_expr () e = simplify_expr (super#visit_expr () e)
    end
  in
  visitor#visit_stub () stub
