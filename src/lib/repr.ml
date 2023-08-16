open Base

type atomic_type = Bool | Int | Float | String | Unit | Custom of string
[@@deriving sexp]

type type_expr =
  | Tvar of string
  | App of atomic_type * type_expr list
  | Tuple of type_expr list
  | List of type_expr
  | Array of type_expr
  | Option of type_expr
  | Callable of type_expr list * type_expr
[@@deriving sexp]

type variant_args =
  | Anonymous of type_expr list
  | Labeled of (string * type_expr) list
[@@deriving sexp]

type type_definition =
  | Alias of type_expr
  | Record of (string * type_expr) list
  | Enum of string list
  | Variant of (string * variant_args) list
[@@deriving sexp]

type type_declaration = {
  type_name : string;
  type_vars : string list;
  definition : type_definition;
}
[@@deriving sexp]

type arg_kind = Positional | Keyword | Optional [@@deriving sexp]

type value_signature =
  | Constant of type_expr
  | Function of { args : (string * arg_kind * type_expr) list; ret : type_expr }
(* Invariant: positional arguments must come first, then named
   arguments and finally optional arguments. *)
(* Invariant: optional arguments must have an option type *)
[@@deriving sexp]

type 'a value = {
  convert : unit -> 'a; [@sexp.opaque]
  name : string;
  signature : value_signature;
}
[@@deriving sexp]
