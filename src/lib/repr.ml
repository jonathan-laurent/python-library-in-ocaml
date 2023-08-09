open Base

type atomic_type = Bool | Int | Float | String | Unit | Custom of string
[@@deriving sexp]

type type_expr =
  | Var of string
  | Atomic of atomic_type
  | App of atomic_type * type_expr list
  | Tuple of type_expr list
  | List of type_expr
  | Array of type_expr
  | Option of type_expr
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

type type_declaration =
  {type_name: string; type_vars: string list; definition: type_definition}
[@@deriving sexp]

type value_signature =
  | Constant of type_expr
  | Function of {args: (string * type_expr) list; ret: type_expr}
[@@deriving sexp]

type 'a value =
  {convert: unit -> 'a [@sexp.opaque]; name: string; signature: value_signature}
[@@deriving sexp]
