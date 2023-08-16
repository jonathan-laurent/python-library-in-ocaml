(** A minimalistic runtime representation for OCaml types and values. *)

(** {1 Type expressions} *)

type atomic_type = Bool | Int | Float | String | Unit | Custom of string

type type_expr =
  | Tvar of string
  | App of atomic_type * type_expr list
  | Tuple of type_expr list
  | List of type_expr
  | Array of type_expr
  | Option of type_expr
  | Callable of type_expr list * type_expr

(** {1 Type declarations} *)

type variant_args =
  | Anonymous of type_expr list
  | Labeled of (string * type_expr) list

type type_definition =
  | Alias of type_expr
  | Record of (string * type_expr) list
  | Enum of string list
  | Variant of (string * variant_args) list

type type_declaration = {
  type_name : string;
  type_vars : string list;
  definition : type_definition;
}

(** {1 Value declarations} *)

type arg_kind = Positional | Keyword | Optional

type value_signature =
  | Constant of type_expr
  | Function of { args : (string * arg_kind * type_expr) list; ret : type_expr }

type value = {
  convert : unit -> Py.Object.t;
  name : string;
  signature : value_signature;
}

(** {1 Printing utilities for debugging} *)

val show_type_declaration : type_declaration -> string
val show_value : value -> string
