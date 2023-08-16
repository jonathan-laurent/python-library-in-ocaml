(** DSL for a subset of Python that is used to generate stubs.

    Using such a DSL instead of directly outputting Python code as strings
    allows performing all kinds of analyses and optimizations: automatically
    adding imports, optimizing the generated code, generating a corresponding
    *.pyi file, adding quotes for dealing with forward references properly... *)

(** {1 Stub definition} *)

(** Left-value expressions that can be treated as variables. *)
type lvalue =
  | Var of string
  | Field of lvalue * string
  | Str_index of lvalue * string
  | Index of lvalue * int

(** When decorating an expression, such an annotation indicates that this
    expression has the same structure than a given left-value. This is useful to
    perform optimizations. For example, the expression [(x[0], x[1])] can be
    simplified into [x] if [x] is known to be a tuple with two elements. This
    information is indicated by a [shape_annot] hint attached to the tuple. *)
type shape_annot = Same_shape of lvalue

(** A Python expression. *)
type expr =
  | Ellipsis_expr  (** Python ellipsis [...], used for interface files *)
  | None_constant
  | String_constant of string
  | Lvalue of lvalue
  | Call of lvalue * expr list
  | Create_tuple of expr list * shape_annot option
  | Lambda of string list * expr
  | Case_not_none of { tested : expr; expr : expr }
      (** [<expr> if <tested> is not None else None] *)
  | Comprehension of { var : string; list : expr; expr : expr }
      (** [[<expr> for <var> in <list>]] *)
  | Dataclass_of_dict of string * lvalue
  | Dict_of_dataclass of lvalue
  | Create_dataclass of string * (string * expr) list * shape_annot option
  | Create_dict of (string * expr) list * shape_annot option
  | Enum_value of lvalue
  | Str_cases of lvalue * (string * expr) list
  | Type_cases of lvalue * (string * expr) list
  | Let_in of { var : string; assigned : expr; expr : expr }
      (** Encoded in Python using lambdas. *)

(** Python instruction. *)
type instr = Assign of string * expr | Return of expr | Ellipsis

type block = instr list
(** Python block. *)

(** Right-hand side of a type alias definition. *)
type alias_def =
  | Simple of Repr.type_expr
  | Union of Repr.type_expr list
  | Tagged_union of (string * Repr.type_expr list) list

(** Python declaration *)
type item =
  | Declare_enum of { name : string; cases : string list }
  | Declare_dataclass of {
      name : string;
      vars : string list;
      fields : (string * Repr.type_expr) list;
    }
  | Declare_typed_dict of {
      name : string;
      vars : string list;
      fields : (string * Repr.type_expr) list;
    }
  | Declare_type of { name : string; vars : string list; def : alias_def }
  | Declare_typed_fun of {
      name : string;
      args : (string * Repr.arg_kind * Repr.type_expr) list;
      docstring : string option;
      ret : Repr.type_expr;
      body : block;
    }
  | Declare_typed_constant of {
      name : string;
      const_type : Repr.type_expr;
      const_def : expr;
    }
  | Declare_fun of { name : string; args : string list; body : block }

and stub = item list
(** Python stub. *)

(** {1 Manipulation functions} *)

val generate_imports : stub -> string list
(** Analyze a stub and generate all necessary import statements (from modules
    such as [typing] or [datclasses]). *)

val quote_forward_references : stub -> stub
(** Find all forward-references to custom types and mangle them by adding a
    custom prefix that is going to cause {!show_stub} to add quotes around them.
    This functino should be called right before {!show_stub}. *)

val interface_only : stub -> stub
(** Generate an interface file for a given stub, mostly by skipping untyped
    definitions and eliding function bodies. *)

val optimize_stub : stub -> stub
(** Optimize a stub by performing a series of transformations on it (e.g. turn
    [[x for x in l]] into [l]). *)

val show_stub : stub -> string
(** Print a stub into valid Python code. *)
