open Base

type python_atomic_type =
  | Py_Bool
  | Py_Int
  | Py_Float
  | Py_String
  | Py_None
  | Py_Custom of string
[@@deriving sexp]

type python_type_expr =
  | Py_Var of string
  | Py_Apply of python_atomic_type * python_type_expr list
  | Py_Atomic of python_atomic_type
  | Py_Tuple of python_type_expr list
  | Py_List of python_type_expr
  | Py_Literal of string
  | Py_Union of python_type_expr list
[@@deriving sexp]

type python_type_definition =
  | Py_Alias of python_type_expr
  | Py_TypedDict of (string * python_type_expr) list
[@@deriving sexp]

type python_type_declaration =
  {type_name: string; type_vars: string list; definition: python_type_definition}
[@@deriving sexp]

type python_value_signature =
  | Py_Constant of python_type_expr
  | Py_Function of
      {args: (string * python_type_expr) list; ret: python_type_expr}
[@@deriving sexp]

type python_value =
  { pyobject:
      (unit -> Py.Object.t
      [@sexp.opaque]
      (* We allow computing the python object lazily so that the
         interpreter can be started first *) )
  ; name: string
  ; doc: string
  ; signature: python_value_signature }
[@@deriving sexp]
