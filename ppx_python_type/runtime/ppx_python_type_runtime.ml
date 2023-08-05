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
  | Py_Atomic of python_atomic_type
  | Py_Tuple of python_type_expr list
  | Py_List of python_type_expr
  | Py_Literal of string
  | Py_Union of python_type_expr list
[@@deriving sexp]

type python_type_def =
  | Py_Alias of string * python_type_expr
  | Py_TypedDict of (string * python_type_expr) list
[@@deriving sexp]
