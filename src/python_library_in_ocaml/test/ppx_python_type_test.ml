open Stdio
open Python_library_in_ocaml

type enum = A | B [@@deriving python_type]

let%expect_test "enum" =
  printf !"%{sexp: python_type_def}" python_typedef_for_enum ;
  [%expect
    {|
    (Py_Alias Enum
     (Py_Union
      ((Py_Tuple ((Py_Literal A) (Py_Atomic Py_None)))
       (Py_Tuple ((Py_Literal B) (Py_Atomic Py_None)))))) |}]

type simple_alias = int * (string * float) [@@deriving python_type]

let%expect_test "simple_alias" =
  printf !"%{sexp: python_type_def}" python_typedef_for_simple_alias ;
  [%expect
    {|
    (Py_Alias SimpleAlias
     (Py_Tuple
      ((Py_Atomic Py_Int)
       (Py_Tuple ((Py_Atomic Py_String) (Py_Atomic Py_Float)))))) |}]

type sum = C of bool * string | D of enum [@@deriving python_type]

let%expect_test "sum" =
  printf !"%{sexp: python_type_def}" python_typedef_for_sum ;
  [%expect
    {|
    (Py_Alias Sum
     (Py_Union
      ((Py_Tuple
        ((Py_Literal C) (Py_Tuple ((Py_Atomic Py_Bool) (Py_Atomic Py_String)))))
       (Py_Tuple ((Py_Literal D) (Py_Tuple ((Py_Atomic (Py_Custom Enum))))))))) |}]

type with_lists = L of int option list [@@deriving python_type]

let%expect_test "with lists" =
  printf !"%{sexp: python_type_def}" python_typedef_for_with_lists ;
  [%expect
    {|
    (Py_Alias WithLists
     (Py_Union
      ((Py_Tuple
        ((Py_Literal L)
         (Py_Tuple
          ((Py_List (Py_Union ((Py_Atomic Py_Int) (Py_Atomic Py_None))))))))))) |}]

type record = {x: int; y: float option} [@@deriving python_type]

let%expect_test "record" =
  printf !"%{sexp: python_type_def}" python_typedef_for_record ;
  [%expect
    {|
    (Py_TypedDict
     ((x (Py_Atomic Py_Int))
      (y (Py_Union ((Py_Atomic Py_Float) (Py_Atomic Py_None)))))) |}]

module M : sig
  type t [@@deriving python_type]

  type u = int [@@deriving python_type]
end = struct
  type t = int [@@deriving python_type]

  and u = t
end
