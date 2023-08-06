open Base
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

let%python_export f (x : int) : int = x + 1

let%python_export rec fact (n : int) : int =
  if n <= 0 then 1 else n * fact (n - 1)

let%python_export sum (l : int list) : int = List.fold_left ~f:( + ) ~init:0 l

let%expect_test "registered" =
  assert (f 5 = 6) ;
  assert (fact 3 = 6) ;
  assert (sum [1; 2; 3] = 6) ;
  List.iter (registered_python_values ()) ~f:(fun v ->
      printf !"%{sexp: python_value}\n\n" v ) ;
  [%expect
    {|
    ((pyobject <opaque>) (name f) (doc "")
     (signature
      (Py_Function (args ((x (Py_Atomic Py_Int)))) (ret (Py_Atomic Py_Int)))))

    ((pyobject <opaque>) (name fact) (doc "")
     (signature
      (Py_Function (args ((n (Py_Atomic Py_Int)))) (ret (Py_Atomic Py_Int)))))

    ((pyobject <opaque>) (name sum) (doc "")
     (signature
      (Py_Function (args ((l (Py_List (Py_Atomic Py_Int)))))
       (ret (Py_Atomic Py_Int))))) |}]
