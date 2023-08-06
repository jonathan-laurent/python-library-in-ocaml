open Base
open Stdio
open Python_library_in_ocaml

type enum = A | B [@@deriving python, python_export_type]

type simple_alias = int * (string * float)
[@@deriving python, python_export_type]

type sum_type = C of bool * string | D of enum
[@@deriving python, python_export_type]

type type_with_lists = L of int option list
[@@deriving python, python_export_type]

type record_type = {x: int; y: float option}
[@@deriving python, python_export_type]

let%python_export f (x : int) : int = x + 1

let%python_export rec fact (n : int) : int =
  if n <= 0 then 1 else n * fact (n - 1)

let%python_docstring sum = "Sum a list of numbers."

let%python_export sum (l : int list) : int = List.fold_left ~f:( + ) ~init:0 l

let%python_export make_record (x : int) : record_type = {x; y= None}

let%test_unit "functions preserved" =
  assert (f 5 = 6) ;
  assert (fact 3 = 6) ;
  assert (sum [1; 2; 3] = 6)

let%test_unit "docstrings" =
  assert (
    [%equal: string option]
      (registered_python_docstring "sum")
      (Some "Sum a list of numbers.") )

let%expect_test "registered types" =
  List.iter (registered_python_types ()) ~f:(fun v ->
      printf !"%{sexp: python_type_declaration}\n\n" v ) ;
  [%expect
    {|
    ((type_name Enum)
     (definition
      (Py_Alias
       (Py_Union
        ((Py_Tuple ((Py_Literal A) (Py_Atomic Py_None)))
         (Py_Tuple ((Py_Literal B) (Py_Atomic Py_None))))))))

    ((type_name SimpleAlias)
     (definition
      (Py_Alias
       (Py_Tuple
        ((Py_Atomic Py_Int)
         (Py_Tuple ((Py_Atomic Py_String) (Py_Atomic Py_Float))))))))

    ((type_name SumType)
     (definition
      (Py_Alias
       (Py_Union
        ((Py_Tuple
          ((Py_Literal C) (Py_Tuple ((Py_Atomic Py_Bool) (Py_Atomic Py_String)))))
         (Py_Tuple ((Py_Literal D) (Py_Tuple ((Py_Atomic (Py_Custom Enum)))))))))))

    ((type_name TypeWithLists)
     (definition
      (Py_Alias
       (Py_Union
        ((Py_Tuple
          ((Py_Literal L)
           (Py_Tuple
            ((Py_List (Py_Union ((Py_Atomic Py_Int) (Py_Atomic Py_None)))))))))))))

    ((type_name RecordType)
     (definition
      (Py_TypedDict
       ((x (Py_Atomic Py_Int))
        (y (Py_Union ((Py_Atomic Py_Float) (Py_Atomic Py_None)))))))) |}]

let%expect_test "registered values" =
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
       (ret (Py_Atomic Py_Int)))))

    ((pyobject <opaque>) (name make_record) (doc "")
     (signature
      (Py_Function (args ((x (Py_Atomic Py_Int))))
       (ret (Py_Atomic (Py_Custom RecordType)))))) |}]
