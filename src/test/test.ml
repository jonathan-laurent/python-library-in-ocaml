open Base
open Stdio
open Python_libgen

type enum = A | B [@@deriving python, python_export_type]

type simple_alias = int * (string * float)
[@@deriving python, python_export_type]

type sum_type = C of bool * string | D of enum
[@@deriving python, python_export_type]

type type_with_lists = L of int option list
[@@deriving python, python_export_type]

type record_type = {x: int; y: float option}
[@@deriving python, python_export_type]

type record_type_alias = record_type = {x: int; y: float option}
[@@deriving python, python_export_type]

type ('a, 'b) polymorphic = {x: 'a; y: 'b}
[@@deriving python, python_export_type]

module M = struct
  type loc = int * int * int * int [@@deriving python, python_export_type]

  type 'a with_loc = {data: 'a; loc: loc}
  [@@deriving python, python_export_type]
end

type located_name = string M.with_loc [@@deriving python, python_export_type]

let%python_export f (x : int) : int = x + 1

let%python_export rec fact (n : int) : int =
  if n <= 0 then 1 else n * fact (n - 1)

let%python_export sum (l : int list) : int = List.fold_left ~f:( + ) ~init:0 l

let%python_export make_record (x : int) : record_type = {x; y= None}

let%test_unit "functions preserved" =
  assert (f 5 = 6) ;
  assert (fact 3 = 6) ;
  assert (sum [1; 2; 3] = 6)

let%expect_test "registered types" =
  let open Python_libgen.Repr in
  List.iter (registered_python_types ()) ~f:(fun v ->
      printf !"%{sexp: type_declaration}\n\n" v ) ;
  [%expect
    {|
    ((type_name Enum) (type_vars ()) (definition (Enum (A B))))

    ((type_name SimpleAlias) (type_vars ())
     (definition
      (Alias (Tuple ((Atomic Int) (Tuple ((Atomic String) (Atomic Float))))))))

    ((type_name SumType) (type_vars ())
     (definition
      (Variant
       ((C (Anonymous ((Atomic Bool) (Atomic String))))
        (D (Anonymous ((Atomic (Custom Enum)))))))))

    ((type_name TypeWithLists) (type_vars ())
     (definition (Variant ((L (Anonymous ((List (Option (Atomic Int))))))))))

    ((type_name RecordType) (type_vars ())
     (definition (Record ((x (Atomic Int)) (y (Option (Atomic Float)))))))

    ((type_name RecordTypeAlias) (type_vars ())
     (definition (Record ((x (Atomic Int)) (y (Option (Atomic Float)))))))

    ((type_name Polymorphic) (type_vars (A B))
     (definition (Record ((x (Var A)) (y (Var B))))))

    ((type_name Loc) (type_vars ())
     (definition
      (Alias (Tuple ((Atomic Int) (Atomic Int) (Atomic Int) (Atomic Int))))))

    ((type_name WithLoc) (type_vars (A))
     (definition (Record ((data (Var A)) (loc (Atomic (Custom Loc)))))))

    ((type_name LocatedName) (type_vars ())
     (definition (Alias (App (Custom WithLoc) ((Atomic String)))))) |}]