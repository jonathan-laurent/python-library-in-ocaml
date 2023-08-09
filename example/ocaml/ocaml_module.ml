open Base
open Python_libgen

type expr = Constant of int | Var of string | Add of expr * expr
[@@deriving python, python_export_type]

let%python_docstring eval =
  {|
    Evaluate an expression given a valuation that maps variables
    to values. Return None if a variable does not appear in the valuation.
  |}

let%python_export rec eval (valuation : (string * int) list) (expr : expr) :
    int option =
  match expr with
  | Constant x ->
      Some x
  | Var s ->
      List.Assoc.find valuation ~equal:String.equal s
  | Add (e, e') ->
      Option.map2 (eval valuation e) (eval valuation e') ~f:( + )

let%python_export rec fact (n : int) : int =
  if n <= 0 then 1 else n * fact (n - 1)

type 'a result = Result of 'a | Error of string
[@@deriving python, python_export_type]

type custommer_data = {age: int; gender: string}
[@@deriving python, python_export_type]

let%python_export custommer_data (name : string) : custommer_data result =
  if String.(name = "Homer Simpson") then Result {age= 39; gender= "male"}
  else Error "Not a custommer."

let () = Python_libgen.Driver.run ~generated:"ocaml_module"
