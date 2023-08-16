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

let%python_export example_expr : expr = Add (Constant 1, Add (Var "x", Var "y"))

let%python_export rec rename_expr (renaming : string -> string) (expr : expr) :
    expr =
  match expr with
  | Var s ->
      Var (renaming s)
  | Constant x ->
      Constant x
  | Add (e, e') ->
      Add (rename_expr renaming e, rename_expr renaming e')

let%python_export rec fact (n : int) : int =
  if n <= 0 then 1 else n * fact (n - 1)

type 'a result = Answer of 'a | Error of string
[@@deriving python, python_export_type]

type custommer_data = {age: int; gender: string}
[@@deriving python, python_export_type]

let%python_export custommer_data (name : string) : custommer_data result =
  if String.(name = "Homer Simpson") then Answer {age= 39; gender= "male"}
  else Error "Not a custommer."

let%python_export log (x : float) ?(base : float = 10.0) : float =
  Float.(log x / log base)

let () = Python_libgen.Driver.run ~generated_module:"ocaml_module"
