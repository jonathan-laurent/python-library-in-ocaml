open Base

(* type expr = Constant of int | Var of string | Add of expr * expr

   (** Evaluate an expression given a valuation of its variables. Return
       None if a variable in the expression does not appear in the valuation. *)
   let%python_export rec eval (valuation : (string * int) list) (expr : expr) :
       int option =
     match expr with
     | Constant x ->
         Some x
     | Var s ->
         List.Assoc.find valuation ~equal:String.equal s
     | Add (e, e') ->
         Option.map2 (eval valuation e) (eval valuation e') ~f:( + ) *)

let () =
  if not (Py.is_initialized ()) then Py.initialize () ;
  let m = Py.Import.add_module "mylib_ocaml" in
  Py.Module.set m "example_value" (Py.List.of_list_map Py.Int.of_int [1; 2; 4])
