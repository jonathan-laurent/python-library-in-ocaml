open Base
open Repr

let __registered_python_values : python_value Queue.t = Queue.create ()

let register_python_value v = Queue.enqueue __registered_python_values v

let registered_python_values () = Queue.to_list __registered_python_values
