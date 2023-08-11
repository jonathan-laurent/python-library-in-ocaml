open Base
open Repr

let __registered_python_names : unit Hashtbl.M(String).t =
  Hashtbl.create (module String)

let __registered_python_values : Py.Object.t Repr.value Queue.t =
  Queue.create ()

let __registered_python_types : Repr.type_declaration Queue.t = Queue.create ()

let __registered_python_docstrings : string Hashtbl.M(String).t =
  Hashtbl.create (module String)

let check_name_available v =
  if Hashtbl.mem __registered_python_names v then
    failwith (Printf.sprintf "Python name '%s' has been defined already." v)

let register_python_value v =
  check_name_available v.name;
  Queue.enqueue __registered_python_values v

let register_python_type td =
  check_name_available td.type_name;
  Queue.enqueue __registered_python_types td

let register_python_docstring ~name ~docstring =
  if Hashtbl.mem __registered_python_docstrings name then
    failwith
      (Printf.sprintf
         "A docstring for Python value '%s' was already registered." name);
  Hashtbl.set __registered_python_docstrings ~key:name ~data:docstring

let registered_python_docstring name =
  Hashtbl.find __registered_python_docstrings name

let registered_python_values () = Queue.to_list __registered_python_values
let registered_python_types () = Queue.to_list __registered_python_types
