open Register

let interpret_command ~lib_name =
  let values = registered_python_values () in
  let types = registered_python_types () in
  function
  | "register" ->
      Create_module.create_module ~lib_name values
  | "generate-py" ->
      print_string (Stubs.generate_py_stub ~lib_name ~types ~values)
  | "generate-pyi" ->
      print_string (Stubs.generate_pyi_stub ~lib_name ~types ~values)
  | _ ->
      print_endline "Invalid command."

let run ~lib_name () = Arg.parse [] (interpret_command ~lib_name) ""
