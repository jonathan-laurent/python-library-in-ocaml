open Register

let interpret_command ~generated ~lib_name =
  let values = registered_python_values () in
  let types = registered_python_types () in
  function
  | "register" ->
      Create_module.create_module ~generated values
  | "generate-py" ->
      let settings = Stubs.{use_dataclasses= false} in
      print_string
        (Stubs.generate_py_stub ~settings ~generated ~lib_name ~types ~values)
  | _ ->
      print_endline "Invalid command."

let run ~generated =
  let command = ref "" in
  let lib_name = ref "" in
  Arg.parse
    [ ( "--lib-name"
      , Arg.Set_string lib_name
      , "Name of the library for which an OCaml module is generated." ) ]
    (fun cmd -> command := cmd)
    (Printf.sprintf "OCaml library '%s'" generated) ;
  interpret_command ~generated ~lib_name:!lib_name !command
