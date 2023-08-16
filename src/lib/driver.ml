open Register

let interpret_command ~generated_module ~use_dataclasses ~lib_name =
  let values = registered_python_values () in
  let types = registered_python_types () in
  function
  | "register" ->
      let internal_module = Create_module.internal_module ~generated_module in
      Create_module.create_module ~name:internal_module values
  | "generate-py" ->
      let settings = Stubs.{ use_dataclasses } in
      print_string
        (Stubs.generate_py_stub ~interface_only:false ~settings
           ~generated_module ~lib_name ~types ~values)
  | "generate-pyi" ->
      let settings = Stubs.{ use_dataclasses } in
      print_string
        (Stubs.generate_py_stub ~interface_only:true ~settings ~generated_module
           ~lib_name ~types ~values)
  | _ -> print_endline "Invalid command."

let run ~generated_module =
  let command = ref "" in
  let lib_name = ref "" in
  let use_dataclasses = ref false in
  Arg.parse
    [
      ( "--lib-name",
        Arg.Set_string lib_name,
        "Name of the library for which an OCaml module is generated." );
      ( "--use-dataclasses",
        Arg.Set use_dataclasses,
        "Use Python dataclasses and enums for a more idiomatic encoding of \
         OCaml values, albeit slower." );
    ]
    (fun cmd -> command := cmd)
    (Printf.sprintf "OCaml module '%s'" generated_module);
  interpret_command ~generated_module ~lib_name:!lib_name
    ~use_dataclasses:!use_dataclasses !command
