let () =
  if not (Py.is_initialized ()) then Py.initialize () ;
  let m = Py.Import.add_module "mylib_ocaml" in
  Py.Module.set m "example_value" (Py.List.of_list_map Py.Int.of_int [1; 2; 4])
