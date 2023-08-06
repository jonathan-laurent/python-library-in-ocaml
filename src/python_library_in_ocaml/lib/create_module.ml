open Base

let create_module ~name values =
  if not (Py.is_initialized ()) then Py.initialize () ;
  let m = Py.Import.add_module name in
  List.iter values ~f:(fun v ->
      Py.Module.set m v.Repr.name (v.Repr.pyobject ()) )
