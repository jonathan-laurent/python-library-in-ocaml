open Base

let internal_module ~generated = "_" ^ generated ^ "_internals"

let create_module ~generated values =
  if not (Py.is_initialized ()) then Py.initialize () ;
  let m = Py.Import.add_module (internal_module ~generated) in
  List.iter values ~f:(fun v ->
      Py.Module.set m v.Repr.name (v.Repr.convert ()) )
