val register_python_value : Py.Object.t Repr.value -> unit
val register_python_type : Repr.type_declaration -> unit
val register_python_docstring : name:string -> docstring:string -> unit
val registered_python_values : unit -> Py.Object.t Repr.value list
val registered_python_types : unit -> Repr.type_declaration list
val registered_python_docstring : string -> string option
