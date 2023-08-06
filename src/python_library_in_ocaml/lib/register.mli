val register_python_value : Repr.python_value -> unit

val register_python_type : Repr.python_type_declaration -> unit

val register_python_docstring : name:string -> docstring:string -> unit

val registered_python_values : unit -> Repr.python_value list

val registered_python_types : unit -> Repr.python_type_declaration list

val registered_python_docstring : string -> string option
