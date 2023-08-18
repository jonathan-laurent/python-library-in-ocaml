(** Generate a library executable. *)

val run : generated_module:string -> unit
(** Create a library executable exporting all registered values and types. This
    executable should be compiled both as a native executable and as a shared
    object. When called with the [register] argument, it creates an internal
    Python module containing all exported values (see [stub.py] for how such a
    module can be loaded and used). When called with the
    [generate-py --lib-name ...] argument, it outputs a Python stub on stdout. *)
