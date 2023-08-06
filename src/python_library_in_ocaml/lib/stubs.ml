open Stdio

let templates_locations = Sites.Sites.templates

let lookup_template filename =
  List.find_map
    (fun dir ->
      let filename' = Stdlib.Filename.concat dir filename in
      if Sys.file_exists filename' then Some filename' else None )
    templates_locations
  |> Option.get

let generate_py_stub ~lib_name ~types:_ ~values:_ =
  In_channel.read_all (lookup_template "stub.py")
  |> Base.String.substr_replace_all ~pattern:"{LIB_NAME}" ~with_:lib_name

let generate_pyi_stub ~lib_name:_ ~types:_ ~values:_ = ""
