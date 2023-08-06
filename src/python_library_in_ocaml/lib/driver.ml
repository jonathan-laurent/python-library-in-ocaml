open Create_module
open Register

let run ~name () = create_module ~name (registered_python_values ())
