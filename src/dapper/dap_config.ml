
type t = {
  backend_port: int;
  backend_cmd: string;
  backend_echo: string;
  stepper_cmd: string;
}
let _backend_port = Defaults.Vals._DEFAULT_BACKEND_PORT
let _backend_cmd = "dune exec -- ./src/main.exe backend"
let _stepper_cmd = "dune exec -- ./src/main.exe stepper"
let _backend_echo = "echo 1"

let make ?(backend_port=_backend_port) ?(backend_cmd=_backend_cmd) ?(stepper_cmd=_stepper_cmd) ?(backend_echo=_backend_echo) () =
  {backend_port; backend_cmd; stepper_cmd; backend_echo}

let backend_port t = t.backend_port

let to_command s = ("", s |> String.split_on_char ' ' |> Array.of_list)
