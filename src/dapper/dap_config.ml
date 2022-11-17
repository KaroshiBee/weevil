
type t = {
  backend_ip: string;
  backend_port: int;
  backend_cmd: string;
  backend_echo: string;
  stepper_cmd: string;
}

let _backend_ip = Defaults.Vals._DEFAULT_LISTEN_ADDRESS
let _backend_port = Defaults.Vals._DEFAULT_BACKEND_PORT
let _backend_cmd = Defaults.Vals._DEFAULT_BACKEND_CMD
let _stepper_cmd = Defaults.Vals._DEFAULT_STEPPER_CMD
let _backend_echo = Defaults.Vals._DEFAULT_BACKEND_ECHO

let make ?(backend_ip=_backend_ip) ?(backend_port=_backend_port) ?(backend_cmd=_backend_cmd) ?(stepper_cmd=_stepper_cmd) ?(backend_echo=_backend_echo) () =
  {backend_ip; backend_port; backend_cmd; stepper_cmd; backend_echo}


let make_address addr_str = match _backend_ip = addr_str with
  | true ->
    Unix.inet_addr_loopback
  | false ->
    Unix.inet_addr_of_string addr_str

let backend_port t = t.backend_port

let backend_ip t = t.backend_ip |> make_address

let backend_cmd t = t.backend_cmd

let to_process_command s = ("", s |> String.split_on_char ' ' |> Array.of_list)

let stepper_cmd t = t.stepper_cmd
