
type t = {
  backend_ip: string;
  backend_port: int;
  backend_cmd: string;
  stepper_cmd:
    script_filename:string ->
    storage:string ->
    parameter:string ->
    entrypoint:string ->
    string;
}

let _backend_ip = Defaults.Vals._DEFAULT_LISTEN_ADDRESS
let _backend_port = Defaults.Vals._DEFAULT_BACKEND_PORT
let _backend_cmd = Defaults.Vals._DEFAULT_BACKEND_CMD
let _stepper_cmd = Defaults.Vals._DEFAULT_STEPPER_CMD

let make
    ?(backend_ip=_backend_ip)
    ?(backend_port=_backend_port)
    ?(backend_cmd=_backend_cmd)
    ?(stepper_cmd=_stepper_cmd)
    () =
  {backend_ip; backend_port; backend_cmd; stepper_cmd; }

let make_address addr_str = match _backend_ip = addr_str with
  | true ->
    Unix.inet_addr_loopback
  | false ->
    Unix.inet_addr_of_string addr_str

let backend_port t = t.backend_port

let backend_ip t = t.backend_ip |> make_address

let backend_cmd t = t.backend_cmd

let to_process_command s = Lwt_process.shell s

let stepper_cmd ~script_filename ~storage ~parameter ~entrypoint t =
  t.stepper_cmd ~script_filename ~storage ~parameter ~entrypoint
