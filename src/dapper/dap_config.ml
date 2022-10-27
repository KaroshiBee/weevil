

type launch_mode = [`Launch of int | `Attach of int | `AttachForSuspendedLaunch of int]
  (* | `Launch of string -> Lwt_io.input_channel * Lwt_io.output_channel *)
  (* | `Attach of Lwt_io.input_channel * Lwt_io.output_channel *)
  (* | `AttachForSuspendedLaunch *)
  (* ] *)


type t = {
  launch_mode : launch_mode;
  backend_cmd: string;
  backend_echo: string;
  stepper_cmd: string;
}

let _backend_cmd = "dune exec -- ./src/main.exe backend"
let _stepper_cmd = "dune exec -- ./src/main.exe stepper"
let _backend_echo = "echo 1"

let make ~launch_mode ?(backend_cmd=_backend_cmd) ?(stepper_cmd=_stepper_cmd) ?(backend_echo=_backend_echo) () =
  {launch_mode; backend_cmd; stepper_cmd; backend_echo}

let backend_port t = match t.launch_mode with
  | `Launch p | `Attach p | `AttachForSuspendedLaunch p -> p

let to_command s = ("", s |> String.split_on_char ' ' |> Array.of_list)
