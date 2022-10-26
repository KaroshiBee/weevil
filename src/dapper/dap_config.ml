

type launch_mode = [`Launch | `Attach | `AttachForSuspendedLaunch ]
  (* | `Launch of string -> Lwt_io.input_channel * Lwt_io.output_channel *)
  (* | `Attach of Lwt_io.input_channel * Lwt_io.output_channel *)
  (* | `AttachForSuspendedLaunch *)
  (* ] *)


type t = {
  launch_mode : launch_mode;
  backend_cmd: string;
  backend_echo: string;
}

let _backend_cmd = "dune exec -- ./src/main.exe backend"
let _backend_echo = "echo 1"

let make ~launch_mode ?(backend_cmd=_backend_cmd) ?(backend_echo=_backend_echo) () =
  {launch_mode; backend_cmd; backend_echo}
