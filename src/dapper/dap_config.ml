

type launch_mode = [`Launch | `Attach | `AttachForSuspendedLaunch ]
  (* | `Launch of string -> Lwt_io.input_channel * Lwt_io.output_channel *)
  (* | `Attach of Lwt_io.input_channel * Lwt_io.output_channel *)
  (* | `AttachForSuspendedLaunch *)
  (* ] *)


type t = {
  launch_mode : launch_mode;
  backend_cmd: string;
}
