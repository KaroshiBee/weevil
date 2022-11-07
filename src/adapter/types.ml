module type State_intf = sig
  type t

  val make_empty : t

  val connect :
    Ipaddr.t -> int -> (Lwt_io.input_channel * Lwt_io.output_channel) Lwt.t

  val process_none : t -> Lwt_process.process_none option

  val set_process_none : t -> Lwt_process.process_none -> unit

  val ic : t -> Lwt_io.input_channel option

  val oc : t -> Lwt_io.output_channel option

  val set_io : t -> Lwt_io.input_channel -> Lwt_io.output_channel -> unit

  val launch_mode : t -> Dapper.Dap.Launch_mode.t option

  val set_launch_mode : t -> Dapper.Dap.Launch_mode.t -> unit
end

module type String_handler_intf = sig
  type t

  type state

  val make : ?state:state -> unit -> t

  val handlers :
    config:Dapper.Dap.Config.t -> t -> (string -> string Lwt.t) list

  val state : t -> state

end
