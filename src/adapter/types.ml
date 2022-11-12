module Dap = Dapper.Dap

module type State_intf = sig
  include Dap.STATE_T

  val process_none : t -> Lwt_process.process_none option

  val ic : t -> Lwt_io.input_channel option

  val oc : t -> Lwt_io.output_channel option

  val start_backend : t -> Ipaddr.t -> int -> string -> unit Dap.Dap_result.t

  val connect_backend : t -> Ipaddr.t -> int -> (Lwt_io.input_channel * Lwt_io.output_channel) Dap.Dap_result.t

  val launch_mode : t -> Dap.Data.Launch_mode.t option

  val set_launch_mode : t -> Dap.Data.Launch_mode.t -> unit

end


module type String_handler_intf = sig

  type state

  val handlers :
    state:state -> config:Dap.Config.t -> (string -> (string, string) Lwt_result.t) list

end


module type Handler_intf = sig

  type t

  type state

  val make : t

  (* NOTE this is subtle,
     we want to turn all messages that need to go back to the front end into strings,
     so happy-path messages *and* error message responses,
     they live in the type 'a = string list of the ('a, _) Result.t,

     however we can still have errors (e.g. not being able to decode the incoming message at all)
     which we also need to deal with
     that is the type 'b = string of the (_, 'b) Result.t

     TODO return these arb errors as ErrorResponse strs too

  *)
  val handle_exn :
    t ->
    state ->
    Dapper.Dap_config.t ->
    string ->
    (string list, string) Lwt_result.t
end
