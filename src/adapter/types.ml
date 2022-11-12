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
  type t

  type state

  val make : ?state:state -> unit -> t

  val handlers :
    state:state -> config:Dap.Config.t -> t -> (string -> string Lwt.t) list

  val state : t -> state

end

module Includes1
    (T1:Dap.TYPED_HANDLER) = struct

  module H1 = Dap.MakeStringHandler (T1)

  let convert_handlers =
    fun ~handler1 ~state ~config t ->
      let h1 =
        let x = H1.make @@ handler1 t in
        H1.handle x state config
      in
      [h1; ]

end

module Includes2
    (T1:Dap.TYPED_HANDLER)
    (T2:Dap.TYPED_HANDLER with type state = T1.state) = struct

  module H1 = Dap.MakeStringHandler (T1)
  module H2 = Dap.MakeStringHandler (T2)

  let convert_handlers =
    fun ~handler1 ~handler2 ~state ~config t ->
      let h1 =
        let x = H1.make @@ handler1 t in
        H1.handle x state config
      in
      let h2 =
        let x = H2.make @@ handler2 t in
        H2.handle x state config
      in
      [h1; h2]

end

module Includes3
    (T1:Dap.TYPED_HANDLER)
    (T2:Dap.TYPED_HANDLER with type state = T1.state)
    (T3:Dap.TYPED_HANDLER with type state = T1.state) = struct

  module H1 = Dap.MakeStringHandler (T1)
  module H2 = Dap.MakeStringHandler (T2)
  module H3 = Dap.MakeStringHandler (T3)

  let convert_handlers =
    fun ~handler1 ~handler2 ~handler3 ~state ~config t ->
      let h1 =
        let x = H1.make @@ handler1 t in
        H1.handle x state config
      in
      let h2 =
        let x = H2.make @@ handler2 t in
        H2.handle x state config
      in
      let h3 =
        let x = H3.make @@ handler3 t in
        H3.handle x state config
      in
      [h1; h2; h3]

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
     which we just want to log (for now) and not respond to,
     that is the type 'b = string of the (_, 'b) Result.t

     TODO return these arb errors as ErrorResponse strs too, will need correct seqr somehow?

  *)
  val handle_exn :
    t ->
    state ->
    Dapper.Dap_config.t ->
    string ->
    (string list, string) Lwt_result.t
end
