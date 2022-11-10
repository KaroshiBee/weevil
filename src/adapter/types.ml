module Dap = Dapper.Dap

module type State_intf = Dap.STATE_T

module type String_handler_intf = sig
  type t

  type state

  val make : ?state:state -> unit -> t

  val handlers :
    config:Dap.Config.t -> t -> (string -> string Lwt.t) list

  val state : t -> state

end

module Includes1
    (T1:Dap.TYPED_HANDLER) = struct

  module H1 = Dap.MakeStringHandler (T1)

  let convert_handlers =
    fun ~handler1 ~config t ->
      let h1 =
        let x = H1.make @@ handler1 t in
        H1.handle x config
      in
      [h1; ]

end

module Includes2
    (T1:Dap.TYPED_HANDLER)
    (T2:Dap.TYPED_HANDLER) = struct

  module H1 = Dap.MakeStringHandler (T1)
  module H2 = Dap.MakeStringHandler (T2)

  let convert_handlers =
    fun ~handler1 ~handler2 ~config t ->
      let h1 =
        let x = H1.make @@ handler1 t in
        H1.handle x config
      in
      let h2 =
        let x = H2.make @@ handler2 t in
        H2.handle x config
      in
      [h1; h2]

end

module Includes3
    (T1:Dap.TYPED_HANDLER)
    (T2:Dap.TYPED_HANDLER)
    (T3:Dap.TYPED_HANDLER) = struct

  module H1 = Dap.MakeStringHandler (T1)
  module H2 = Dap.MakeStringHandler (T2)
  module H3 = Dap.MakeStringHandler (T3)

  let convert_handlers =
    fun ~handler1 ~handler2 ~handler3 ~config t ->
      let h1 =
        let x = H1.make @@ handler1 t in
        H1.handle x config
      in
      let h2 =
        let x = H2.make @@ handler2 t in
        H2.handle x config
      in
      let h3 =
        let x = H3.make @@ handler3 t in
        H3.handle x config
      in
      [h1; h2; h3]

end
