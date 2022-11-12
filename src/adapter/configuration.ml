module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.State_intf) = struct

  module On_request = Dap.Configuration.On_request (S)
  include Types.Includes1 ( On_request )

  type state = S.t

  type t = S.t

  let make ?state () = Option.value state ~default:S.make

  let state t = t

  let configuration_handler (_:t) =
    On_request.make ~handler:(fun _state _config _req ->
        let resp =
          let command = Dap.Commands.configurationDone in
          let body = D.EmptyObject.make () in
          Dap.Response.default_response_opt command body
        in
        let ret = Dap.Response.configurationDoneResponse resp in
        Dap_result.ok ret)

  let handlers = convert_handlers ~handler1:configuration_handler
end

include T (State)
