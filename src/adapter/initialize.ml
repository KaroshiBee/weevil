module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.State_intf) = struct
  include
    Types.Includes2 (Dap.Initialize.On_request) (Dap.Initialize.On_response)

  type state = S.t

  type t = S.t

  let make ?state () = Option.value state ~default:S.make_empty

  let state t = t

  let initialize_handler (_:t) =
    Dap.Initialize.On_request.make ~handler:(fun _config _req ->
        let resp =
          let command = Dap.Commands.initialize in
          (* TODO hardcode capabilities or pull in from config *)
          let body = D.Capabilities.make () in
          Dap.Response.default_response_opt command body
        in
        let ret = Dap.Response.initializeResponse resp in
        Dap_result.ok ret)

  let raise_initialized (_:t) =
    Dap.Initialize.On_response.make ~handler:(fun _config _req ->
        let ev =
          let event = Dap.Events.initialized in
          let body = D.EmptyObject.make () in
          Dap.Event.default_event_opt event body
        in
        let ret = Dap.Event.initializedEvent ev in
        Dap_result.ok ret)

  let handlers =
    convert_handlers ~handler1:initialize_handler ~handler2:raise_initialized
end

include T (State)
