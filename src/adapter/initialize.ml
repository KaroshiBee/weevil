module Dap = Dapper.Dap
module D = Dap.Data
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.STATE_T) = struct

  module On_request = Dap.Initialize.On_request (S)
  module On_response = Dap.Initialize.Raise_initialized (S)

  let initialize_handler =
    On_request.make ~handler:(fun ~state req ->
        let getargs = Req.(Fmap Message.arguments) in
        let args = Req.(eval @@ map_ (val_ getargs, val_ req)) in
        let () = S.set_client_config state args in
        let resp =
          let command = Dap.Commands.initialize in
          (* TODO pull in from a config file? *)
          let body = D.Capabilities.make
              ~supportsConfigurationDoneRequest:true
              ~supportsRestartRequest:true
              ~supportsTerminateRequest:true
              ()
          in
          Dap.Response.default_response_opt command body
        in
        let ret = Dap.Response.initializeResponse resp in
        Dap.Result.ok ret)

  let raise_initialized =
    On_response.make ~handler:(fun ~state:_ _req ->
        let ev =
          let event = Dap.Events.initialized in
          let body = D.EmptyObject.make () in
          Dap.Event.default_event_opt event body
        in
        let ret = Dap.Event.initializedEvent ev in
        Dap.Result.ok ret)

  let handlers ~state = [
    initialize_handler ~state;
    raise_initialized ~state;
  ]

end
