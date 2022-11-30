module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

(* TODO old one had a stopped event for thread id *)
module T (S : Types.STATE_READONLY_T) = struct

  module On_request = Dap.Configuration.On_request (S)

  let configuration_handler =
    On_request.make ~handler:(fun ~state:_ _req ->
        let resp =
          let command = Dap.Commands.configurationDone in
          let body = D.EmptyObject.make () in
          Dap.Response.default_response_opt command body
        in
        let ret = Dap.Response.configurationDoneResponse resp in
        Dap_result.ok ret)

  let handlers ~state = [
    configuration_handler ~state;
  ]

end
