open Dapper.Dap_handler_t
open Dapper.Dap_message
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
module Dap_flow = Dapper.Dap_flow

module Request = struct
  type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
  type command = Dap_commands.configurationDone
  let command = Dap_commands.configurationDone
  type args = ConfigurationDoneArguments.t option
  type presence = RequestMessage.opt
  let enc = RequestMessage.enc_opt command ConfigurationDoneArguments.enc
  let ctor = fun req -> ConfigurationDoneRequest req
  let extract = Dap_flow.to_request
end

module Response = struct
  type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
  type command = Dap_commands.configurationDone
  let command = Dap_commands.configurationDone
  type body = EmptyObject.t option
  type presence = ResponseMessage.opt
  let enc = ResponseMessage.enc_opt command EmptyObject.enc
  let ctor = fun resp -> ConfigurationDoneResponse resp
  let extract = Dap_flow.to_response
end


include MakeReqRespIncludes (Backend_io) (Request) (Response)

let on_configurationDone_request = function
  | ConfigurationDoneRequest req ->
    let resp =
      let command = RequestMessage.command req in
      let body = EmptyObject.make () in
      default_response_opt command body
    in
    let ret = ConfigurationDoneResponse resp in
    Dap_flow.from_response ret
  | _ -> assert false


let handle _t _config req =
  let response = Dap_flow.bind_request req on_configurationDone_request in
  Lwt.return {response}
