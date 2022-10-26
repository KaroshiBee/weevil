open Dapper.Dap_handler_t
open Dapper.Dap_message
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
module Dap_flow = Dapper.Dap_flow
module Dap_config = Dapper.Dap_config


module Request = struct
  type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
  type command = Dap_commands.attach
  let command = Dap_commands.attach
  type args = AttachRequestArguments.t
  type presence = RequestMessage.req
  let enc = RequestMessage.enc command AttachRequestArguments.enc
  let ctor = fun req -> AttachRequest req
  let extract = Dap_flow.to_request
end

module Response = struct
  type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
  type command = Dap_commands.attach
  let command = Dap_commands.attach
  type body = EmptyObject.t option
  type presence = ResponseMessage.opt
  let enc = ResponseMessage.enc_opt command EmptyObject.enc
  let ctor = fun resp -> AttachResponse resp
  let extract = Dap_flow.to_response
end

module Event = struct
  type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
  type event = Dap_events.process
  let event = Dap_events.process
  type body = ProcessEvent_body.t
  type presence = EventMessage.req
  let enc = EventMessage.enc event ProcessEvent_body.enc
  let ctor = fun ev -> ProcessEvent ev
  let extract = Dap_flow.to_event
end

include MakeReqRespIncludes_withEvent (Request) (Response) (Event)

let on_attach_request config = function
  | AttachRequest req when Dap_config.(config.launch_mode = `Attach) ->
    let resp =
      let command = RequestMessage.command req in
      let body = EmptyObject.make () in
      default_response_opt command body
    in
    let ret = AttachResponse resp in
    Dap_flow.from_response ret
  | AttachRequest _req when config.launch_mode = `Launch ->
    let err = "wrong attach mode - config is set to Launch but got an Attach request message" in
    Logs.err (fun m -> m "%s" err) ;
    Dap_flow.from_result @@ Result.error err
  | _ -> assert false

let on_attach_response config = function
  | AttachResponse _ when Dap_config.(config.launch_mode = `Attach) ->
    let ev =
      let event = Dap_events.process in
      let startMethod = ProcessEvent_body_startMethod.Attach in
      let body =
        ProcessEvent_body.make
          ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
          ~startMethod
          ()
      in
      default_event_req event body
    in
    let ret = ProcessEvent ev in
    Dap_flow.from_event ret
  | _ -> assert false

let handle _t config req =
  let open Dap_flow in
  let response = bind_request  req (on_attach_request config) in
  let event = Option.some @@ bind_response response (on_attach_response config) in
  Lwt.return {response; event}
