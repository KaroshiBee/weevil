open Dapper.Dap_handler_t
open Dapper.Dap_message
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
module Dap_flow = Dapper.Dap_flow

(* The terminate request is sent from the client to the debug adapter in order to shut down the debuggee gracefully. Clients should only call this request if the capability supportsTerminateRequest is true. *)

(* Typically a debug adapter implements terminate by sending a software signal which the debuggee intercepts in order to clean things up properly before terminating itself. *)

(* Please note that this request does not directly affect the state of the debug session: if the debuggee decides to veto the graceful shutdown for any reason by not terminating itself, then the debug session just continues. *)

(* Clients can surface the terminate request as an explicit command or they can integrate it into a two stage Stop command that first sends terminate to request a graceful shutdown, and if that fails uses disconnect for a forceful shutdown. *)


module Request = struct
  type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
  type command = Dap_commands.terminate
  let command = Dap_commands.terminate
  type args = TerminateArguments.t option
  type presence = RequestMessage.opt
  let enc = RequestMessage.enc_opt command TerminateArguments.enc
  let ctor = fun req -> TerminateRequest req
  let extract = Dap_flow.to_request
end

module Response = struct
  type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
  type command = Dap_commands.terminate
  let command = Dap_commands.terminate
  type body = EmptyObject.t option
  type presence = ResponseMessage.opt
  let enc = ResponseMessage.enc_opt command EmptyObject.enc
  let ctor = fun resp -> TerminateResponse resp
  let extract = Dap_flow.to_response
end

module Event = struct
  type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
  type event = Dap_events.exited
  let event = Dap_events.exited
  type body = ExitedEvent_body.t
  type presence = EventMessage.req
  let enc = EventMessage.enc event ExitedEvent_body.enc
  let ctor = fun ev -> ExitedEvent ev
  let extract = Dap_flow.to_event
end


include MakeReqRespIncludes_withEvent (Request) (Response) (Event)

let on_terminate_request = function
  | TerminateRequest req ->
    let resp =
      let command = RequestMessage.command req in
      let body = EmptyObject.make () in
      default_response_opt command body
    in
    let ret = TerminateResponse resp in
    Dap_flow.from_response ret
  | _ -> assert false

(* If the debuggee has ended (and the debug adapter is able to detect this), *)
(* an optional exited event can be issued to return the exit code to the development tool. *)
let on_terminate_response exitCode = function
  | TerminateResponse _ ->
    let ev =
      let event = Dap_events.exited in
      let body =
        ExitedEvent_body.make
          ~exitCode
          ()
      in
      default_event_req event body
    in
    let ret = ExitedEvent ev in
    Dap_flow.from_event ret
  | _ -> assert false

let handle _t _config req =
  let open Dap_flow in
  let response = bind_request req on_terminate_request in
  Logs.warn (fun m -> m "TODO: shutdown debuggee gracefully; shutdown channel");
  let event =
    let exitCode = 0 in
    Option.some @@ bind_response response (on_terminate_response exitCode)
  in
  Lwt.return {response; event}
