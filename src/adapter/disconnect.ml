open Dapper.Dap_handler_t
open Dapper.Dap_message
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
module Dap_flow = Dapper.Dap_flow
module Dap_config = Dapper.Dap_config

(* When the development tool ends a debug session, the sequence of events is slightly different based on whether the session has been initially “launched” or “attached”: *)

(*     Debuggee launched: if a debug adapter supports the terminate request, the development tool uses it to terminate the debuggee gracefully,
       i.e. it gives the debuggee a chance to cleanup everything before terminating.

       If the debuggee does not terminate but continues to run (or hits a breakpoint), the debug session will continue,
       but if the development tool tries again to terminate the debuggee, it will then use the disconnect request to end the debug session unconditionally.

       The disconnect request is expected to terminate the debuggee (and any child processes) forcefully. *)

(*     Debuggee attached: If the debuggee has been “attached” initially, the development tool issues a disconnect request.
       This should detach the debugger from the debuggee but will allow it to continue. *)

(* In all situations where a debug adapter wants to end the debug session, a terminated event must be fired. *)

(* If the debuggee has ended (and the debug adapter is able to detect this), an optional exited event can be issued to return the exit code to the development tool.Debug session end *)



(* The disconnect request asks the debug adapter to disconnect from the debuggee (thus ending the debug session) and then to shut down itself (the debug adapter). *)

(* In addition, the debug adapter must terminate the debuggee if it was started with the launch request. If an attach request was used to connect to the debuggee, then the debug adapter must not terminate the debuggee. *)

(* This implicit behavior of when to terminate the debuggee can be overridden with the terminateDebuggee argument (which is only supported by a debug adapter if the corresponding capability supportTerminateDebuggee is true). *)

module Request = struct
  type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
  type command = Dap_commands.disconnect
  let command = Dap_commands.disconnect
  type args = DisconnectArguments.t option
  type presence = RequestMessage.opt
  let enc = RequestMessage.enc_opt command DisconnectArguments.enc
  let ctor = fun req -> DisconnectRequest req
  let extract = Dap_flow.to_request
end

module Response = struct
  type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
  type command = Dap_commands.disconnect
  let command = Dap_commands.disconnect
  type body = EmptyObject.t option
  type presence = ResponseMessage.opt
  let enc = ResponseMessage.enc_opt command EmptyObject.enc
  let ctor = fun resp -> DisconnectResponse resp
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

let on_disconnect_request = function
  | DisconnectRequest req ->
    let resp =
      let command = RequestMessage.command req in
      let body = EmptyObject.make () in
      default_response_opt command body
    in
    let ret = DisconnectResponse resp in
    Dap_flow.from_response ret
  | _ -> assert false

(* If the debuggee has ended (and the debug adapter is able to detect this), *)
(* an optional exited event can be issued to return the exit code to the development tool. *)
let on_disconnect_response exitCode = function
  | DisconnectResponse _ ->
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


let handle t _config req =
  let open Dap_flow in
  let response = request_response  req on_disconnect_request in
  (* diconnect when launched - terminate debuggee forcefully  *)
  (* disconnect when attached - dont terminate the debuggee *)
  let event = match State.launch_mode t with
    | Some `Launch ->
      Logs.warn (fun m -> m "TODO: shutdown debuggee forcefully; shutdown channel");
      let exitCode = 0 in
      Option.some @@ response_event response (on_disconnect_response exitCode)
    | Some `Attach | Some `AttachForSuspendedLaunch | None ->
      Logs.warn (fun m -> m "TODO: shutdown channel");
      None
  in
  Lwt.return {response; event; error=None}
