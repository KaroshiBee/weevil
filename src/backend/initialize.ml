open Handlers
open Dapper.Dap_message
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
module Dap_flow = Dapper.Dap_flow

  (*
Initialization

The Debug Adapter Protocol defines many features and this number is still growing, albeit slowly. However, the protocol is still at its first version because it was an explicit design goal to support new feature in a completely backward compatible way. Making this possible without version numbers requires that every new feature gets a corresponding flag that lets a development tool know whether a debug adapter supports the feature or not. The absence of the flag always means that the feature is not supported.

A single feature and its corresponding flag is called a “capability” in the Debug Adapter Protocol. The open-ended set of all features flags is called DAP’s “capabilities.”

When starting a debug session, the development tool sends an initialize request to the adapter in order to exchange capabilities between the development tool and the debug adapter.

The development tool capabilities are provided in the InitializeRequestArguments structure of the initialize request and typically start with the prefix supports. Other pieces of information passed from the tool to the debug adapter are:

    the name of the development tool,
    the format of file paths, native or uri,
    whether line and column values are 0 or 1 based,
    the locale used by the development tool. A debug adapter is expected to return error messages that honor this locale.

The debug adapter returns the supported capabilities in the InitializeResponse via the Capabilities type. It is not necessary to return an explicit false for unsupported capabilities.
 *)

include MakeReqRespIncludes_withEvent
    (struct
      type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
      type command = Dap_commands.initialize
      let command = Dap_commands.initialize
      type args = InitializeRequestArguments.t
      type presence = RequestMessage.req
      let enc = RequestMessage.enc command InitializeRequestArguments.enc
      let ctor = fun req -> InitializeRequest req
      let extract = Dap_flow.to_request
    end)
    (struct
      type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
      type command = Dap_commands.initialize
      let command = Dap_commands.initialize
      type body = Capabilities.t option
      type presence = ResponseMessage.opt
      let enc = ResponseMessage.enc_opt command Capabilities.enc
      let ctor = fun resp -> InitializeResponse resp
      let extract = Dap_flow.to_response
    end)
    (struct
      type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
      type event = Dap_events.initialized
      let event = Dap_events.initialized
      type body = EmptyObject.t option
      type presence = EventMessage.opt
      let enc = EventMessage.enc_opt event EmptyObject.enc
      let ctor = fun ev -> InitializedEvent ev
      let extract = Dap_flow.to_event
    end)

let on_initialize_request = function
  | InitializeRequest req ->
    let resp =
      let command = RequestMessage.command req in
      (* TODO hardcode capabilities or pull in from config *)
      let body = Capabilities.make () in
      default_response_opt command body
    in
    let ret = InitializeResponse resp in
    Dap_flow.from_response ret
  | _ -> assert false

let on_initialization_response = function
  | InitializeResponse _ ->
    let ev =
      let event = Dap_events.initialized in
      let body = EmptyObject.make () in
      default_event_opt event body
    in
    let ret = InitializedEvent ev in
    Dap_flow.from_event ret
  | _ -> assert false

let handle ~config:_ req =
  let open Dap_flow in
  let response = on_request req on_initialize_request in
  let event = Option.some @@ on_response response on_initialization_response in
  Lwt.return {response; event}
