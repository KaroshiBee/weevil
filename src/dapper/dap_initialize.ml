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

module D = Dap_message.Data

module On_request = Dap_handlers.Request_response.Make (struct
  type enum = Dap_commands.initialize

  type in_contents = D.InitializeRequestArguments.t

  type in_presence = D.Presence.req

  type out_contents = D.Capabilities.t option

  type out_presence = D.Presence.opt

  type in_msg = (enum, in_contents, in_presence) Dap_request.Message.t

  type out_msg = (enum, out_contents, out_presence) Dap_response.Message.t

  let ctor_in = Dap_request.initializeRequest

  let enc_in = Dap_request.Message.enc Dap_commands.initialize D.InitializeRequestArguments.enc

  let ctor_out = Dap_response.initializeResponse

  let enc_out = Dap_response.Message.enc_opt Dap_commands.initialize D.Capabilities.enc
end)

module On_response = Dap_handlers.Response_event.Make (struct
  type in_enum = Dap_commands.initialize

  type in_contents = D.Capabilities.t option

  type in_presence = D.Presence.opt

  type out_enum = Dap_events.initialized

  type out_contents = D.EmptyObject.t option

  type out_presence = D.Presence.opt

  type in_msg = (in_enum, in_contents, in_presence) Dap_response.Message.t

  type out_msg = (out_enum, out_contents, out_presence) Dap_event.Message.t

  let ctor_in = Dap_response.initializeResponse

  let enc_in = Dap_response.Message.enc_opt Dap_commands.initialize D.Capabilities.enc

  let ctor_out = Dap_event.initializedEvent

  let enc_out = Dap_event.Message.enc_opt Dap_events.initialized D.EmptyObject.enc
end)


(* let on_initialize_request = function *)
(*   | InitializeRequest req -> *)
(*     let resp = *)
(*       let command = RequestMessage.command req in *)
(*       (\* TODO hardcode capabilities or pull in from config *\) *)
(*       let body = Capabilities.make () in *)
(*       default_response_opt command body *)
(*     in *)
(*     let ret = InitializeResponse resp in *)
(*     Dap_flow.from_response ret *)
(*   | _ -> assert false *)

(* let on_initialization_response = function *)
(*   | InitializeResponse _ -> *)
(*     let ev = *)
(*       let event = Dap_events.initialized in *)
(*       let body = EmptyObject.make () in *)
(*       default_event_opt event body *)
(*     in *)
(*     let ret = InitializedEvent ev in *)
(*     Dap_flow.from_event ret *)
(*   | _ -> assert false *)

(* let handle _t _config req = *)
(*   let open Dap_flow in *)
(*   let response = request_response req on_initialize_request in *)
(*   let event = Option.some @@ response_event response on_initialization_response in *)
(*   Lwt.return {response; event; error=None} *)
