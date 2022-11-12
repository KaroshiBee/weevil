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

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.initialize

      type contents = D.InitializeRequestArguments.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.initializeRequest

      let enc =
        Dap_request.Message.enc
          Dap_commands.initialize
          D.InitializeRequestArguments.enc
    end)
    (struct
      type enum = Dap_commands.initialize

      type contents = D.Capabilities.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.initializeResponse

      let enc =
        Dap_response.Message.enc_opt Dap_commands.initialize D.Capabilities.enc
    end)

module On_response =
  Dap_handlers.Response_event.Make
    (struct
      type enum = Dap_commands.initialize

      type contents = D.Capabilities.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.initializeResponse

      let enc =
        Dap_response.Message.enc_opt Dap_commands.initialize D.Capabilities.enc
    end)
    (struct
      type enum = Dap_events.initialized

      type contents = D.EmptyObject.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_event.Message.t

      type t = msg Dap_event.t

      let ctor = Dap_event.initializedEvent

      let enc =
        Dap_event.Message.enc_opt Dap_events.initialized D.EmptyObject.enc
    end)
