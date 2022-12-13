module D = Dap_messages.Data

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.disconnect

      type contents = D.DisconnectArguments.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.disconnectRequest

      let enc =
        Dap_request.Message.enc_opt Dap_commands.disconnect D.DisconnectArguments.enc
    end)
    (struct
      type enum = Dap_commands.disconnect

      type contents = D.EmptyObject.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.disconnectResponse

      let enc =
        Dap_response.Message.enc_opt Dap_commands.disconnect D.EmptyObject.enc
    end)

module Raise_exited =
  Dap_handlers.Raise_event.Make
    (struct
      type enum = Dap_events.exited

      type contents = D.ExitedEvent_body.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_event.Message.t

      type t = msg Dap_event.t

      let ctor = Dap_event.exitedEvent

      let enc = Dap_event.Message.enc Dap_events.exited D.ExitedEvent_body.enc
    end)

module Internal_request_terminate =
  Dap_handlers.Raise_request.Make
    (struct
      type enum = Dap_commands.terminate

      type contents = D.TerminateArguments.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.terminateRequest

      let enc =
        Dap_request.Message.enc_opt Dap_commands.terminate D.TerminateArguments.enc
    end)
