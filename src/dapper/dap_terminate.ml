module D = Dap_messages.Data

module On_request =
  Dap_handlers.Request_response.Make
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
    (struct
      type enum = Dap_commands.terminate

      type contents = D.EmptyObject.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.terminateResponse

      let enc =
        Dap_response.Message.enc_opt Dap_commands.terminate D.EmptyObject.enc
    end)

module Raise_terminated =
  Dap_handlers.Raise_event.Make
    (struct
      type enum = Dap_events.terminated

      type contents = D.TerminatedEvent_body.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_event.Message.t

      type t = msg Dap_event.t

      let ctor = Dap_event.terminatedEvent

      let enc = Dap_event.Message.enc_opt Dap_events.terminated D.TerminatedEvent_body.enc
    end)
