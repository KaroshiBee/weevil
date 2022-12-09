module D = Dap_messages.Data

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.next

      type contents = D.NextArguments.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.nextRequest

      let enc =
        Dap_request.Message.enc Dap_commands.next D.NextArguments.enc
    end)
    (struct
      type enum = Dap_commands.next

      type contents = D.EmptyObject.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.nextResponse

      let enc =
        Dap_response.Message.enc_opt Dap_commands.next D.EmptyObject.enc
    end)

module Raise_stopped =
  Dap_handlers.Raise_event.Make
    (struct
      type enum = Dap_events.stopped

      type contents = D.StoppedEvent_body.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_event.Message.t

      type t = msg Dap_event.t

      let ctor = Dap_event.stoppedEvent

      let enc = Dap_event.Message.enc Dap_events.stopped D.StoppedEvent_body.enc
    end)
