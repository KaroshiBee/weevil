module D = Dap_message.Data

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.launch

      type contents = D.LaunchRequestArguments.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.launchRequest

      let enc =
        Dap_request.Message.enc Dap_commands.launch D.LaunchRequestArguments.enc
    end)
    (struct
      type enum = Dap_commands.launch

      type contents = D.EmptyObject.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.launchResponse

      let enc =
        Dap_response.Message.enc_opt Dap_commands.launch D.EmptyObject.enc
    end)

module On_response =
  Dap_handlers.Response_event.Make
    (struct
      type enum = Dap_commands.launch

      type contents = D.EmptyObject.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.launchResponse

      let enc =
        Dap_response.Message.enc_opt Dap_commands.launch D.EmptyObject.enc
    end)
    (struct
      type enum = Dap_events.process

      type contents = D.ProcessEvent_body.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_event.Message.t

      type t = msg Dap_event.t

      let ctor = Dap_event.processEvent

      let enc = Dap_event.Message.enc Dap_events.process D.ProcessEvent_body.enc
    end)
