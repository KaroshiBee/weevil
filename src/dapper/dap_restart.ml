module D = Dap_messages.Data

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.restart

      type contents = D.RestartArguments.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.restartRequest

      let enc =
        Dap_request.Message.enc_opt Dap_commands.restart D.RestartArguments.enc
    end)
    (struct
      type enum = Dap_commands.restart

      type contents = D.EmptyObject.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.restartResponse

      let enc =
        Dap_response.Message.enc_opt Dap_commands.restart D.EmptyObject.enc
    end)

module Internal_request_launch =
  Dap_handlers.Raise_request.Make
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

module Internal_request_attach =
  Dap_handlers.Raise_request.Make
    (struct
      type enum = Dap_commands.attach

      type contents = D.AttachRequestArguments.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.attachRequest

      let enc =
        Dap_request.Message.enc Dap_commands.attach D.AttachRequestArguments.enc
    end)
