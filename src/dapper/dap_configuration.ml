module D = Dap_message.Data

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.configurationDone

      type contents = D.ConfigurationDoneArguments.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.configurationDoneRequest

      let enc =
        Dap_request.Message.enc_opt
          Dap_commands.configurationDone
          D.ConfigurationDoneArguments.enc
    end)
    (struct
      type enum = Dap_commands.configurationDone

      type contents = D.EmptyObject.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.configurationDoneResponse

      let enc =
        Dap_response.Message.enc_opt
          Dap_commands.configurationDone
          D.EmptyObject.enc
    end)
