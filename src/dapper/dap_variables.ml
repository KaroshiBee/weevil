module D = Dap_message.Data

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.variables

      type contents = D.VariablesArguments.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.variablesRequest

      let enc =
        Dap_request.Message.enc
          Dap_commands.variables
          D.VariablesArguments.enc
    end)
    (struct
      type enum = Dap_commands.variables

      type contents = D.VariablesResponse_body.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.variablesResponse

      let enc =
        Dap_response.Message.enc
          Dap_commands.variables
          D.VariablesResponse_body.enc
    end)
