module D = Dap_messages.Data

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.scopes

      type contents = D.ScopesArguments.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.scopesRequest

      let enc =
        Dap_request.Message.enc
          Dap_commands.scopes
          D.ScopesArguments.enc
    end)
    (struct
      type enum = Dap_commands.scopes

      type contents = D.ScopesResponse_body.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.scopesResponse

      let enc =
        Dap_response.Message.enc
          Dap_commands.scopes
          D.ScopesResponse_body.enc
    end)
