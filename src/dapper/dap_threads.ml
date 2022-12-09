module D = Dap_messages.Data

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.threads

      type contents = D.EmptyObject.t option

      type presence = D.Presence.opt

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.threadsRequest

      let enc =
        Dap_request.Message.enc_opt
          Dap_commands.threads
          D.EmptyObject.enc
    end)
    (struct
      type enum = Dap_commands.threads

      type contents = D.ThreadsResponse_body.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.threadsResponse

      let enc =
        Dap_response.Message.enc
          Dap_commands.threads
          D.ThreadsResponse_body.enc
    end)
