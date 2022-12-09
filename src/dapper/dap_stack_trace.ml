module D = Dap_messages.Data

module On_request =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap_commands.stackTrace

      type contents = D.StackTraceArguments.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_request.Message.t

      type t = msg Dap_request.t

      let ctor = Dap_request.stackTraceRequest

      let enc =
        Dap_request.Message.enc
          Dap_commands.stackTrace
          D.StackTraceArguments.enc
    end)
    (struct
      type enum = Dap_commands.stackTrace

      type contents = D.StackTraceResponse_body.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.stackTraceResponse

      let enc =
        Dap_response.Message.enc
          Dap_commands.stackTrace
          D.StackTraceResponse_body.enc
    end)
