module D = Dap_message.Data

module Raise_error =
  Dap_handlers.Raise_response.Make
    (struct
      type enum = Dap_commands.error

      type contents = D.ErrorResponse_body.t

      type presence = D.Presence.req

      type msg = (enum, contents, presence) Dap_response.Message.t

      type t = msg Dap_response.t

      let ctor = Dap_response.errorResponse

      let enc = Dap_response.Message.enc Dap_commands.error D.ErrorResponse_body.enc
    end)
