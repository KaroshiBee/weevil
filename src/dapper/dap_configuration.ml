module D = Dap_message.Data

module On_request = Dap_handlers.Request_response.Make (struct
  type enum = Dap_commands.configurationDone

  type in_contents = D.ConfigurationDoneArguments.t option

  type in_presence = D.Presence.opt

  type out_contents = D.EmptyObject.t option

  type out_presence = D.Presence.opt

  type in_msg = (enum, in_contents, in_presence) Dap_request.Message.t

  type out_msg = (enum, out_contents, out_presence) Dap_response.Message.t

  let ctor_in = Dap_request.configurationDoneRequest

  let enc_in = Dap_request.Message.enc_opt Dap_commands.configurationDone D.ConfigurationDoneArguments.enc

  let ctor_out = Dap_response.configurationDoneResponse

  let enc_out = Dap_response.Message.enc_opt Dap_commands.configurationDone D.EmptyObject.enc
end)
