module D = Dap_message.Data

module On_request = Dap_handlers.Request_response.Make (struct
  type enum = Dap_commands.attach

  type in_contents = D.AttachRequestArguments.t

  type in_presence = D.Presence.req

  type out_contents = D.EmptyObject.t option

  type out_presence = D.Presence.opt

  type in_msg = (enum, in_contents, in_presence) Dap_request.Message.t

  type out_msg = (enum, out_contents, out_presence) Dap_response.Message.t

  let ctor_in = Dap_request.attachRequest

  let enc_in = Dap_request.Message.enc Dap_commands.attach D.AttachRequestArguments.enc

  let ctor_out = Dap_response.attachResponse

  let enc_out = Dap_response.Message.enc_opt Dap_commands.attach D.EmptyObject.enc
end)

module On_response = Dap_handlers.Response_event.Make (struct
  type in_enum = Dap_commands.attach

  type in_contents = D.EmptyObject.t option

  type in_presence = D.Presence.opt

  type out_enum = Dap_events.process

  type out_contents = D.ProcessEvent_body.t

  type out_presence = D.Presence.req

  type in_msg = (in_enum, in_contents, in_presence) Dap_response.Message.t

  type out_msg = (out_enum, out_contents, out_presence) Dap_event.Message.t

  let ctor_in = Dap_response.attachResponse

  let enc_in = Dap_response.Message.enc_opt Dap_commands.attach D.EmptyObject.enc

  let ctor_out = Dap_event.processEvent

  let enc_out = Dap_event.Message.enc Dap_events.process D.ProcessEvent_body.enc
end)
