(* main module for everything to do with messages/event enums/command enums *)
module Commands = Dap_commands
module Events = Dap_events
module Js_msg = Dap_js_msg
module Config = Dap_config

include Dap_message.Data
module Request = Dap_request
module Response = Dap_response
module Event = Dap_event


open Dap_handlers

module Attach = Request_response.Make(
  struct
    type enum = Commands.attach
    type in_contents = AttachRequestArguments.t
    type in_presence = Presence.req
    type out_contents = EmptyObject.t option
    type out_presence = Presence.opt
    type in_msg = (enum, in_contents, in_presence) Request.Message.t
    type out_msg = (enum, out_contents, out_presence) Response.Message.t

    let ctor_in = Request.attachRequest
    let enc_in = Request.Message.enc Commands.attach AttachRequestArguments.enc
    let ctor_out = Response.attachResponse
    let enc_out = Response.Message.enc_opt Commands.attach EmptyObject.enc
  end)


module Process = Response_event.Make(
  struct
    type in_enum = Commands.attach
    type in_contents = EmptyObject.t option
    type in_presence = Presence.opt
    type out_enum = Events.process
    type out_contents = ProcessEvent_body.t
    type out_presence = Presence.req
    type in_msg = (in_enum, in_contents, in_presence) Response.Message.t
    type out_msg = (out_enum, out_contents, out_presence) Event.Message.t

    let ctor_in = Response.attachResponse
    let enc_in = Response.Message.enc_opt Commands.attach EmptyObject.enc
    let ctor_out = Event.processEvent
    let enc_out = Event.Message.enc Events.process ProcessEvent_body.enc
  end)
