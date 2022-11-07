(* main module for everything to do with messages/event enums/command enums *)
module Commands = Dap_commands
module Events = Dap_events
module Js_msg = Dap_js_msg
module Config = Dap_config

include Dap_message.Data
module Request = Dap_request
module Response = Dap_response
module Event = Dap_event


let default_response_error e =
  let id = Hashtbl.hash e in
  let variables = `O [("error", `String e)] in
  let error = Message.make ~id ~format:"{error}" ~variables () in
  let body = ErrorResponse_body.make ~error () in
  Response.Message.make ~seq:Dap_base.Seqr.not_set ~request_seq:Dap_base.Seqr.not_set ~success:false ~command:Commands.error ~body ()
