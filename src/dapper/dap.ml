(* main module for everything to do with messages/event enums/command enums *)
module Commands = Dap_commands
module Events = Dap_events
module Js_msg = Dap_js_msg
module Header = Dap_header

module Config = Dap_config

module Data = Dap_message.Data
module Request = Dap_request
module Response = Dap_response
module Event = Dap_event
module Dap_result = Dap_result

module type STATE_T = Dap_handlers.STATE_T
module MakeState = Dap_handlers.State
module Seqr = Dap_base.Seqr

module Attach = Dap_attach
module Launch = Dap_launch
module Initialize = Dap_initialize
module Configuration = Dap_configuration

exception Wrong_encoder = Js_msg.Wrong_encoder
