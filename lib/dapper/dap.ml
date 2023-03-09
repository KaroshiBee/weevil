(* main module for everything to do with messages/event enums/command enums *)
module Commands = Dap_commands
module Events = Dap_events
module Js_msg = Dap_js_msg
module Config = Dap_config
module Launch_mode = Dap_launch_mode
module Header = Dap_header
module Data = Dap_messages.Data
module Request = Dap_request
module Response = Dap_response
module Event = Dap_event
module Result = Dap_result (* TODO use tezos error plumbing? *)
module Defaults = Utilities.Defaults
module Utils = Dap_utils

module type STATE_T = Dap_types.STATE_T

module MakeState = Dap_state.T
module Seqr = Dap_base.Seqr
module Error = Dap_error
module Attach = Dap_attach
module Launch = Dap_launch
module Initialize = Dap_initialize
module Configuration = Dap_configuration
module Threads = Dap_threads
module Stack_trace = Dap_stack_trace
module Scopes = Dap_scopes
module Variables = Dap_variables
module Next = Dap_next
module Restart = Dap_restart
module Terminate = Dap_terminate
module Disconnect = Dap_disconnect


exception Wrong_encoder = Js_msg.Wrong_encoder

let content_length_message_handler = Header.content_length_message_handler

module Testing_utils = Utilities.Testing_utils
