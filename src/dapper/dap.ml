(* main module for everything to do with messages/event enums/command enums *)
module Commands = Dap_commands
module Events = Dap_events
module Js_msg = Dap_js_msg
module Config = Dap_config
module Launch_mode = Dap_launch_mode
module Header = Dap_header
module Data = Dap_message.Data
module Request = Dap_request
module Response = Dap_response
module Event = Dap_event
module Result = Dap_result

module type STATE_T = Dap_types.STATE_T

module MakeState = Dap_state.T
module Seqr = Dap_base.Seqr
module Error = Dap_error
module Attach = Dap_attach
module Launch = Dap_launch
module Initialize = Dap_initialize
module Configuration = Dap_configuration

exception Wrong_encoder = Js_msg.Wrong_encoder

let rec content_length_message_handler
    ~name
    ~handle_message
    ~content_length
    ic oc =
  let open Lwt in
  match content_length with
  | None -> (
      Logs_lwt.info (fun m -> m "[%s] waiting for content-length messages" name)
      >>= fun _ ->
      Lwt_io.read_line_opt ic >>= function
      | Some msg ->
          Logs_lwt.info (fun m -> m "[%s] got messsage '%s'" name msg) >>= fun _ ->
          (* if theres content length info in there, strip it out *)
          let content_length = Header.content_length msg in
          content_length_message_handler
            ~name
            ~handle_message
            ~content_length
            ic
            oc
      | None -> Logs_lwt.info (fun m -> m "[%s] connection closed" name))
  | Some count ->
      Logs_lwt.info (fun m ->
          m "[%s] got content-length message with length %d" name count)
      >>= fun _ ->
      (* \r\n throw away *)
      Lwt_io.read ~count:2 ic >>= fun header_break ->
      Logs_lwt.info (fun m ->
          m "[%s] got content-length message with length %d and header_break '%s'" name count header_break)
      >>= fun _ ->
      assert (header_break = "\r\n") |> Lwt.return >>= fun _ ->
      Lwt_io.read ~count ic >>= fun msg ->
      Logs_lwt.info (fun m ->
          m "[%s] got content-length message with message '%s'" name msg)
      >>= fun _ ->
      handle_message msg ic oc >>= fun _ ->
      let content_length = None in
      content_length_message_handler ~name ~handle_message ~content_length ic oc
