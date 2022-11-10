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
module type TYPED_HANDLER = Dap_handlers.HANDLER

module Attach = Dap_attach
module Launch = Dap_launch
module Initialize = Dap_initialize
module Configuration = Dap_configuration

exception Wrong_encoder = Js_msg.Wrong_encoder

(* machinery to turn our typed handlers into string -> string handlers *)
module type STRING_HANDLER = sig
  type typed_handler

  type t

  val make : typed_handler -> t

  val handle : t -> Dap_config.t -> string -> string Lwt.t
end

module MakeStringHandler (H : TYPED_HANDLER) :
  STRING_HANDLER
  with type typed_handler = H.t = struct
  type typed_handler = H.t
  type t = {
    typed_handler : H.t;
    string_to_input : string -> H.in_t Dap_result.t;
    handle : H.t -> config:Dap_config.t -> H.in_t -> H.out_t Dap_result.t;
    output_to_string : H.out_t -> (string, string) Lwt_result.t;
  }

  let make typed_handler =
    {
      typed_handler;
      string_to_input = H.string_to_input;
      output_to_string = H.output_to_string;
      handle = H.handle;
    }

  let handle t config s =
    let v =
      t.string_to_input s
      |> Dap_result.bind ~f:(t.handle t.typed_handler ~config)
      |> Dap_result.to_lwt_error_as_str
    in
    (* turn into (string, string) Result.t and return either msg as the thing to send on to client *)
    let%lwt out_msg =
      match%lwt Lwt_result.(v >>= t.output_to_string) with
      | Result.Ok msg -> Lwt.return msg
      | Result.Error err -> Lwt.return err
    in
    Lwt.return out_msg

end

