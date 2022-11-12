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

module Converter (S:STATE_T) = struct

  module type TYPED_HANDLER = sig
    type ('inp, 'out) t

    val make : handler:(S.t -> Config.t -> 'inp -> 'out Dap_result.t) -> ('inp, 'out) t

    val handle :
      ('inp, 'out) t ->
      state:S.t ->
      config:Dap_config.t ->
      string ->
      (string, string) Lwt_result.t
  end

  (* machinery to turn our typed handlers into string -> string handlers *)
  module type STRING_HANDLER = sig
    type ('inp, 'out) typed_handler

    type ('inp, 'out) t

    val make : ('inp, 'out) typed_handler -> ('inp, 'out) t

    val handle : ('inp, 'out) t -> S.t -> Dap_config.t -> string -> string Lwt.t

  end

  module Make (H : TYPED_HANDLER) :
    STRING_HANDLER
    with type ('inp, 'out) typed_handler := ('inp, 'out) H.t = struct

    type ('inp, 'out) t = {
      typed_handler : ('inp, 'out) H.t;
    }

    let make typed_handler = {typed_handler}

    let handle t state config s =
      let%lwt out_msg =
        match%lwt H.handle t.typed_handler ~state ~config s with
        | Result.Ok msg -> Lwt.return msg
        | Result.Error err -> Lwt.return err
      in
      Lwt.return out_msg

  end

end



