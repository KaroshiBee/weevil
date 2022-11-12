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

module type TYPED_HANDLER = Dap_handlers.LINK_T

(* (\* machinery to turn our typed handlers into string -> string handlers *\) *)
(* module type STRING_HANDLER = sig *)
(*   type typed_handler *)

(*   type t *)

(*   type state *)

(*   val make : typed_handler -> t *)

(*   val handle : t -> state -> Dap_config.t -> string -> string Lwt.t *)

(* end *)

(* module MakeStringHandler (H : TYPED_HANDLER) : *)
(*   STRING_HANDLER *)
(*   with type typed_handler := H.t and type state := H.state = struct *)

(*   type t = { *)
(*     typed_handler : H.t; *)
(*   } *)

(*   let make typed_handler = *)
(*     { *)
(*       typed_handler; *)
(*     } *)

(*   let handle t state config s = *)
(*     let%lwt out_msg = *)
(*       match%lwt H.handle t.typed_handler ~state ~config s with *)
(*       | Result.Ok msg -> Lwt.return msg *)
(*       | Result.Error err -> Lwt.return err *)
(*     in *)
(*     Lwt.return out_msg *)

(* end *)
