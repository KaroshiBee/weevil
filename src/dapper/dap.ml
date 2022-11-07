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

module Attach = Request_response.Make (struct
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

module Process = Response_event.Make (struct
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

(* machinery to turn our typed handlers into string -> string handlers *)
module type MAKE_STRING_HANDLER = sig
  type input

  type output

  type backend

  type t

  val make : backend -> t

  val handle : t -> Dap_config.t -> string -> string Lwt.t
end

module MakeStringHandler (H : HANDLER) :
  MAKE_STRING_HANDLER
    with type input := H.in_t
     and type output := H.out_t
     and type backend := H.t = struct
  type t = {
    backend : H.t;
    string_to_input : string -> H.in_t Dap_result.t;
    handle : H.t -> config:Dap_config.t -> H.in_t -> H.out_t Dap_result.t;
    output_to_string : H.out_t -> (string, string) Lwt_result.t;
  }

  let make backend =
    {
      backend;
      string_to_input = H.string_to_input;
      output_to_string = H.output_to_string;
      handle = H.handle;
    }

  let handle t config s =
    let v =
      t.string_to_input s
      |> Dap_result.bind ~f:(t.handle t.backend ~config)
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
