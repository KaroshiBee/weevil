(* there are three message types: Request, Response, Event
   they all three have to conform to the ProtocolMessage spec
   which is having fields "seq" & "type",
   they also have their own additional specs,
   all leaf types are auto-generated (enums, plain objects etc)
   and written to Dap_enum.ml and then used in the following functors

   NOTE According to the spec
   - The arguments field of Requests can sometimes be optional,

   - The body field of Event and Response can sometimes be optional

   - The message field of Response is supposed to be an _enum
      but the spec only specifies "cancelled" as a suggestion
      and no-where is it used, so we keep it as a string option
*)

(* NOTE hard code this as it rarely changes, as noted above
   the whole protocol is based on requests, responses and events *)
module ProtocolMessage_type = struct
  type t = Request | Response | Event

  let enc =
    let open Data_encoding in
     conv_with_guard
      (function Request -> "request" | Response -> "response" | Event -> "event")
      (function "request" -> Ok Request | "response" -> Ok Response | "event" -> Ok Event
              | _ as s -> Error (Printf.sprintf "Unknown ProtocolMessage_type '%s'" s))
      string

end

module type ENC0 = sig
  type t
  val value : t
  val enc : t Data_encoding.t
end

(* NOTE in the following we parameterise over command/event and args/body types
   because these are auto-generated from schema json *)
(* NOTE we also put a unit on the end of all make functions sigs because
   the opt versions require ?fields and we believe conformity in the api is useful *)
module RequestReq : sig
  type ('command, 'args) t
  val seq : ('command, 'args) t -> int
  val incr : ('command, 'args) t -> ('command, 'args) t
  val type_ : ('command, 'args) t -> ProtocolMessage_type.t
  val command : ('command, 'args) t -> 'command
  val arguments : ('command, 'args) t -> 'args
  val enc : 'command Data_encoding.t -> 'args Data_encoding.t -> ('command, 'args) t Data_encoding.t
  val make : seq:int -> command:'command -> arguments:'args -> unit -> ('command, 'args) t
end = struct

  type ('command, 'args) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    command : 'command;
    arguments : 'args;
  }

  let seq t = t.seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let command t = t.command
  let arguments t = t.arguments


  let enc command args =
    let open Data_encoding in
    conv
      (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
      (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "command" command)
         (req "arguments" args))

  let make ~seq ~command ~arguments () =
    let type_ = ProtocolMessage_type.Request in
    {seq; type_; command; arguments}

end


module RequestOpt : sig
  type ('command, 'args) t
  val seq : ('command, 'args) t -> int
  val incr : ('command, 'args) t -> ('command, 'args) t
  val type_ : ('command, 'args) t -> ProtocolMessage_type.t
  val command : ('command, 'args) t -> 'command
  val arguments : ('command, 'args) t -> 'args option
  val enc : 'command Data_encoding.t -> 'args Data_encoding.t -> ('command, 'args) t Data_encoding.t
  val make : seq:int -> command:'command -> ?arguments:'args -> unit -> ('command, 'args) t
end = struct

  type ('command, 'args) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    command : 'command;
    arguments : 'args option;
  }

  let seq t = t.seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let command t = t.command
  let arguments t = t.arguments


  let enc command args =
    let open Data_encoding in
    conv
      (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
      (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "command" command)
         (opt "arguments" args))

  let make ~seq ~command ?arguments () =
    let type_ = ProtocolMessage_type.Request in
    {seq; type_; command; arguments}

end



module ResponseReq : sig
  type ('command, 'body) t
  val seq : ('command, 'body) t -> int
  val incr : ('command, 'body) t -> ('command, 'body) t
  val type_ : ('command, 'body) t -> ProtocolMessage_type.t
  val request_seq : ('command, 'body) t -> int
  val success : ('command, 'body) t -> bool
  val command : ('command, 'body) t -> 'command
  val message : ('command, 'body) t -> string option
  val body : ('command, 'body) t -> 'body
  val enc : 'command Data_encoding.t -> 'body Data_encoding.t -> ('command, 'body) t Data_encoding.t
  val make : seq:int -> request_seq:int -> success:bool -> command:'command -> ?message:string -> body:'body -> unit -> ('command, 'body) t
end  = struct

  type ('command, 'body) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    request_seq : int;
    success : bool;
    command : 'command;
    message : string option; (* only used once I think, keep as string for now *)
    body : 'body;
  }

  let seq t = t. seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let request_seq t = t.request_seq
  let success t = t.success
  let command t = t.command
  let message t = t.message
  let body t = t.body

  let enc command body =
    let open Data_encoding in
    conv
      (fun {seq; type_; request_seq; success; command; message; body} ->
        (seq, type_, request_seq, success, command, message, body))
      (fun (seq, type_, request_seq, success, command, message, body) ->
        {seq; type_; request_seq; success; command; message; body})
      (obj7
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "request_seq" int31)
         (req "success" bool)
         (req "command" command)
         (opt "message" string)
         (req "body" body))

  let make ~seq ~request_seq ~success ~command ?message ~body () =
    let type_ = ProtocolMessage_type.Response in
    {seq; type_; request_seq; success; command; message; body}

end


module ResponseOpt : sig
  type ('command, 'body) t
  val seq : ('command, 'body) t -> int
  val incr : ('command, 'body) t -> ('command, 'body) t
  val type_ : ('command, 'body) t -> ProtocolMessage_type.t
  val request_seq : ('command, 'body) t -> int
  val success : ('command, 'body) t -> bool
  val command : ('command, 'body) t -> 'command
  val message : ('command, 'body) t -> string option
  val body : ('command, 'body) t -> 'body option
  val enc : 'command Data_encoding.t -> 'body Data_encoding.t -> ('command, 'body) t Data_encoding.t
  val make : seq:int -> request_seq:int -> success:bool -> command:'command -> ?message:string -> ?body:'body -> unit -> ('command, 'body) t
end  = struct

  type ('command, 'body) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    request_seq : int;
    success : bool;
    command : 'command;
    message : string option; (* only used once I think, keep as string for now *)
    body : 'body option;
  }

  let seq t = t. seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let request_seq t = t.request_seq
  let success t = t.success
  let command t = t.command
  let message t = t.message
  let body t = t.body

  let enc command body =
    let open Data_encoding in
    conv
      (fun {seq; type_; request_seq; success; command; message; body} ->
        (seq, type_, request_seq, success, command, message, body))
      (fun (seq, type_, request_seq, success, command, message, body) ->
        {seq; type_; request_seq; success; command; message; body})
      (obj7
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "request_seq" int31)
         (req "success" bool)
         (req "command" command)
         (opt "message" string)
         (opt "body" body))

  let make ~seq ~request_seq ~success ~command ?message ?body () =
    let type_ = ProtocolMessage_type.Response in
    {seq; type_; request_seq; success; command; message; body}

end

module EventReq (Ev:ENC0) : sig
  type 'body t
  val seq : 'body t -> int
  val incr : 'body t -> 'body t
  val type_ : 'body t -> ProtocolMessage_type.t
  val event : 'body t -> Ev.t
  val body : 'body t -> 'body
  val enc : 'body Data_encoding.t -> 'body t Data_encoding.t
  val make : seq:int -> body:'body -> unit -> 'body t
end = struct

  type 'body t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    event : Ev.t;
    body : 'body;
  }

  let seq t = t.seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let event t = t.event
  let body t = t.body

  let enc body =
    let open Data_encoding in
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "event" Ev.enc)
         (req "body" body))

  let make ~seq ~body () =
    let type_ = ProtocolMessage_type.Event in
    let event = Ev.value in
    {seq; type_; event; body}

end


module EventOpt (Ev:ENC0) : sig
  type 'body t
  val seq : 'body t -> int
  val incr : 'body t -> 'body t
  val type_ : 'body t -> ProtocolMessage_type.t
  val event : 'body t -> Ev.t
  val body : 'body t -> 'body option
  val enc : 'body Data_encoding.t -> 'body t Data_encoding.t
  val make : seq:int -> ?body:'body -> unit -> 'body t
end = struct

  type 'body t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    event : Ev.t;
    body : 'body option;
  }

  let seq t = t.seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let event t = t.event
  let body t = t.body

  let enc body =
    let open Data_encoding in
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "event" Ev.enc)
         (opt "body" body))

  let make ~seq ?body () =
    let type_ = ProtocolMessage_type.Event in
    let event = Ev.value in
    {seq; type_; event; body}

end

module M = EventOpt (struct type t = ProtocolMessage_type.t let value = ProtocolMessage_type.Event let enc = ProtocolMessage_type.enc end)
type t = | EventExample of int M.t



module type SEQUENCED = sig
  type a
  type b
  type ('a, 'b) t
  val incr : (a, b) t -> (a, b) t
end

module Flow (Request:SEQUENCED) (Response:SEQUENCED) = struct

  module JS = Data_encoding.Json

  type ('request, 'response, 'event, 'error, 'cancel) t = {
    request: ('request, 'response) Request.t Data_encoding.t;
    response: ('request, 'response) Response.t Data_encoding.t;
    events: 'event list;
    on_error: (unit -> 'error) option;
    on_cancel: (unit -> 'cancel) option;
  }

  let make ?on_error ?on_cancel ?(events=[]) request response = {
      request; response; events; on_error; on_cancel
    }

  let _HEADER_FIELD = "Content-Length: "
  let _HEADER_TOKEN = "\r\n\r\n"

  let _replace input output =
    Str.global_replace (Str.regexp_string input) output

  let wrap_header js =
    let s = js
            |> JS.to_string
            |> _replace "\n" ""
    in
    let n = String.length s in
    Printf.sprintf "%s%d%s%s" _HEADER_FIELD n _HEADER_TOKEN s

  let destruct_request t msg =
      match JS.from_string msg with
      | Ok js -> (
          try
            Ok (JS.destruct t.request js)
          with _ as err ->
            Logs.err (fun m -> m "Cannot parse json '%s' as request: '%s'" msg @@ Printexc.to_string err);
            Error (Printexc.to_string err)
        )
      | Error err ->
        Logs.err (fun m -> m "Cannot parse json '%s': '%s'" msg err);
        (* TODO should return an error response *)
        Error err


  let construct_response t response =
    let r = Response.incr response in
    JS.construct t.response r |> wrap_header


end

