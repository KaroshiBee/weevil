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

(* NOTE in the following we parameterise over command/event and args/body types
   because these are auto-generated from schema json *)
module Request : sig
  type ('command, 'args) t
  val seq : ('command, 'args) t -> int
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



module Response : sig
  type ('command, 'body) t
  val seq : ('command, 'body) t -> int
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


module Event : sig
  type ('event, 'body) t
  val seq : ('event, 'body) t -> int
  val type_ : ('event, 'body) t -> ProtocolMessage_type.t
  val event : ('event, 'body) t -> 'event
  val body : ('event, 'body) t -> 'body
  val enc : 'event Data_encoding.t -> 'body Data_encoding.t -> ('event, 'body) t Data_encoding.t
  val make : seq:int -> event:'event -> body:'body -> unit -> ('event, 'body) t
end = struct

  type ('event, 'body) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    event : 'event;
    body : 'body;
  }

  let seq t = t.seq
  let type_ t = t.type_
  let event t = t.event
  let body t = t.body

  let enc event body =
    let open Data_encoding in
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "event" event)
         (req "body" body))

  let make ~seq ~event ~body () =
    let type_ = ProtocolMessage_type.Event in
    {seq; type_; event; body}

end


module EventOpt : sig
  type ('event, 'body) t
  val seq : ('event, 'body) t -> int
  val type_ : ('event, 'body) t -> ProtocolMessage_type.t
  val event : ('event, 'body) t -> 'event
  val body : ('event, 'body) t -> 'body option
  val enc : 'event Data_encoding.t -> 'body Data_encoding.t -> ('event, 'body) t Data_encoding.t
  val make : seq:int -> event:'event -> ?body:'body -> unit -> ('event, 'body) t
end = struct

  type ('event, 'body) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    event : 'event;
    body : 'body option;
  }

  let seq t = t.seq
  let type_ t = t.type_
  let event t = t.event
  let body t = t.body

  let enc event body =
    let open Data_encoding in
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "event" event)
         (opt "body" body))

  let make ~seq ~event ?body () =
    let type_ = ProtocolMessage_type.Event in
    {seq; type_; event; body}

end



(* (\* Main functor for constructing flows of request/response pairings *)
(* along with events that may be raised or Errors that could be returned *\) *)
(* module MakeJSFlow *)
(*     (ERR:RESPONSE) (\* this one first so can curry it away *\) *)
(*     (REQ:REQUEST) (REQ_OPT:REQUEST_OPTIONAL_ARGS) *)
(*     (RESP:RESPONSE) (RESP_OPT:RESPONSE_OPTIONAL_BODY) *)
(*     (EV:EVENT) (EV_OPT:EVENT_OPTIONAL_BODY) *)
(*   : (FLOW with type *)
(*       input := string and type *)
(*       request := [ `Request of REQ.t | `RequestOpt of REQ_OPT.t ] and type *)
(*       response := [ `Response of RESP.t | `ResponseOpt of RESP_OPT.t ] and type *)
(*       event := [ `Event of EV.t | `EventOpt of EV_OPT.t ] and type *)
(*       err_response := ERR.t *)
(*     ) *)
(*      = struct *)

(*   module JS = Data_encoding.Json *)

(*   let _HEADER_FIELD = "Content-Length: " *)
(*   let _HEADER_TOKEN = "\r\n\r\n" *)

(*   let _replace input output = *)
(*     Str.global_replace (Str.regexp_string input) output *)

(*   let wrap_header js = *)
(*     let s = js *)
(*             |> JS.to_string *)
(*             |> _replace "\n" "" *)
(*     in *)
(*     let n = String.length s in *)
(*     Printf.sprintf "%s%d%s%s" _HEADER_FIELD n _HEADER_TOKEN s *)

(*   (\* type t = { *\) *)
(*   (\*   request:request; *\) *)
(*   (\*   response:response; *\) *)
(*   (\*   events:event list; *\) *)

(*   (\* } *\) *)

(*   (\* let make ~request ~response ?(events=[]) () = { *\) *)
(*   (\*   request; response; events *\) *)
(*   (\* } *\) *)

(*   let destruct_request msg = *)
(*       match JS.from_string msg with *)
(*       | Ok js -> ( *)
(*           (\* TODO nice to be able to check this in types *\) *)
(*           try *)
(*             let r = JS.destruct REQ.enc js in `Request r *)
(*           with _ -> *)
(*             let r = JS.destruct REQ_OPT.enc js in `RequestOpt r *)
(*         ) *)
(*       | Error err -> *)
(*         (\* TODO be better *\) *)
(*         failwith err *)

(*   let construct_response = function *)
(*     (\* TODO wrap header, inc seq number somewhere central *\) *)
(*     | `Response r -> JS.construct RESP.enc r |> wrap_header *)
(*     | `ResponseOpt r -> JS.construct RESP_OPT.enc r |> wrap_header *)

(*   let construct_events events = *)
(*     (\* TODO wrap header, inc seq number somewhere central *\) *)
(*     events |> List.map (fun ev -> *)
(*         match ev with *)
(*         | `Event ev -> JS.construct EV.enc ev |> wrap_header *)
(*         | `EventOpt ev -> JS.construct EV_OPT.enc ev |> wrap_header *)
(*       ) |> String.concat "" *)

(*   let construct_error err = *)
(*     JS.construct ERR.enc err |> wrap_header *)




(*      end *)
