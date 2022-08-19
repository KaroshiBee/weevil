(* there are three message types: Request, Response, Event
   they all three have to conform to the ProtocolMessage spec
   which is having fields "seq" & "type",
   they also have their own additional specs,
   all leaf types are auto-generated (enums, plain objects etc)
   and written to Dap_enum.ml and then used in the following functors

   NOTE According to the spec
   - The arguments field of Requests can sometimes be optional,
      so we have it required always and use an empty object when
      it is not needed
   - The body field of Event and Response can sometimes be optional
      so we have it required always and use an empty object when
      it is not needed
   - The message field of Response is supposed to be an _enum
      but the spec only specifies "cancelled" as a suggestion
      and no-where is it used, so we keep it as a string option
*)

module D = Dap_enum

(* encoders that are only parameterised by one thing, t *)
module type ENC0 = sig
  type t
  val enc : t Data_encoding.encoding
end

(* Empty Object encoder for when arguments or body fields aren't present,
   NOTE this is different to when there are optional arg/body but schema is still stated *)
module EmptyObject = struct
  type t = unit
  let enc =
    Data_encoding.unit
end

(* MakeRequest and MakeResponse functor output are parameterised by a Command.t value *)
module type CMD = sig
  val command : D.Command.t
end


module type REQUEST = sig
  type args
  type t
  val seq : t -> int64
  val type_ : t -> D.ProtocolMessage_type.t
  val command : t -> D.Command.t
  val arguments : t -> args

  val enc : t Data_encoding.encoding
  val make : seq:int64 -> arguments:args -> unit -> t
end

(* NOTE sometimes args has a schema but it is optional *)
module type REQUEST_OPTIONAL_ARGS = sig
  type args
  type t
  val seq : t -> int64
  val type_ : t -> D.ProtocolMessage_type.t
  val command : t -> D.Command.t
  val arguments : t -> args option

  val enc : t Data_encoding.encoding
  val make : seq:int64 -> ?arguments:args -> unit -> t
end


module MakeRequest (C:CMD) (ARGS:ENC0) : (REQUEST with type args := ARGS.t) = struct
  type t = {
    seq : int64;
    type_ : D.ProtocolMessage_type.t;
    command : D.Command.t;
    arguments : ARGS.t;
  }

  let seq t = t.seq
  let type_ t = t.type_
  let command t = t.command
  let arguments t = t.arguments


  let enc =
    let open Data_encoding in
    conv
      (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
      (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
      (obj4
         (req "seq" int64)
         (req "type" D.ProtocolMessage_type.enc)
         (req "command" D.Command.enc)
         (req "arguments" ARGS.enc))

  let make ~seq ~arguments () =
    let type_ = D.ProtocolMessage_type.Request in
    let command = C.command in
    {seq; type_; command; arguments}

end

module MakeRequest_optionalArgs (C:CMD) (ARGS:ENC0) : (REQUEST_OPTIONAL_ARGS with type args := ARGS.t) = struct
  type t = {
    seq : int64;
    type_ : D.ProtocolMessage_type.t;
    command : D.Command.t;
    arguments : ARGS.t option;
  }

  let seq t = t.seq
  let type_ t = t.type_
  let command t = t.command
  let arguments t = t.arguments


  let enc =
    let open Data_encoding in
    conv
      (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
      (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
      (obj4
         (req "seq" int64)
         (req "type" D.ProtocolMessage_type.enc)
         (req "command" D.Command.enc)
         (opt "arguments" ARGS.enc))

  let make ~seq ?arguments () =
    let type_ = D.ProtocolMessage_type.Request in
    let command = C.command in
    {seq; type_; command; arguments}

end


module type RESPONSE = sig
  type body
  type t

  val seq : t -> int64
  val type_ : t -> D.ProtocolMessage_type.t
  val request_seq : t -> int64
  val success : t -> bool
  val command : t -> D.Command.t
  val message : t -> string option
  val body : t -> body

  val enc : t Data_encoding.encoding
  val make : seq:int64 -> request_seq:int64 -> success:bool -> ?message:string -> body:body -> unit -> t

end

(* NOTE sometimes body has a schema but it is optional *)
module type RESPONSE_OPTIONAL_BODY = sig
  type body
  type t

  val seq : t -> int64
  val type_ : t -> D.ProtocolMessage_type.t
  val request_seq : t -> int64
  val success : t -> bool
  val command : t -> D.Command.t
  val message : t -> string option
  val body : t -> body option

  val enc : t Data_encoding.encoding
  val make : seq:int64 -> request_seq:int64 -> success:bool -> ?message:string -> ?body:body -> unit -> t

end

module MakeResponse (C:CMD) (B:ENC0) : (RESPONSE with type body := B.t) = struct
  type t = {
    seq : int64;
    type_ : D.ProtocolMessage_type.t;
    request_seq : int64;
    success : bool;
    command : D.Command.t;
    message : string option; (* only used once I think, keep as string for now *)
    body : B.t;
  }

  let seq t = t. seq
  let type_ t = t.type_
  let request_seq t = t.request_seq
  let success t = t.success
  let command t = t.command
  let message t = t.message
  let body t = t.body

  let enc =
    let open Data_encoding in
    conv
      (fun {seq; type_; request_seq; success; command; message; body} ->
        (seq, type_, request_seq, success, command, message, body))
      (fun (seq, type_, request_seq, success, command, message, body) ->
        {seq; type_; request_seq; success; command; message; body})
      (obj7
         (req "seq" int64)
         (req "type" D.ProtocolMessage_type.enc)
         (req "request_seq" int64)
         (req "success" bool)
         (req "command" D.Command.enc)
         (opt "message" string)
         (req "body" B.enc))

  let make ~seq ~request_seq ~success ?message ~body () =
    let type_ = D.ProtocolMessage_type.Response in
    let command = C.command in
    {seq; type_; request_seq; success; command; message; body}

end

module MakeResponse_optionalBody (C:CMD) (B:ENC0) : (RESPONSE_OPTIONAL_BODY with type body := B.t) = struct
  type t = {
    seq : int64;
    type_ : D.ProtocolMessage_type.t;
    request_seq : int64;
    success : bool;
    command : D.Command.t;
    message : string option; (* only used once I think, keep as string for now *)
    body : B.t option;
  }

  let seq t = t. seq
  let type_ t = t.type_
  let request_seq t = t.request_seq
  let success t = t.success
  let command t = t.command
  let message t = t.message
  let body t = t.body

  let enc =
    let open Data_encoding in
    conv
      (fun {seq; type_; request_seq; success; command; message; body} ->
        (seq, type_, request_seq, success, command, message, body))
      (fun (seq, type_, request_seq, success, command, message, body) ->
        {seq; type_; request_seq; success; command; message; body})
      (obj7
         (req "seq" int64)
         (req "type" D.ProtocolMessage_type.enc)
         (req "request_seq" int64)
         (req "success" bool)
         (req "command" D.Command.enc)
         (opt "message" string)
         (opt "body" B.enc))

  let make ~seq ~request_seq ~success ?message ?body () =
    let type_ = D.ProtocolMessage_type.Response in
    let command = C.command in
    {seq; type_; request_seq; success; command; message; body}

end


(* MakeEvent functor output is parameterised by a Event.t value *)
module type EV = sig
  val event : D.Event.t
end

module type EVENT = sig
  type body
  type t

  val seq : t -> int64
  val type_ : t -> D.ProtocolMessage_type.t
  val event : t -> D.Event.t
  val body : t -> body

  val enc : t Data_encoding.encoding
  val make : seq:int64 -> body:body -> unit -> t

end

(* NOTE sometimes event schema have optional body  *)
module type EVENT_OPTIONAL_BODY = sig
  type body
  type t

  val seq : t -> int64
  val type_ : t -> D.ProtocolMessage_type.t
  val event : t -> D.Event.t
  val body : t -> body option

  val enc : t Data_encoding.encoding
  val make : seq:int64 -> ?body:body -> unit -> t

end

module MakeEvent (E:EV) (B:ENC0) : (EVENT with type body := B.t) = struct
  type t = {
    seq : int64;
    type_ : D.ProtocolMessage_type.t;
    event : D.Event.t;
    body : B.t;
  }

  let seq t = t.seq
  let type_ t = t.type_
  let event t = t.event
  let body t = t.body

  let enc =
    let open Data_encoding in
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int64)
         (req "type" D.ProtocolMessage_type.enc)
         (req "event" D.Event.enc)
         (req "body" B.enc))

  let make ~seq ~body () =
    let type_ = D.ProtocolMessage_type.Event in
    let event = E.event in
    {seq; type_; event; body}

end

module MakeEvent_optionalBody (E:EV) (B:ENC0) : (EVENT_OPTIONAL_BODY with type body := B.t) = struct
  type t = {
    seq : int64;
    type_ : D.ProtocolMessage_type.t;
    event : D.Event.t;
    body : B.t option;
  }

  let seq t = t.seq
  let type_ t = t.type_
  let event t = t.event
  let body t = t.body

  let enc =
    let open Data_encoding in
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int64)
         (req "type" D.ProtocolMessage_type.enc)
         (req "event" D.Event.enc)
         (opt "body" B.enc))

  let make ~seq ?body () =
    let type_ = D.ProtocolMessage_type.Event in
    let event = E.event in
    {seq; type_; event; body}

end


