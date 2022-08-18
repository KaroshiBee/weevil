(* there are three message types: Request, Response, Event
   they all three have to conform to the ProtocolMessage spec
   which is having fields "seq" & "type"
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

module E = Dap_enum

module type ENC0 = sig
  type t
  val enc : t Data_encoding.encoding
end

module type CMD = sig
  val cmd : E.Command.t
end


module type REQUEST = sig
  type args
  type t
  val seq : t -> int64
  val type_ : t -> E.ProtocolMessage_type.t
  val command : t -> E.Command.t
  val arguments : t -> args

  val enc : t Data_encoding.encoding
  val make : seq:int64 -> arguments:args -> unit -> t


end


module MakeRequest (C:CMD) (ARGS:ENC0) : (REQUEST with type args := ARGS.t) = struct
  type t = {
    seq : int64;
    type_ : E.ProtocolMessage_type.t;
    command : E.Command.t;
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
         (req "type" E.ProtocolMessage_type.enc)
         (req "command" E.Command.enc)
         (req "arguments" ARGS.enc))

  let make ~seq ~arguments () =
    let type_ = E.ProtocolMessage_type.Request in
    let command = C.cmd in
    {seq; type_; command; arguments}

end

module Event = struct
  type 'body t = {
    seq : int64;
    type_ : E.ProtocolMessage_type.t;
    event : E.Event.t;
    body : 'body;
  }

  let enc body =
    let open Data_encoding in
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int64)
         (req "type" E.ProtocolMessage_type.enc)
         (req "event" E.Event.enc)
         (req "body" body))

  let make ~seq ~type_ ~event ~body () =
    {seq; type_; event; body}

end

module Response = struct
  type 'body t = {
    seq : int64;
    type_ : E.ProtocolMessage_type.t;
    request_seq : int64;
    success : bool;
    command : E.Command.t;
    message : string option; (* only used once I think, keep as string for now *)
    body : 'body;
  }

  let enc body =
    let open Data_encoding in
    conv
      (fun {seq; type_; request_seq; success; command; message; body} ->
        (seq, type_, request_seq, success, command, message, body))
      (fun (seq, type_, request_seq, success, command, message, body) ->
        {seq; type_; request_seq; success; command; message; body})
      (obj7
         (req "seq" int64)
         (req "type" E.ProtocolMessage_type.enc)
         (req "request_seq" int64)
         (req "success" bool)
         (req "command" E.Command.enc)
         (opt "message" string)
         (req "body" body))

  let make ~seq ~type_ ~request_seq ~success ~command ?message ~body () =
    {seq; type_; request_seq; success; command; message; body}

end
