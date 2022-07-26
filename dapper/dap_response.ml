open Dap_base


module Response = struct

  type t =
    | Cancelled

  let enc_t =
    let open Data_encoding in
    conv
      (function | Cancelled -> "cancelled")
      (function | "cancelled" -> Cancelled | _ -> failwith "Unknown request")
      string

  type 'json cls_t = <
    ProtocolMessage.cls_t;
    request_seq:int64;
    success:bool;
    command:string;
    message:t option;
    body:'json
  >

  class ['json] cls
      (seq:int64)
      (request_seq:int64)
      (success:bool)
      (command:string)
      (message:t option)
      (body:'json)
      = object(_self)
    inherit ProtocolMessage.cls seq Response as _super

    method request_seq = request_seq
    method success = success
    method command = command
    method message = message
    method body = body

  end

  let enc js =
    let open Data_encoding in
    conv
      (fun (r : < 'json cls_t >) ->
         (r#seq, r#type_, r#request_seq, r#success, r#command, r#message, r#body) )

      (fun (seq, _, request_seq, success, command, message, body) ->
         new cls seq request_seq success command message body)

      (obj7
         (req "seq" int64)
         (req "type" ProtocolMessage.enc_t)
         (req "request_seq" int64)
         (req "success" bool)
         (req "command" string)
         (opt "message" enc_t)
         (req "body" js)
      )

end

module ErrorResponse = struct

  type body = {
    error: Message.t option
  }

  type cls_t = body Response.cls_t

  class cls
      (seq:int64)
      (request_seq:int64)
      (success:bool)
      (command:string)
      (body:body) = object
    inherit [body] Response.cls seq request_seq success command None body
  end

  let enc =
    let open Data_encoding in
    Response.enc @@
    conv
      (fun {error} -> error)
      (fun error -> {error})
      (obj1
         (opt "error" Message.enc))

end

module type EMPTY_BODY = sig type t val body : t val enc : t Data_encoding.t  end

module MakeEmptyBodyResponse (B:EMPTY_BODY) = struct

  type body = B.t

  type cls_t = body Response.cls_t

  class cls
      (seq:int64)
      (request_seq:int64)
      (success:bool)
      (command:string) = object
    inherit [body] Response.cls seq request_seq success command None B.body
  end

  let enc = Response.enc B.enc

end

module UnitBody = struct type t = unit let body = () let enc = Data_encoding.unit end

module CancelResponse = MakeEmptyBodyResponse(UnitBody)

module NextResponse = MakeEmptyBodyResponse(UnitBody)
