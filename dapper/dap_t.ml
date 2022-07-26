open Data_encoding

module ProtocolMessage = struct

  type msg =
    | Request
    | Response
    | Event

  type t = {
    seq: int64;
    type_: msg;
  }

  let msg_e =
    conv
      ( function | Request -> "request" | Response -> "response" | Event -> "event" )
      ( fun msg -> match msg with | "request" -> Request | "response" -> Response | "event" -> Event | _ -> failwith "ERROR: unkown msg type" )
      string

  let e =
    let description = "Sequence number (also known as message ID). For protocol messages of type 'request' this ID can be used to cancel the request." in
    conv
      (fun {seq; type_} -> (seq, type_))
      (fun (seq, type_) -> {seq; type_})
      (obj2
         (req ~description "seq" int64)
         (req ~description:"Message type." "type" msg_e))
end

module Request = struct

  type 'args t = {
    seq: int64;
    type_: ProtocolMessage.msg;
    command: string;
    arguments: 'args option;
  }

  let msg_e =
    let open ProtocolMessage in
    conv
      ( function | Request -> "request" | _ -> failwith "ERROR: expected Request")
      ( fun msg -> match msg with | "request" -> Request | _ -> failwith "ERROR: expected 'request'" )
      string

  let e args =
    conv
      (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
      (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
      (obj4
         (req "seq" int64)
         (req "type" msg_e)
         (req "command" string)
         (opt "arguments" args))

end

module Event = struct

  type 'body t = {
    seq: int64;
    type_: ProtocolMessage.msg;
    event: string;
    body: 'body option;
  }

  let msg_e =
    let open ProtocolMessage in
    conv
      ( function | Event -> "event" | _ -> failwith "ERROR: expected Event")
      ( fun msg -> match msg with | "event" -> Event | _ -> failwith "ERROR: expected 'event'" )
      string

  let e body =
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int64)
         (req "type" msg_e)
         (req "event" string)
         (opt "body" body))

end

module Response = struct

  type 'body t = {
    seq: int64;
    type_: ProtocolMessage.msg;
    request_seq: int64;
    success: bool;
    command: string;
    message: string option;
    body: 'body option;
  }

  let msg_e =
    let open ProtocolMessage in
    conv
      ( function | Response -> "response" | _ -> failwith "ERROR: expected Response")
      ( fun msg -> match msg with | "response" -> Response | _ -> failwith "ERROR: expected 'response'" )
      string

  let response_e body =
    obj7
      (req "seq" int64)
      (req "type" msg_e)
      (req "request_seq" int64)
      (req "success" bool)
      (req "command" string)
      (opt "message" string)
      (opt "body" body)

  let e body =
    conv
      (fun {seq; type_; request_seq; success; command; message; body} -> (seq, type_, request_seq, success, command, message, body))
      (fun (seq, type_, request_seq, success, command, message, body) -> {seq; type_; request_seq; success; command; message; body})
      (response_e body)
end

module Message = struct
  type t = {
    id: int64;
    format: string;
    variables: (string * string) list option;
    sendTelemetry: bool option;
    showUser: bool option;
    url: string option;
    urlLabel: string option;
  }

  let msg_e =
    obj7
      (req "id" int64)
      (req "format" string)
      (opt "variables" (list @@ (tup2 string string)))
      (opt "sendTelemetry" bool)
      (opt "showUser" bool)
      (opt "url" string)
      (opt "urlLabel" string)

  let e =
    conv
      (fun {id; format; variables; sendTelemetry; showUser; url; urlLabel} -> (id, format, variables, sendTelemetry, showUser, url, urlLabel))
      (fun (id, format, variables, sendTelemetry, showUser, url, urlLabel) -> {id; format; variables; sendTelemetry; showUser; url; urlLabel})
      msg_e
end

module Error = struct
    type t = {
      error: Message.t option;
    }

    let e =
      conv
        (fun {error} -> error)
        (fun error -> {error})
        (obj1 (opt "error" Message.e))

  end


module ErrorResponse = struct

  type t = {
    seq: int64;
    type_: ProtocolMessage.msg;
    request_seq: int64;
    success: bool;
    command: string;
    message: string option;
    body: Error.t;
  }

  let msg_e =
    let open ProtocolMessage in
    conv
      ( function | Response -> "response" | _ -> failwith "ERROR: expected Response")
      ( fun msg -> match msg with | "response" -> Response | _ -> failwith "ERROR: expected 'response'" )
      string

  let response_e =
    obj7
      (req "seq" int64)
      (req "type" msg_e)
      (req "request_seq" int64)
      (req "success" bool)
      (req "command" string)
      (opt "message" string)
      (req "body" Error.e)

  let e =
    conv
      (fun {seq; type_; request_seq; success; command; message; body} -> (seq, type_, request_seq, success, command, message, body))
      (fun (seq, type_, request_seq, success, command, message, body) -> {seq; type_; request_seq; success; command; message; body})
      response_e
end

module CancelArguments = struct
  type t = {
    requestId: int64 option;
    progressId: string option;
  }

  let e =
    conv
      (fun {requestId; progressId} -> (requestId, progressId))
      (fun (requestId, progressId) -> {requestId; progressId})
      (obj2
         (opt "requestId" int64)
         (opt "progressId" string))

  let e_ =
    obj2
      (opt "requestId" int64)
      (opt "progressId" string)
end

module CancelRequest = struct

  let cmd_e =
    obj2
      (req "command" (constant "cancel"))
      (opt "arguments" CancelArguments.e)

  let e =
    merge_objs (Request.e CancelArguments.e) cmd_e
end

module CancelResponse = struct
  let e =
    Response.e unit
end
