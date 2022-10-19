open Dap_base

type req

type opt

type ('cmd, 'body, 'presence) t = {
  seq : int;
  type_ : ProtocolMessage_type.t;
  request_seq : int;
  success : bool;
  command : 'cmd Dap_commands.t;
  message : string option; (* only used once I think, keep as string for now *)
  body : 'body;
}

let seq t = t.seq

let set_seq t ~seq = {t with seq}

let type_ t = t.type_

let request_seq t = t.request_seq

let set_request_seq t ~request_seq = {t with request_seq}

let success t = t.success

let command t = t.command

let message t = t.message

let body t = t.body

let enc body =
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
       (req "command" Dap_commands.enc)
       (opt "message" string)
       (req "body" body))

let enc_opt body =
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
       (req "command" Dap_commands.enc)
       (opt "message" string)
       (opt "body" body))

let make ~seq ~request_seq ~success ~command ?message ~body () =
  let type_ = ProtocolMessage_type.Response in
  {seq; type_; request_seq; success; command; message; body}

let make_opt ~seq ~request_seq ~success ~command ?message ?body () =
  let type_ = ProtocolMessage_type.Response in
  {seq; type_; request_seq; success; command; message; body}
