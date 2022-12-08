open Dap_base

type ('cmd, 'body, 'presence) t = {
  seq : int;
  type_ : ProtocolMessage_type.t;
  request_seq : int;
  success : bool;
  command : 'cmd Dap_commands.t;
  message : string option; (* only used once I think, keep as string for now *)
  body : 'body;
}

let equal ~equal_body t1 t2 =
  Int.equal t1.seq t2.seq &&
  Dap_base.ProtocolMessage_type.equal t1.type_ t2.type_ &&
  Dap_commands.equal t1.command t2.command &&
  equal_body t1.body t2.body

let seq t = t.seq

let set_seq ~seq:s t =
  let seq, request_seq = Dap_base.Seqr.(
      seq s, request_seq s
    ) in
    {t with seq; request_seq}

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
       (req "command" @@ Dap_commands.enc ~value:command)
       (opt "message" string)
       (req "body" body))

let enc_opt command body =
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
       (req "command" @@ Dap_commands.enc ~value:command)
       (opt "message" string)
       (opt "body" body))

let make ~seq ~request_seq ~success ~command ?message ~body () =
  let type_ = ProtocolMessage_type.Response in
  {seq; type_; request_seq; success; command; message; body}

let make_opt ~seq ~request_seq ~success ~command ?message ?body () =
  let type_ = ProtocolMessage_type.Response in
  {seq; type_; request_seq; success; command; message; body}
