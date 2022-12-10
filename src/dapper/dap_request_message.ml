open Dap_base

type ('cmd, 'args, 'presence) t = {
  seq : int;
  type_ : ProtocolMessage_type.t;
  command : 'cmd Dap_commands.t;
  arguments : 'args;
}

let equal ~equal_arguments t1 t2 =
  Int.equal t1.seq t2.seq &&
  Dap_base.ProtocolMessage_type.equal t1.type_ t2.type_ &&
  Dap_commands.equal t1.command t2.command &&
  equal_arguments t1.arguments t2.arguments

let seq t = t.seq

let set_seq ~seq:s t =
  let seq = Dap_base.Seqr.seq s in
  {t with seq}

let message t = t.type_

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
       (req "command" @@ Dap_commands.enc ~value:command)
       (req "arguments" args))

let enc_opt command args =
  let open Data_encoding in
  conv
    (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
    (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
    (obj4
       (req "seq" int31)
       (req "type" ProtocolMessage_type.enc)
       (req "command" @@ Dap_commands.enc ~value:command)
       (opt "arguments" args))

let make ~seq ~command ~arguments () =
  let type_ = ProtocolMessage_type.Request in
  {seq; type_; command; arguments}

let make_opt ~seq ~command ?arguments () =
  let type_ = ProtocolMessage_type.Request in
  {seq; type_; command; arguments}

module MakeRO (Req:Dap_message_types.REQUEST_T) = struct
  include Req
end
