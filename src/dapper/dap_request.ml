open Dap_base

type req

type opt

type ('cmd, 'args, 'presence) t = {
  seq : int;
  type_ : ProtocolMessage_type.t;
  command : 'cmd Dap_commands.t;
  arguments : 'args;
}

let seq t = t.seq

let message t = t.type_

let command t = t.command

let arguments t = t.arguments

let enc args =
  let open Data_encoding in
  conv
    (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
    (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
    (obj4
       (req "seq" int31)
       (req "type" ProtocolMessage_type.enc)
       (req "command" Dap_commands.enc)
       (req "arguments" args))

let enc_opt args =
  let open Data_encoding in
  conv
    (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
    (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
    (obj4
       (req "seq" int31)
       (req "type" ProtocolMessage_type.enc)
       (req "command" Dap_commands.enc)
       (opt "arguments" args))

let make ~seq ~command ~arguments () =
  let type_ = ProtocolMessage_type.Request in
  {seq; type_; command; arguments}

let make_opt ~seq ~command ?arguments () =
  let type_ = ProtocolMessage_type.Request in
  {seq; type_; command; arguments}
