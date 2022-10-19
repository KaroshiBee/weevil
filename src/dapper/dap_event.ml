open Dap_base

type req

type opt

type ('event, 'body, 'presence) t = {
  seq : int;
  type_ : ProtocolMessage_type.t;
  event : 'event Dap_events.t;
  body : 'body;
}

let seq t = t.seq

let set_seq t ~seq = {t with seq}

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
       (req "event" Dap_events.enc)
       (req "body" body))

let enc_opt body =
  let open Data_encoding in
  conv
    (fun {seq; type_; event; body} -> (seq, type_, event, body))
    (fun (seq, type_, event, body) -> {seq; type_; event; body})
    (obj4
       (req "seq" int31)
       (req "type" ProtocolMessage_type.enc)
       (req "event" Dap_events.enc)
       (opt "body" body))

let make ~seq ~event ~body () =
  let type_ = ProtocolMessage_type.Event in
  {seq; type_; event; body}

let make_opt ~seq ~event ?body () =
  let type_ = ProtocolMessage_type.Event in
  {seq; type_; event; body}
