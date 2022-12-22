open Dap_base

type ('event, 'body, 'presence) t = {
  seq : int;
  type_ : ProtocolMessage_type.t;
  event : 'event Dap_events.t;
  body : 'body;
}

let equal ~equal_body t1 t2 =
  Int.equal t1.seq t2.seq &&
  Dap_base.ProtocolMessage_type.equal t1.type_ t2.type_ &&
  Dap_events.equal t1.event t2.event &&
  equal_body t1.body t2.body

let seq t = t.seq

let set_seq ~seqr t =
  let seq = Dap_base.Seqr.seq seqr in
  {t with seq}

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
       (req "event" @@ Dap_events.enc ~value:event)
       (req "body" body))

let enc_opt event body =
  let open Data_encoding in
  conv
    (fun {seq; type_; event; body} -> (seq, type_, event, body))
    (fun (seq, type_, event, body) -> {seq; type_; event; body})
    (obj4
       (req "seq" int31)
       (req "type" ProtocolMessage_type.enc)
       (req "event" @@ Dap_events.enc ~value:event)
       (opt "body" body))

let make ~seq ~event ~body () =
  let type_ = ProtocolMessage_type.Event in
  {seq; type_; event; body}

let make_opt ~seq ~event ?body () =
  let type_ = ProtocolMessage_type.Event in
  {seq; type_; event; body}
