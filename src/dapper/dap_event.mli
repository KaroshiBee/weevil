type ('event, 'body, 'presence) t

val seq : ('event, 'body, 'presence) t -> int

val set_seq :
  ('event, 'body, 'presence) t -> seq:int -> ('event, 'body, 'presence) t

val type_ : ('event, 'body, 'presence) t -> Dap_base.ProtocolMessage_type.t

val event : ('event, 'body, 'presence) t -> 'event Dap_events.t

val body : ('event, 'body, 'presence) t -> 'body

val enc : 'event Dap_events.t -> 'body Data_encoding.t -> ('event, 'body, Dap_base.Presence.req) t Data_encoding.t

val enc_opt :
  'event Dap_events.t -> 'body Data_encoding.t -> ('event, 'body option, Dap_base.Presence.opt) t Data_encoding.t

val make :
  seq:int ->
  event:'event Dap_events.t ->
  body:'body ->
  unit ->
  ('event, 'body, Dap_base.Presence.req) t

val make_opt :
  seq:int ->
  event:'event Dap_events.t ->
  ?body:'body ->
  unit ->
  ('event, 'body option, Dap_base.Presence.opt) t
