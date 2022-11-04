type ('cmd, 'body, 'presence) t

val seq : ('cmd, 'body, 'presence) t -> int

val set_seq :
  seq:int -> ('cmd, 'body, 'presence) t -> ('cmd, 'body, 'presence) t

val type_ : ('cmd, 'body, 'presence) t -> Dap_base.ProtocolMessage_type.t

val request_seq : ('cmd, 'body, 'presence) t -> int

val set_request_seq :
  request_seq:int -> ('cmd, 'body, 'presence) t -> ('cmd, 'body, 'presence) t

val success : ('cmd, 'body, 'presence) t -> bool

val command : ('cmd, 'body, 'presence) t -> 'cmd Dap_commands.t

val message : ('cmd, 'body, 'presence) t -> string option

val body : ('cmd, 'body, 'presence) t -> 'body

val enc : 'cmd Dap_commands.t -> 'body Data_encoding.t -> ('cmd, 'body, Dap_base.Presence.req) t Data_encoding.t

val enc_opt :
  'cmd Dap_commands.t -> 'body Data_encoding.t -> ('cmd, 'body option, Dap_base.Presence.opt) t Data_encoding.t

val make :
  seq:int ->
  request_seq:int ->
  success:bool ->
  command:'cmd Dap_commands.t ->
  ?message:string ->
  body:'body ->
  unit ->
  ('cmd, 'body, Dap_base.Presence.req) t

val make_opt :
  seq:int ->
  request_seq:int ->
  success:bool ->
  command:'cmd Dap_commands.t ->
  ?message:string ->
  ?body:'body ->
  unit ->
  ('cmd, 'body option, Dap_base.Presence.opt) t
