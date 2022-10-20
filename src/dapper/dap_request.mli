include Dap_base.PRESENCE

type ('cmd, 'args, 'presence) t

val seq : ('cmd, 'args, 'presence) t -> int

val message : ('cmd, 'args, 'presence) t -> Dap_base.ProtocolMessage_type.t

(* val command : ('cmd, 'args, 'presence) t -> 'cmd Dap_commands.t *)

val arguments : ('cmd, 'args, 'presence) t -> 'args

val enc : 'cmd Dap_commands.t -> 'args Data_encoding.t -> ('cmd, 'args, req) t Data_encoding.t

val enc_opt :
  'cmd Dap_commands.t -> 'args Data_encoding.t -> ('cmd, 'args option, opt) t Data_encoding.t

val make :
  seq:int ->
  command:'cmd Dap_commands.t ->
  arguments:'args ->
  unit ->
  ('cmd, 'args, req) t

val make_opt :
  seq:int ->
  command:'cmd Dap_commands.t ->
  ?arguments:'args ->
  unit ->
  ('cmd, 'args option, opt) t
