module type REQUEST_READONLY_T = sig
  type ('cmd, 'args, 'presence) t

  val equal :
    equal_arguments:('args1 -> 'args2 -> bool) ->
    ('cmd1, 'args1, 'presence1) t ->
    ('cmd2, 'args2, 'presence2) t ->
    bool

  val seq : ('cmd, 'args, 'presence) t -> int

  val message : ('cmd, 'args, 'presence) t -> Dap_base.ProtocolMessage_type.t

  val command : ('cmd, 'args, 'presence) t -> 'cmd Dap_commands.t

  val arguments : ('cmd, 'args, 'presence) t -> 'args

  val enc : 'cmd Dap_commands.t -> 'args Data_encoding.t -> ('cmd, 'args, Dap_base.Presence.req) t Data_encoding.t

  val enc_opt :
    'cmd Dap_commands.t -> 'args Data_encoding.t -> ('cmd, 'args option, Dap_base.Presence.opt) t Data_encoding.t

  val make :
    seq:int ->
    command:'cmd Dap_commands.t ->
    arguments:'args ->
    unit ->
    ('cmd, 'args, Dap_base.Presence.req) t

  val make_opt :
    seq:int ->
    command:'cmd Dap_commands.t ->
    ?arguments:'args ->
    unit ->
    ('cmd, 'args option, Dap_base.Presence.opt) t
end

module type REQUEST_T = sig
  include REQUEST_READONLY_T
  val set_seq :
    seqr:Dap_base.Seqr.t -> ('cmd, 'args, 'presence) t -> ('cmd, 'args, 'presence) t
end


module type RESPONSE_READONLY_T = sig
  type ('cmd, 'body, 'presence) t

  val equal :
    equal_body:('body1 -> 'body2 -> bool) ->
    ('cmd1, 'body1, 'presence1) t ->
    ('cmd2, 'body2, 'presence2) t ->
    bool

  val seq : ('cmd, 'body, 'presence) t -> int

  val type_ : ('cmd, 'body, 'presence) t -> Dap_base.ProtocolMessage_type.t

  val request_seq : ('cmd, 'body, 'presence) t -> int

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
end

module type RESPONSE_T = sig
  include RESPONSE_READONLY_T
  val set_seq :
    seqr:Dap_base.Seqr.t -> ('cmd, 'body, 'presence) t -> ('cmd, 'body, 'presence) t
end


module type EVENT_READONLY_T = sig
  type ('event, 'body, 'presence) t

  val equal :
    equal_body:('body1 -> 'body2 -> bool) ->
    ('event1, 'body1, 'presence1) t ->
    ('event2, 'body2, 'presence2) t ->
    bool

  val seq : ('event, 'body, 'presence) t -> int

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

end


module type EVENT_T = sig
  include EVENT_READONLY_T
  val set_seq :
    seqr:Dap_base.Seqr.t -> ('event, 'body, 'presence) t -> ('event, 'body, 'presence) t
end
