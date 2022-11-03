
type error_msg = (Dap_commands.error, Dap_message_exi.ErrorResponse_body.t, Dap_base.Presence.req) Dap_message_exi.ResponseMessage.t
type 'a t

val ok : 'a -> 'a t
val get_ok : 'a t -> 'a

val error : error_msg -> 'a t
val get_error : 'a t -> error_msg
val get_error_str : 'a t -> string

val map : ('a -> 'b) -> 'a t -> 'b t
val map_error : (error_msg -> error_msg) -> 'a t -> 'a t
