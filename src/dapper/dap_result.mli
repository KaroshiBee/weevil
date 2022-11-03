
type error =
  (Dap_commands.error, Dap_utils.Response.ErrorResponse_body.t, Dap_message_exi.Presence.req) Dap_utils.Response.Message.t Dap_utils.Response.t

type 'a t

val ok : 'a -> 'a t
val get_ok : 'a t -> 'a

val error : error -> 'a t
val get_error : 'a t -> error
val get_error_str : 'a t -> string

val map : ('a -> 'b) -> 'a t -> 'b t
val map_error : (error -> error) -> 'a t -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
