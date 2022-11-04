type error =
  ( Dap.Commands.error,
    Dap.ErrorResponse_body.t,
    Dap.Presence.req )
  Dap.Response.Message.t
  Dap.Response.t

type 'a t

val ok : 'a -> 'a t

val get_ok : 'a t -> 'a

val error : error -> 'a t

val get_error : 'a t -> error

val get_error_str : 'a t -> string

val map : f:('a -> 'b) -> 'a t -> 'b t

val map_error : f:(error -> error) -> 'a t -> 'a t

val bind : f:('a -> 'b t) -> 'a t -> 'b t
