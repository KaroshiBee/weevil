type error =
  ( Dap.Commands.error,
    Dap.ErrorResponse_body.t,
    Dap.Presence.req )
  Dap.Response.Message.t
  Dap.Response.t

type 'a t

val ok : 'a -> 'a t
val error : error -> 'a t

val to_lwt_result : 'a t -> ('a, error) Lwt_result.t
val to_lwt_error_as_str : 'a t -> ('a, string) Lwt_result.t

val map : f:('a -> 'b) -> 'a t -> 'b t

val map_error : f:(error -> error) -> 'a t -> 'a t

val bind : f:('a -> 'b t) -> 'a t -> 'b t
