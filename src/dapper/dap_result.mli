(* deosnt do much apart from lock in the error type
   most user code will just do to_lwt_result and work with that
*)
type error =
  ( Dap_commands.error,
    Dap_message.Data.ErrorResponse_body.t,
    Dap_base.Presence.req )
  Dap_response.Message.t
  Dap_response.t

type 'a t

val ok : 'a -> 'a t
val error : error -> 'a t

val to_lwt_result : 'a t -> ('a, error) Lwt_result.t
val from_result : ('a, error) Result.t -> 'a t
val from_lwt_result : ('a, error) Lwt_result.t -> 'a t

val to_lwt_error_as_str : 'a t -> ('a, string) Lwt_result.t

val map : f:('a -> 'b) -> 'a t -> 'b t

val map_error : f:(error -> error) -> 'a t -> 'a t

val bind : f:('a -> 'b t) -> 'a t -> 'b t
