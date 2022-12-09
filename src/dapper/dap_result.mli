(* doesn't do much apart from lock in the error type
*)
type error =
  ( Dap_commands.error,
    Dap_messages.Data.ErrorResponse_body.t,
    Dap_base.Presence.req )
  Dap_response.Message.t
  Dap_response.t

(* TODO think i need to expose this *)
type 'a t = ('a, error) Lwt_result.t

val ok : 'a -> 'a t
val error : error -> 'a t

val to_lwt_result : 'a t -> ('a, error) Lwt_result.t
val from_result : ('a, error) Result.t -> 'a t
val from_lwt_result : ('a, error) Lwt_result.t -> 'a t
val from_error_string : string -> 'a t

val to_lwt_error_as_str : 'a t -> ('a, string) Lwt_result.t

val map : f:('a -> 'b) -> 'a t -> 'b t

val bind : f:('a -> 'b t) -> 'a t -> 'b t

val map_error : f:(error -> error) -> 'a t -> 'a t

val or_log_error : 'a t -> 'a t
