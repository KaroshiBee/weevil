open Dap_message_exi
type error_msg = (Dap_commands.error, ErrorResponse_body.t, Presence.req) ResponseMessage.t
type 'a t = ('a, error_msg) Result.t
(* type 'a result_lwt = 'a result Lwt.t *)
