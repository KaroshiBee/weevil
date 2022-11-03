module Js = Data_encoding.Json
open Dap_message_exi

type error_msg = (Dap_commands.error, ErrorResponse_body.t, Presence.req) ResponseMessage.t
type 'a t = ('a, error_msg) Result.t
(* type 'a result_lwt = 'a result Lwt.t *)

let ok = Result.ok
let get_ok = Result.get_ok

let error = Result.error
let get_error = Result.get_error
let enc = ResponseMessage.enc Dap_commands.error ErrorResponse_body.enc
let get_error_str t = t |> Result.map_error (fun err -> Js.construct enc err |> Js.to_string) |> get_error

let map = Result.map
let map_error = Result.map_error
