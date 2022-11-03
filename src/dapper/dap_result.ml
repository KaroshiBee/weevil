module Js = Data_encoding.Json
module Presence = Dap_base.Presence
open Dap_utils.Response

type error = (Dap_commands.error, ErrorResponse_body.t, Presence.req) ResponseMessage.t t
type 'a t = ('a, error) Result.t
(* type 'a result_lwt = 'a result Lwt.t *)

let ok = Result.ok
let get_ok = Result.get_ok

let error = Result.error
let get_error = Result.get_error
let get_error_str t =
  (* NOTE seem to need to do the get_error first otherwise type checker complains *)
  let err_resp = get_error t in
  let enc = ResponseMessage.enc Dap_commands.error ErrorResponse_body.enc in
  let ss err = Js.construct enc err |> Js.to_string in
  let ss = Fmap ss in
  eval @@ Map (Val ss, Val err_resp)

let map = Result.map
let map_error = Result.map_error
let bind = Result.bind
