module Js = Data_encoding.Json
module R = Dap.Response

type error =
  (Dap.Commands.error, Dap.ErrorResponse_body.t, Dap.Presence.req) R.Message.t
  R.t

type 'a t = ('a, error) Lwt_result.t

let ok = Lwt_result.return
let error = Lwt_result.fail

let to_lwt_result t = t
let from_result = Lwt.return
let from_lwt_result t = t

let to_lwt_error_as_str t =
  let enc = R.Message.enc Dap.Commands.error Dap.ErrorResponse_body.enc in
  let to_string = R.Fmap (fun err -> Js.construct enc err |> Js.to_string) in
  (* NOTE seem to need the t otherwise type checker complains *)
  Lwt_result.map_err
    (fun err_resp -> R.(eval @@ Map (Val to_string, Val err_resp)))
    t

let map ~f = Lwt_result.map f

let map_error ~f = Lwt_result.map_err f

let bind ~f x = Lwt_result.bind x f
