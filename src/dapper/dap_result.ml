module Js = Data_encoding.Json
module R = Dap_response

type error =
  (Dap_commands.error, Dap_message.Data.ErrorResponse_body.t, Dap_base.Presence.req) R.Message.t
  R.t

type 'a t = ('a, error) Lwt_result.t

let ok = Lwt_result.return
let error = Lwt_result.fail

let to_lwt_result t = t
let from_result = Lwt.return
let from_lwt_result t = t

let _err_str =
    let enc = R.Message.enc Dap_commands.error Dap_message.Data.ErrorResponse_body.enc in
    fun err -> Js.construct enc err |> Js.to_string

let to_lwt_error_as_str t =
  let to_string = R.Fmap (_err_str) in
  (* NOTE seem to need the t otherwise type checker complains *)
  Lwt_result.map_err
    (fun err_resp -> R.(eval @@ Map (Val to_string, Val err_resp)))
    t

let map ~f = Lwt_result.map f

let bind ~f x = Lwt_result.bind x f

let map_error ~f = Lwt_result.map_err f

let or_log_error result =
  let to_string = R.Fmap (_err_str) in
  Lwt.bind result (fun r ->
    match r with
    | Result.Ok _ as x -> Lwt.return x
    | Result.Error err_resp as err ->
      let err_msg = R.(eval @@ Map (Val to_string, Val err_resp)) in
      let%lwt () = Logs_lwt.err (fun m -> m "%s" err_msg) in
      Lwt.return err
  )
