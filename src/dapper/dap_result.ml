module Js = Data_encoding.Json
module R = Dap_response

type error =
  (Dap_commands.error, Dap_messages.Data.ErrorResponse_body.t, Dap_base.Presence.req) R.Message.t
  R.t

type 'a t = ('a, error) Lwt_result.t

let ok = Lwt_result.return
let error = Lwt_result.fail

let to_lwt_result t = t
let from_result = Lwt.return
let from_lwt_result t = t
let from_error_string s = R.(errorResponse @@ default_response_error s) |> error

let err_js =
    let enc = R.Message.enc Dap_commands.error Dap_messages.Data.ErrorResponse_body.enc in
    fun err -> let err_msg = R.extract err in Js.construct enc err_msg |> Js.to_string

let to_lwt_error_as_str t =
  (* NOTE seem to need the t otherwise type checker complains *)
  Lwt_result.map_error err_js t

let map ~f = Lwt_result.map f

let bind ~f x = Lwt_result.bind x f

let map_error ~f = Lwt_result.map_error f

let or_log_error result =
  Lwt.bind result (fun r ->
    match r with
    | Result.Ok _ as x -> Lwt.return x
    | Result.Error err_resp as err ->
      let err_msg = err_js err_resp in
      let%lwt () = Logs_lwt.err (fun m -> m "%s" err_msg) in
      Lwt.return err
  )
