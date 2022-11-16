module Dap = Dapper.Dap
module Conduit = Conduit_lwt_unix
open Lwt

module GlobalState = State.T (Dap.MakeState ())

module Handler = Handler.T (GlobalState)

let handle_message ~frontend_io hdl state msg =
  let _ic, oc = frontend_io in
  match%lwt Handler.handle_exn hdl state msg with
  | Ok js ->
    let js = List.map (Header.wrap ~add_header:true) js
             |> String.concat ""
        in
    let%lwt _ = Logs_lwt.info (fun m -> m "[DAP] got response: '%s'" js) in
    Lwt_io.write oc js
  | Error err ->
    Logs_lwt.err (fun m -> m "[DAP] %s" err)


let rec main_handler hdl state content_length flow ic oc =
  let frontend_io = (ic, oc) in
  match content_length with
  | Some count ->
      Logs_lwt.info (fun m -> m "[DAP] got message with length %d" count) >>= fun _ ->
      (* \r\n throw away *)
      Lwt_io.read ~count:2 ic >>= fun header_break ->
      assert (header_break = "\r\n") |> Lwt.return >>= fun _ ->
      Lwt_io.read ~count ic >>= fun msg ->
      Logs_lwt.info (fun m -> m "[DAP] Got message '%s'" msg) >>= fun _ ->
      handle_message ~frontend_io hdl state msg >>= fun _ ->
      let content_length = None in
      main_handler hdl state content_length flow ic oc
  | None -> (
      Logs_lwt.info (fun m -> m "[DAP] waiting for messages") >>= fun _ ->
      Lwt_io.read_line_opt ic >>= function
      | Some msg ->
          let content_length = Header.content_length msg in
          main_handler hdl state content_length flow ic oc
      | None -> Logs_lwt.info (fun m -> m "[DAP] connection closed")
    )


let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let on_connection hdl state content_length flow ic oc =
  let%lwt () = Logs_lwt.info (fun m -> m "[DAP] got connection") in
  main_handler hdl state content_length flow ic oc

let svc ~port =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let mode = `TCP (`Port port) in
  let hdl = Handler.make in
  let content_length = None in
  let state = GlobalState.make () in
  let () = Logs.info (fun m -> m "[DAP] starting adapter server on port %d" port) in
  Lwt_main.run (
    Conduit.init () >>= fun ctx ->
    Conduit.serve ~on_exn ~ctx ~mode (on_connection hdl state content_length)
    >|= fun _ ->
    `Ok ()
  )
