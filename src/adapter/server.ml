module Dap_header = Dapper.Dap_header
module Conduit = Conduit_lwt_unix
open Lwt


let handle_message hdl frontend_io config msg =
  let _ic, oc = frontend_io in
  match%lwt Handler.handle_exn hdl config msg with
  | Ok js ->
    let%lwt _ = Logs_lwt.info (fun m -> m "[DAP] got response: '%s'" js) in
    Lwt_io.write oc js
  | Error err ->
    Logs_lwt.err (fun m -> m "[DAP] %s" err)


let rec main_handler hdl config content_length flow ic oc =
  let frontend_io = (ic, oc) in
  match content_length with
  | Some count ->
      Logs_lwt.info (fun m -> m "[DAP] got count %d" count) >>= fun _ ->
      (* \r\n throw away *)
      Lwt_io.read ~count:2 ic >>= fun header_break ->
      assert (header_break = "\r\n") |> Lwt.return >>= fun _ ->
      Lwt_io.read ~count ic >>= fun msg ->
      Logs_lwt.info (fun m -> m "[DAP] Got message '%s'" msg) >>= fun _ ->
      handle_message hdl frontend_io config msg >>= fun _ ->
      let content_length = None in
      main_handler hdl config content_length flow ic oc
  | None -> (
      Logs_lwt.info (fun m -> m "[DAP] no content length yet") >>= fun _ ->
      Lwt_io.read_line_opt ic >>= function
      | Some msg ->
          let content_length = Dap_header.content_length msg in
          main_handler hdl config content_length flow ic oc
      | None -> Logs_lwt.info (fun m -> m "[DAP] connection closed")
    )


let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)


let svc ~listen_address ~port =
  let () = assert (listen_address = Unix.inet_addr_loopback) in
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Debug) in
  let mode = `TCP (`Port port) in
  let config = Dapper.Dap_config.make () in
  let hdl = Handler.make in
  let content_length = None in
  Lwt_main.run (
    Conduit.init () >>= fun ctx ->
    Conduit.serve ~on_exn ~ctx ~mode (main_handler hdl config content_length)
    >|= fun _ ->
    `Ok ()
  )
