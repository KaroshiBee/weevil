module Conduit = Conduit_lwt_unix
open Lwt


let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let main_handler ~mode ~content_length =
  let config : Dapper.Dap_handler_t.config = {
    launch_mode=`Attach;
  } in
  let hdl = Handler.make in
  let dap_svc =
    Conduit.init () >>= fun ctx ->
    Conduit.serve ~on_exn ~ctx ~mode (Handler.main_handler hdl config ~content_length)
  in

  Lwt.return dap_svc


let svc ~listen_address ~port =
  let () = assert (listen_address = Unix.inet_addr_loopback) in
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Debug) in
  let mode = `TCP (`Port port) in
  let content_length = None in
  let the_svc = (main_handler ~mode ~content_length) >|= fun _ ->
    `Ok ()
  in

  Lwt_main.run the_svc
