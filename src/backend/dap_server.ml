module Conduit = Conduit_lwt_unix
open Lwt

let step_handler ~ic_process:_ = failwith "TODO"
let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)
let dap_handler = Dap_handler.main_handler

let main_handler ~mode ~content_length (process_full:Lwt_process.process_full) =
  let p = process_full in
  let config : Dapper.Dap_handler_t.config = {
    launch_mode=`Attach;
  } in
  let hdl = Dap_handler.make in
  let dap_svc =
    Conduit.init () >>= fun ctx ->
    Conduit.serve ~on_exn ~ctx ~mode (dap_handler hdl config ~content_length)
  in

  let step_svc =
    step_handler ~ic_process:p#stdout
  in

  Lwt.join [dap_svc; step_svc]


let svc ~listen_address ~port =
  let () = assert (listen_address = Unix.inet_addr_loopback) in
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let mode = `TCP (`Port port) in
  let content_length = None in
  let cmd = ("", [|"dune"; "exec"; "--"; "./main.exe"; "stepper"; "example.tz"|]) in
  let the_svc = Lwt_process.with_process_full cmd (main_handler ~mode ~content_length) >|= fun _ ->
    `Ok ()
  in

  Lwt_main.run the_svc
