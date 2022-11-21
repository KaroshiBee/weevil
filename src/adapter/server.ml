module Dap = Dapper.Dap
module Conduit = Conduit_lwt_unix
open Lwt

module GlobalState = State.T (Dap.MakeState ())

module Handler = Handler.T (GlobalState)

let handle_message ~hdl ~state msg _ic oc =
  match%lwt Handler.handle_exn hdl state msg with
  | Ok js ->
    let js = List.map (Dap.Header.wrap ~add_header:true) js
             |> String.concat ""
        in
    let%lwt _ = Logs_lwt.info (fun m -> m "[DAP] got response: '%s'" js) in
    Lwt_io.write oc js
  | Error err ->
    Logs_lwt.err (fun m -> m "[DAP] %s" err)

let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let on_connection hdl state _flow ic oc =
  let%lwt () = Logs_lwt.info (fun m -> m "[DAP] got connection") in
  Dap.content_length_message_handler
    ~name:"DAP"
    ~handle_message:(handle_message ~hdl ~state)
    ~content_length:None
    ic
    oc

let svc ~port =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let mode = `TCP (`Port port) in
  (* handler and state are global to the svc *)
  let hdl = Handler.make in
  let state = GlobalState.make () in
  let () = Logs.info (fun m -> m "[DAP] starting adapter server on port %d" port) in
  Lwt_main.run (
    Conduit.init () >>= fun ctx ->
    Conduit.serve ~on_exn ~ctx ~mode (on_connection hdl state)
    >|= fun _ ->
    `Ok ()
  )
