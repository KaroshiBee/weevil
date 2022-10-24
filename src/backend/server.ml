module Conduit = Conduit_lwt_unix
open Lwt



(* let read_weevil_recs ln = *)
(*   if 0 < String.length ln && String.get ln 0 != '#' then ( *)
(*     match from_string ln with *)
(*     | Ok ln -> *)
(*       let ln = destruct Model.Weevil_json.enc ln in *)
(*       Some ln *)
(*     | Error e -> *)
(*       Logs.warn (fun m -> m "Cannot decode '%s': %s" ln e); *)
(*       None *)
(*   ) else None *)

(* let rec step_handler ~ic_process = *)
(*   Lwt_io.read_line_opt ic_process >>= function *)
(*   | Some msg -> *)
(*     Logs_lwt.info (fun m -> m "[STEPPER] got msg from subprocess '%s'" msg) >>= fun _ -> ( *)
(*       match read_weevil_recs msg with *)
(*       | Some wrec -> *)
(*         recs := wrec :: !recs; *)
(*         Logs_lwt.info (fun m -> m "[STEPPER] got weevil log record from subprocess '%s'" msg) *)
(*       | None -> Lwt.return_unit *)
(*     ) >>= fun _ -> *)
(*     step_handler ~ic_process *)
(*   | None -> *)
(*     Logs_lwt.info (fun m -> m "[STEPPER] subprocess complete") *)

let step_handler ~ic_process:_ = failwith "TODO"
let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)
let dap_handler = Handler.main_handler

let main_handler ~mode ~content_length (process_full:Lwt_process.process_full) =
  let p = process_full in
  let config : Handler_t.config = {
    launch_mode=`Attach;
    ic=None;
    oc=None;
  } in
  let hdl = Handler.make in
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
