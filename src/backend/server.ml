module Conduit = Conduit_lwt_unix
open Lwt


let handle_message hdl config msg =
  match%lwt Handler.handle_exn hdl config msg with
  | Ok _js -> failwith "TODO"
  | Error _err -> failwith "TODO"


let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

type content_length = int option

let rec dap_handler hdl (config:Handler_t.config) ~content_length _flow ic _oc =
  match content_length with
  | Some count ->
      Logs_lwt.info (fun m -> m "[DAP] got count %d" count) >>= fun _ ->
      (* \r\n throw away *)
      Lwt_io.read ~count:2 ic >>= fun header_break ->
      assert (header_break = "\r\n") |> Lwt.return >>= fun _ ->
      Lwt_io.read ~count ic >>= fun msg ->
      Logs_lwt.info (fun m -> m "[DAP] Got message '%s'" msg) >>= fun _ ->
      handle_message hdl config msg >>= fun _ ->
      dap_handler hdl config ~content_length:None _flow ic _oc
  | None -> (
      Logs_lwt.info (fun m -> m "[DAP] no content length yet") >>= fun _ ->
      Lwt_io.read_line_opt ic >>= function
      | Some msg ->
          let content_length = Header.content_length msg in
          dap_handler hdl config ~content_length _flow ic _oc
      | None -> Logs_lwt.info (fun m -> m "[DAP] connection closed"))

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
