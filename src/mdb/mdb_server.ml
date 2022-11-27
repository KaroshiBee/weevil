module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Dap = Dapper.Dap
module MichEvent = Mdb_event


let rec main_handler ~input_mvar ~output_mvar ~stepper_process ic oc =
  let%lwt ln = Lwt_io.read_line_opt ic in
  match ln, stepper_process with
  | Some _msg, None -> (
      (* let fname = "/home/wyn/dev/weevil/src/backend/tests/backend_cram_tests/stepper_test.t/multiply_2_x_250_equals_500.tz" in *)
      (* let p = Lwt_preemptive.detach (fun (headless, filename) -> process ~input_mvar headless (Some filename)) (true, fname) in *)
      (* let%lwt () = Logs_lwt.info (fun m -> m "[MICH] spawned '%s'" fname) in *)
      (* Lwt.join [p; main_handler ~input_mvar ~stepper_process:(Some p) ic oc] *)
      main_handler ~input_mvar ~output_mvar ~stepper_process:None ic oc
    )

  | Some _msg, Some p -> (
    let%lwt () = Logs_lwt.info (fun m -> m "[MICH] process already spawned") in
    let%lwt () = Logs_lwt.info (fun m -> m "[MICH] process state %s" @@ match Lwt.state p with | Return _x -> "finished" | Sleep -> "sleep" | Fail _exn -> "failed") in
    match Lwt.state p with
    | Sleep ->
      let p = Lwt_mvar.put input_mvar _msg in
      Lwt.join [p; main_handler ~input_mvar ~output_mvar ~stepper_process ic oc]
    | Return _ | Fail _ ->
      main_handler ~input_mvar ~output_mvar ~stepper_process:None ic oc
  )

  | None, _ -> Logs_lwt.info (fun m -> m "[MICH] connection closed")

let on_exn exn =
  Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let on_connection _flow ic oc =
  let%lwt () = Logs_lwt.info (fun m -> m "[MICH] got connection") in
  let input_mvar = Lwt_mvar.create_empty () in
  let output_mvar = Lwt_mvar.create_empty () in
  main_handler ~input_mvar ~output_mvar ~stepper_process:None ic oc

let lwt_svc ?stopper port =
  let open Lwt in
  let mode = `TCP (`Port port) in
  let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d" port) in
  (* we run the stepper in a preemptive thread
     so that can pause it without pausing the main thread,
     NOTE currently only allowing one interp process at a time *)
  let () = Lwt_preemptive.simple_init () in
  let () = Lwt_preemptive.set_bounds (1,1) in
  let ret =
    Conduit.init () >>= fun ctx -> (
      match stopper with
      | Some stop -> Conduit.serve ~stop ~on_exn ~ctx ~mode on_connection
      | None -> Conduit.serve ~on_exn ~ctx ~mode on_connection
    )
    >|= fun _ ->
    `Ok ()
  in
  ret

let svc ~port =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  Lwt_main.run (lwt_svc port)
