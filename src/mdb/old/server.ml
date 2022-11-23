module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Dap = Dapper.Dap
module MichEvent = Stepper_event

open Lwt

exception Stepper_error of string

let recs : Model.Weevil_json.t list ref  = ref []

let read_weevil_recs = function
  | ln when 0 < String.length ln && String.get ln 0 != '#' -> (
      (* non-comments *)
      match Js.from_string ln with
      | Ok js -> (
          try
            let t = Js.destruct Model.Weevil_json.enc js in
            Lwt.return_some t
          with e ->
            let%lwt () = Logs_lwt.warn (fun m -> m "Cannot js destruct '%s': %s" ln @@ Printexc.to_string e) in
            Lwt.return_none
        )
      | Error e ->
        let%lwt () = Logs_lwt.warn (fun m -> m "Cannot decode '%s': %s" ln e) in
        Lwt.return_none
    )
  | ln when 0 < String.length ln && String.get ln 0 = '#' ->
    (* comments *)
    let%lwt () = Logs_lwt.info (fun m -> m "%s" ln) in
    Lwt.return_none
  | ln ->
    (* other stuff - octez writes out to stdout sometimes *)
    let%lwt () = Logs_lwt.info (fun m -> m "other: %s" ln) in
    Lwt.return_none

let step_handler msg _ic _oc =
  let%lwt () = Logs_lwt.info (fun m -> m "[STEPPER] got msg from subprocess '%s'" msg) in
  match%lwt read_weevil_recs msg with
  | Some wrec ->
    recs := wrec :: !recs;
    Logs_lwt.info (fun m -> m "[STEPPER] got weevil log record from subprocess '%s'" msg)
  | None -> Lwt.return_unit

and step_err_handler err _ic _oc =
  let%lwt () = Logs_lwt.err (fun m -> m "step_err_handler: %s" err) in
	raise @@ Stepper_error err

let rec main_handler ~stepper_process ic oc =
  let%lwt ln = Lwt_io.read_line_opt ic in
  match ln with
  | Some msg -> (
    let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] got msg '%s'" msg) in
    match (MichEvent.from_msg_opt msg, stepper_process) with

    | Some (RunScript _), Some process when process#state = Lwt_process.Running ->
      let%lwt _ = Logs_lwt.err (fun m -> m "[MICH] trying to start a new stepper with old one still running, ignore") in
      main_handler ~stepper_process ic oc

    | Some (RunScript cmd), Some _
    | Some (RunScript cmd), None ->
      let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] starting new stepper with cmd '%s'" cmd) in
      let cmd = ("", String.split_on_char ' ' cmd |> Array.of_list) in
      Lwt_process.with_process_full cmd (stepper_process_start ic oc)

    | Some (Step 1), Some process ->
      let _ =
        let oc_process = process#stdin in
        try
          Lwt_io.write oc_process "step\n" >>= fun _ ->
          Logs_lwt.info (fun m -> m "[MICH] Got Next request\n%s\n" msg)
        with Sys_error _ -> (
            (* run out of contract to step through *)
            try
              (* let _ = Unix.close_process (ic_process, oc_process) in (); *)
              Logs_lwt.warn (fun m -> m "[MICH] Process finished: sys error")
            with Unix.Unix_error _ ->
              Logs_lwt.warn (fun m -> m "[MICH] Process finished: unix error")
          )
      in
      main_handler ~stepper_process ic oc
    | Some (Step n), Some _ ->
      let%lwt _ = Logs_lwt.warn (fun m -> m "[MICH] TODO Step %d" n) in
      main_handler ~stepper_process ic oc

    | Some Terminate, Some process when process#state = Lwt_process.Running ->
      (* NOTE debug this message because cram tests use Info and we dont want PIDs in the cram test data *)
      let%lwt () = Logs_lwt.debug (fun m -> m "[MICH] Terminating pid '%d'" process#pid) in
      let () = process#terminate in
      let stepper_process = None in
      main_handler ~stepper_process ic oc

    | Some Terminate, Some _
    | Some Terminate, None ->
      let%lwt () = Logs_lwt.info (fun m -> m "[MICH] already terminated") in
      let stepper_process = None in
      main_handler ~stepper_process ic oc

    | Some _, None ->
      let%lwt _ = Logs_lwt.warn (fun m -> m "[MICH] LOST PROCESS '%s'" msg) in
      main_handler ~stepper_process ic oc

    | None, _ ->
      let%lwt _ = Logs_lwt.warn (fun m -> m "[MICH] couldn't decode '%s'" msg) in
      main_handler ~stepper_process ic oc
  )

  | None -> Logs_lwt.info (fun m -> m "[MICH] connection closed")

and stepper_process_start ic oc process_full =
  let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] stepper_process_start") in
  let%lwt _ = Logs_lwt.info (fun m -> m "[STEPPER] starting") in
  let stepper_process = Some process_full in
  let st_out = Dap.content_length_message_handler
      ~name:"STEPPER"
      ~handle_message:step_handler
      ~content_length:None
      process_full#stdout
      process_full#stdin
  in
  let st_err = Dap.content_length_message_handler
      ~name:"STEPPER ERR"
      ~handle_message:step_err_handler
      ~content_length:None
      process_full#stderr
      process_full#stdin
  in
  let m = main_handler ~stepper_process ic oc in
  Lwt.join [st_err; st_out; m]

let on_exn exn =
  Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let on_connection _flow ic oc =
  let%lwt () = Logs_lwt.info (fun m -> m "[MICH] got connection") in
  main_handler ~stepper_process:None ic oc

let lwt_svc ?stopper port =
  let mode = `TCP (`Port port) in
  let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d" port) in
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
