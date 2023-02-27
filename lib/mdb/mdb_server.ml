module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Dap = Dapper.Dap
module MichEvent = Mdb_event
module Model = Mdb_model
module Tbl = Mdb_log_records
open Lwt

let terminated = ref false

let rec stopper () =
  let%lwt () = Lwt_unix.sleep 0.1 in
  if !terminated then
    Logs_lwt.warn (fun m -> m "[MICH] Stopping mdb server")
  else
    stopper ()

exception Stepper_error of string

let _MDB_NAME = "MDB"

let read_weevil_recs ~enc = function
  | ln when 0 < String.length ln && String.get ln 0 != '#' -> (
      (* non-comments *)
      match Js.from_string ln with
      | Ok js -> (
          try
            let t = Js.destruct enc js in
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
    let%lwt () = Logs_lwt.debug (fun m -> m "comment: %s" ln) in
    Lwt.return_none
  | ln ->
    (* other stuff - octez writes out to stdout sometimes *)
    let%lwt () = Logs_lwt.debug (fun m -> m "other: %s" ln) in
    Lwt.return_none

let step_handler ~recs msg _ic _oc =
  let%lwt () = Logs_lwt.debug (fun m -> m "[STEPPER] got msg from subprocess '%s'" msg) in
  match%lwt read_weevil_recs ~enc:Model.enc msg with
  | Some wrec ->
    let%lwt () = Logs_lwt.debug (fun m -> m "[STEPPER] got weevil log record from subprocess '%s'" msg) in
    Lwt.return @@ Tbl.add_new recs wrec
  | None ->
    Logs_lwt.debug (fun m -> m "[STEPPER] couldnt read weevil log record from subprocess msg '%s'" msg)

and step_err_handler err _ic _oc =
  let%lwt () = Logs_lwt.err (fun m -> m "[STEPPER ERR] step_err_handler: %s" err) in
  Lwt.return_unit

let stepper_process : Lwt_process.process_full option ref = ref None

let rec main_handler ~recs msg ic oc =

  let%lwt _ = Logs_lwt.debug (fun m -> m "[MICH] got msg '%s'" msg) in

  match (MichEvent.from_msg_opt msg, !stepper_process) with

  | Some MichEvent.GetRecords _, _ -> (
    let%lwt _ = Logs_lwt.debug (fun m -> m "[MICH] getting current records") in
    let enc = Data_encoding.list Model.enc in
    let%lwt records =
      Tbl.to_list recs
      |> function
      | [] ->
        let%lwt () = Logs_lwt.debug (fun m -> m "[MICH] no current records") in
        Lwt.return []
      | _ as ls ->
        let%lwt () = Logs_lwt.debug (fun m -> m "[MICH] %d current records" @@ List.length ls) in
        Lwt.return ls
    in
    (* TODO write to irmin *)
    let msg = Js.(construct enc records |> to_string |> Dap.Header.wrap) in
    let%lwt () = Logs_lwt.debug (fun m -> m "[MICH] writing msg '%s' backend -> adaptor" msg) in
    let%lwt () = Lwt_io.write oc msg in
    (* NOTE keep old ones so we can do back stepping - TODO back stepping *)
    Lwt.return @@ Tbl.new_to_old_inplace ~keep_old:true recs
  )

  (* NOTE currently can only run one debugger at a time *)
  | Some (MichEvent.RunScript _), Some process when process#state = Lwt_process.Running ->
    Logs_lwt.err (fun m -> m "[MICH] trying to start a new stepper with old one still running, ignore")

  | Some (MichEvent.RunScript {cmd}), Some _
  | Some (MichEvent.RunScript {cmd}), None ->
    let%lwt _ = Logs_lwt.debug (fun m -> m "[MICH] starting new stepper with cmd '%s'" cmd) in
    (* NOTE clear out old log records *)
    let () = Tbl.remove_all recs in
    let cmd = Dap.Config.to_process_command cmd in
    Lwt_process.with_process_full cmd (stepper_process_start ~recs ic oc)

  | Some (MichEvent.Step {step_size=1}), Some process ->
    let oc_process = process#stdin in
    let%lwt () =
      try%lwt
        Lwt_io.write oc_process "step\n" >>= fun _ ->
        Logs_lwt.debug (fun m -> m "[MICH] Got Next request\n%s\n" msg)
      with Sys_error _ -> (
          (* run out of contract to step through *)
          try%lwt
            (* let _ = Unix.close_process (ic_process, oc_process) in (); *)
            Logs_lwt.warn (fun m -> m "[MICH] Process finished: sys error")
          with Unix.Unix_error _ ->
            Logs_lwt.warn (fun m -> m "[MICH] Process finished: unix error")
        )
    in
    Lwt.return_unit

  | Some (MichEvent.Step {step_size=n}), Some _ ->
    Logs_lwt.warn (fun m -> m "[MICH] TODO Step %d" n)

  | Some MichEvent.Terminate _, Some process when process#state = Lwt_process.Running ->
    (* NOTE debug this message because cram tests use Info and we dont want PIDs in the cram test data *)
    let%lwt () = Logs_lwt.debug (fun m -> m "[MICH] Terminating stepper pid '%d'" process#pid) in
    let%lwt () =
      match%lwt process#close with
      | Unix.WEXITED i -> Logs_lwt.debug (fun m -> m "[MICH] stepper processed closed normally %d" i)
      | Unix.WSIGNALED i -> Logs_lwt.debug (fun m -> m "[MICH] stepper processed killed by signal %d" i)
      | Unix.WSTOPPED i -> Logs_lwt.debug (fun m -> m "[MICH] stepper processed stopped by signal %d" i)
    in
    let%lwt () = Logs_lwt.debug (fun m -> m "[MICH] Setting stepper process to None") in
    let () = stepper_process := None in
    (* set the global terminated to true so that the stopper activates *)
    Lwt.return (terminated := true)

  | Some MichEvent.Terminate _, Some _
  | Some MichEvent.Terminate _, None ->
    let%lwt () = Logs_lwt.debug (fun m -> m "[MICH] already terminated") in
    let () = stepper_process := None in
    (* set the global terminated to true so that the stopper activates *)
    Lwt.return (terminated := true)

  | Some _, None ->
    Logs_lwt.warn (fun m -> m "[MICH] LOST PROCESS '%s'" msg)

  | None, _ ->
    Logs_lwt.warn (fun m -> m "[MICH] couldn't decode '%s'" msg)

and stepper_process_start ~recs ic oc process_full =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[MICH] stepper_process_start") in
  let%lwt _ = Logs_lwt.debug (fun m -> m "[STEPPER] starting") in
  let () = stepper_process := Some process_full in
  let st_out = Dap.content_length_message_handler
      ~name:"STEPPER"
      ~handle_message:(step_handler ~recs)
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
  let m = Dap.content_length_message_handler
    ~name:_MDB_NAME
    ~handle_message:(main_handler ~recs)
    ~content_length:None
    ic
    oc
  in
  Lwt.join [st_err; st_out; m]

let on_exn exn =
  Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let on_connection _flow ic oc =
  let%lwt () = Logs_lwt.debug (fun m -> m "[MICH] got connection") in
  let recs : Model.t Tbl.t = Tbl.make () in
  Dap.content_length_message_handler
    ~name:_MDB_NAME
    ~handle_message:(main_handler ~recs)
    ~content_length:None
    ic
    oc

let lwt_svc ?stopper port =
  let open Lwt_result_syntax in
  let mode = `TCP (`Port port) in
  let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d" port) in
  let*! ctx = Conduit.init () in
  let*! ret =
    match stopper with
    | Some stop ->
      let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d with stopper" port) in
      Conduit.serve ~stop ~on_exn ~ctx ~mode @@ on_connection
    | None ->
      let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d without stopper" port) in
      Conduit.serve ~on_exn ~ctx ~mode @@ on_connection
  in
  return ret

let svc ~port =
  let stopper = stopper () in
  match Lwt_main.run (lwt_svc ~stopper port) with
  | Ok _ -> `Ok ()
  | Error err -> `Error (true, Data_encoding.Json.(construct trace_encoding err |> to_string))
