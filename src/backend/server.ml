module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json

open Lwt

let recs : Model.Weevil_json.t list ref  = ref []

let read_weevil_recs ln =
  if 0 < String.length ln && String.get ln 0 != '#' then (
    match Js.from_string ln with
    | Ok ln ->
      let ln = Js.destruct Model.Weevil_json.enc ln in
      Some ln
    | Error e ->
      Logs.warn (fun m -> m "Cannot decode '%s': %s" ln e);
      None
  ) else None

let rec step_handler ~ic_process =
  Lwt_io.read_line_opt ic_process >>= function
  | Some msg ->
    Logs_lwt.info (fun m -> m "[STEPPER] got msg from subprocess '%s'" msg) >>= fun _ -> (
      match read_weevil_recs msg with
      | Some wrec ->
        recs := wrec :: !recs;
        Logs_lwt.info (fun m -> m "[STEPPER] got weevil log record from subprocess '%s'" msg)
      | None -> Lwt.return_unit
    ) >>= fun _ ->
    step_handler ~ic_process
  | None ->
    Logs_lwt.info (fun m -> m "[STEPPER] subprocess complete")


module MichEvent = struct

  type ev =
  | RunScript of string
  | Terminate
  | Step of int

  type t = {
    event: ev;
  }

  let enc_ev =
    let open Data_encoding in
    union [
      case ~title:"RunScript" (Tag 0)
        string
        (function RunScript s -> Some s | _ -> None)
        (fun s -> RunScript s);
      case ~title:"Terminate" (Tag 1)
        empty
        (function Terminate -> Some () | _ -> None)
        (fun _ -> Terminate);
      case ~title:"Step" (Tag 2)
        int31
        (function Step n -> Some n | _ -> None)
        (fun n -> Step n);
    ]

  let enc =
    let open Data_encoding in
    conv
      (function {event;} -> event)
      (fun event -> {event;})
      (obj1
         (req "event" enc_ev))

  let from_msg_opt msg =
    try
      (* TODO be better *)
      let r : t = Js.(from_string msg |> Result.get_ok |> destruct enc) in
      Option.Some (r.event)
    with _ ->
      None


end

let rec main_handler ~sub_process _flow ic oc =
  let%lwt ln = Lwt_io.read_line_opt ic in
  match ln with
  | Some msg -> (
    let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] got msg '%s'" msg) in
    match (MichEvent.from_msg_opt msg, sub_process) with

    | Some (RunScript _), Some process when process#state = Lwt_process.Running ->
      let%lwt _ = Logs_lwt.err (fun m -> m "[MICH] trying to start a new stepper with old one still running, ignore") in
      main_handler ~sub_process _flow ic oc

    | Some (RunScript cmd), Some _
    | Some (RunScript cmd), None ->
      let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] starting new stepper with cmd '%s'" cmd) in
      let cmd = ("", String.split_on_char ' ' cmd |> Array.of_list) in
      Lwt_process.with_process_full cmd (subprocess_start _flow ic oc)

    | Some (Step 1), Some process ->
      let _ =
        let oc_process = process#stdin in
        try
          Lwt_io.write oc_process "step\n" >>= fun _ ->
          Lwt_io.write oc "0" >>= fun _ ->
          Logs_lwt.debug (fun m -> m "[MICH] Got Next request\n%s\n" msg)
        with Sys_error _ -> (
            Lwt_io.write oc "1" >>= fun _ ->
            (* run out of contract to step through *)
            try
              (* let _ = Unix.close_process (ic_process, oc_process) in (); *)
              Logs_lwt.warn (fun m -> m "[MICH] Process finished: sys error")
            with Unix.Unix_error _ ->
              Logs_lwt.warn (fun m -> m "[MICH] Process finished: unix error")
          )
      in
      main_handler ~sub_process _flow ic oc

    | Some Terminate, Some process when process#state = Lwt_process.Running ->
      let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] Terminating pid '%d'" process#pid) in
      let _ = process#terminate in
      main_handler ~sub_process:None _flow ic oc

    | Some Terminate, Some _
    | Some Terminate, None ->
      let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] already terminated") in
      main_handler ~sub_process:None _flow ic oc

    | Some _, _ ->
      let%lwt _ = Logs_lwt.debug (fun m -> m "[MICH] TODO '%s'" msg) in
      main_handler ~sub_process _flow ic oc

    | None, _ ->
      let%lwt _ = Logs_lwt.warn (fun m -> m "[MICH] couldn't decode '%s'" msg) in
      main_handler ~sub_process _flow ic oc
  )

  | None -> Logs_lwt.info (fun m -> m "[MICH] connection closed")

and subprocess_start _flow ic oc process_full =
  let st = step_handler ~ic_process:process_full#stdout in
  let m = main_handler ~sub_process:(Some process_full) _flow ic oc in
  Lwt.join [st; m]


let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let svc ~listen_address ~port =
  let () = assert (listen_address = Unix.inet_addr_loopback) in
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let mode = `TCP (`Port port) in
  Lwt_main.run (
    Conduit.init () >>= fun ctx ->
    Conduit.serve ~on_exn ~ctx ~mode (main_handler ~sub_process:None)
    >|= fun _ ->
    `Ok ()
  )
