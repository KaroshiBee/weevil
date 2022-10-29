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
  let%lwt _ = Logs_lwt.info (fun m -> m "[STEPPER] step handler start, waiting for messages") in
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

  let make ~event () = {event;}

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
(* NOTE needs to be mutable global so different connections can access *)
let stepper_process : Lwt_process.process_full option ref = ref None

let rec main_handler flow ic oc =
  let%lwt ln = Lwt_io.read_line_opt ic in
  match ln with
  | Some msg -> (
    let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] got msg '%s'" msg) in
    match (MichEvent.from_msg_opt msg, !stepper_process) with

    | Some (RunScript _), Some process when process#state = Lwt_process.Running ->
      let%lwt _ = Logs_lwt.err (fun m -> m "[MICH] trying to start a new stepper with old one still running, ignore") in
      main_handler flow ic oc

    | Some (RunScript cmd), Some _
    | Some (RunScript cmd), None ->
      let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] starting new stepper with cmd '%s'" cmd) in
      let cmd = ("", String.split_on_char ' ' cmd |> Array.of_list) in
      Lwt_process.with_process_full cmd (stepper_process_start flow ic oc)

    | Some (Step 1), Some process ->
      let _ =
        let oc_process = process#stdin in
        try
          Lwt_io.write oc_process "step\n" >>= fun _ ->
          Logs_lwt.debug (fun m -> m "[MICH] Got Next request\n%s\n" msg)
        with Sys_error _ -> (
            (* run out of contract to step through *)
            try
              (* let _ = Unix.close_process (ic_process, oc_process) in (); *)
              Logs_lwt.warn (fun m -> m "[MICH] Process finished: sys error")
            with Unix.Unix_error _ ->
              Logs_lwt.warn (fun m -> m "[MICH] Process finished: unix error")
          )
      in
      main_handler flow ic oc
    | Some (Step n), Some _ ->
      let%lwt _ = Logs_lwt.debug (fun m -> m "[MICH] TODO Step %d" n) in
      main_handler flow ic oc

    | Some Terminate, Some process when process#state = Lwt_process.Running ->
      let%lwt () = Logs_lwt.info (fun m -> m "[MICH] Terminating pid '%d'" process#pid) in
      let () = process#terminate in
      let () = stepper_process := None in
      main_handler flow ic oc

    | Some Terminate, Some _
    | Some Terminate, None ->
      let%lwt () = Logs_lwt.info (fun m -> m "[MICH] already terminated") in
      let () = stepper_process := None in
      main_handler flow ic oc

    | Some _, None ->
      let%lwt _ = Logs_lwt.warn (fun m -> m "[MICH] LOST PROCESS '%s'" msg) in
      main_handler flow ic oc

    | None, _ ->
      let%lwt _ = Logs_lwt.warn (fun m -> m "[MICH] couldn't decode '%s'" msg) in
      main_handler flow ic oc
  )

  | None -> Logs_lwt.info (fun m -> m "[MICH] connection closed")

and stepper_process_start flow ic oc process_full =
  let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] stepper_process_start") in
  let () = stepper_process := Some process_full in
  let st = step_handler ~ic_process:process_full#stdout in
  let m = main_handler flow ic oc in
  Lwt.join [st; m]


let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let svc ~listen_address ~port =
  let () = assert (listen_address = Unix.inet_addr_loopback) in
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Debug) in
  let mode = `TCP (`Port port) in
  let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d" port) in
  Lwt_main.run (
    Conduit.init () >>= fun ctx ->
    Conduit.serve ~on_exn ~ctx ~mode main_handler
    >|= fun _ ->
    `Ok ()
  )
