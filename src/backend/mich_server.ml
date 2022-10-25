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

  type t =
  | RunScript of string
  | Terminate
  | Disconnect
  | Step of int

  let enc =
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
      case ~title:"Disconnect" (Tag 2)
        empty
        (function Disconnect -> Some () | _ -> None)
        (fun _ -> Disconnect);
      case ~title:"Step" (Tag 3)
        int31
        (function Step n -> Some n | _ -> None)
        (fun n -> Step n);
    ]

  let from_msg_opt msg =
    try
      Option.some @@ Js.(from_string msg |> Result.get_ok |> destruct enc)
    with _ ->
      None


end

let rec main_handler ~oc_process _flow ic oc =
  let%lwt ln = Lwt_io.read_line_opt ic in
  match ln with
  | Some msg -> (
    let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] got msg '%s'" msg) in
    match (MichEvent.from_msg_opt msg, oc_process) with
    | Some (RunScript _), Some _ ->
      let%lwt _ = Logs_lwt.err (fun m -> m "[MICH] trying to start a new stepper with old one still running, ignore") in
      main_handler ~oc_process _flow ic oc
    | Some (RunScript cmd), None ->
      let%lwt _ = Logs_lwt.info (fun m -> m "[MICH] starting new stepper with cmd '%s'" cmd) in
      let cmd = ("", String.split_on_char ' ' cmd |> Array.of_list) in
      Lwt_process.with_process_full cmd (subprocess_start _flow ic oc)
    | Some (Step 1), Some oc_process ->
      let _ =
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
      main_handler ~oc_process:(Some oc_process) _flow ic oc
    | Some _, _ ->
      let%lwt _ = Logs_lwt.debug (fun m -> m "TODO '%s'" msg) in
      main_handler ~oc_process _flow ic oc
    | None, _ ->
      let%lwt _ = Logs_lwt.warn (fun m -> m "[MICH] couldn't decode '%s'" msg) in
      main_handler ~oc_process _flow ic oc
  )
  | None -> Logs_lwt.info (fun m -> m "[MICH] connection closed")

and subprocess_start _flow ic oc process_full =
  let st = step_handler ~ic_process:process_full#stdout in
  let m = main_handler ~oc_process:(Some process_full#stdin) _flow ic oc in
  Lwt.join [st; m]


let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let svc ~listen_address ~port =
  let () = assert (listen_address = Unix.inet_addr_loopback) in
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let mode = `TCP (`Port port) in
  Lwt_main.run (
    Conduit.init () >>= fun ctx ->
    Conduit.serve ~on_exn ~ctx ~mode (main_handler ~oc_process:None)
  )
