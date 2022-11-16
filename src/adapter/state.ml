open Conduit_lwt_unix
module Dap = Dapper.Dap

module T (Dap_state:Dap.STATE_T) = struct
  type t = {
    sequencing: Dap_state.t;
    mutable process : Lwt_process.process_none option;
    mutable ic : Lwt_io.input_channel option;
    mutable oc : Lwt_io.output_channel option;
    mutable launch_mode : Launch_mode.t option;
    mutable config : Config.t;
    mutable client_config : Dap.Data.InitializeRequestArguments.t option;
  }

  let make () = {
    sequencing = Dap_state.make ();
    process = None;
    ic = None;
    oc = None;
    launch_mode = None;
    config = Config.make ();
    client_config = None;
  }

  let backend_svc t = t.process

  let backend_ic t = t.ic

  let backend_oc t = t.oc

  let _set_io t ic oc =
    t.ic <- Some ic ;
    t.oc <- Some oc

  let _set_process_none t process = t.process <- Some process

  let set_start_backend t _ip _port cmd =
    let%lwt () =
      Logs_lwt.debug (fun m ->
          m "launching backend service with cmd: '%s'" cmd)
    in
    let pcmd = Config.(to_process_command cmd) in
    let p =
      try%lwt
        Dap.Result.ok @@ Lwt_process.open_process_none pcmd
      with
      | _ as err ->
        let err_s = Printexc.to_string err in
        Dap.Result.(from_error_string err_s |> or_log_error)
    in

    p |> Dap.Result.bind ~f:(fun process ->
        let%lwt () = Logs_lwt.debug (fun m ->
            m "backend service has state: '%s'"
            @@
            match process#state with
            | Lwt_process.Running -> "running"
            | Lwt_process.Exited _ -> "exited")
        in
        Dap.Result.ok @@ _set_process_none t process
      )

  (* loop a fixed number of times with a sleep, to make sure to connect when up *)
  let rec _aux ~ctx ~client ~port i =
    let%lwt () =
      Logs_lwt.debug (fun m ->
          m "[%d] trying to connect on locahost port: %d" i port)
    in
    let%lwt () = Lwt_unix.sleep @@ float_of_int i in
    try%lwt
      let%lwt cn = connect ~ctx client in
      let%lwt () =
        Logs_lwt.debug (fun m -> m "connected on locahost port: %d" port)
      in
      Lwt.return cn
    with Unix.Unix_error (Unix.ECONNREFUSED, "connect", "") as e ->
      if i > 5 then raise e else _aux ~ctx ~client ~port (i + 1)

  let set_connect_backend t ip port =
    let client = `TCP (`IP ip, `Port port) in
    let%lwt ctx = init () in
    let res =
      try%lwt
        let%lwt (_flow, ic, oc) = _aux ~ctx ~client ~port 1 in
        (ic, oc) |> Dap.Result.ok
      with _ as err ->
        let err_s = Printexc.to_string err in
        Dap.Result.(from_error_string err_s |> or_log_error)
    in
    res
    |> Dap.Result.bind ~f:(fun (ic, oc) ->
        let () = _set_io t ic oc in
        Dap.Result.ok (ic, oc))

  let launch_mode t = t.launch_mode

  let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode

  let current_seqr t = Dap_state.current_seqr t.sequencing

  let set_seqr t seqr = Dap_state.set_seqr t.sequencing seqr

  let config t = t.config

  let set_config t config = t.config <- config

  let client_config t = t.client_config

  let set_client_config t config = t.client_config <- Some config

end
