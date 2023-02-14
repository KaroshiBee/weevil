open Conduit_lwt_unix
module Dap = Dapper.Dap
module Helpers = Utils.Helpers
module Model = Mdb.Mdb_model


module T (Dap_state:Dap.STATE_T) = struct
  type t = {
    sequencing: Dap_state.t;
    mutable process : Lwt_process.process_none option;
    mutable ic : Lwt_io.input_channel option;
    mutable oc : Lwt_io.output_channel option;
    mutable launch_mode : Dap.Launch_mode.t option;
    mutable config : Dap.Config.t;
    mutable client_config : Dap.Data.InitializeRequestArguments.t option;
    mutable mdb_config : Mdb.Mdb_config.t option;
    mutable log_records : Model.Weevil_json.t list;
    mutable should_restart_on_terminate : bool option;
  }

  let make () = {
    sequencing = Dap_state.make ();
    process = None;
    ic = None;
    oc = None;
    launch_mode = None;
    config = Dap.Config.make ();
    client_config = None;
    mdb_config = None;
    log_records = [];
    should_restart_on_terminate = None;
  }

  let reset_backend t =
    t.process <- None;
    t.ic <- None;
    t.oc <- None;
    t.log_records <- [];
    t.mdb_config <- None

  let backend_svc t = t.process

  let backend_ic t = t.ic

  let backend_oc t = t.oc

  let set_new_log_records t recs =
    t.log_records <- (recs @ t.log_records)

  let log_records t = t.log_records

  let should_restart_on_terminate t = t.should_restart_on_terminate

  let _set_io t ic oc =
    t.ic <- Some ic ;
    t.oc <- Some oc

  let _set_process_none t process = t.process <- Some process

  let set_start_backend t _ip _port cmd =
    let%lwt () =
      Logs_lwt.debug (fun m ->
          m "launching backend service with cmd: '%s'" cmd)
    in
    let pcmd = Dap.Config.to_process_command cmd in
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

  let set_connect_backend t ip port =
    let client = `TCP (`IP ip, `Port port) in
    let%lwt ctx = init () in
    let res =
      try%lwt
        let%lwt (_flow, ic, oc) = Helpers.loop_connect ~ctx ~client ~port 5 in
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

  (* set in launch/attach *)
  let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode

  let current_seqr t = Dap_state.current_seqr t.sequencing

  let set_seqr t seqr = Dap_state.set_seqr t.sequencing seqr

  let config t = t.config

  (* not used I dont think *)
  let set_config t config = t.config <- config

  let client_config t = t.client_config

  (* set in initialize *)
  let set_client_config t config = t.client_config <- Some config

  let mdb_config t = t.mdb_config

  (* set in launch/attach  *)
  let set_mdb_config t config = t.mdb_config <- Some config

  let set_should_restart_on_terminate t restart = t.should_restart_on_terminate <- restart

end
