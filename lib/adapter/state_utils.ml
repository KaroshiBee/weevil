module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Mich_event = Mdb.Mdb_server.MichEvent
module Js = Data_encoding.Json

module T (S : Types.STATE_T) = struct

  let connect_background_svc st ip port =
    let%lwt () =
      Logs_lwt.debug (fun m ->
          m "trying to connect to backend service on port %d" port)
    in
    S.set_connect_backend st ip port |> Dap_result.or_log_error

  let attach state dap_config mdb_config =
    match S.backend_oc state with
    | Some _ ->
      (* already connected so just update state *)
      let () = S.set_mdb_config state mdb_config in
      let () = S.set_launch_mode state `Attach in
      Dap_result.ok ()
    | None -> (
        (* NOTE dont need to start the backend as we are in attach mode, just connect to the backend *)
        let ip = Dap.Config.backend_ip dap_config |> Ipaddr_unix.of_inet_addr in
        let port = Dap.Config.backend_port dap_config in
        connect_background_svc state ip port
        |> Dap_result.map ~f:(fun _ ->
            (* not errored so update state *)
            let () = S.set_mdb_config state mdb_config in
            let () = S.set_launch_mode state `Attach in
            ())
      )

  let launch state dap_config mdb_config =
    let ip = Dap.Config.backend_ip dap_config |> Ipaddr_unix.of_inet_addr in
    let port = Dap.Config.backend_port dap_config in
    connect_background_svc state ip port
    |> Dap_result.bind ~f:(fun (_ic, oc) ->
        let stepper_cmd = Mdb.Mdb_config.(
            Dap.Config.stepper_cmd
              ~script_filename:mdb_config.script_filename
              ~storage:mdb_config.storage
              ~parameter:mdb_config.parameter
              ~entrypoint:mdb_config.entrypoint
              dap_config
          ) in
        let%lwt () =
          Logs_lwt.debug (fun m ->
              m
                "trying to start the debugger with cmd: '%s'"
                stepper_cmd)
        in
        let runscript =
          Mich_event.make ~event:(RunScript {cmd=stepper_cmd}) ()
        in
        (* NOTE remove all \n with wrap *)
        let runscript_s =
          Js.(
            construct Mich_event.enc runscript
            |> to_string
            |> Dap.Header.wrap
          )
        in
        (* NOTE then write_line to make server consume *)
        let%lwt () = Lwt_io.write_line oc runscript_s in
        (* only change state if ok connection *)
        let () = S.set_mdb_config state mdb_config in
        let () = S.set_launch_mode state `Launch in
        Dap_result.ok ()
      )


  let terminate state =
    (* terminate the backend process *)
    match S.backend_oc state, S.backend_svc state with
    | None, _ ->
      Logs_lwt.err (fun m -> m "no backend oc channel, cannot terminate")
    | _, None ->
      Logs_lwt.err (fun m -> m "no backend svc, cannot terminate")
    | Some oc, Some process ->
      let%lwt () = Logs_lwt.debug (fun m -> m "closing stepper process") in
      let mich_stop = Mdb.Mdb_event.(make ~event:(Terminate {terminate=()}) ()) in
      let mich_msg = Data_encoding.Json.(construct Mdb.Mdb_event.enc mich_stop |> to_string |> Dap.Header.wrap) in
      let%lwt () = Lwt_io.write oc mich_msg in
      (* TODO sequence in a better way after closing stepper *)
      let%lwt () = Lwt_unix.sleep 0.5 in
      let%lwt () = Logs_lwt.debug (fun m -> m "Terminating backend with pid '%d'" process#pid) in
      let%lwt () =
        match%lwt process#close with
        | Unix.WEXITED i -> Logs_lwt.debug (fun m -> m "backend process closed normally %d" i)
        | Unix.WSIGNALED i -> Logs_lwt.debug (fun m -> m "backend process killed by signal %d" i)
        | Unix.WSTOPPED i -> Logs_lwt.debug (fun m -> m "backend process stopped by signal %d" i)
      in
      Lwt.return_unit

end
