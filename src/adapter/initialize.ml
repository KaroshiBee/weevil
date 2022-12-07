module Dap = Dapper.Dap
module D = Dap.Data
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.STATE_T) = struct

  module On_request = Dap.Initialize.On_request (S)
  module On_response = Dap.Initialize.Raise_initialized (S)

  let _start_background_svc st ip port cmd =
    match S.backend_svc st with
    | None ->
      S.set_start_backend st ip port cmd
    | Some _ ->
        let%lwt () =
          Logs_lwt.info (fun m ->
              m "backend service already running on port %d" port)
        in
        Dap.Result.ok ()

  (* saves the config and starts the mdb backend *)
  let initialize_handler =
    On_request.make ~handler:(fun ~state req ->
        let getargs = Req.(fmap_ Message.arguments) in
        let args = Req.(eval @@ map_ (val_ getargs, val_ req)) in
        let () = S.set_client_config state args in
        let config = S.config state in
        let ip = Dap.Config.backend_ip config |> Ipaddr_unix.of_inet_addr in
        let port = Dap.Config.backend_port config in
        let cmd = Dap.Config.backend_cmd config in
        _start_background_svc state ip port cmd
        |> Dap.Result.bind ~f:(fun () ->
            let resp =
              let command = Dap.Commands.initialize in
              (* TODO pull in from a config file? *)
              let body = D.Capabilities.make
                  ~supportsConfigurationDoneRequest:true
                  ~supportsRestartRequest:true
                  ~supportsTerminateRequest:true
                  ()
              in
              Dap.Response.default_response_opt command body
            in
            let ret = Dap.Response.initializeResponse resp in
            Dap.Result.ok ret))

  let raise_initialized =
    On_response.make ~handler:(fun ~state:_ _req ->
        let ev =
          let event = Dap.Events.initialized in
          let body = D.EmptyObject.make () in
          Dap.Event.default_event_opt event body
        in
        let ret = Dap.Event.initializedEvent ev in
        Dap.Result.ok ret)

  let handlers ~state = [
    initialize_handler ~state;
    raise_initialized ~state;
  ]

end
