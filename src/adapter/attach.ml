module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.State_intf) = struct

  module On_request = Dap.Attach.On_request (S)
  module On_response = Dap.Attach.Raise_process (S)

  let attach_handler =
    On_request.make ~handler:(fun ~state _req ->
        let config = S.config state in
        let body = D.EmptyObject.make () in
        let command = Dap.Commands.attach in
        let resp =
          Res.attachResponse @@ Res.default_response_opt command body
        in
        let () = S.set_launch_mode state `Attach in
        match S.backend_oc state with
        | Some _ ->
          Dap_result.ok resp
        | None -> (
            (* NOTE dont need to start the backend as we are in attach mode, just connect to the backend *)
            let ip = Config.backend_ip config |> Ipaddr_unix.of_inet_addr in
            let port = Config.backend_port config in
            S.set_connect_backend state ip port
            |> Dap_result.or_log_error
            |> Dap_result.map ~f:(fun _ -> resp)
          ))

  let process_handler =
    On_response.make ~handler:(fun ~state:_ _resp ->
        let event = Dap.Events.process in
        let startMethod = D.ProcessEvent_body_startMethod.Attach in
        let body =
          D.ProcessEvent_body.make
            ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
            ~startMethod
            ()
        in
        Ev.default_event_req event body |> Ev.processEvent |> Dap_result.ok)

  let handlers ~state = [
    attach_handler ~state;
    process_handler ~state;
  ]

end
