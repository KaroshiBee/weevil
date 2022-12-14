module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event
module Mich_event = Mdb.Mdb_server.MichEvent
module Mich_config = Mdb.Mdb_types.Mich_config
module Js = Data_encoding.Json


module T (S : Types.STATE_T) = struct

  module On_request = Dap.Terminate.On_request (S)
  module On_response = Dap.Terminate.Raise_terminated (S)

  module Utils = struct
    let update_state state restart =
      let () = S.set_should_restart_on_terminate state restart in
      (* terminate the backend process *)
      match S.backend_svc state with
      | None ->
        Logs_lwt.warn (fun m -> m "process already terminated")
      | Some p ->
        (* using close because it cleans up the io channels too *)
        let%lwt () = Logs_lwt.debug (fun m -> m "closing backend process") in
        let%lwt _status = p#close in
        let%lwt () = Logs_lwt.debug (fun m -> m "reset backend state") in
        let () = S.reset_backend state in
        Lwt.return_unit

  end


  let terminate_handler =
    On_request.make ~handler:(fun ~state req ->
        let args = Req.(Message.arguments @@ extract req) in
        let restart = Option.bind args (fun args -> D.TerminateArguments.restart args) in
        let%lwt () = Utils.update_state state restart in

        let body = D.EmptyObject.make () in
        let command = Dap.Commands.terminate in
        let resp =
          Res.terminateResponse @@ Res.default_response_opt command body
        in
        Dap_result.ok resp
      )

  let terminated_handler =
    On_response.make ~handler:(fun ~state _ ->
        (* make json of mdb_config *)
        let restart =
          match S.should_restart_on_terminate state, S.mdb_config state  with
          | Some true, Some mdb_config ->
            Option.some @@ Data_encoding.Json.construct Mich_config.enc mdb_config
          | Some true, None ->
            let () = Logs.warn (fun m -> m "Restart requested but no config data found, ignoring") in
            None
          | Some false, _ | None, _ ->
            None
        in
        let () = Logs.debug (fun m -> m "Got restart data: %s" @@ Data_encoding.Json.to_string (Option.value restart ~default:`Null)) in
        let ev =
          let event = Dap.Events.terminated in
          let body =
            D.TerminatedEvent_body.make ?restart ()
          in
          Ev.default_event_opt event body
        in
        let ret = Ev.terminatedEvent ev in
        (* reset the restart field to None *)
        let () = S.set_should_restart_on_terminate state None in
        Dap_result.ok ret
      )

  let handlers ~state = [
    terminate_handler ~state;
    terminated_handler ~state;
  ]

end
