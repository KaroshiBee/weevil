module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event
module Mich_config = Mdb.Mdb_types.Mich_config


module T (S : Types.STATE_T) = struct

  module On_request = Dap.Terminate.On_request (S)
  module On_response = Dap.Terminate.Raise_terminated (S)
  module Utils = State_utils.T (S)

  let terminate_handler =
    On_request.make ~handler:(fun ~state req ->
        let%lwt () = Utils.terminate state in
        let args = Req.(Message.arguments @@ extract req) in
        let restart = Option.bind args (fun args -> D.TerminateArguments.restart args) in
        let () = S.set_should_restart_on_terminate state restart in

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
        Dap_result.ok ret
      )

  let handlers ~state = [
    terminate_handler ~state;
    terminated_handler ~state;
  ]


  let clean_up ~state =
    (* reset the restart field to None *)
    S.set_should_restart_on_terminate state None

  let on_success = clean_up

  let on_error = clean_up


end
