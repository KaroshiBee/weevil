module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event
module Mich_config = Mdb.Mdb_config

(* The disconnect request asks the debug adapter to disconnect from the debuggee (thus ending the debug session)
   and then to shut down itself (the debug adapter).

   In addition, the debug adapter must terminate the debuggee if it was started with the launch request.
   If an attach request was used to connect to the debuggee, then the debug adapter must not terminate the debuggee.

   This implicit behavior of when to terminate the debuggee can be overridden with the terminateDebuggee argument
   (which is only supported by a debug adapter if the corresponding capability supportTerminateDebuggee is true).
*)
module T (S : Types.STATE_T) = struct

  module On_request = Dap.Disconnect.On_request (S)
  module On_response = Dap.Disconnect.Raise_terminate (S)
  module On_terminated = Dap.Disconnect.Raise_exited (S)

  module Utils = struct
    module U = State_utils.T (S)

    let update_state state =
      match S.launch_mode state with
      | Some `Launch -> (* terminate debugee *)
        U.terminate state
      | Some `Attach | Some `AttachForSuspendedLaunch | None -> Lwt.return_unit

  end

  let exit_code state =
    match (S.backend_svc state |> Option.map (fun p -> p#state)) with
    | Some Lwt_process.Exited Unix.WEXITED i ->
      let () = Logs.debug (fun m -> m "backend process closed normally %d" i) in
      Result.Ok i
    | Some Lwt_process.Exited Unix.WSIGNALED i ->
      let () = Logs.debug (fun m -> m "backend process killed by signal %d" i) in
      Result.Ok i
    | Some Lwt_process.Exited Unix.WSTOPPED i ->
      let () = Logs.debug (fun m -> m "backend process stopped by signal %d" i) in
      Result.Ok i
    | None -> Result.Ok 0
    | _ -> Result.error "backend process not terminated"


  let disconnect_handler =
    On_request.make ~handler:(fun ~state req ->
        let args = Req.(Message.arguments @@ extract req) in
        let restart = Option.bind args (fun args -> D.DisconnectArguments.restart args) in
        let () = S.set_should_restart_on_terminate state restart in
        let%lwt () = Utils.update_state state in
        match exit_code state with
        | Result.Error err ->
          Dap_result.from_error_string err
        | _ ->
          let body = D.EmptyObject.make () in
          let command = Dap.Commands.disconnect in
          let resp =
            Res.disconnectResponse @@ Res.default_response_opt command body
          in
          Dap_result.ok resp
      )

  let terminated_event =
    On_response.make ~handler:(fun ~state _ ->
        match exit_code state with
        | Result.Ok _ ->
          let body = D.TerminatedEvent_body.make () in
          let ev = Dap.Events.terminated in
          let event = Ev.terminatedEvent @@ Ev.default_event_opt ev body in
          Dap_result.ok event
        | Result.Error err ->
          Dap_result.from_error_string err
      )

  let exited_event =
    On_terminated.make ~handler:(fun ~state _ ->
        match exit_code state with
        | Result.Ok exitCode ->
          let body = D.ExitedEvent_body.make ~exitCode () in
          let ev = Dap.Events.exited in
          let event = Ev.exitedEvent @@ Ev.default_event_req ev body in
          Dap_result.ok event
        | Result.Error err ->
          Dap_result.from_error_string err
      )

  let handlers ~state = [
    disconnect_handler ~state;
    terminated_event ~state;
    exited_event ~state;
  ]

  let _cleanup state =
    let () = Logs.debug (fun m -> m "reset backend state") in
    S.reset_backend state

  let on_success ~state = _cleanup state
  let on_error ~state = _cleanup state

end
