module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event
module Mich_config = Mdb.Mdb_types.Mich_config

(* The disconnect request asks the debug adapter to disconnect from the debuggee (thus ending the debug session)
   and then to shut down itself (the debug adapter).

   In addition, the debug adapter must terminate the debuggee if it was started with the launch request.
   If an attach request was used to connect to the debuggee, then the debug adapter must not terminate the debuggee.

   This implicit behavior of when to terminate the debuggee can be overridden with the terminateDebuggee argument
   (which is only supported by a debug adapter if the corresponding capability supportTerminateDebuggee is true).
*)
module T (S : Types.STATE_T) = struct

  module On_request = Dap.Disconnect.On_request (S)
  module On_response = Dap.Disconnect.Raise_exited (S)
  module On_exited = Dap.Disconnect.Raise_terminate (S)

  module Utils = struct
    module U = State_utils.T (S)

    let update_state state =
      match S.launch_mode state with
      | Some `Launch -> (* terminate debugee *)
        U.terminate state
      | Some `Attach -> (* just detach from backend *)
          S.reset_backend state;
          Lwt.return_unit
      | Some `AttachForSuspendedLaunch | None -> Lwt.return_unit

  end


  let disconnect_handler =
    On_request.make ~handler:(fun ~state req ->
        let args = Req.(Message.arguments @@ extract req) in
        let restart = Option.bind args (fun args -> D.DisconnectArguments.restart args) in
        let () = S.set_should_restart_on_terminate state restart in
        let%lwt () = Utils.update_state state in

        let body = D.EmptyObject.make () in
        let command = Dap.Commands.disconnect in
        let resp =
          Res.disconnectResponse @@ Res.default_response_opt command body
        in
        Dap_result.ok resp
      )


  let handlers ~state = [
    disconnect_handler ~state;
  ]

end
