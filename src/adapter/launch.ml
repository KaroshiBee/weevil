module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event
module Mich_event = Mdb.Mdb_server.MichEvent
module Js = Data_encoding.Json

(* Launching and attaching *)

(* After the debug adapter has been initialized, it is ready to accept requests for starting debugging. Two requests exist for this: *)

(*     launch request: the debug adapter launches the program (“debuggee”) in debug mode and then starts to communicate with it. Since the debug adapter is responsible for launching the debuggee, it should provide a mechanism for the end user to configure the debuggee. For example, passing arguments or specifying a working directory. *)
(*         Debug adapters are free to launch the debuggee however they choose. Typically the debuggee is launched as a child process and its output channels are connected to a client’s debug console via output events. However, this has certain limitations, such as not being able to write to the terminal device directly and not being able to accept standard input. For those cases, launching the debuggee in a terminal is preferable. A debug adapter can use the the runInTerminal request to ask the client to launch the debuggee in a terminal that is integrated into the client or in a terminal that runs outside of the client (but still configured and managed from the client). *)
(*     attach request: the debug adapter connects to an already running program. Here the end user is responsible for launching and terminating the program. *)

(* Since arguments for both requests are highly dependent on a specific debugger and debug adapter implementation, the Debug Adapter Protocol does not specify any arguments for these requests. Instead, the development tool is expected to get information about debugger specific arguments from elsewhere (e.g. contributed by some plugin or extension mechanism) and to build a UI and validation mechanism on top of that. *)
(*   *\) *)

module T (S : Types.STATE_T) = struct

  module On_request = Dap.Launch.On_request (S)
  module On_response = Dap.Launch.Raise_process (S)
  module On_launched = Dap.Launch.Raise_stopped (S)
  module Utils = State_utils.T (S)

  let _get_onDebug = function
    | Req.LaunchRequest req ->
      let args = Req.Message.arguments req in
      D.LaunchRequestArguments.noDebug args |> Option.value ~default:false
    | _ -> false

  (* connects to the mdb backend and runs a script *)
  let launch_handler =
    On_request.make ~handler:(fun ~state req ->
        let args = Req.(Message.arguments @@ extract req) in
        let script_filename = D.LaunchRequestArguments.script_filename args in
        let storage = D.LaunchRequestArguments.storage args in
        let parameter = D.LaunchRequestArguments.parameter args in
        let mdb_config = Mdb.Mdb_types.Mich_config.make ~script_filename ~storage ~parameter () in
        let dap_config = S.config state in

        let body = D.EmptyObject.make () in
        let command = Dap.Commands.launch in
        let resp =
          Res.launchResponse @@ Res.default_response_opt command body
        in
        Utils.launch state dap_config mdb_config
        |> Dap_result.map ~f:(fun _ -> resp)
      )

  let process_handler =
    On_response.make ~handler:(fun ~state:_ _ ->
        let ev =
          let event = Dap.Events.process in
          let startMethod = D.ProcessEvent_body_startMethod.Launch in
          let body =
            D.ProcessEvent_body.make
              (* * The logical name of the process. This is usually the full path to *)
              (* * process's executable file. Example: /home/example/myproj/program.js. *)
              ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
              ~startMethod
              ()
          in
          Ev.default_event_req event body
        in
        let ret = Ev.processEvent ev in
        Dap_result.ok ret)

  let launched_handler =
    On_launched.make ~handler:(fun ~state:_ _ ->
        let ev =
          let event = Dap.Events.stopped in
          let reason = D.StoppedEvent_body_reason.Entry in
          let body =
            D.StoppedEvent_body.make
              ~reason
              ~threadId:Defaults.Vals._THE_THREAD_ID
              ~preserveFocusHint:true
              ~allThreadsStopped:true
              ()
          in
          Ev.default_event_req event body
        in
        let ret = Ev.stoppedEvent ev in
        Dap_result.ok ret)

  let handlers ~state = [
    launch_handler ~state;
    process_handler ~state;
    launched_handler ~state;
  ]

end
