module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module Mich_event = Backend.Server.MichEvent
module Js = Data_encoding.Json

(* Launching and attaching *)

(* After the debug adapter has been initialized, it is ready to accept requests for starting debugging. Two requests exist for this: *)

(*     launch request: the debug adapter launches the program (“debuggee”) in debug mode and then starts to communicate with it. Since the debug adapter is responsible for launching the debuggee, it should provide a mechanism for the end user to configure the debuggee. For example, passing arguments or specifying a working directory. *)
(*         Debug adapters are free to launch the debuggee however they choose. Typically the debuggee is launched as a child process and its output channels are connected to a client’s debug console via output events. However, this has certain limitations, such as not being able to write to the terminal device directly and not being able to accept standard input. For those cases, launching the debuggee in a terminal is preferable. A debug adapter can use the the runInTerminal request to ask the client to launch the debuggee in a terminal that is integrated into the client or in a terminal that runs outside of the client (but still configured and managed from the client). *)
(*     attach request: the debug adapter connects to an already running program. Here the end user is responsible for launching and terminating the program. *)

(* Since arguments for both requests are highly dependent on a specific debugger and debug adapter implementation, the Debug Adapter Protocol does not specify any arguments for these requests. Instead, the development tool is expected to get information about debugger specific arguments from elsewhere (e.g. contributed by some plugin or extension mechanism) and to build a UI and validation mechanism on top of that. *)
(*   *\) *)

let _get_onDebug = function
  | Req.LaunchRequest req ->
    let args = Req.Message.arguments req in
    D.LaunchRequestArguments.noDebug args |> Option.value ~default:false
  | _ -> false


module T (S:Types.State_intf) = struct

  type state = S.t
  type t = S.t

  let make ?state () = Option.value state ~default:S.make_empty

  let state t = t

  let launch_handler t port ip =
    Dap.Launch.On_request.make
      ~handler:(fun config _req ->
          let resp =
            let command = Dap.Commands.launch in
            let body = D.EmptyObject.make () in
            Res.default_response_opt command body
          in
          let ret = Res.launchResponse resp in
          let%lwt () =
            match S.process_none t with
            | Some _ ->
              let () = S.set_launch_mode t `Launch in
              let%lwt () = Logs_lwt.debug (fun m -> m "trying to connect to already running backend service") in
              let%lwt (ic, oc) = S.connect ip port in
              Lwt.return @@ S.set_io t ic oc

            | None ->
              let cmd = Dap.Config.(to_command config.backend_cmd) in
              let%lwt () = Logs_lwt.debug (fun m -> m "launching backend service with cmd: '%s'" config.backend_cmd) in
              let process = Lwt_process.open_process_none cmd in
              let%lwt () = Logs_lwt.debug (fun m -> m "backend service has state: '%s'" @@
                                            match process#state with | Lwt_process.Running -> "running" | Lwt_process.Exited _ -> "exited" ) in
              let () = S.set_process_none t process in
              let () = S.set_launch_mode t `Launch in
              let%lwt () = Logs_lwt.debug (fun m -> m "trying to connect to backend service") in
              let%lwt (ic, oc) = S.connect ip port in
              Lwt.return @@ S.set_io t ic oc
          in
          Dap_result.ok ret
        )

  let process_handler t port =
    Dap.Launch.On_response.make
      ~handler:(fun config _resp ->
          let ev =
            let event = Dap.Events.process in
            let startMethod = D.ProcessEvent_body_startMethod.Launch in
            let body =
              D.ProcessEvent_body.make
                ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
                ~startMethod
                ()
            in
            Ev.default_event_req event body
          in
          let ret = Ev.processEvent ev in
          match S.oc t with
          | Some backend_oc ->
            let%lwt () = Logs_lwt.debug (fun m -> m "trying to start the debugger with cmd: '%s'" config.stepper_cmd) in
            let ev = Mich_event.make ~event:(RunScript config.stepper_cmd) () in
            (* NOTE remove all \n with wrap *)
            let ev_s = Js.(construct Mich_event.enc ev |> to_string |> Dapper.Dap_header.wrap ~add_header:false) in
            (* NOTE then write_line to make server consume *)
            let%lwt () = Lwt_io.write_line backend_oc ev_s in
            (* this event is a DAP event message *)
            Dap_result.ok ret

          | None ->
            let%lwt () = Logs_lwt.err (fun m -> m "no backend output channel found on localhost port %d" port) in
            let error = Printf.sprintf "failed to connect to backend svc on localhost port %d" port in
            Dap_result.error Res.(errorResponse @@ default_response_error error)
        )

  module M1 = Dap.MakeStringHandler (Dap.Launch.On_request)
  module M2 = Dap.MakeStringHandler (Dap.Launch.On_response)

  let handlers ~config t =
    let port = Dap.Config.backend_port config in
    let ip = Dap.Config.backend_ip config  |> Ipaddr_unix.of_inet_addr in

    let launcher =
      let x = M1.make @@ launch_handler t port ip in
      M1.handle x config
    in
    let processer =
      let x = M2.make @@ process_handler t port in
      M2.handle x config
    in
    [launcher; processer]
end

include T (State)
