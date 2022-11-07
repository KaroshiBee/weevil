module Dap = Dapper.Dap
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event


(* Launching and attaching *)

(* After the debug adapter has been initialized, it is ready to accept requests for starting debugging. Two requests exist for this: *)

(*     launch request: the debug adapter launches the program (“debuggee”) in debug mode and then starts to communicate with it. Since the debug adapter is responsible for launching the debuggee, it should provide a mechanism for the end user to configure the debuggee. For example, passing arguments or specifying a working directory. *)
(*         Debug adapters are free to launch the debuggee however they choose. Typically the debuggee is launched as a child process and its output channels are connected to a client’s debug console via output events. However, this has certain limitations, such as not being able to write to the terminal device directly and not being able to accept standard input. For those cases, launching the debuggee in a terminal is preferable. A debug adapter can use the the runInTerminal request to ask the client to launch the debuggee in a terminal that is integrated into the client or in a terminal that runs outside of the client (but still configured and managed from the client). *)
(*     attach request: the debug adapter connects to an already running program. Here the end user is responsible for launching and terminating the program. *)

(* Since arguments for both requests are highly dependent on a specific debugger and debug adapter implementation, the Debug Adapter Protocol does not specify any arguments for these requests. Instead, the development tool is expected to get information about debugger specific arguments from elsewhere (e.g. contributed by some plugin or extension mechanism) and to build a UI and validation mechanism on top of that. *)
(*   *\) *)


let get_onDebug = function
  | Req.LaunchRequest req ->
    let args = Req.Message.arguments req in
    Dap.Data.LaunchRequestArguments.noDebug args |> Option.value ~default:false
  | _ -> false

module T (S:Types.State_intf) = struct

  type state = S.t
  type t = S.t

  let make ?state () = Option.value state ~default:S.make_empty

  let state t = t

  let launch_handler _t =
    Dap.Launch.On_request.make
      ~handler:(fun _ _ -> failwith "TODO")
end
(*   match request with *)
(*   | LaunchRequest req -> *)
(*     let resp = *)
(*       let command = RequestMessage.command req in *)
(*       let body = EmptyObject.make () in *)
(*       default_response_opt command body *)
(*     in *)
(*     let ret = LaunchResponse resp in *)
(*     Dap_flow.from_response ret *)
(*   | _ -> assert false *)

(* let on_launch_response response = *)
(*   match response with *)
(*   | LaunchResponse _ -> *)
(*     let ev = *)
(*       let event = Dap_events.process in *)
(*       let startMethod = ProcessEvent_body_startMethod.Launch in *)
(*       let body = *)
(*         ProcessEvent_body.make *)
(*           ~name:"TODO PROCESS EVENT NAME e.g. test.tz" *)
(*           ~startMethod *)
(*           () *)
(*       in *)
(*       default_event_req event body *)
(*     in *)
(*     let ret = ProcessEvent ev in *)
(*     Dap_flow.from_event ret *)
(*   | _ -> assert false *)

(* let on_bad_request e _request = *)
(*   let resp = default_response_error e in *)
(*   let ret = ErrorResponse resp in *)
(*   Dap_flow.from_response ret *)

(* let connect t config req response = *)
(*   let port = Dap_config.backend_port config in *)
(*   let ip = Dap_config.backend_ip config in *)
(*   let client = `TCP (`IP ip, `Port port) in *)
(*   let%lwt ctx = init () in *)
(*   let%lwt (_, ic, oc) = *)
(*     (\* loop a fixed number of times with a sleep, to make sure to connect when up *\) *)
(*     let rec aux i = *)
(*       let%lwt () = Logs_lwt.debug (fun m -> m "[%d] trying to connect on locahost port: %d" i port) in *)
(*       let%lwt () = Lwt_unix.sleep @@ float_of_int i in *)
(*       try%lwt *)
(*         connect ~ctx client *)
(*       with *)
(*       | Unix.Unix_error(Unix.ECONNREFUSED, "connect", "") as e -> if i > 5 then raise e else aux (i+1) *)
(*     in *)
(*     aux 1 *)
(*   in *)
(*   let%lwt () = Logs_lwt.debug (fun m -> m "connected on locahost port: %d" port) in *)
(*   let () = State.set_io t ic oc in *)
(*   match State.oc t with *)
(*   | Some backend_oc -> *)
(*     let%lwt () = Logs_lwt.debug (fun m -> m "trying to start the debugger with cmd: '%s'" config.stepper_cmd) in *)
(*     let ev = Mich_event.make ~event:(RunScript config.stepper_cmd) () in *)
(*     (\* NOTE remove all \n with wrap *\) *)
(*     let ev_s = JS.(construct Mich_event.enc ev |> to_string |> Dapper.Dap_header.wrap ~add_header:false) in *)
(*     (\* NOTE then write_line to make server consume *\) *)
(*     let%lwt () = Lwt_io.write_line backend_oc ev_s in *)
(*     (\* this event is a DAP event message *\) *)
(*     let event = Option.some @@ Dap_flow.response_event response on_launch_response in *)
(*     {response; event; error=None} |> Lwt.return *)
(*   | None -> *)
(*     let%lwt () = Logs_lwt.err (fun m -> m "no backend output channel found on localhost port %d" port) in *)
(*     let error = Option.some @@ Dap_flow.raise_error req ( *)
(*         on_bad_request @@ Printf.sprintf "failed to connect to backend svc on localhost port %d" port *)
(*       ) in *)
(*     {response; event=None; error} |> Lwt.return *)

(* let handle t config req = *)
(*   let response = Dap_flow.request_response req on_launch_request in *)
(*   let _onDebug = Dap_flow.map_request req get_onDebug in *)
(*   match Dap_flow.to_result response, State.process_none t with *)
(*   | Result.Ok _, None -> *)
(*     let cmd = Dap_config.(to_command config.backend_cmd) in *)
(*     let%lwt () = Logs_lwt.debug (fun m -> m "launching backend service with cmd: '%s'" config.backend_cmd) in *)
(*     let process = Lwt_process.open_process_none cmd in *)
(*     let%lwt () = Logs_lwt.debug (fun m -> m "backend service has state: '%s'" @@ *)
(*                                   match process#state with | Lwt_process.Running -> "running" | Lwt_process.Exited _ -> "exited" ) in *)
(*     let () = State.set_process_none t process in *)
(*     let () = State.set_launch_mode t `Launch in *)
(*     let%lwt () = Logs_lwt.debug (fun m -> m "trying to connect to backend service") in *)
(*     connect t config req response *)
(*   | Result.Ok _, Some _ -> *)
(*     let () = State.set_launch_mode t `Launch in *)
(*     let%lwt () = Logs_lwt.debug (fun m -> m "trying to connect to already running backend service") in *)
(*     connect t config req response *)
(*   | Result.Error err, _ -> *)
(*     let error = Option.some @@ Dap_flow.raise_error req (on_bad_request err) in *)
(*     {response; event=None; error} |> Lwt.return *)
