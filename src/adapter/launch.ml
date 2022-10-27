open Dapper.Dap_handler_t
open Dapper.Dap_message
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
module Dap_flow = Dapper.Dap_flow
module Dap_config = Dapper.Dap_config

(* Launching and attaching *)

(* After the debug adapter has been initialized, it is ready to accept requests for starting debugging. Two requests exist for this: *)

(*     launch request: the debug adapter launches the program (“debuggee”) in debug mode and then starts to communicate with it. Since the debug adapter is responsible for launching the debuggee, it should provide a mechanism for the end user to configure the debuggee. For example, passing arguments or specifying a working directory. *)
(*         Debug adapters are free to launch the debuggee however they choose. Typically the debuggee is launched as a child process and its output channels are connected to a client’s debug console via output events. However, this has certain limitations, such as not being able to write to the terminal device directly and not being able to accept standard input. For those cases, launching the debuggee in a terminal is preferable. A debug adapter can use the the runInTerminal request to ask the client to launch the debuggee in a terminal that is integrated into the client or in a terminal that runs outside of the client (but still configured and managed from the client). *)
(*     attach request: the debug adapter connects to an already running program. Here the end user is responsible for launching and terminating the program. *)

(* Since arguments for both requests are highly dependent on a specific debugger and debug adapter implementation, the Debug Adapter Protocol does not specify any arguments for these requests. Instead, the development tool is expected to get information about debugger specific arguments from elsewhere (e.g. contributed by some plugin or extension mechanism) and to build a UI and validation mechanism on top of that. *)
(*   *\) *)

module Request = struct
  type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
  type command = Dap_commands.launch
  let command = Dap_commands.launch
  type args = LaunchRequestArguments.t
  type presence = RequestMessage.req
  let enc = RequestMessage.enc command LaunchRequestArguments.enc
  let ctor = fun req -> LaunchRequest req
  let extract = Dap_flow.to_request
end

module Response = struct
  type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
  type command = Dap_commands.launch
  let command = Dap_commands.launch
  type body = EmptyObject.t option
  type presence = ResponseMessage.opt
  let enc = ResponseMessage.enc_opt command EmptyObject.enc
  let ctor = fun resp -> LaunchResponse resp
  let extract = Dap_flow.to_response
end

module Event = struct
  type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
  type event = Dap_events.process
  let event = Dap_events.process
  type body = ProcessEvent_body.t
  type presence = EventMessage.req
  let enc = EventMessage.enc event ProcessEvent_body.enc
  let ctor = fun ev -> ProcessEvent ev
  let extract = Dap_flow.to_event
end


include MakeReqRespIncludes_withEvent (Request) (Response) (Event)

let get_onDebug = function
  | LaunchRequest req ->
    let args = RequestMessage.arguments req in
    LaunchRequestArguments.noDebug args |> Option.value ~default:false
  | _ -> false

let on_launch_request Dap_config.{launch_mode; _} = function
  | LaunchRequest req when launch_mode = `Launch ->
    let args = RequestMessage.arguments req in
    let _ = LaunchRequestArguments.noDebug args in
    let resp =
      let command = RequestMessage.command req in
      let body = EmptyObject.make () in
      default_response_opt command body
    in
    let ret = LaunchResponse resp in
    Dap_flow.from_response ret
  | LaunchRequest _req when launch_mode = `Attach ->
    let err = "wrong launch mode - config is set to Attach but got a Launch request message" in
    Logs.err (fun m -> m "%s" err) ;
    Dap_flow.from_result @@ Result.error err
  | _ -> assert false

let on_launch_response Dap_config.{launch_mode; _} = function
  | LaunchResponse _ when launch_mode = `Launch ->
    let ev =
      let event = Dap_events.process in
      let startMethod = ProcessEvent_body_startMethod.Launch in
      let body =
        ProcessEvent_body.make
          ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
          ~startMethod
          ()
      in
      default_event_req event body
    in
    let ret = ProcessEvent ev in
    Dap_flow.from_event ret
  | _ -> assert false

let on_bad_request e _request =
  let resp = default_response_error e in
  let ret = ErrorResponse resp in
  Dap_flow.from_response ret

let handle t config req =
  let open Dap_flow in
  let response = request_response req (on_launch_request config) in
  let _onDebug = Dap_flow.to_result req |> Result.map get_onDebug in
  match to_result response, oc t with
  | Result.Ok _, Some backend_oc ->
    let%lwt _ = Lwt_io.write backend_oc config.backend_cmd in
    let event = Option.some @@ response_event response (on_launch_response config) in
    {response; event; error=None} |> Lwt.return
  | Result.Ok _, None ->
    (* TODO launch backend server *)
    (* let%lwt _ = Lwt_process.with_process_full ("", [|""|]) t.subprocess_start in *)
    let event = Option.some @@ response_event response (on_launch_response config) in
    {response; event; error=None} |> Lwt.return
  | Result.Error err, _ ->
    let error = Option.some @@ raise_error req (on_bad_request err) in
    {response; event=None; error} |> Lwt.return
