open Conduit_lwt_unix
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

let on_launch_request request =
  match request with
  | LaunchRequest req ->
    let resp =
      let command = RequestMessage.command req in
      let body = EmptyObject.make () in
      default_response_opt command body
    in
    let ret = LaunchResponse resp in
    Dap_flow.from_response ret
  | _ -> assert false

let on_launch_response response =
  match response with
  | LaunchResponse _ ->
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

let connect t config req response =
  let port = Dap_config.backend_port config in
  let ip = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr in
  let client = `TCP (`IP ip, `Port port) in
  let%lwt ctx = init () in
  let%lwt (_, ic, oc) = connect ~ctx client in
  let () = Backend.set_io t ic oc in
  match Backend.oc t with
  | Some backend_oc ->
    let%lwt () = Lwt_io.write backend_oc config.stepper_cmd in
    let event = Option.some @@ Dap_flow.response_event response on_launch_response in
    {response; event; error=None} |> Lwt.return
  | None ->
    let error = Option.some @@ Dap_flow.raise_error req (
        on_bad_request @@ Printf.sprintf "failed to connect to backend svc on localhost port %d" port
      ) in
    {response; event=None; error} |> Lwt.return

let handle t config req =
  let response = Dap_flow.request_response req on_launch_request in
  let _onDebug = Dap_flow.map_request req get_onDebug in
  match Dap_flow.to_result response, Backend.process_full t with
  | Result.Ok _, None ->
    let cmd = Dap_config.(to_command config.backend_cmd) in
    let process = Lwt_process.open_process_full cmd in
    let () = Backend.set_process_full t process in
    let () = Backend.set_launch_mode t `Launch in
    connect t config req response
  | Result.Ok _, Some _ ->
    let () = Backend.set_launch_mode t `Launch in
    connect t config req response
  | Result.Error err, _ ->
    let error = Option.some @@ Dap_flow.raise_error req (on_bad_request err) in
    {response; event=None; error} |> Lwt.return
