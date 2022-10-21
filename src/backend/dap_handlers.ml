open Dapper.Dap_message
module Js = Data_encoding.Json
module Q = Json_query

module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
module Dap_flow = Dapper.Dap_flow

module JsMsg : sig
  type t = Js.json
  exception Wrong_encoder of string
  val from_string : string -> (t, string) Result.t
  val to_string : t -> string
  val construct : 'a Data_encoding.t -> 'a -> t
  val destruct : 'a Data_encoding.t -> t -> 'a
end = struct

  let _HEADER_FIELD = "Content-Length: "
  let _HEADER_TOKEN = "\r\n\r\n"

  type t = Js.json

  let _replace input output =
    Str.global_replace (Str.regexp_string input) output

  let from_string = Js.from_string

  let to_string t =
    let s = t
            |> Js.to_string
            |> _replace "\n" ""
    in
    let n = String.length s in
    Printf.sprintf "%s%d%s%s" _HEADER_FIELD n _HEADER_TOKEN s

  exception Wrong_encoder of string

  let construct enc i =
    try
      Js.construct enc i
    with _ as e ->
      let s = Printf.sprintf "cannnot construct: %s" (Printexc.to_string e) in
      raise @@ Wrong_encoder s

  let destruct enc i =
    try
      Js.destruct enc i
    with
    | Js.Cannot_destruct (pth, Js.Unexpected(typestr, value)) ->
      let s = Printf.sprintf "cannot destruct @ %s, expected '%s':%s"
          (Q.json_pointer_of_path pth) value typestr in
      raise @@ Wrong_encoder s

    | Data_encoding__.Binary_error_types.Invariant_guard err ->
      let s = Printf.sprintf "cannnot destruct: %s" err in
      raise @@ Wrong_encoder s

    | _ as e ->
      let s = Printf.sprintf "cannnot destruct: %s" (Printexc.to_string e) in
      raise @@ Wrong_encoder s

end

type launch_mode = [`Launch | `Attach | `AttachForSuspendedLaunch]

type config = {launch_mode : launch_mode}

let default_response_req
  = fun command body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  let request_seq = -1 in
  let success = true in
  ResponseMessage.make
    ~seq
    ~request_seq
    ~success
    ~command
    ~body
    ()

let default_response_opt
  = fun command body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  let request_seq = -1 in
  let success = true in
  ResponseMessage.make_opt
    ~seq
    ~request_seq
    ~success
    ~command
    ~body
    ()

let default_event_req
  = fun event body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  EventMessage.make
    ~seq
    ~event
    ~body
    ()

let default_event_opt
  = fun event body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  EventMessage.make_opt
    ~seq
    ~event
    ~body
    ()


(*
Previous handler func:
  client:Lwt_io.output Lwt_io.channel ->
  msg:string ->
  backend:Lwt_io.output Lwt_io.channel ->
  unit Lwt.t
*)
module type HANDLER = sig

  type input
  type output

  type backend_channel

  val from_string : string -> input
  (* NOTE when handling a request we will be interacting with the backend, hence the Lwt.t  *)
  val handle : config:config -> input -> output Lwt.t
  val to_string : output -> (string, string) Result.t Lwt.t

end


module type REQ_T = sig
  type ('command, 'args, 'presence) t
  type command
  val command : command Dap_commands.t
  type args
  type presence
  val enc : (command, args, presence) t Data_encoding.t
  val ctor : (command, args, presence) t -> (command, args, presence) request
  val extract : (command, args, presence) request -> (command, args, presence) t
end

module type RESP_T = sig
  type ('command, 'body, 'presence) t
  type command
  val command : command Dap_commands.t
  type body
  type presence
  val enc : (command, body, presence) t Data_encoding.t
  val ctor : (command, body, presence) t -> (command, body, presence) response
  val extract : (command, body, presence) response -> (command, body, presence) t
end

module type EV_T = sig
  type ('event, 'body, 'presence) t
  type event
  val event : event Dap_events.t
  type body
  type presence
  val enc : (event, body, presence) t Data_encoding.t
  val ctor : (event, body, presence) t -> (event, body, presence) Dapper.Dap_message.event
  val extract : (event, body, presence) Dapper.Dap_message.event -> (event, body, presence) t
end

module MakeReqRespIncludes
    (REQ:REQ_T)
    (RESP:RESP_T with type command = REQ.command)
    = struct

  type req = (REQ.command, REQ.args, REQ.presence) request
  type input = req Dap_flow.t

  type resp = (RESP.command, RESP.body, RESP.presence) response

  type output = {
    response: resp Dap_flow.t;
  }

  type backend_channel = Lwt_io.output_channel

  let from_string =
    fun input ->
      JsMsg.from_string input
      |> Result.map (JsMsg.destruct REQ.enc)
      |> Result.map (fun x -> REQ.ctor x)
      |> Dap_flow.from_result

  let to_string =
    fun {response; } ->
      match Dap_flow.(to_result response) with
      | Result.Ok response ->
        let resp = RESP.extract response |> JsMsg.construct RESP.enc |> JsMsg.to_string in
        Result.ok resp
        |> Lwt.return

      | Result.Error _ ->
        failwith "TODO - response errored, need to make an error response str from the initial request seq#"

end


module MakeReqRespIncludes_withEvent
    (REQ:REQ_T)
    (RESP:RESP_T with type command = REQ.command)
    (EV:EV_T)
    = struct

  type req = (REQ.command, REQ.args, REQ.presence) request
  type input = req Dap_flow.t

  type resp = (RESP.command, RESP.body, RESP.presence) response
  type ev = (EV.event, EV.body, EV.presence) event

  type output = {
    response: resp Dap_flow.t;
    event: ev Dap_flow.t option;
  }

  type backend_channel = Lwt_io.output_channel

  let from_string =
    fun input ->
      JsMsg.from_string input
      |> Result.map (JsMsg.destruct REQ.enc)
      |> Result.map (fun x -> REQ.ctor x)
      |> Dap_flow.from_result

  let to_string = function
    | {response; event=Some event} -> (
      match Dap_flow.(to_result response, to_result event) with
      | Result.Ok response, Result.Ok event ->
        let resp = RESP.extract response |> JsMsg.construct RESP.enc |> JsMsg.to_string in
        let ev = EV.extract event |> JsMsg.construct EV.enc |> JsMsg.to_string in
        Result.ok @@ resp^ev
        |> Lwt.return

      | Result.Error _, Result.Ok (InitializedEvent _) ->
        failwith "TODO - response errored, need to make an error response str from the initial request seq#"
      | Result.Ok (InitializeResponse _resp), Result.Error _ ->
        failwith "TODO - response ok, but couldn't raise the event??"
      | Result.Error _, Result.Error _ ->
        failwith "TODO - nothing worked"
      | _, _ -> assert false
    )

    | {response; event=None} -> (
      match Dap_flow.(to_result response) with
      | Result.Ok response ->
        let resp = RESP.extract response |> JsMsg.construct RESP.enc |> JsMsg.to_string in
        Result.ok @@ resp
        |> Lwt.return

      | Result.Error _ ->
        failwith "TODO - response errored, need to make an error response str from the initial request seq#"
    )

end


module Cancel : HANDLER = struct
  include MakeReqRespIncludes
      (struct
        type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
        type command = Dap_commands.cancel
        let command = Dap_commands.cancel
        type args = CancelArguments.t option
        type presence = RequestMessage.opt
        let enc = RequestMessage.enc_opt command CancelArguments.enc
        let ctor = fun req -> CancelRequest req
        let extract = Dap_flow.to_request
      end)
      (struct
        type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
        type command = Dap_commands.cancel
        let command = Dap_commands.cancel
        type body = EmptyObject.t option
        type presence = ResponseMessage.opt
        let enc = ResponseMessage.enc_opt command EmptyObject.enc
        let ctor = fun resp -> CancelResponse resp
        let extract = Dap_flow.to_response
      end)

  let on_cancel_request = function
    | CancelRequest req ->
      let resp =
        let command = RequestMessage.command req in
        let body = EmptyObject.make () in
        default_response_opt command body
      in
      let ret = CancelResponse resp in
      Dap_flow.from_response ret
    | _ -> assert false

  let handle ~config:_ req =
    let response = Dap_flow.on_request req on_cancel_request in
    Lwt.return {response;}

end

  (*
Initialization

The Debug Adapter Protocol defines many features and this number is still growing, albeit slowly. However, the protocol is still at its first version because it was an explicit design goal to support new feature in a completely backward compatible way. Making this possible without version numbers requires that every new feature gets a corresponding flag that lets a development tool know whether a debug adapter supports the feature or not. The absence of the flag always means that the feature is not supported.

A single feature and its corresponding flag is called a “capability” in the Debug Adapter Protocol. The open-ended set of all features flags is called DAP’s “capabilities.”

When starting a debug session, the development tool sends an initialize request to the adapter in order to exchange capabilities between the development tool and the debug adapter.

The development tool capabilities are provided in the InitializeRequestArguments structure of the initialize request and typically start with the prefix supports. Other pieces of information passed from the tool to the debug adapter are:

    the name of the development tool,
    the format of file paths, native or uri,
    whether line and column values are 0 or 1 based,
    the locale used by the development tool. A debug adapter is expected to return error messages that honor this locale.

The debug adapter returns the supported capabilities in the InitializeResponse via the Capabilities type. It is not necessary to return an explicit false for unsupported capabilities.
 *)
module Initialize : HANDLER = struct
  include MakeReqRespIncludes_withEvent
      (struct
        type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
        type command = Dap_commands.initialize
        let command = Dap_commands.initialize
        type args = InitializeRequestArguments.t
        type presence = RequestMessage.req
        let enc = RequestMessage.enc command InitializeRequestArguments.enc
        let ctor = fun req -> InitializeRequest req
        let extract = Dap_flow.to_request
      end)
      (struct
        type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
        type command = Dap_commands.initialize
        let command = Dap_commands.initialize
        type body = Capabilities.t option
        type presence = ResponseMessage.opt
        let enc = ResponseMessage.enc_opt command Capabilities.enc
        let ctor = fun resp -> InitializeResponse resp
        let extract = Dap_flow.to_response
      end)
      (struct
        type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
        type event = Dap_events.initialized
        let event = Dap_events.initialized
        type body = EmptyObject.t option
        type presence = EventMessage.opt
        let enc = EventMessage.enc_opt event EmptyObject.enc
        let ctor = fun ev -> InitializedEvent ev
        let extract = Dap_flow.to_event
      end)

  let on_initialize_request = function
      | InitializeRequest req ->
        let resp =
          let command = RequestMessage.command req in
          (* TODO hardcode capabilities or pull in from config *)
          let body = Capabilities.make () in
          default_response_opt command body
        in
        let ret = InitializeResponse resp in
        Dap_flow.from_response ret
      | _ -> assert false

  let on_initialization_response = function
      | InitializeResponse _ ->
        let ev =
          let event = Dap_events.initialized in
          let body = EmptyObject.make () in
          default_event_opt event body
        in
        let ret = InitializedEvent ev in
        Dap_flow.from_event ret
      | _ -> assert false

  let handle ~config:_ req =
    let open Dap_flow in
    let response = on_request req on_initialize_request in
    let event = Option.some @@ on_response response on_initialization_response in
    Lwt.return {response; event}

end

module Configuration : HANDLER = struct
  include MakeReqRespIncludes
      (struct
        type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
        type command = Dap_commands.configurationDone
        let command = Dap_commands.configurationDone
        type args = ConfigurationDoneArguments.t option
        type presence = RequestMessage.opt
        let enc = RequestMessage.enc_opt command ConfigurationDoneArguments.enc
        let ctor = fun req -> ConfigurationDoneRequest req
        let extract = Dap_flow.to_request
      end)
      (struct
        type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
        type command = Dap_commands.configurationDone
        let command = Dap_commands.configurationDone
        type body = EmptyObject.t option
        type presence = ResponseMessage.opt
        let enc = ResponseMessage.enc_opt command EmptyObject.enc
        let ctor = fun resp -> ConfigurationDoneResponse resp
        let extract = Dap_flow.to_response
      end)


  let on_configurationDone_request = function
      | ConfigurationDoneRequest req ->
        let resp =
          let command = RequestMessage.command req in
          let body = EmptyObject.make () in
          default_response_opt command body
        in
        let ret = ConfigurationDoneResponse resp in
        Dap_flow.from_response ret
      | _ -> assert false


  let handle ~config:_ req =
    let response = Dap_flow.on_request req on_configurationDone_request in
    Lwt.return {response}

end

(* Launching and attaching *)

(* After the debug adapter has been initialized, it is ready to accept requests for starting debugging. Two requests exist for this: *)

(*     launch request: the debug adapter launches the program (“debuggee”) in debug mode and then starts to communicate with it. Since the debug adapter is responsible for launching the debuggee, it should provide a mechanism for the end user to configure the debuggee. For example, passing arguments or specifying a working directory. *)
(*         Debug adapters are free to launch the debuggee however they choose. Typically the debuggee is launched as a child process and its output channels are connected to a client’s debug console via output events. However, this has certain limitations, such as not being able to write to the terminal device directly and not being able to accept standard input. For those cases, launching the debuggee in a terminal is preferable. A debug adapter can use the the runInTerminal request to ask the client to launch the debuggee in a terminal that is integrated into the client or in a terminal that runs outside of the client (but still configured and managed from the client). *)
(*     attach request: the debug adapter connects to an already running program. Here the end user is responsible for launching and terminating the program. *)

(* Since arguments for both requests are highly dependent on a specific debugger and debug adapter implementation, the Debug Adapter Protocol does not specify any arguments for these requests. Instead, the development tool is expected to get information about debugger specific arguments from elsewhere (e.g. contributed by some plugin or extension mechanism) and to build a UI and validation mechanism on top of that. *)
(*   *\) *)

module Launch : HANDLER = struct
  include MakeReqRespIncludes_withEvent
      (struct
        type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
        type command = Dap_commands.launch
        let command = Dap_commands.launch
        type args = LaunchRequestArguments.t
        type presence = RequestMessage.req
        let enc = RequestMessage.enc command LaunchRequestArguments.enc
        let ctor = fun req -> LaunchRequest req
        let extract = Dap_flow.to_request
      end)
      (struct
        type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
        type command = Dap_commands.launch
        let command = Dap_commands.launch
        type body = EmptyObject.t option
        type presence = ResponseMessage.opt
        let enc = ResponseMessage.enc_opt command EmptyObject.enc
        let ctor = fun resp -> LaunchResponse resp
        let extract = Dap_flow.to_response
      end)
      (struct
        type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
        type event = Dap_events.process
        let event = Dap_events.process
        type body = ProcessEvent_body.t
        type presence = EventMessage.req
        let enc = EventMessage.enc event ProcessEvent_body.enc
        let ctor = fun ev -> ProcessEvent ev
        let extract = Dap_flow.to_event
      end)

  let on_launch_request ~config = function
  | LaunchRequest req when config.launch_mode = `Launch ->
      let resp =
        let command = RequestMessage.command req in
        let body = EmptyObject.make () in
        default_response_opt command body
      in
      let ret = LaunchResponse resp in
      Dap_flow.from_response ret
  | LaunchRequest _req when config.launch_mode = `Attach ->
    let err = "wrong launch mode - config is set to Attach but got a Launch request message" in
    Logs.err (fun m -> m "%s" err) ;
    Dap_flow.from_result @@ Result.error err
  | _ -> assert false

  let on_launch_response ~config = function
  | LaunchResponse _ when config.launch_mode = `Launch ->
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

  let handle ~config req =
    let open Dap_flow in
    let response = on_request req (on_launch_request ~config) in
    let event = Option.some @@ on_response response (on_launch_response ~config) in
    Lwt.return {response; event}

end


module Attach : HANDLER = struct
  include MakeReqRespIncludes_withEvent
      (struct
        type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
        type command = Dap_commands.attach
        let command = Dap_commands.attach
        type args = AttachRequestArguments.t
        type presence = RequestMessage.req
        let enc = RequestMessage.enc command AttachRequestArguments.enc
        let ctor = fun req -> AttachRequest req
        let extract = Dap_flow.to_request
      end)
      (struct
        type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
        type command = Dap_commands.attach
        let command = Dap_commands.attach
        type body = EmptyObject.t option
        type presence = ResponseMessage.opt
        let enc = ResponseMessage.enc_opt command EmptyObject.enc
        let ctor = fun resp -> AttachResponse resp
        let extract = Dap_flow.to_response
      end)
      (struct
        type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
        type event = Dap_events.process
        let event = Dap_events.process
        type body = ProcessEvent_body.t
        type presence = EventMessage.req
        let enc = EventMessage.enc event ProcessEvent_body.enc
        let ctor = fun ev -> ProcessEvent ev
        let extract = Dap_flow.to_event
      end)

  let on_attach_request ~config = function
  | AttachRequest req when config.launch_mode = `Attach ->
      let resp =
        let command = RequestMessage.command req in
        let body = EmptyObject.make () in
        default_response_opt command body
      in
      let ret = AttachResponse resp in
      Dap_flow.from_response ret
  | AttachRequest _req when config.launch_mode = `Launch ->
    let err = "wrong attach mode - config is set to Launch but got an Attach request message" in
    Logs.err (fun m -> m "%s" err) ;
    Dap_flow.from_result @@ Result.error err
  | _ -> assert false

  let on_attach_response ~config = function
  | AttachResponse _ when config.launch_mode = `Attach ->
      let ev =
        let event = Dap_events.process in
        let startMethod = ProcessEvent_body_startMethod.Attach in
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

  let handle ~config req =
    let open Dap_flow in
    let response = on_request req (on_attach_request ~config) in
    let event = Option.some @@ on_response response (on_attach_response ~config) in
    Lwt.return {response; event}

end

module Restart : HANDLER = struct
  include MakeReqRespIncludes
      (struct
        type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
        type command = Dap_commands.restart
        let command = Dap_commands.restart
        type args = RestartArguments.t option
        type presence = RequestMessage.opt
        let enc = RequestMessage.enc_opt command RestartArguments.enc
        let ctor = fun req -> RestartRequest req
        let extract = Dap_flow.to_request
      end)
      (struct
        type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
        type command = Dap_commands.restart
        let command = Dap_commands.restart
        type body = EmptyObject.t option
        type presence = ResponseMessage.opt
        let enc = ResponseMessage.enc_opt command EmptyObject.enc
        let ctor = fun resp -> RestartResponse resp
        let extract = Dap_flow.to_response
      end)

  let on_restart_request = function
      | RestartRequest req ->
        let resp =
          let command = RequestMessage.command req in
          let body = EmptyObject.make () in
          default_response_opt command body
        in
        let ret = RestartResponse resp in
        Dap_flow.from_response ret
      | _ -> assert false


  let handle ~config:_ req =
    let response = Dap_flow.on_request req on_restart_request in
    Lwt.return {response}

end

(* When the development tool ends a debug session, the sequence of events is slightly different based on whether the session has been initially “launched” or “attached”: *)

(*     Debuggee launched: if a debug adapter supports the terminate request, the development tool uses it to terminate the debuggee gracefully,
       i.e. it gives the debuggee a chance to cleanup everything before terminating.

       If the debuggee does not terminate but continues to run (or hits a breakpoint), the debug session will continue,
       but if the development tool tries again to terminate the debuggee, it will then use the disconnect request to end the debug session unconditionally.

       The disconnect request is expected to terminate the debuggee (and any child processes) forcefully. *)

(*     Debuggee attached: If the debuggee has been “attached” initially, the development tool issues a disconnect request.
       This should detach the debugger from the debuggee but will allow it to continue. *)

(* In all situations where a debug adapter wants to end the debug session, a terminated event must be fired. *)

(* If the debuggee has ended (and the debug adapter is able to detect this), an optional exited event can be issued to return the exit code to the development tool.Debug session end *)



(* The disconnect request asks the debug adapter to disconnect from the debuggee (thus ending the debug session) and then to shut down itself (the debug adapter). *)

(* In addition, the debug adapter must terminate the debuggee if it was started with the launch request. If an attach request was used to connect to the debuggee, then the debug adapter must not terminate the debuggee. *)

(* This implicit behavior of when to terminate the debuggee can be overridden with the terminateDebuggee argument (which is only supported by a debug adapter if the corresponding capability supportTerminateDebuggee is true). *)

module Disconnect : HANDLER = struct
  include MakeReqRespIncludes_withEvent
      (struct
        type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
        type command = Dap_commands.disconnect
        let command = Dap_commands.disconnect
        type args = DisconnectArguments.t option
        type presence = RequestMessage.opt
        let enc = RequestMessage.enc_opt command DisconnectArguments.enc
        let ctor = fun req -> DisconnectRequest req
        let extract = Dap_flow.to_request
      end)
      (struct
        type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
        type command = Dap_commands.disconnect
        let command = Dap_commands.disconnect
        type body = EmptyObject.t option
        type presence = ResponseMessage.opt
        let enc = ResponseMessage.enc_opt command EmptyObject.enc
        let ctor = fun resp -> DisconnectResponse resp
        let extract = Dap_flow.to_response
      end)
      (struct
        type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
        type event = Dap_events.exited
        let event = Dap_events.exited
        type body = ExitedEvent_body.t
        type presence = EventMessage.req
        let enc = EventMessage.enc event ExitedEvent_body.enc
        let ctor = fun ev -> ExitedEvent ev
        let extract = Dap_flow.to_event
      end)


  let on_disconnect_request = function
    | DisconnectRequest req ->
      let resp =
        let command = RequestMessage.command req in
        let body = EmptyObject.make () in
        default_response_opt command body
      in
      let ret = DisconnectResponse resp in
      Dap_flow.from_response ret
    | _ -> assert false

  (* If the debuggee has ended (and the debug adapter is able to detect this), *)
  (* an optional exited event can be issued to return the exit code to the development tool. *)
  let on_disconnect_response exitCode = function
  | DisconnectResponse _ ->
    let ev =
      let event = Dap_events.exited in
      let body =
        ExitedEvent_body.make
          ~exitCode
          ()
      in
      default_event_req event body
    in
    let ret = ExitedEvent ev in
    Dap_flow.from_event ret
  | _ -> assert false

  let handle ~config req =
    let open Dap_flow in
    let response = on_request req on_disconnect_request in
    (* diconnect when launched - terminate debuggee forcefully  *)
    (* disconnect when attached - dont terminate the debuggee *)
    let event = match config.launch_mode with
      | `Launch ->
        Logs.warn (fun m -> m "TODO: shutdown debuggee forcefully; shutdown channel");
        let exitCode = 0 in
        Option.some @@ on_response response (on_disconnect_response exitCode)
      | `Attach | `AttachForSuspendedLaunch ->
        Logs.warn (fun m -> m "TODO: shutdown channel");
        None
      in
    Lwt.return {response; event}

end

(* The terminate request is sent from the client to the debug adapter in order to shut down the debuggee gracefully. Clients should only call this request if the capability supportsTerminateRequest is true. *)

(* Typically a debug adapter implements terminate by sending a software signal which the debuggee intercepts in order to clean things up properly before terminating itself. *)

(* Please note that this request does not directly affect the state of the debug session: if the debuggee decides to veto the graceful shutdown for any reason by not terminating itself, then the debug session just continues. *)

(* Clients can surface the terminate request as an explicit command or they can integrate it into a two stage Stop command that first sends terminate to request a graceful shutdown, and if that fails uses disconnect for a forceful shutdown. *)

module Terminate : HANDLER = struct
  include MakeReqRespIncludes_withEvent
      (struct
        type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
        type command = Dap_commands.terminate
        let command = Dap_commands.terminate
        type args = TerminateArguments.t option
        type presence = RequestMessage.opt
        let enc = RequestMessage.enc_opt command TerminateArguments.enc
        let ctor = fun req -> TerminateRequest req
        let extract = Dap_flow.to_request
      end)
      (struct
        type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
        type command = Dap_commands.terminate
        let command = Dap_commands.terminate
        type body = EmptyObject.t option
        type presence = ResponseMessage.opt
        let enc = ResponseMessage.enc_opt command EmptyObject.enc
        let ctor = fun resp -> TerminateResponse resp
        let extract = Dap_flow.to_response
      end)
      (struct
        type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
        type event = Dap_events.exited
        let event = Dap_events.exited
        type body = ExitedEvent_body.t
        type presence = EventMessage.req
        let enc = EventMessage.enc event ExitedEvent_body.enc
        let ctor = fun ev -> ExitedEvent ev
        let extract = Dap_flow.to_event
      end)


  let on_terminate_request = function
    | TerminateRequest req ->
      let resp =
        let command = RequestMessage.command req in
        let body = EmptyObject.make () in
        default_response_opt command body
      in
      let ret = TerminateResponse resp in
      Dap_flow.from_response ret
    | _ -> assert false

  (* If the debuggee has ended (and the debug adapter is able to detect this), *)
  (* an optional exited event can be issued to return the exit code to the development tool. *)
  let on_terminate_response exitCode = function
  | TerminateResponse _ ->
    let ev =
      let event = Dap_events.exited in
      let body =
        ExitedEvent_body.make
          ~exitCode
          ()
      in
      default_event_req event body
    in
    let ret = ExitedEvent ev in
    Dap_flow.from_event ret
  | _ -> assert false

  let handle ~config:_ req =
    let open Dap_flow in
    let response = on_request req on_terminate_request in
    Logs.warn (fun m -> m "TODO: shutdown debuggee gracefully; shutdown channel");
    let event =
        let exitCode = 0 in
        Option.some @@ on_response response (on_terminate_response exitCode)
    in
    Lwt.return {response; event}
end

module type MAKE_HANDLER = sig
  type input
  type output
  type t
  val make : t
  val handle : t -> config:config -> string -> string Lwt.t
end

module Handler (H:HANDLER) : (MAKE_HANDLER with type input := H.input and type output := H.output) = struct

  type t = {
    from_string : string -> H.input;
    to_string : H.output -> (string, string) Result.t Lwt.t ;
    handle : config:config -> H.input -> H.output Lwt.t;
  }

  let make = {
    from_string = H.from_string;
    to_string = H.to_string;
    handle = H.handle;
  }

  let handle t ~config s =
    let%lwt output =
      t.from_string s
      |> t.handle ~config
    in
    match%lwt t.to_string output with
    | Result.Ok msg ->
      Lwt.return msg
    | Result.Error err ->
      Lwt.return err


end
