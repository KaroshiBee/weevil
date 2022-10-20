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


module type HANDLER = sig

  type input
  type output

  type backend_channel

  val from_string : string -> input
  (* NOTE when handling a request we will be interacting with the backend, hence the Lwt.t  *)
  val handle : config:config -> input -> output Lwt.t
  val to_string : output -> (string, string) Result.t Lwt.t

end
(*
Previous handler func:
  client:Lwt_io.output Lwt_io.channel ->
  msg:string ->
  backend:Lwt_io.output Lwt_io.channel ->
  unit Lwt.t
*)
module Cancel : HANDLER = struct

  type req = (Dap_commands.cancel, CancelArguments.t option, RequestMessage.opt) request
  type input = req Dap_flow.t

  type resp = (Dap_commands.cancel, EmptyObject.t option, ResponseMessage.opt) response
  type output = resp Dap_flow.t

  type backend_channel = Lwt_io.output_channel

  let on_cancel_request ~config:_ = function
    | CancelRequest req ->
      let resp =
        let command = RequestMessage.command req in
        let body = EmptyObject.make () in
        default_response_opt command body
      in
      let ret = CancelResponse resp in
      Dap_flow.from_response ret
    | _ -> assert false

  let from_string =
    let enc = RequestMessage.enc_opt Dap_commands.cancel CancelArguments.enc in
    fun input ->
      JsMsg.from_string input
      |> Result.map (JsMsg.destruct enc)
      |> Result.map (fun x -> CancelRequest x)
      |> Dap_flow.from_result

  let handle ~config req =
    Dap_flow.on_request req (on_cancel_request ~config)
    |> Lwt.return

  let to_string =
    let enc = ResponseMessage.enc_opt Dap_commands.cancel EmptyObject.enc in
    fun output ->
      match Dap_flow.to_result output with
      | Result.Ok (CancelResponse resp) ->
        (* TODO cancel the backend svc *)
        JsMsg.construct enc resp
        |> JsMsg.to_string
        |> Result.ok
        |> Lwt.return

      | Result.Error _ as err -> Lwt.return err
      | _ -> assert false

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

  type req = (Dap_commands.initialize, InitializeRequestArguments.t, RequestMessage.req) request
  type input = req Dap_flow.t

  type resp = (Dap_commands.initialize, Capabilities.t option, ResponseMessage.opt) response
  type ev = (Dap_events.initialized, EmptyObject.t option, EventMessage.opt) event
  type output = {
    response: resp Dap_flow.t;
    event: ev Dap_flow.t;
  }

  type backend_channel = Lwt_io.output_channel

  let from_string =
    let enc = RequestMessage.enc Dap_commands.initialize InitializeRequestArguments.enc in
    fun input ->
      JsMsg.from_string input
      |> Result.map (JsMsg.destruct enc)
      |> Result.map (fun x -> InitializeRequest x)
      |> Dap_flow.from_result

  let on_initialize_request ~config:_ = function
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

  let on_initialization_response ~config:_ = function
      | InitializeResponse _ ->
        let ev =
          let event = Dap_events.initialized in
          let body = EmptyObject.make () in
          default_event_opt event body
        in
        let ret = InitializedEvent ev in
        Dap_flow.from_event ret
      | _ -> assert false

  let handle ~config req =
    let open Dap_flow in
    let response = on_request req (on_initialize_request ~config) in
    let event = on_response response (on_initialization_response ~config) in
    Lwt.return {response; event}

  let to_string =
    let enc_resp = ResponseMessage.enc_opt Dap_commands.initialize Capabilities.enc in
    let enc_ev = EventMessage.enc_opt Dap_events.initialized EmptyObject.enc in
    fun {response; event} ->
      match Dap_flow.(to_result response, to_result event) with
      | Result.Ok (InitializeResponse resp), Result.Ok (InitializedEvent ev) ->
        let resp = JsMsg.construct enc_resp resp |> JsMsg.to_string in
        let ev = JsMsg.construct enc_ev ev |> JsMsg.to_string in
        Result.ok @@ resp^ev
        |> Lwt.return
      | Result.Error _, Result.Ok (InitializedEvent _)
      | Result.Ok (InitializeResponse _), Result.Error _
      | Result.Error _, Result.Error _ ->
        failwith "TODO"
      | _, _ -> assert false

end

module Configuration = struct

  let on_configurationDone_request ~config:_ = function
      | ConfigurationDoneRequest req ->
        let resp =
          let command = RequestMessage.command req in
          let body = EmptyObject.make () in
          default_response_opt command body
        in
        let ret = ConfigurationDoneResponse resp in
        Dap_flow.from_response ret
      | _ -> assert false


  let on_configurationDone ~config req =
    Dap_flow.on_request req (on_configurationDone_request ~config)

end

(* Launching and attaching *)

(* After the debug adapter has been initialized, it is ready to accept requests for starting debugging. Two requests exist for this: *)

(*     launch request: the debug adapter launches the program (“debuggee”) in debug mode and then starts to communicate with it. Since the debug adapter is responsible for launching the debuggee, it should provide a mechanism for the end user to configure the debuggee. For example, passing arguments or specifying a working directory. *)
(*         Debug adapters are free to launch the debuggee however they choose. Typically the debuggee is launched as a child process and its output channels are connected to a client’s debug console via output events. However, this has certain limitations, such as not being able to write to the terminal device directly and not being able to accept standard input. For those cases, launching the debuggee in a terminal is preferable. A debug adapter can use the the runInTerminal request to ask the client to launch the debuggee in a terminal that is integrated into the client or in a terminal that runs outside of the client (but still configured and managed from the client). *)
(*     attach request: the debug adapter connects to an already running program. Here the end user is responsible for launching and terminating the program. *)

(* Since arguments for both requests are highly dependent on a specific debugger and debug adapter implementation, the Debug Adapter Protocol does not specify any arguments for these requests. Instead, the development tool is expected to get information about debugger specific arguments from elsewhere (e.g. contributed by some plugin or extension mechanism) and to build a UI and validation mechanism on top of that. *)
(*   *\) *)

module Launch : HANDLER = struct

  type req = (Dap_commands.launch, LaunchRequestArguments.t, RequestMessage.req) request
  type input = req Dap_flow.t

  type resp = (Dap_commands.launch, EmptyObject.t option, ResponseMessage.opt) response
  type ev = (Dap_events.process, ProcessEvent_body.t, EventMessage.req) event
  type output = {
    response: resp Dap_flow.t;
    event: ev Dap_flow.t;
  }

  type backend_channel = Lwt_io.output_channel

  let from_string =
    let enc = RequestMessage.enc Dap_commands.launch LaunchRequestArguments.enc in
    fun input ->
      JsMsg.from_string input
      |> Result.map (JsMsg.destruct enc)
      |> Result.map (fun x -> LaunchRequest x)
      |> Dap_flow.from_result

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
    let event = on_response response (on_launch_response ~config) in
    Lwt.return {response; event}

  let to_string =
    let enc_resp = ResponseMessage.enc_opt Dap_commands.launch EmptyObject.enc in
    let enc_ev = EventMessage.enc Dap_events.process ProcessEvent_body.enc in
    fun {response; event} ->
      match Dap_flow.(to_result response, to_result event) with
      | Result.Ok (LaunchResponse resp), Result.Ok (ProcessEvent ev) ->
        let resp = JsMsg.construct enc_resp resp |> JsMsg.to_string in
        let ev = JsMsg.construct enc_ev ev |> JsMsg.to_string in
        Result.ok @@ resp^ev
        |> Lwt.return
      | Result.Error _, Result.Ok (ProcessEvent _)
      | Result.Ok (LaunchResponse _), Result.Error _
      | Result.Error _, Result.Error _ ->
        failwith "TODO"
      | _, _ -> assert false

end

(*   | AttachRequest req when config.launch_mode = `Attach -> *)
(*       let resp = *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         default_response command body *)
(*       in *)
(*       (\* let ev = *\) *)
(*       (\*   let event = Dap_events.process in *\) *)
(*       (\*   let startMethod = ProcessEvent_body_startMethod.Attach in *\) *)
(*       (\*   let body = *\) *)
(*       (\*     ProcessEvent_body.make *\) *)
(*       (\*       ~name:"TODO PROCESS EVENT NAME e.g. test.tz" *\) *)
(*       (\*       ~startMethod *\) *)
(*       (\*       () *\) *)
(*       (\*   in *\) *)
(*       (\*   EventMessage.make ~seq ~event ~body () *\) *)
(*       (\* in *\) *)

(*       let ret = AttachResponse resp in *)
(*       Result.ok ret *)
(*   | AttachRequest _req when config.launch_mode = `Launch -> *)
(*       Logs.err (fun m -> *)
(*           m *)
(*             "wrong launch mode - config is set to Launch but got a Attach \ *)
(*              request message") ; *)
(*       let error = *)
(*         Message.make *)
(*           ~id:0 *)
(*           ~format: *)
(*             "wrong launch mode - config is set to Launch but got a Attach \ *)
(*              request message" *)
(*           () *)
(*       in *)
(*       let error = ErrorResponse_body.make ~error () in *)
(*       Result.error error *)
(*   | RestartRequest req -> *)
(*       let resp = *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         default_response command body *)
(*       in *)
(*       (\* TODO any events to raise? *\) *)
(*       let ret = RestartResponse resp in *)
(*       Result.ok ret *)
(*   (\* *)
(*  Debug session end *)

(* When the development tool ends a debug session, the sequence of events is slightly different based on whether the session has been initially “launched” or “attached”: *)

(*     Debuggee launched: if a debug adapter supports the terminate request, the development tool uses it to terminate the debuggee gracefully, i.e. it gives the debuggee a chance to cleanup everything before terminating. If the debuggee does not terminate but continues to run (or hits a breakpoint), the debug session will continue, but if the development tool tries again to terminate the debuggee, it will then use the disconnect request to end the debug session unconditionally. The disconnect request is expected to terminate the debuggee (and any child processes) forcefully. *)
(*     Debuggee attached: If the debuggee has been “attached” initially, the development tool issues a disconnect request. This should detach the debugger from the debuggee but will allow it to continue. *)

(* In all situations where a debug adapter wants to end the debug session, a terminated event must be fired. *)

(* If the debuggee has ended (and the debug adapter is able to detect this), an optional exited event can be issued to return the exit code to the development tool.Debug session end *)

(* When the development tool ends a debug session, the sequence of events is slightly different based on whether the session has been initially “launched” or “attached”: *)

(*     Debuggee launched: if a debug adapter supports the terminate request, the development tool uses it to terminate the debuggee gracefully, i.e. it gives the debuggee a chance to cleanup everything before terminating. If the debuggee does not terminate but continues to run (or hits a breakpoint), the debug session will continue, but if the development tool tries again to terminate the debuggee, it will then use the disconnect request to end the debug session unconditionally. The disconnect request is expected to terminate the debuggee (and any child processes) forcefully. *)
(*     Debuggee attached: If the debuggee has been “attached” initially, the development tool issues a disconnect request. This should detach the debugger from the debuggee but will allow it to continue. *)

(* In all situations where a debug adapter wants to end the debug session, a terminated event must be fired. *)

(* If the debuggee has ended (and the debug adapter is able to detect this), an optional exited event can be issued to return the exit code to the development tool. *)
(*   *\) *)
(*   | DisconnectRequest req -> *)
(*       let resp = *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         default_response command body *)
(*       in *)
(*       (\* TODO any events to raise? *\) *)
(*       let ret = DisconnectResponse resp in *)
(*       Result.ok ret *)
(*   | TerminateRequest req -> *)
(*       let resp = *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         default_response command body *)
(*       in *)
(*       (\* TODO any events to raise? *\) *)
(*       let ret = TerminateResponse resp in *)
(*       Result.ok ret *)
(*   | _ -> failwith "TODO: every request should have a response" *)

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
