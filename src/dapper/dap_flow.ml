
open Dap_t
(* open Dap_message *)

(* module JS = Data_encoding.Json *)
(* module SReq = Sequencing(RequestMessage) *)
(* module SResp = Sequencing(ResponseMessage) *)
(* module SEv = Sequencing(EventMessage) *)

(* type launch_mode = [ `Launch | `Attach | `AttachForSuspendedLaunch ] *)
(* type config = {launch_mode: launch_mode} *)
(* type ret = (response * event list, ErrorResponse_body.t) Result.t *)
type 'a t = ('a, Dap_message.ErrorResponse_body.t) Result.t
type ('command, 'args, 'pargs) req  = ('command, 'args, 'pargs) Dap_t.RequestMessage.t
type ('command, 'body, 'pbody) resp = ('command, 'body, 'pbody) Dap_t.ResponseMessage.t
type ('event, 'body, 'pbody) ev = ('event, 'body, 'pbody) Dap_t.EventMessage.t

let req_resp :
  (('command, 'args, 'pargs) req t ->
   (('command, 'args, 'pargs) req -> ('command, 'body, 'pbody) resp t) ->
   ('command, 'body, 'pbody) resp t) = fun v f ->
  match v with
  | Result.Ok req ->
    let request_seq = RequestMessage.seq req in
    let seq = succ @@ request_seq in
    let resp = f req in
    Result.bind resp (fun resp ->
        let resp = ResponseMessage.set_request_seq resp request_seq in
        let resp = ResponseMessage.set_seq resp seq in
        (Result.Ok resp)
      )
  | Result.Error _ as err -> err

let resp_ev :
  ('command, 'body, 'pbody) resp t ->
  (('command, 'body, 'pbody) resp -> ('event, 'evbody, 'pevbody) ev t) ->
  ('event, 'evbody, 'pevbody) ev t = fun v f ->
  match v with
  | Result.Ok resp ->
    let request_seq = ResponseMessage.seq resp in
    let seq = succ @@ request_seq in
    let ev = f resp in
    Result.bind ev (fun ev ->
        let ev = EventMessage.set_seq ev seq in
        (Result.Ok ev)
      )
  | Result.Error _ as err -> err


let next_ev :
  ('event, 'evbody, 'pevbody) ev t ->
  (('event, 'evbody, 'pevbody) ev -> ('event_, 'evbody_, 'pevbody_) ev t) ->
  ('event_, 'evbody_, 'pevbody_) ev t = fun v f ->
  match v with
  | Result.Ok ev ->
    let request_seq = EventMessage.seq ev in
    let seq = succ @@ request_seq in
    let ev = f ev in
    Result.bind ev (fun ev ->
        let ev = EventMessage.set_seq ev seq in
        (Result.Ok ev)
      )
  | Result.Error _ as err -> err



(*   let request_response_ ~config = function *)
(*     | CancelRequest req -> *)
(*       let resp = *)
(*         let sequencer = SReq.next_sequence req in *)
(*         let request_seq = SReq.request_seq sequencer in *)
(*         let seq = SReq.seq sequencer in *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body () *)
(*       in *)
(*       let ret = CancelResponse resp, [] in *)
(*       Result.ok ret *)

(* (\* *)
(* Initialization *)

(* The Debug Adapter Protocol defines many features and this number is still growing, albeit slowly. However, the protocol is still at its first version because it was an explicit design goal to support new feature in a completely backward compatible way. Making this possible without version numbers requires that every new feature gets a corresponding flag that lets a development tool know whether a debug adapter supports the feature or not. The absence of the flag always means that the feature is not supported. *)

(* A single feature and its corresponding flag is called a “capability” in the Debug Adapter Protocol. The open-ended set of all features flags is called DAP’s “capabilities.” *)

(* When starting a debug session, the development tool sends an initialize request to the adapter in order to exchange capabilities between the development tool and the debug adapter. *)

(* The development tool capabilities are provided in the InitializeRequestArguments structure of the initialize request and typically start with the prefix supports. Other pieces of information passed from the tool to the debug adapter are: *)

(*     the name of the development tool, *)
(*     the format of file paths, native or uri, *)
(*     whether line and column values are 0 or 1 based, *)
(*     the locale used by the development tool. A debug adapter is expected to return error messages that honor this locale. *)

(* The debug adapter returns the supported capabilities in the InitializeResponse via the Capabilities type. It is not necessary to return an explicit false for unsupported capabilities. *)
(*  *\) *)
(*     | InitializeRequest req -> *)
(*       let resp = *)
(*         let sequencer = SReq.next_sequence req in *)
(*         let request_seq = SReq.request_seq sequencer in *)
(*         let seq = SReq.seq sequencer in *)
(*         let command = RequestMessage.command req in *)
(*         (\* TODO hardcode capabilities or pull in from config *\) *)
(*         let body = Capabilities.make () in *)
(*         ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body () *)
(*       in *)
(*       let ev = *)
(*         let seq = SResp.(next_sequence resp |> seq) in *)
(*         let event = Dap_event.initialized in *)
(*         let body = EmptyObject.make () in *)
(*         EventMessage.make_opt ~seq ~event ~body () *)
(*       in *)
(*       let ret = InitializeResponse resp, [InitializedEvent ev] in *)
(*       Result.ok ret *)

(*     (\* TODO breakpoints stuff - here *\) *)

(*     | ConfigurationDoneRequest req -> *)
(*       let resp = *)
(*         let sequencer = SReq.next_sequence req in *)
(*         let request_seq = SReq.request_seq sequencer in *)
(*         let seq = SReq.seq sequencer in *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body () *)
(*       in *)
(*       let ret = ConfigurationDoneResponse resp, [] in *)
(*       Result.ok ret *)

(*     (\* *)
(* Launching and attaching *)

(* After the debug adapter has been initialized, it is ready to accept requests for starting debugging. Two requests exist for this: *)

(*     launch request: the debug adapter launches the program (“debuggee”) in debug mode and then starts to communicate with it. Since the debug adapter is responsible for launching the debuggee, it should provide a mechanism for the end user to configure the debuggee. For example, passing arguments or specifying a working directory. *)
(*         Debug adapters are free to launch the debuggee however they choose. Typically the debuggee is launched as a child process and its output channels are connected to a client’s debug console via output events. However, this has certain limitations, such as not being able to write to the terminal device directly and not being able to accept standard input. For those cases, launching the debuggee in a terminal is preferable. A debug adapter can use the the runInTerminal request to ask the client to launch the debuggee in a terminal that is integrated into the client or in a terminal that runs outside of the client (but still configured and managed from the client). *)
(*     attach request: the debug adapter connects to an already running program. Here the end user is responsible for launching and terminating the program. *)

(* Since arguments for both requests are highly dependent on a specific debugger and debug adapter implementation, the Debug Adapter Protocol does not specify any arguments for these requests. Instead, the development tool is expected to get information about debugger specific arguments from elsewhere (e.g. contributed by some plugin or extension mechanism) and to build a UI and validation mechanism on top of that. *)
(*   *\) *)
(*     | LaunchRequest req when config.launch_mode = `Launch -> *)
(*       let resp = *)
(*         let sequencer = SReq.next_sequence req in *)
(*         let request_seq = SReq.request_seq sequencer in *)
(*         let seq = SReq.seq sequencer in *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body () *)
(*       in *)
(*       let ev = *)
(*         let seq = SResp.(next_sequence resp |> seq) in *)
(*         let event = Dap_event.process in *)
(*         let startMethod = ProcessEvent_body_startMethod.Launch in *)
(*         let body = ProcessEvent_body.make ~name:"TODO PROCESS EVENT NAME e.g. test.tz" ~startMethod () in *)
(*         EventMessage.make ~seq ~event ~body () *)
(*       in *)

(*       let ret = LaunchResponse resp, [ProcessEvent ev] in *)
(*       Result.ok ret *)

(*     | LaunchRequest _req when config.launch_mode = `Attach -> *)
(*       Logs.err (fun m -> m "wrong launch mode - config is set to Attach but got a Launch request message"); *)
(*       let error = Message.make ~id:0 ~format:"wrong launch mode - config is set to Attach but got a Launch request message" () in *)
(*       let error = ErrorResponse_body.make ~error () in *)
(*       Result.error error *)

(*     | AttachRequest req when config.launch_mode = `Attach -> *)
(*       let resp = *)
(*         let sequencer = SReq.next_sequence req in *)
(*         let request_seq = SReq.request_seq sequencer in *)
(*         let seq = SReq.seq sequencer in *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body () *)
(*       in *)
(*       let ev = *)
(*         let seq = SResp.(next_sequence resp |> seq) in *)
(*         let event = Dap_event.process in *)
(*         let startMethod = ProcessEvent_body_startMethod.Attach in *)
(*         let body = ProcessEvent_body.make ~name:"TODO PROCESS EVENT NAME e.g. test.tz" ~startMethod () in *)
(*         EventMessage.make ~seq ~event ~body () *)
(*       in *)

(*       let ret = AttachResponse resp, [ProcessEvent ev] in *)
(*       Result.ok ret *)

(*     | AttachRequest _req when config.launch_mode = `Launch -> *)
(*       Logs.err (fun m -> m "wrong launch mode - config is set to Launch but got a Attach request message"); *)
(*       let error = Message.make ~id:0 ~format:"wrong launch mode - config is set to Launch but got a Attach request message" () in *)
(*       let error = ErrorResponse_body.make ~error () in *)
(*       Result.error error *)

(*     | RestartRequest req -> *)
(*       let resp = *)
(*         let sequencer = SReq.next_sequence req in *)
(*         let request_seq = SReq.request_seq sequencer in *)
(*         let seq = SReq.seq sequencer in *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body () *)
(*       in *)
(*       (\* TODO any events to raise? *\) *)
(*       let ret = RestartResponse resp, [] in *)
(*       Result.ok ret *)


(* (\* *)
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

(*     | DisconnectRequest req -> *)
(*       let resp = *)
(*         let sequencer = SReq.next_sequence req in *)
(*         let request_seq = SReq.request_seq sequencer in *)
(*         let seq = SReq.seq sequencer in *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body () *)
(*       in *)
(*       (\* TODO any events to raise? *\) *)
(*       let ret = DisconnectResponse resp, [] in *)
(*       Result.ok ret *)

(*     | TerminateRequest req -> *)
(*       let resp = *)
(*         let sequencer = SReq.next_sequence req in *)
(*         let request_seq = SReq.request_seq sequencer in *)
(*         let seq = SReq.seq sequencer in *)
(*         let command = RequestMessage.command req in *)
(*         let body = EmptyObject.make () in *)
(*         ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body () *)
(*       in *)
(*       (\* TODO any events to raise? *\) *)
(*       let ret = TerminateResponse resp, [] in *)
(*       Result.ok ret *)

(*     | _ -> failwith "TODO" *)





  (* let _HEADER_FIELD = "Content-Length: " *)
  (* let _HEADER_TOKEN = "\r\n\r\n" *)

  (* let _replace input output = *)
  (*   Str.global_replace (Str.regexp_string input) output *)

  (* let wrap_header js = *)
  (*   let s = js *)
  (*           |> JS.to_string *)
  (*           |> _replace "\n" "" *)
  (*   in *)
  (*   let n = String.length s in *)
  (*   Printf.sprintf "%s%d%s%s" _HEADER_FIELD n _HEADER_TOKEN s *)

  (* let destruct_request t msg = *)
  (*     match JS.from_string msg with *)
  (*     | Ok js -> ( *)
  (*         try *)
  (*           Ok (JS.destruct Request.enc t.request js) *)
  (*         with _ as err -> *)
  (*           Logs.err (fun m -> m "Cannot parse json '%s' as request: '%s'" msg @@ Printexc.to_string err); *)
  (*           Error (Printexc.to_string err) *)
  (*       ) *)
  (*     | Error err -> *)
  (*       Logs.err (fun m -> m "Cannot parse json '%s': '%s'" msg err); *)
  (*       (\* TODO should return an error response *\) *)
  (*       Error err *)


  (* let construct_response t response = *)
  (*   let r = Response.incr response in *)
  (*   JS.construct t.response r |> wrap_header *)
(* end *)
