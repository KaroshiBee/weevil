open Dap_message_exi

type 'a t = ('a, string) Result.t
type seqr = {seq:int; request_seq:int}

let from_result x = x
let to_result x = x

let from_request = Result.ok
let from_response = Result.ok
let from_event = Result.ok
(* is there a way of lifting
   (cmd, _, _) RequestMessage.t -> (cmd, _, _) ResponseMessage.t
   into
   request -> response
   ?
*)

module type Handle_T = sig

  type cmd
  type args
  type pargs
  type body
  type pbody

  type t
  val make : ((cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t) -> t
  val lift : t -> (request -> (response, string) Result.t)
end



module Seqr (H:Handle_T)
  : sig

    type t
    val make : ((H.cmd, H.args, H.pargs) RequestMessage.t -> (H.cmd, H.body, H.pbody) ResponseMessage.t) -> t
    val sequence : t -> request -> (response, string) Result.t

  end
= struct

  type t = H.t

  let make f =
    let f' = fun req ->
      let request_seq = RequestMessage.seq req in
      let seq = 1 + request_seq in
      let resp = f req in
      let resp = ResponseMessage.set_request_seq resp ~request_seq in
      let resp = ResponseMessage.set_seq resp ~seq in
      resp
    in
    H.make f'

  let sequence t req =
    H.lift t req

end

module Launch = Seqr (struct
  type cmd = Dap_commands.launch
  type args = LaunchRequestArguments.t
  type pargs = Dap_base.Presence.req
  type body = EmptyObject.t option
  type pbody = Dap_base.Presence.opt
  type t = {
    f: (cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t;
  }

  let make f = {f;}

  let lift {f;} = function
    | LaunchRequest req -> f req |> launchResponse |> Result.ok
    | _ -> Result.Error "wrong request"

end)


module Attach = Seqr (struct
  type cmd = Dap_commands.attach
  type args = AttachRequestArguments.t
  type pargs = Dap_base.Presence.req
  type body = EmptyObject.t option
  type pbody = Dap_base.Presence.opt
  type t = {
    f: (cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t;
  }

  let make f = {f;}

  let lift {f;} = function
    | AttachRequest req -> f req |> attachResponse |> Result.ok
    | _ -> Result.Error "wrong request"

end)

module Completions = Seqr (struct
  type cmd = Dap_commands.completions
  type args = CompletionsArguments.t
  type pargs = Dap_base.Presence.req
  type body = CompletionsResponse_body.t
  type pbody = Dap_base.Presence.req
  type t = {
    f: (cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t;
  }

  let make f = {f;}

  let lift {f;} = function
    | CompletionsRequest req -> f req |> completionsResponse |> Result.ok
    | _ -> Result.Error "wrong request"

end)

(* problem is making the response or event,
   dont know what presence or event type it is *)

module Seq1 (Msg:sig type ('a,'b,'c) t val seq : (_, _, _) t -> int end) = struct
  let sequence msg f =
    let seq = 1 + Msg.seq msg in
    f seq
end

module Seq2 (Msg:sig type ('a,'b,'c) t val seq : (_, _, _) t -> int val command : ('cmd, _, _) t -> 'cmd Dap_commands.t end) = struct
  type ('a,'b,'c) t = ('a,'b,'c) Msg.t
  let sequence msg f =
    let request_seq = Msg.seq msg in
    let seq = 1 + request_seq in
    let command = Msg.command msg in
    f seq request_seq command
end

module Resp_event = Seq1(ResponseMessage)
module Raise_event = Seq1(EventMessage)
module Request_response = Seq2(RequestMessage)
let req_attach = attachRequest @@ RequestMessage.make ~seq:0 ~command:Dap_commands.attach ~arguments:(AttachRequestArguments.make ()) ()
let req_launch = launchRequest @@ RequestMessage.make ~seq:0 ~command:Dap_commands.launch ~arguments:(LaunchRequestArguments.make ()) ()

let req_resp req =
  let ff : int -> int -> 'cmd Dap_commands.t -> ('cmd, _, _) ResponseMessage.t
    = fun seq request_seq command ->
      ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ?message:None ?body:None ()
  in
  Request_response.sequence req ff

(* correct seq and correct command link *)
module type T = sig
  val req_resp_1 : request -> response
  (* val map : 'request t -> ('request -> 'response) -> 'response t *)
  val bind : request t -> (request -> response t) -> response t
end

module ReqResp : T = struct

  (* let map (type request response) : request t -> (request -> response) -> response t = fun req f -> *)
  (*   let seqr r resp = *)
  (*     let request_seq = RequestMessage.seq r in *)
  (*     let seq = 1 + request_seq in *)
  (*     let resp = ResponseMessage.set_seq resp ~seq in *)
  (*     let resp = ResponseMessage.set_request_seq resp ~request_seq in *)
  (*     resp *)
  (*   in *)
  (*   Result.map (function *)
  (*       | AttachRequest req -> let resp = f req in attachResponse @@ seqr req resp *)
  (*       | LaunchRequest req -> let resp = f req in launchResponse @@ seqr req resp *)
  (*       | ExceptionInfoRequest req -> let resp = f req in exceptionInfoResponse @@ seqr req resp *)
  (*       | _ -> failwith "TODO" *)
  (*   ) req *)

  let bind req f = Result.bind req f

  let req_resp_1 =
    let makeseq req =
      let request_seq = RequestMessage.seq req in
      let seq = 1 + request_seq in
      let command = RequestMessage.command req in
      (request_seq, seq, command)
    in
    function
    | AttachRequest req ->
      let request_seq, seq, command = makeseq req in
      let body = EmptyObject.make () in
      attachResponse @@ ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body ?message:None ()
    | LaunchRequest req ->
      let request_seq, seq, command = makeseq req in
      let body = EmptyObject.make () in
      launchResponse @@ ResponseMessage.make_opt ~seq ~request_seq ~success:true ~command ~body ?message:None ()
    | CompletionsRequest req ->
      let request_seq, seq, command = makeseq req in
      let body = CompletionsResponse_body.make ~targets:[] () in
      completionsResponse @@ ResponseMessage.make ~seq ~request_seq ~success:true ~command ~body ?message:None ()
    | _ -> failwith "WRONG"

end

(* let req_resp_2 : type cmd. type body. type pbody. *)
(*   (int -> int -> cmd Dap_commands.t -> (cmd, body, pbody) ResponseMessage.t) -> request -> response = *)
(*   fun f -> function *)
(*   | AttachRequest areq -> *)
(*     let request_seq = RequestMessage.seq areq in *)
(*     let seq = 1 + request_seq in *)
(*     let command = RequestMessage.command areq in *)
(*     attachResponse @@ f seq request_seq command *)
(*   | LaunchRequest lreq -> *)
(*     let request_seq = RequestMessage.seq lreq in *)
(*     let seq = 1 + request_seq in *)
(*     let command = RequestMessage.command lreq in *)
(*     launchResponse @@ f seq request_seq command *)

(*   | _ -> failwith "WRONG" *)

(* let r11 = req_resp req_launch *)
(* let r22 = req_resp req_attach *)

(* let request_response : type cmd args pargs body pbody. *)
(*   request -> *)
(*   ( (cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t  ) -> *)
(*   response *)
(*   = fun v f -> *)
(*     match v with *)
(*     | CancelRequest req -> let resp = f req in CancelResponse resp *)
(*     | DisassembleRequest req -> let resp = f req in DisassembleResponse resp *)
(*     | _ -> failwith "TODO" *)


(* let raise_error (type cmd args pargs) : *)
(*   (cmd, args, pargs) request t -> *)
(*   ((cmd, args, pargs) request -> (Dap_commands.error, ErrorResponse_body.t, ResponseMessage.req) response t) -> *)
(*   (Dap_commands.error, ErrorResponse_body.t, ResponseMessage.req) response t *)
(*   = fun v f -> *)
(*    let seqr req = *)
(*      let request_seq = RequestMessage.seq req in *)
(*      let seq = succ request_seq in *)
(*      Response.set_seqr {seq; request_seq} *)
(*    in *)
(*    Result.bind v (function *)
(*        | CancelRequest req as v -> map_response (f v) @@ seqr req *)
(*        | RunInTerminalRequest req as v -> map_response (f v) @@ seqr req *)
(*        | InitializeRequest req as v -> map_response (f v) @@ seqr req *)
(*        | ConfigurationDoneRequest req as v -> map_response (f v) @@ seqr req *)
(*        | LaunchRequest req as v -> map_response (f v) @@ seqr req *)
(*        | AttachRequest req as v -> map_response (f v) @@ seqr req *)
(*        | RestartRequest req as v -> map_response (f v) @@ seqr req *)
(*        | DisconnectRequest req as v -> map_response (f v) @@ seqr req *)
(*        | TerminateRequest req as v -> map_response (f v) @@ seqr req *)
(*        | BreakpointLocationsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | SetBreakpointsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | SetFunctionBreakpointsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | SetExceptionBreakpointsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | DataBreakpointInfoRequest req as v -> map_response (f v) @@ seqr req *)
(*        | SetDataBreakpointsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | SetInstructionBreakpointsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | ContinueRequest req as v -> map_response (f v) @@ seqr req *)
(*        | NextRequest req as v -> map_response (f v) @@ seqr req *)
(*        | StepInRequest req as v -> map_response (f v) @@ seqr req *)
(*        | StepOutRequest req as v -> map_response (f v) @@ seqr req *)
(*        | StepBackRequest req as v -> map_response (f v) @@ seqr req *)
(*        | ReverseContinueRequest req as v -> map_response (f v) @@ seqr req *)
(*        | RestartFrameRequest req as v -> map_response (f v) @@ seqr req *)
(*        | GotoRequest req as v -> map_response (f v) @@ seqr req *)
(*        | PauseRequest req as v -> map_response (f v) @@ seqr req *)
(*        | StackTraceRequest req as v -> map_response (f v) @@ seqr req *)
(*        | ScopesRequest req as v -> map_response (f v) @@ seqr req *)
(*        | VariablesRequest req as v -> map_response (f v) @@ seqr req *)
(*        | SetVariableRequest req as v -> map_response (f v) @@ seqr req *)
(*        | SourceRequest req as v -> map_response (f v) @@ seqr req *)
(*        | ThreadsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | TerminateThreadsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | ModulesRequest req as v -> map_response (f v) @@ seqr req *)
(*        | LoadedSourcesRequest req as v -> map_response (f v) @@ seqr req *)
(*        | EvaluateRequest req as v -> map_response (f v) @@ seqr req *)
(*        | SetExpressionRequest req as v -> map_response (f v) @@ seqr req *)
(*        | StepInTargetsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | GotoTargetsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | CompletionsRequest req as v -> map_response (f v) @@ seqr req *)
(*        | ExceptionInfoRequest req as v -> map_response (f v) @@ seqr req *)
(*        | ReadMemoryRequest req as v -> map_response (f v) @@ seqr req *)
(*        | WriteMemoryRequest req as v -> map_response (f v) @@ seqr req *)
(*        | DisassembleRequest req as v -> map_response (f v) @@ seqr req *)
(*      ) *)


(* module Event = struct *)

(*   let map (type ev body pbody a) : *)
(*     (ev, body, pbody) event t -> *)
(*     ((ev, body, pbody) event -> a) -> *)
(*     a t *)
(*     = fun v f -> *)
(*       Result.map (fun v -> f v) v *)

(*   let bind (type ev body pbody a) : *)
(*     (ev, body, pbody) event t -> *)
(*     ((ev, body, pbody) event -> a t) -> *)
(*     a t *)
(*     = fun v f -> *)
(*       Result.bind v (fun v -> f v) *)

(*   let set_seqr (type ev body pbody) : *)
(*     seqr -> (ev, body, pbody) event -> (ev, body, pbody) event *)
(*     = *)
(*     let aux ev {seq; _} = *)
(*       EventMessage.set_seq ev ~seq *)
(*     in *)
(*     fun seqr -> function *)
(*       | InitializedEvent ev -> InitializedEvent (aux ev seqr) *)
(*       | StoppedEvent ev -> StoppedEvent (aux ev seqr) *)
(*       | ContinuedEvent ev -> ContinuedEvent (aux ev seqr) *)
(*       | ExitedEvent ev -> ExitedEvent (aux ev seqr) *)
(*       | TerminatedEvent ev -> TerminatedEvent (aux ev seqr) *)
(*       | ThreadEvent ev -> ThreadEvent (aux ev seqr) *)
(*       | OutputEvent ev -> OutputEvent (aux ev seqr) *)
(*       | BreakpointEvent ev -> BreakpointEvent (aux ev seqr) *)
(*       | ModuleEvent ev -> ModuleEvent (aux ev seqr) *)
(*       | LoadedSourceEvent ev -> LoadedSourceEvent (aux ev seqr) *)
(*       | ProcessEvent ev -> ProcessEvent (aux ev seqr) *)
(*       | CapabilitiesEvent ev -> CapabilitiesEvent (aux ev seqr) *)
(*       | ProgressStartEvent ev -> ProgressStartEvent (aux ev seqr) *)
(*       | ProgressUpdateEvent ev -> ProgressUpdateEvent (aux ev seqr) *)
(*       | ProgressEndEvent ev -> ProgressEndEvent (aux ev seqr) *)
(*       | InvalidatedEvent ev -> InvalidatedEvent (aux ev seqr) *)
(*       | MemoryEvent ev -> MemoryEvent (aux ev seqr) *)

(* end *)

(* let map_event = Event.map *)
(* let bind_event = Event.bind *)

(* let response_event (type cmd body pbody) : *)
(*   (cmd, body, pbody) response t -> *)
(*   ((cmd, body, pbody) response -> (_, _, _) event t) -> *)
(*   (_, _, _) event t *)
(*  = fun v f -> *)
(*    let seqr resp = *)
(*      let request_seq = ResponseMessage.seq resp in *)
(*      let seq = succ request_seq in *)
(*      Event.set_seqr {seq; request_seq} *)
(*    in *)
(*    Result.bind v (function *)
(*        | ErrorResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | CancelResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | RunInTerminalResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | InitializeResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | ConfigurationDoneResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | LaunchResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | AttachResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | RestartResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | DisconnectResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | TerminateResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | BreakpointLocationsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | SetBreakpointsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | SetFunctionBreakpointsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | SetExceptionBreakpointsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | DataBreakpointInfoResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | SetDataBreakpointsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | SetInstructionBreakpointsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | ContinueResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | NextResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | StepInResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | StepOutResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | StepBackResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | ReverseContinueResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | RestartFrameResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | GotoResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | PauseResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | StackTraceResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | ScopesResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | VariablesResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | SetVariableResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | SourceResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | ThreadsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | TerminateThreadsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | ModulesResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | LoadedSourcesResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | EvaluateResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | SetExpressionResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | StepInTargetsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | GotoTargetsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | CompletionsResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | ExceptionInfoResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | ReadMemoryResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | WriteMemoryResponse resp as v -> map_event (f v) @@ seqr resp *)
(*        | DisassembleResponse resp as v -> map_event (f v) @@ seqr resp *)
(*      ) *)

(* let raise_event (type ev body pbody) : *)
(*   (ev, body, pbody) event t -> *)
(*   ((ev, body, pbody) event -> (_, _, _) event t) -> *)
(*   (_, _, _) event t *)
(*  = fun v f -> *)
(*    let seqr ev = *)
(*      let request_seq = EventMessage.seq ev in *)
(*      let seq = succ request_seq in *)
(*      Event.set_seqr {seq; request_seq} *)
(*    in *)
(*    Result.bind v (function *)
(*        | InitializedEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | StoppedEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | ContinuedEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | ExitedEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | TerminatedEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | ThreadEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | OutputEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | BreakpointEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | ModuleEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | LoadedSourceEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | ProcessEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | CapabilitiesEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | ProgressStartEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | ProgressUpdateEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | ProgressEndEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | InvalidatedEvent ev as v -> map_event (f v) @@ seqr ev *)
(*        | MemoryEvent ev as v -> map_event (f v) @@ seqr ev *)
(*      ) *)
