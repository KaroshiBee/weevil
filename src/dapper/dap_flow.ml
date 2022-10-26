open Dap_message

type 'a t = ('a, string) Result.t
type seqr = {seq:int; request_seq:int}

let from_result x = x
let to_result x = x

let from_request = Result.ok
let from_response = Result.ok
let from_event = Result.ok

let to_request (type cmd args presence) :
  (cmd, args, presence) request ->
  (cmd, args, presence) RequestMessage.t
  = function
  | CancelRequest req -> req
  | RunInTerminalRequest req -> req
  | InitializeRequest req -> req
  | ConfigurationDoneRequest req -> req
  | LaunchRequest req -> req
  | AttachRequest req -> req
  | RestartRequest req -> req
  | DisconnectRequest req -> req
  | TerminateRequest req -> req
  | BreakpointLocationsRequest req -> req
  | SetBreakpointsRequest req -> req
  | SetFunctionBreakpointsRequest req -> req
  | SetExceptionBreakpointsRequest req -> req
  | DataBreakpointInfoRequest req -> req
  | SetDataBreakpointsRequest req -> req
  | SetInstructionBreakpointsRequest req -> req
  | ContinueRequest req -> req
  | NextRequest req -> req
  | StepInRequest req -> req
  | StepOutRequest req -> req
  | StepBackRequest req -> req
  | ReverseContinueRequest req -> req
  | RestartFrameRequest req -> req
  | GotoRequest req -> req
  | PauseRequest req -> req
  | StackTraceRequest req -> req
  | ScopesRequest req -> req
  | VariablesRequest req -> req
  | SetVariableRequest req -> req
  | SourceRequest req -> req
  | ThreadsRequest req -> req
  | TerminateThreadsRequest req -> req
  | ModulesRequest req -> req
  | LoadedSourcesRequest req -> req
  | EvaluateRequest req -> req
  | SetExpressionRequest req -> req
  | StepInTargetsRequest req -> req
  | GotoTargetsRequest req -> req
  | CompletionsRequest req -> req
  | ExceptionInfoRequest req -> req
  | ReadMemoryRequest req -> req
  | WriteMemoryRequest req -> req
  | DisassembleRequest req -> req

let to_response (type cmd body presence) :
  (cmd, body, presence) response ->
  (cmd, body, presence) ResponseMessage.t
  = function
      | ErrorResponse resp -> resp
      | CancelResponse resp -> resp
      | RunInTerminalResponse resp -> resp
      | InitializeResponse resp -> resp
      | ConfigurationDoneResponse resp -> resp
      | LaunchResponse resp -> resp
      | AttachResponse resp -> resp
      | RestartResponse resp -> resp
      | DisconnectResponse resp -> resp
      | TerminateResponse resp -> resp
      | BreakpointLocationsResponse resp -> resp
      | SetBreakpointsResponse resp -> resp
      | SetFunctionBreakpointsResponse resp -> resp
      | SetExceptionBreakpointsResponse resp -> resp
      | DataBreakpointInfoResponse resp -> resp
      | SetDataBreakpointsResponse resp -> resp
      | SetInstructionBreakpointsResponse resp -> resp
      | ContinueResponse resp -> resp
      | NextResponse resp -> resp
      | StepInResponse resp -> resp
      | StepOutResponse resp -> resp
      | StepBackResponse resp -> resp
      | ReverseContinueResponse resp -> resp
      | RestartFrameResponse resp -> resp
      | GotoResponse resp -> resp
      | PauseResponse resp -> resp
      | StackTraceResponse resp -> resp
      | ScopesResponse resp -> resp
      | VariablesResponse resp -> resp
      | SetVariableResponse resp -> resp
      | SourceResponse resp -> resp
      | ThreadsResponse resp -> resp
      | TerminateThreadsResponse resp -> resp
      | ModulesResponse resp -> resp
      | LoadedSourcesResponse resp -> resp
      | EvaluateResponse resp -> resp
      | SetExpressionResponse resp -> resp
      | StepInTargetsResponse resp -> resp
      | GotoTargetsResponse resp -> resp
      | CompletionsResponse resp -> resp
      | ExceptionInfoResponse resp -> resp
      | ReadMemoryResponse resp -> resp
      | WriteMemoryResponse resp -> resp
      | DisassembleResponse resp -> resp

let to_event (type ev body presence) :
  (ev, body, presence) event ->
  (ev, body, presence) EventMessage.t
  = function
    | InitializedEvent ev -> ev
    | StoppedEvent ev -> ev
    | ContinuedEvent ev -> ev
    | ExitedEvent ev -> ev
    | TerminatedEvent ev -> ev
    | ThreadEvent ev -> ev
    | OutputEvent ev -> ev
    | BreakpointEvent ev -> ev
    | ModuleEvent ev -> ev
    | LoadedSourceEvent ev -> ev
    | ProcessEvent ev -> ev
    | CapabilitiesEvent ev -> ev
    | ProgressStartEvent ev -> ev
    | ProgressUpdateEvent ev -> ev
    | ProgressEndEvent ev -> ev
    | InvalidatedEvent ev -> ev
    | MemoryEvent ev -> ev

module Response = struct

  let map (type cmd body pbody) :
    (cmd, body, pbody) response t ->
    ((cmd, body, pbody) response -> (cmd, body, pbody) response) ->
    (cmd, body, pbody) response t
    = fun v f ->
      Result.map (fun v -> f v) v

  let set_seqr (type cmd body pbody) :
    seqr -> (cmd, body, pbody) response -> (cmd, body, pbody) response
    =
    let aux resp {seq; request_seq} =
      let resp = ResponseMessage.set_request_seq resp ~request_seq in
      ResponseMessage.set_seq resp ~seq
    in
    fun seqr -> function
      | ErrorResponse resp -> ErrorResponse (aux resp seqr)
      | CancelResponse resp -> CancelResponse (aux resp seqr)
      | RunInTerminalResponse resp -> RunInTerminalResponse (aux resp seqr)
      | InitializeResponse resp -> InitializeResponse (aux resp seqr)
      | ConfigurationDoneResponse resp -> ConfigurationDoneResponse (aux resp seqr)
      | LaunchResponse resp -> LaunchResponse (aux resp seqr)
      | AttachResponse resp -> AttachResponse (aux resp seqr)
      | RestartResponse resp -> RestartResponse (aux resp seqr)
      | DisconnectResponse resp -> DisconnectResponse (aux resp seqr)
      | TerminateResponse resp -> TerminateResponse (aux resp seqr)
      | BreakpointLocationsResponse resp -> BreakpointLocationsResponse (aux resp seqr)
      | SetBreakpointsResponse resp -> SetBreakpointsResponse (aux resp seqr)
      | SetFunctionBreakpointsResponse resp -> SetFunctionBreakpointsResponse (aux resp seqr)
      | SetExceptionBreakpointsResponse resp -> SetExceptionBreakpointsResponse (aux resp seqr)
      | DataBreakpointInfoResponse resp -> DataBreakpointInfoResponse (aux resp seqr)
      | SetDataBreakpointsResponse resp -> SetDataBreakpointsResponse (aux resp seqr)
      | SetInstructionBreakpointsResponse resp -> SetInstructionBreakpointsResponse (aux resp seqr)
      | ContinueResponse resp -> ContinueResponse (aux resp seqr)
      | NextResponse resp -> NextResponse (aux resp seqr)
      | StepInResponse resp -> StepInResponse (aux resp seqr)
      | StepOutResponse resp -> StepOutResponse (aux resp seqr)
      | StepBackResponse resp -> StepBackResponse (aux resp seqr)
      | ReverseContinueResponse resp -> ReverseContinueResponse (aux resp seqr)
      | RestartFrameResponse resp -> RestartFrameResponse (aux resp seqr)
      | GotoResponse resp -> GotoResponse (aux resp seqr)
      | PauseResponse resp -> PauseResponse (aux resp seqr)
      | StackTraceResponse resp -> StackTraceResponse (aux resp seqr)
      | ScopesResponse resp -> ScopesResponse (aux resp seqr)
      | VariablesResponse resp -> VariablesResponse (aux resp seqr)
      | SetVariableResponse resp -> SetVariableResponse (aux resp seqr)
      | SourceResponse resp -> SourceResponse (aux resp seqr)
      | ThreadsResponse resp -> ThreadsResponse (aux resp seqr)
      | TerminateThreadsResponse resp -> TerminateThreadsResponse (aux resp seqr)
      | ModulesResponse resp -> ModulesResponse (aux resp seqr)
      | LoadedSourcesResponse resp -> LoadedSourcesResponse (aux resp seqr)
      | EvaluateResponse resp -> EvaluateResponse (aux resp seqr)
      | SetExpressionResponse resp -> SetExpressionResponse (aux resp seqr)
      | StepInTargetsResponse resp -> StepInTargetsResponse (aux resp seqr)
      | GotoTargetsResponse resp -> GotoTargetsResponse (aux resp seqr)
      | CompletionsResponse resp -> CompletionsResponse (aux resp seqr)
      | ExceptionInfoResponse resp -> ExceptionInfoResponse (aux resp seqr)
      | ReadMemoryResponse resp -> ReadMemoryResponse (aux resp seqr)
      | WriteMemoryResponse resp -> WriteMemoryResponse (aux resp seqr)
      | DisassembleResponse resp -> DisassembleResponse (aux resp seqr)
end

let bind_request (type cmd args pargs) :
  (cmd, args, pargs) request t ->
  ((cmd, args, pargs) request -> (cmd, _, _) response t) ->
  (cmd, _, _) response t
 = fun v f ->
   let seqr req =
     let request_seq = RequestMessage.seq req in
     let seq = succ request_seq in
     Response.set_seqr {seq; request_seq}
   in
   Result.bind v (function
       | CancelRequest req as v -> Response.map (f v) @@ seqr req
       | RunInTerminalRequest req as v -> Response.map (f v) @@ seqr req
       | InitializeRequest req as v -> Response.map (f v) @@ seqr req
       | ConfigurationDoneRequest req as v -> Response.map (f v) @@ seqr req
       | LaunchRequest req as v -> Response.map (f v) @@ seqr req
       | AttachRequest req as v -> Response.map (f v) @@ seqr req
       | RestartRequest req as v -> Response.map (f v) @@ seqr req
       | DisconnectRequest req as v -> Response.map (f v) @@ seqr req
       | TerminateRequest req as v -> Response.map (f v) @@ seqr req
       | BreakpointLocationsRequest req as v -> Response.map (f v) @@ seqr req
       | SetBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | SetFunctionBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | SetExceptionBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | DataBreakpointInfoRequest req as v -> Response.map (f v) @@ seqr req
       | SetDataBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | SetInstructionBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | ContinueRequest req as v -> Response.map (f v) @@ seqr req
       | NextRequest req as v -> Response.map (f v) @@ seqr req
       | StepInRequest req as v -> Response.map (f v) @@ seqr req
       | StepOutRequest req as v -> Response.map (f v) @@ seqr req
       | StepBackRequest req as v -> Response.map (f v) @@ seqr req
       | ReverseContinueRequest req as v -> Response.map (f v) @@ seqr req
       | RestartFrameRequest req as v -> Response.map (f v) @@ seqr req
       | GotoRequest req as v -> Response.map (f v) @@ seqr req
       | PauseRequest req as v -> Response.map (f v) @@ seqr req
       | StackTraceRequest req as v -> Response.map (f v) @@ seqr req
       | ScopesRequest req as v -> Response.map (f v) @@ seqr req
       | VariablesRequest req as v -> Response.map (f v) @@ seqr req
       | SetVariableRequest req as v -> Response.map (f v) @@ seqr req
       | SourceRequest req as v -> Response.map (f v) @@ seqr req
       | ThreadsRequest req as v -> Response.map (f v) @@ seqr req
       | TerminateThreadsRequest req as v -> Response.map (f v) @@ seqr req
       | ModulesRequest req as v -> Response.map (f v) @@ seqr req
       | LoadedSourcesRequest req as v -> Response.map (f v) @@ seqr req
       | EvaluateRequest req as v -> Response.map (f v) @@ seqr req
       | SetExpressionRequest req as v -> Response.map (f v) @@ seqr req
       | StepInTargetsRequest req as v -> Response.map (f v) @@ seqr req
       | GotoTargetsRequest req as v -> Response.map (f v) @@ seqr req
       | CompletionsRequest req as v -> Response.map (f v) @@ seqr req
       | ExceptionInfoRequest req as v -> Response.map (f v) @@ seqr req
       | ReadMemoryRequest req as v -> Response.map (f v) @@ seqr req
       | WriteMemoryRequest req as v -> Response.map (f v) @@ seqr req
       | DisassembleRequest req as v -> Response.map (f v) @@ seqr req
     )

let raise_error (type cmd args pargs) :
  (cmd, args, pargs) request t ->
  ((cmd, args, pargs) request -> (Dap_commands.error, ErrorResponse_body.t, ResponseMessage.req) response t) ->
  (Dap_commands.error, ErrorResponse_body.t, ResponseMessage.req) response t
  = fun v f ->
   let seqr req =
     let request_seq = RequestMessage.seq req in
     let seq = succ request_seq in
     Response.set_seqr {seq; request_seq}
   in
   Result.bind v (function
       | CancelRequest req as v -> Response.map (f v) @@ seqr req
       | RunInTerminalRequest req as v -> Response.map (f v) @@ seqr req
       | InitializeRequest req as v -> Response.map (f v) @@ seqr req
       | ConfigurationDoneRequest req as v -> Response.map (f v) @@ seqr req
       | LaunchRequest req as v -> Response.map (f v) @@ seqr req
       | AttachRequest req as v -> Response.map (f v) @@ seqr req
       | RestartRequest req as v -> Response.map (f v) @@ seqr req
       | DisconnectRequest req as v -> Response.map (f v) @@ seqr req
       | TerminateRequest req as v -> Response.map (f v) @@ seqr req
       | BreakpointLocationsRequest req as v -> Response.map (f v) @@ seqr req
       | SetBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | SetFunctionBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | SetExceptionBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | DataBreakpointInfoRequest req as v -> Response.map (f v) @@ seqr req
       | SetDataBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | SetInstructionBreakpointsRequest req as v -> Response.map (f v) @@ seqr req
       | ContinueRequest req as v -> Response.map (f v) @@ seqr req
       | NextRequest req as v -> Response.map (f v) @@ seqr req
       | StepInRequest req as v -> Response.map (f v) @@ seqr req
       | StepOutRequest req as v -> Response.map (f v) @@ seqr req
       | StepBackRequest req as v -> Response.map (f v) @@ seqr req
       | ReverseContinueRequest req as v -> Response.map (f v) @@ seqr req
       | RestartFrameRequest req as v -> Response.map (f v) @@ seqr req
       | GotoRequest req as v -> Response.map (f v) @@ seqr req
       | PauseRequest req as v -> Response.map (f v) @@ seqr req
       | StackTraceRequest req as v -> Response.map (f v) @@ seqr req
       | ScopesRequest req as v -> Response.map (f v) @@ seqr req
       | VariablesRequest req as v -> Response.map (f v) @@ seqr req
       | SetVariableRequest req as v -> Response.map (f v) @@ seqr req
       | SourceRequest req as v -> Response.map (f v) @@ seqr req
       | ThreadsRequest req as v -> Response.map (f v) @@ seqr req
       | TerminateThreadsRequest req as v -> Response.map (f v) @@ seqr req
       | ModulesRequest req as v -> Response.map (f v) @@ seqr req
       | LoadedSourcesRequest req as v -> Response.map (f v) @@ seqr req
       | EvaluateRequest req as v -> Response.map (f v) @@ seqr req
       | SetExpressionRequest req as v -> Response.map (f v) @@ seqr req
       | StepInTargetsRequest req as v -> Response.map (f v) @@ seqr req
       | GotoTargetsRequest req as v -> Response.map (f v) @@ seqr req
       | CompletionsRequest req as v -> Response.map (f v) @@ seqr req
       | ExceptionInfoRequest req as v -> Response.map (f v) @@ seqr req
       | ReadMemoryRequest req as v -> Response.map (f v) @@ seqr req
       | WriteMemoryRequest req as v -> Response.map (f v) @@ seqr req
       | DisassembleRequest req as v -> Response.map (f v) @@ seqr req
     )


module Event = struct

let map (type ev body pbody) :
  (ev, body, pbody) event t ->
  ((ev, body, pbody) event -> (ev, _, _) event) ->
  (ev, _, _) event t
 = fun v f ->
   Result.map (fun v -> f v) v

  let set_seqr (type ev body pbody) :
    seqr -> (ev, body, pbody) event -> (ev, body, pbody) event
    =
    let aux ev {seq; _} =
      EventMessage.set_seq ev ~seq
    in
    fun seqr -> function
      | InitializedEvent ev -> InitializedEvent (aux ev seqr)
      | StoppedEvent ev -> StoppedEvent (aux ev seqr)
      | ContinuedEvent ev -> ContinuedEvent (aux ev seqr)
      | ExitedEvent ev -> ExitedEvent (aux ev seqr)
      | TerminatedEvent ev -> TerminatedEvent (aux ev seqr)
      | ThreadEvent ev -> ThreadEvent (aux ev seqr)
      | OutputEvent ev -> OutputEvent (aux ev seqr)
      | BreakpointEvent ev -> BreakpointEvent (aux ev seqr)
      | ModuleEvent ev -> ModuleEvent (aux ev seqr)
      | LoadedSourceEvent ev -> LoadedSourceEvent (aux ev seqr)
      | ProcessEvent ev -> ProcessEvent (aux ev seqr)
      | CapabilitiesEvent ev -> CapabilitiesEvent (aux ev seqr)
      | ProgressStartEvent ev -> ProgressStartEvent (aux ev seqr)
      | ProgressUpdateEvent ev -> ProgressUpdateEvent (aux ev seqr)
      | ProgressEndEvent ev -> ProgressEndEvent (aux ev seqr)
      | InvalidatedEvent ev -> InvalidatedEvent (aux ev seqr)
      | MemoryEvent ev -> MemoryEvent (aux ev seqr)

end

let bind_response (type cmd body pbody) :
  (cmd, body, pbody) response t ->
  ((cmd, body, pbody) response -> (_, _, _) event t) ->
  (_, _, _) event t
 = fun v f ->
   let seqr resp =
     let request_seq = ResponseMessage.seq resp in
     let seq = succ request_seq in
     Event.set_seqr {seq; request_seq}
   in
   Result.bind v (function
       | ErrorResponse resp as v -> Event.map (f v) @@ seqr resp
       | CancelResponse resp as v -> Event.map (f v) @@ seqr resp
       | RunInTerminalResponse resp as v -> Event.map (f v) @@ seqr resp
       | InitializeResponse resp as v -> Event.map (f v) @@ seqr resp
       | ConfigurationDoneResponse resp as v -> Event.map (f v) @@ seqr resp
       | LaunchResponse resp as v -> Event.map (f v) @@ seqr resp
       | AttachResponse resp as v -> Event.map (f v) @@ seqr resp
       | RestartResponse resp as v -> Event.map (f v) @@ seqr resp
       | DisconnectResponse resp as v -> Event.map (f v) @@ seqr resp
       | TerminateResponse resp as v -> Event.map (f v) @@ seqr resp
       | BreakpointLocationsResponse resp as v -> Event.map (f v) @@ seqr resp
       | SetBreakpointsResponse resp as v -> Event.map (f v) @@ seqr resp
       | SetFunctionBreakpointsResponse resp as v -> Event.map (f v) @@ seqr resp
       | SetExceptionBreakpointsResponse resp as v -> Event.map (f v) @@ seqr resp
       | DataBreakpointInfoResponse resp as v -> Event.map (f v) @@ seqr resp
       | SetDataBreakpointsResponse resp as v -> Event.map (f v) @@ seqr resp
       | SetInstructionBreakpointsResponse resp as v -> Event.map (f v) @@ seqr resp
       | ContinueResponse resp as v -> Event.map (f v) @@ seqr resp
       | NextResponse resp as v -> Event.map (f v) @@ seqr resp
       | StepInResponse resp as v -> Event.map (f v) @@ seqr resp
       | StepOutResponse resp as v -> Event.map (f v) @@ seqr resp
       | StepBackResponse resp as v -> Event.map (f v) @@ seqr resp
       | ReverseContinueResponse resp as v -> Event.map (f v) @@ seqr resp
       | RestartFrameResponse resp as v -> Event.map (f v) @@ seqr resp
       | GotoResponse resp as v -> Event.map (f v) @@ seqr resp
       | PauseResponse resp as v -> Event.map (f v) @@ seqr resp
       | StackTraceResponse resp as v -> Event.map (f v) @@ seqr resp
       | ScopesResponse resp as v -> Event.map (f v) @@ seqr resp
       | VariablesResponse resp as v -> Event.map (f v) @@ seqr resp
       | SetVariableResponse resp as v -> Event.map (f v) @@ seqr resp
       | SourceResponse resp as v -> Event.map (f v) @@ seqr resp
       | ThreadsResponse resp as v -> Event.map (f v) @@ seqr resp
       | TerminateThreadsResponse resp as v -> Event.map (f v) @@ seqr resp
       | ModulesResponse resp as v -> Event.map (f v) @@ seqr resp
       | LoadedSourcesResponse resp as v -> Event.map (f v) @@ seqr resp
       | EvaluateResponse resp as v -> Event.map (f v) @@ seqr resp
       | SetExpressionResponse resp as v -> Event.map (f v) @@ seqr resp
       | StepInTargetsResponse resp as v -> Event.map (f v) @@ seqr resp
       | GotoTargetsResponse resp as v -> Event.map (f v) @@ seqr resp
       | CompletionsResponse resp as v -> Event.map (f v) @@ seqr resp
       | ExceptionInfoResponse resp as v -> Event.map (f v) @@ seqr resp
       | ReadMemoryResponse resp as v -> Event.map (f v) @@ seqr resp
       | WriteMemoryResponse resp as v -> Event.map (f v) @@ seqr resp
       | DisassembleResponse resp as v -> Event.map (f v) @@ seqr resp
     )

let bind_event (type ev body pbody) :
  (ev, body, pbody) event t ->
  ((ev, body, pbody) event -> (_, _, _) event t) ->
  (_, _, _) event t
 = fun v f ->
   let seqr ev =
     let request_seq = EventMessage.seq ev in
     let seq = succ request_seq in
     Event.set_seqr {seq; request_seq}
   in
   Result.bind v (function
       | InitializedEvent ev as v -> Event.map (f v) @@ seqr ev
       | StoppedEvent ev as v -> Event.map (f v) @@ seqr ev
       | ContinuedEvent ev as v -> Event.map (f v) @@ seqr ev
       | ExitedEvent ev as v -> Event.map (f v) @@ seqr ev
       | TerminatedEvent ev as v -> Event.map (f v) @@ seqr ev
       | ThreadEvent ev as v -> Event.map (f v) @@ seqr ev
       | OutputEvent ev as v -> Event.map (f v) @@ seqr ev
       | BreakpointEvent ev as v -> Event.map (f v) @@ seqr ev
       | ModuleEvent ev as v -> Event.map (f v) @@ seqr ev
       | LoadedSourceEvent ev as v -> Event.map (f v) @@ seqr ev
       | ProcessEvent ev as v -> Event.map (f v) @@ seqr ev
       | CapabilitiesEvent ev as v -> Event.map (f v) @@ seqr ev
       | ProgressStartEvent ev as v -> Event.map (f v) @@ seqr ev
       | ProgressUpdateEvent ev as v -> Event.map (f v) @@ seqr ev
       | ProgressEndEvent ev as v -> Event.map (f v) @@ seqr ev
       | InvalidatedEvent ev as v -> Event.map (f v) @@ seqr ev
       | MemoryEvent ev as v -> Event.map (f v) @@ seqr ev
     )
