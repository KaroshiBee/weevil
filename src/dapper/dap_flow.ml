open Dap_t
open Dap_message

type 'a t = ('a, string) Result.t
type seqr = {seq:int; request_seq:int}

let response_map (type cmd body pbody) :
  (cmd, body, pbody) response t ->
  ((cmd, body, pbody) response -> (cmd, _, _) response) ->
  (cmd, _, _) response t
 = fun v f ->
   Result.map (fun v -> f v) v

let set_seqr_response (type cmd body pbody) :
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


let from_request :
  ('command, 'args, 'pargs) request ->
  ('command, 'args, 'pargs) request t
    = Result.ok


let on_request (type cmd args pargs) :
  (cmd, args, pargs) request t ->
  ((cmd, args, pargs) request -> (cmd, _, _) response t) ->
  (cmd, _, _) response t
 = fun v f ->
   let seqr req =
     let request_seq = RequestMessage.seq req in
     let seq = succ request_seq in
     set_seqr_response {seq; request_seq}
   in
   Result.bind v (function
       | CancelRequest req as v -> response_map (f v) @@ seqr req
       | RunInTerminalRequest req as v -> response_map (f v) @@ seqr req
       | InitializeRequest req as v -> response_map (f v) @@ seqr req
       | ConfigurationDoneRequest req as v -> response_map (f v) @@ seqr req
       | LaunchRequest req as v -> response_map (f v) @@ seqr req
       | AttachRequest req as v -> response_map (f v) @@ seqr req
       | RestartRequest req as v -> response_map (f v) @@ seqr req
       | DisconnectRequest req as v -> response_map (f v) @@ seqr req
       | TerminateRequest req as v -> response_map (f v) @@ seqr req
       | BreakpointLocationsRequest req as v -> response_map (f v) @@ seqr req
       | SetBreakpointsRequest req as v -> response_map (f v) @@ seqr req
       | SetFunctionBreakpointsRequest req as v -> response_map (f v) @@ seqr req
       | SetExceptionBreakpointsRequest req as v -> response_map (f v) @@ seqr req
       | DataBreakpointInfoRequest req as v -> response_map (f v) @@ seqr req
       | SetDataBreakpointsRequest req as v -> response_map (f v) @@ seqr req
       | SetInstructionBreakpointsRequest req as v -> response_map (f v) @@ seqr req
       | ContinueRequest req as v -> response_map (f v) @@ seqr req
       | NextRequest req as v -> response_map (f v) @@ seqr req
       | StepInRequest req as v -> response_map (f v) @@ seqr req
       | StepOutRequest req as v -> response_map (f v) @@ seqr req
       | StepBackRequest req as v -> response_map (f v) @@ seqr req
       | ReverseContinueRequest req as v -> response_map (f v) @@ seqr req
       | RestartFrameRequest req as v -> response_map (f v) @@ seqr req
       | GotoRequest req as v -> response_map (f v) @@ seqr req
       | PauseRequest req as v -> response_map (f v) @@ seqr req
       | StackTraceRequest req as v -> response_map (f v) @@ seqr req
       | ScopesRequest req as v -> response_map (f v) @@ seqr req
       | VariablesRequest req as v -> response_map (f v) @@ seqr req
       | SetVariableRequest req as v -> response_map (f v) @@ seqr req
       | SourceRequest req as v -> response_map (f v) @@ seqr req
       | ThreadsRequest req as v -> response_map (f v) @@ seqr req
       | TerminateThreadsRequest req as v -> response_map (f v) @@ seqr req
       | ModulesRequest req as v -> response_map (f v) @@ seqr req
       | LoadedSourcesRequest req as v -> response_map (f v) @@ seqr req
       | EvaluateRequest req as v -> response_map (f v) @@ seqr req
       | SetExpressionRequest req as v -> response_map (f v) @@ seqr req
       | StepInTargetsRequest req as v -> response_map (f v) @@ seqr req
       | GotoTargetsRequest req as v -> response_map (f v) @@ seqr req
       | CompletionsRequest req as v -> response_map (f v) @@ seqr req
       | ExceptionInfoRequest req as v -> response_map (f v) @@ seqr req
       | ReadMemoryRequest req as v -> response_map (f v) @@ seqr req
       | WriteMemoryRequest req as v -> response_map (f v) @@ seqr req
       | DisassembleRequest req as v -> response_map (f v) @@ seqr req
  )

let event_map (type ev body pbody) :
  (ev, body, pbody) event t ->
  ((ev, body, pbody) event -> (ev, _, _) event) ->
  (ev, _, _) event t
 = fun v f ->
   Result.map (fun v -> f v) v

let set_seqr_event (type ev body pbody) :
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


let on_response (type cmd body pbody) :
  (cmd, body, pbody) response t ->
  ((cmd, body, pbody) response -> (_, _, _) event t) ->
  (_, _, _) event t
 = fun v f ->
   let seqr resp =
     let request_seq = ResponseMessage.seq resp in
     let seq = succ request_seq in
     set_seqr_event {seq; request_seq}
   in
   Result.bind v (function
       | ErrorResponse resp as v -> event_map (f v) @@ seqr resp
       | CancelResponse resp as v -> event_map (f v) @@ seqr resp
       | RunInTerminalResponse resp as v -> event_map (f v) @@ seqr resp
       | InitializeResponse resp as v -> event_map (f v) @@ seqr resp
       | ConfigurationDoneResponse resp as v -> event_map (f v) @@ seqr resp
       | LaunchResponse resp as v -> event_map (f v) @@ seqr resp
       | AttachResponse resp as v -> event_map (f v) @@ seqr resp
       | RestartResponse resp as v -> event_map (f v) @@ seqr resp
       | DisconnectResponse resp as v -> event_map (f v) @@ seqr resp
       | TerminateResponse resp as v -> event_map (f v) @@ seqr resp
       | BreakpointLocationsResponse resp as v -> event_map (f v) @@ seqr resp
       | SetBreakpointsResponse resp as v -> event_map (f v) @@ seqr resp
       | SetFunctionBreakpointsResponse resp as v -> event_map (f v) @@ seqr resp
       | SetExceptionBreakpointsResponse resp as v -> event_map (f v) @@ seqr resp
       | DataBreakpointInfoResponse resp as v -> event_map (f v) @@ seqr resp
       | SetDataBreakpointsResponse resp as v -> event_map (f v) @@ seqr resp
       | SetInstructionBreakpointsResponse resp as v -> event_map (f v) @@ seqr resp
       | ContinueResponse resp as v -> event_map (f v) @@ seqr resp
       | NextResponse resp as v -> event_map (f v) @@ seqr resp
       | StepInResponse resp as v -> event_map (f v) @@ seqr resp
       | StepOutResponse resp as v -> event_map (f v) @@ seqr resp
       | StepBackResponse resp as v -> event_map (f v) @@ seqr resp
       | ReverseContinueResponse resp as v -> event_map (f v) @@ seqr resp
       | RestartFrameResponse resp as v -> event_map (f v) @@ seqr resp
       | GotoResponse resp as v -> event_map (f v) @@ seqr resp
       | PauseResponse resp as v -> event_map (f v) @@ seqr resp
       | StackTraceResponse resp as v -> event_map (f v) @@ seqr resp
       | ScopesResponse resp as v -> event_map (f v) @@ seqr resp
       | VariablesResponse resp as v -> event_map (f v) @@ seqr resp
       | SetVariableResponse resp as v -> event_map (f v) @@ seqr resp
       | SourceResponse resp as v -> event_map (f v) @@ seqr resp
       | ThreadsResponse resp as v -> event_map (f v) @@ seqr resp
       | TerminateThreadsResponse resp as v -> event_map (f v) @@ seqr resp
       | ModulesResponse resp as v -> event_map (f v) @@ seqr resp
       | LoadedSourcesResponse resp as v -> event_map (f v) @@ seqr resp
       | EvaluateResponse resp as v -> event_map (f v) @@ seqr resp
       | SetExpressionResponse resp as v -> event_map (f v) @@ seqr resp
       | StepInTargetsResponse resp as v -> event_map (f v) @@ seqr resp
       | GotoTargetsResponse resp as v -> event_map (f v) @@ seqr resp
       | CompletionsResponse resp as v -> event_map (f v) @@ seqr resp
       | ExceptionInfoResponse resp as v -> event_map (f v) @@ seqr resp
       | ReadMemoryResponse resp as v -> event_map (f v) @@ seqr resp
       | WriteMemoryResponse resp as v -> event_map (f v) @@ seqr resp
       | DisassembleResponse resp as v -> event_map (f v) @@ seqr resp
     )


let raise_event (type ev body pbody) :
  (ev, body, pbody) event t ->
  ((ev, body, pbody) event -> (_, _, _) event t) ->
  (_, _, _) event t
 = fun v f ->
   let seqr ev =
     let request_seq = EventMessage.seq ev in
     let seq = succ request_seq in
     set_seqr_event {seq; request_seq}
   in
   Result.bind v (function
       | InitializedEvent ev as v -> event_map (f v) @@ seqr ev
       | StoppedEvent ev as v -> event_map (f v) @@ seqr ev
       | ContinuedEvent ev as v -> event_map (f v) @@ seqr ev
       | ExitedEvent ev as v -> event_map (f v) @@ seqr ev
       | TerminatedEvent ev as v -> event_map (f v) @@ seqr ev
       | ThreadEvent ev as v -> event_map (f v) @@ seqr ev
       | OutputEvent ev as v -> event_map (f v) @@ seqr ev
       | BreakpointEvent ev as v -> event_map (f v) @@ seqr ev
       | ModuleEvent ev as v -> event_map (f v) @@ seqr ev
       | LoadedSourceEvent ev as v -> event_map (f v) @@ seqr ev
       | ProcessEvent ev as v -> event_map (f v) @@ seqr ev
       | CapabilitiesEvent ev as v -> event_map (f v) @@ seqr ev
       | ProgressStartEvent ev as v -> event_map (f v) @@ seqr ev
       | ProgressUpdateEvent ev as v -> event_map (f v) @@ seqr ev
       | ProgressEndEvent ev as v -> event_map (f v) @@ seqr ev
       | InvalidatedEvent ev as v -> event_map (f v) @@ seqr ev
       | MemoryEvent ev as v -> event_map (f v) @@ seqr ev
     )
