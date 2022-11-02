open Dap_message_exi

let default_response_req ?(success = true) command body =
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  let request_seq = -1 in
  ResponseMessage.make ~seq ~request_seq ~success ~command ~body ()

let default_response_opt ?(success = true) command body =
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  let request_seq = -1 in
  ResponseMessage.make_opt ~seq ~request_seq ~success ~command ~body ()

let default_response_error e =
  let id = Hashtbl.hash e in
  let variables = `O [("error", `String e)] in
  let error = Message.make ~id ~format:"{error}" ~variables () in
  let body = ErrorResponse_body.make ~error () in
  default_response_req ~success:false Dap_commands.error body

let default_event_req event body =
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  EventMessage.make ~seq ~event ~body ()

let default_event_opt event body =
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  EventMessage.make_opt ~seq ~event ~body ()

module RequestUtils = struct
  let get_seq = function
    | CancelRequest req -> RequestMessage.seq req
    | RunInTerminalRequest req -> RequestMessage.seq req
    | InitializeRequest req -> RequestMessage.seq req
    | ConfigurationDoneRequest req -> RequestMessage.seq req
    | LaunchRequest req -> RequestMessage.seq req
    | AttachRequest req -> RequestMessage.seq req
    | RestartRequest req -> RequestMessage.seq req
    | DisconnectRequest req -> RequestMessage.seq req
    | TerminateRequest req -> RequestMessage.seq req
    | BreakpointLocationsRequest req -> RequestMessage.seq req
    | SetBreakpointsRequest req -> RequestMessage.seq req
    | SetFunctionBreakpointsRequest req -> RequestMessage.seq req
    | SetExceptionBreakpointsRequest req -> RequestMessage.seq req
    | DataBreakpointInfoRequest req -> RequestMessage.seq req
    | SetDataBreakpointsRequest req -> RequestMessage.seq req
    | SetInstructionBreakpointsRequest req -> RequestMessage.seq req
    | ContinueRequest req -> RequestMessage.seq req
    | NextRequest req -> RequestMessage.seq req
    | StepInRequest req -> RequestMessage.seq req
    | StepOutRequest req -> RequestMessage.seq req
    | StepBackRequest req -> RequestMessage.seq req
    | ReverseContinueRequest req -> RequestMessage.seq req
    | RestartFrameRequest req -> RequestMessage.seq req
    | GotoRequest req -> RequestMessage.seq req
    | PauseRequest req -> RequestMessage.seq req
    | StackTraceRequest req -> RequestMessage.seq req
    | ScopesRequest req -> RequestMessage.seq req
    | VariablesRequest req -> RequestMessage.seq req
    | SetVariableRequest req -> RequestMessage.seq req
    | SourceRequest req -> RequestMessage.seq req
    | ThreadsRequest req -> RequestMessage.seq req
    | TerminateThreadsRequest req -> RequestMessage.seq req
    | ModulesRequest req -> RequestMessage.seq req
    | LoadedSourcesRequest req -> RequestMessage.seq req
    | EvaluateRequest req -> RequestMessage.seq req
    | SetExpressionRequest req -> RequestMessage.seq req
    | StepInTargetsRequest req -> RequestMessage.seq req
    | GotoTargetsRequest req -> RequestMessage.seq req
    | CompletionsRequest req -> RequestMessage.seq req
    | ExceptionInfoRequest req -> RequestMessage.seq req
    | ReadMemoryRequest req -> RequestMessage.seq req
    | WriteMemoryRequest req -> RequestMessage.seq req
    | DisassembleRequest req -> RequestMessage.seq req
end

module ResponseUtils = struct
  let get_seq = function
    | ErrorResponse resp -> ResponseMessage.seq resp
    | CancelResponse resp -> ResponseMessage.seq resp
    | RunInTerminalResponse resp -> ResponseMessage.seq resp
    | InitializeResponse resp -> ResponseMessage.seq resp
    | ConfigurationDoneResponse resp -> ResponseMessage.seq resp
    | LaunchResponse resp -> ResponseMessage.seq resp
    | AttachResponse resp -> ResponseMessage.seq resp
    | RestartResponse resp -> ResponseMessage.seq resp
    | DisconnectResponse resp -> ResponseMessage.seq resp
    | TerminateResponse resp -> ResponseMessage.seq resp
    | BreakpointLocationsResponse resp -> ResponseMessage.seq resp
    | SetBreakpointsResponse resp -> ResponseMessage.seq resp
    | SetFunctionBreakpointsResponse resp -> ResponseMessage.seq resp
    | SetExceptionBreakpointsResponse resp -> ResponseMessage.seq resp
    | DataBreakpointInfoResponse resp -> ResponseMessage.seq resp
    | SetDataBreakpointsResponse resp -> ResponseMessage.seq resp
    | SetInstructionBreakpointsResponse resp -> ResponseMessage.seq resp
    | ContinueResponse resp -> ResponseMessage.seq resp
    | NextResponse resp -> ResponseMessage.seq resp
    | StepInResponse resp -> ResponseMessage.seq resp
    | StepOutResponse resp -> ResponseMessage.seq resp
    | StepBackResponse resp -> ResponseMessage.seq resp
    | ReverseContinueResponse resp -> ResponseMessage.seq resp
    | RestartFrameResponse resp -> ResponseMessage.seq resp
    | GotoResponse resp -> ResponseMessage.seq resp
    | PauseResponse resp -> ResponseMessage.seq resp
    | StackTraceResponse resp -> ResponseMessage.seq resp
    | ScopesResponse resp -> ResponseMessage.seq resp
    | VariablesResponse resp -> ResponseMessage.seq resp
    | SetVariableResponse resp -> ResponseMessage.seq resp
    | SourceResponse resp -> ResponseMessage.seq resp
    | ThreadsResponse resp -> ResponseMessage.seq resp
    | TerminateThreadsResponse resp -> ResponseMessage.seq resp
    | ModulesResponse resp -> ResponseMessage.seq resp
    | LoadedSourcesResponse resp -> ResponseMessage.seq resp
    | EvaluateResponse resp -> ResponseMessage.seq resp
    | SetExpressionResponse resp -> ResponseMessage.seq resp
    | StepInTargetsResponse resp -> ResponseMessage.seq resp
    | GotoTargetsResponse resp -> ResponseMessage.seq resp
    | CompletionsResponse resp -> ResponseMessage.seq resp
    | ExceptionInfoResponse resp -> ResponseMessage.seq resp
    | ReadMemoryResponse resp -> ResponseMessage.seq resp
    | WriteMemoryResponse resp -> ResponseMessage.seq resp
    | DisassembleResponse resp -> ResponseMessage.seq resp

  let set_sequencing ~seq ~request_seq = function
    | ErrorResponse resp ->
        ErrorResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | CancelResponse resp ->
        CancelResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | RunInTerminalResponse resp ->
        RunInTerminalResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | InitializeResponse resp ->
        InitializeResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | ConfigurationDoneResponse resp ->
        ConfigurationDoneResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | LaunchResponse resp ->
        LaunchResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | AttachResponse resp ->
        AttachResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | RestartResponse resp ->
        RestartResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | DisconnectResponse resp ->
        DisconnectResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | TerminateResponse resp ->
        TerminateResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | BreakpointLocationsResponse resp ->
        BreakpointLocationsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | SetBreakpointsResponse resp ->
        SetBreakpointsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | SetFunctionBreakpointsResponse resp ->
        SetFunctionBreakpointsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | SetExceptionBreakpointsResponse resp ->
        SetExceptionBreakpointsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | DataBreakpointInfoResponse resp ->
        DataBreakpointInfoResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | SetDataBreakpointsResponse resp ->
        SetDataBreakpointsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | SetInstructionBreakpointsResponse resp ->
        SetInstructionBreakpointsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | ContinueResponse resp ->
        ContinueResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | NextResponse resp ->
        NextResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | StepInResponse resp ->
        StepInResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | StepOutResponse resp ->
        StepOutResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | StepBackResponse resp ->
        StepBackResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | ReverseContinueResponse resp ->
        ReverseContinueResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | RestartFrameResponse resp ->
        RestartFrameResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | GotoResponse resp ->
        GotoResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | PauseResponse resp ->
        PauseResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | StackTraceResponse resp ->
        StackTraceResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | ScopesResponse resp ->
        ScopesResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | VariablesResponse resp ->
        VariablesResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | SetVariableResponse resp ->
        SetVariableResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | SourceResponse resp ->
        SourceResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | ThreadsResponse resp ->
        ThreadsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | TerminateThreadsResponse resp ->
        TerminateThreadsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | ModulesResponse resp ->
        ModulesResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | LoadedSourcesResponse resp ->
        LoadedSourcesResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | EvaluateResponse resp ->
        EvaluateResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | SetExpressionResponse resp ->
        SetExpressionResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | StepInTargetsResponse resp ->
        StepInTargetsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | GotoTargetsResponse resp ->
        GotoTargetsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | CompletionsResponse resp ->
        CompletionsResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | ExceptionInfoResponse resp ->
        ExceptionInfoResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | ReadMemoryResponse resp ->
        ReadMemoryResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | WriteMemoryResponse resp ->
        WriteMemoryResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
    | DisassembleResponse resp ->
        DisassembleResponse
          (let resp = ResponseMessage.set_seq resp ~seq in
           ResponseMessage.set_request_seq resp ~request_seq)
end

module EventUtils = struct
  let get_seq = function
    | InitializedEvent ev -> EventMessage.seq ev
    | StoppedEvent ev -> EventMessage.seq ev
    | ContinuedEvent ev -> EventMessage.seq ev
    | ExitedEvent ev -> EventMessage.seq ev
    | TerminatedEvent ev -> EventMessage.seq ev
    | ThreadEvent ev -> EventMessage.seq ev
    | OutputEvent ev -> EventMessage.seq ev
    | BreakpointEvent ev -> EventMessage.seq ev
    | ModuleEvent ev -> EventMessage.seq ev
    | LoadedSourceEvent ev -> EventMessage.seq ev
    | ProcessEvent ev -> EventMessage.seq ev
    | CapabilitiesEvent ev -> EventMessage.seq ev
    | ProgressStartEvent ev -> EventMessage.seq ev
    | ProgressUpdateEvent ev -> EventMessage.seq ev
    | ProgressEndEvent ev -> EventMessage.seq ev
    | InvalidatedEvent ev -> EventMessage.seq ev
    | MemoryEvent ev -> EventMessage.seq ev

  let set_sequencing ~seq = function
    | InitializedEvent ev -> InitializedEvent (EventMessage.set_seq ev ~seq)
    | StoppedEvent ev -> StoppedEvent (EventMessage.set_seq ev ~seq)
    | ContinuedEvent ev -> ContinuedEvent (EventMessage.set_seq ev ~seq)
    | ExitedEvent ev -> ExitedEvent (EventMessage.set_seq ev ~seq)
    | TerminatedEvent ev -> TerminatedEvent (EventMessage.set_seq ev ~seq)
    | ThreadEvent ev -> ThreadEvent (EventMessage.set_seq ev ~seq)
    | OutputEvent ev -> OutputEvent (EventMessage.set_seq ev ~seq)
    | BreakpointEvent ev -> BreakpointEvent (EventMessage.set_seq ev ~seq)
    | ModuleEvent ev -> ModuleEvent (EventMessage.set_seq ev ~seq)
    | LoadedSourceEvent ev -> LoadedSourceEvent (EventMessage.set_seq ev ~seq)
    | ProcessEvent ev -> ProcessEvent (EventMessage.set_seq ev ~seq)
    | CapabilitiesEvent ev -> CapabilitiesEvent (EventMessage.set_seq ev ~seq)
    | ProgressStartEvent ev -> ProgressStartEvent (EventMessage.set_seq ev ~seq)
    | ProgressUpdateEvent ev -> ProgressUpdateEvent (EventMessage.set_seq ev ~seq)
    | ProgressEndEvent ev -> ProgressEndEvent (EventMessage.set_seq ev ~seq)
    | InvalidatedEvent ev -> InvalidatedEvent (EventMessage.set_seq ev ~seq)
    | MemoryEvent ev -> MemoryEvent (EventMessage.set_seq ev ~seq)

end
