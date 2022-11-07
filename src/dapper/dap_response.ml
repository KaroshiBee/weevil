include Dap_message.Response
module Message = Dap_message.ResponseMessage

let default_response_req ?(success = true) command body =
  Message.make ~seq:Dap_base.Seqr.not_set ~request_seq:Dap_base.Seqr.not_set ~success ~command ~body ()

let default_response_opt ?(success = true) command body =
  Message.make_opt ~seq:Dap_base.Seqr.not_set ~request_seq:Dap_base.Seqr.not_set ~success ~command ~body ()

type _ expr =
  | Val : 'msg t -> 'msg expr
  | Map : ('msg -> 'b) expr * 'msg expr -> 'b expr

let fmap_ f = Fmap f
let val_ x = Val x
let map_ (f, x) = Map (f, x)

let rec eval : type msg. msg expr -> msg = function
  | Val (Fmap f) -> f
  | Val (ErrorResponse msg) -> msg
  | Val (CancelResponse msg) -> msg
  | Val (RunInTerminalResponse msg) -> msg
  | Val (InitializeResponse msg) -> msg
  | Val (ConfigurationDoneResponse msg) -> msg
  | Val (LaunchResponse msg) -> msg
  | Val (AttachResponse msg) -> msg
  | Val (RestartResponse msg) -> msg
  | Val (DisconnectResponse msg) -> msg
  | Val (TerminateResponse msg) -> msg
  | Val (BreakpointLocationsResponse msg) -> msg
  | Val (SetBreakpointsResponse msg) -> msg
  | Val (SetFunctionBreakpointsResponse msg) -> msg
  | Val (SetExceptionBreakpointsResponse msg) -> msg
  | Val (DataBreakpointInfoResponse msg) -> msg
  | Val (SetDataBreakpointsResponse msg) -> msg
  | Val (SetInstructionBreakpointsResponse msg) -> msg
  | Val (ContinueResponse msg) -> msg
  | Val (NextResponse msg) -> msg
  | Val (StepInResponse msg) -> msg
  | Val (StepOutResponse msg) -> msg
  | Val (StepBackResponse msg) -> msg
  | Val (ReverseContinueResponse msg) -> msg
  | Val (RestartFrameResponse msg) -> msg
  | Val (GotoResponse msg) -> msg
  | Val (PauseResponse msg) -> msg
  | Val (StackTraceResponse msg) -> msg
  | Val (ScopesResponse msg) -> msg
  | Val (VariablesResponse msg) -> msg
  | Val (SetVariableResponse msg) -> msg
  | Val (SourceResponse msg) -> msg
  | Val (ThreadsResponse msg) -> msg
  | Val (TerminateThreadsResponse msg) -> msg
  | Val (ModulesResponse msg) -> msg
  | Val (LoadedSourcesResponse msg) -> msg
  | Val (EvaluateResponse msg) -> msg
  | Val (SetExpressionResponse msg) -> msg
  | Val (StepInTargetsResponse msg) -> msg
  | Val (GotoTargetsResponse msg) -> msg
  | Val (CompletionsResponse msg) -> msg
  | Val (ExceptionInfoResponse msg) -> msg
  | Val (ReadMemoryResponse msg) -> msg
  | Val (WriteMemoryResponse msg) -> msg
  | Val (DisassembleResponse msg) -> msg
  | Map (f, v) -> let f' = (eval f) and v' = eval v in (f' v')
