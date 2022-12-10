include Dap_messages.Request
module Message = Dap_messages.RequestMessage

type _ expr =
  | Val : 'msg t -> 'msg expr
  | Map : ('msg -> 'b) expr * 'msg expr -> 'b expr

let rec eval : type msg. msg expr -> msg = function
  | Val (Fmap f) -> f
  | Val (CancelRequest msg) -> msg
  | Val (RunInTerminalRequest msg) -> msg
  | Val (InitializeRequest msg) -> msg
  | Val (ConfigurationDoneRequest msg) -> msg
  | Val (LaunchRequest msg) -> msg
  | Val (AttachRequest msg) -> msg
  | Val (RestartRequest msg) -> msg
  | Val (DisconnectRequest msg) -> msg
  | Val (TerminateRequest msg) -> msg
  | Val (BreakpointLocationsRequest msg) -> msg
  | Val (SetBreakpointsRequest msg) -> msg
  | Val (SetFunctionBreakpointsRequest msg) -> msg
  | Val (SetExceptionBreakpointsRequest msg) -> msg
  | Val (DataBreakpointInfoRequest msg) -> msg
  | Val (SetDataBreakpointsRequest msg) -> msg
  | Val (SetInstructionBreakpointsRequest msg) -> msg
  | Val (ContinueRequest msg) -> msg
  | Val (NextRequest msg) -> msg
  | Val (StepInRequest msg) -> msg
  | Val (StepOutRequest msg) -> msg
  | Val (StepBackRequest msg) -> msg
  | Val (ReverseContinueRequest msg) -> msg
  | Val (RestartFrameRequest msg) -> msg
  | Val (GotoRequest msg) -> msg
  | Val (PauseRequest msg) -> msg
  | Val (StackTraceRequest msg) -> msg
  | Val (ScopesRequest msg) -> msg
  | Val (VariablesRequest msg) -> msg
  | Val (SetVariableRequest msg) -> msg
  | Val (SourceRequest msg) -> msg
  | Val (ThreadsRequest msg) -> msg
  | Val (TerminateThreadsRequest msg) -> msg
  | Val (ModulesRequest msg) -> msg
  | Val (LoadedSourcesRequest msg) -> msg
  | Val (EvaluateRequest msg) -> msg
  | Val (SetExpressionRequest msg) -> msg
  | Val (StepInTargetsRequest msg) -> msg
  | Val (GotoTargetsRequest msg) -> msg
  | Val (CompletionsRequest msg) -> msg
  | Val (ExceptionInfoRequest msg) -> msg
  | Val (ReadMemoryRequest msg) -> msg
  | Val (WriteMemoryRequest msg) -> msg
  | Val (DisassembleRequest msg) -> msg
  | Map (f, v) -> let f' = (eval f) and v' = eval v in (f' v')

let map_f ~f x = Map (Val (Fmap f), Val x)

let map2_f ~f x y =
  let f' x' = fun y' -> f x' y' in
  let f' = eval @@ map_f ~f:f' x in
  map_f ~f:f' y

let equal ~equal_f = map2_f ~f:equal_f

let extract x = eval @@ map_f ~f:(fun x' -> x') x
