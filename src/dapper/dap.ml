(* main module for everything to do with messages/event enums/command enums *)
module Commands = Dap_commands
module Events = Dap_events
module Js_msg = Dap_js_msg
module Config = Dap_config

(* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
let seq = -1
let request_seq = -1

include Dap_message.Data

module Request = struct
  include Dap_message.Request
  module Message = Dap_message.RequestMessage

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

end

module Response = struct
  include Dap_message.Response
  module Message = Dap_message.ResponseMessage

  let default_response_req ?(success = true) command body =
    Message.make ~seq ~request_seq ~success ~command ~body ()

  let default_response_opt ?(success = true) command body =
    Message.make_opt ~seq ~request_seq ~success ~command ~body ()

  type _ expr =
    | Val : 'msg t -> 'msg expr
    | Map : ('msg -> 'b) expr * 'msg expr -> 'b expr

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

end

module Event = struct
  include Dap_message.Event
  module Message = Dap_message.EventMessage

  let default_event_req event body =
    Message.make ~seq ~event ~body ()

  let default_event_opt event body =
    Message.make_opt ~seq ~event ~body ()

  type _ expr =
    | Val : 'msg t -> 'msg expr
    | Map : ('msg -> 'b) expr * 'msg expr -> 'b expr

  let rec eval : type msg. msg expr -> msg = function
    | Val (Fmap f) -> f
    | Val (InitializedEvent msg) -> msg
    | Val (StoppedEvent msg) -> msg
    | Val (ContinuedEvent msg) -> msg
    | Val (ExitedEvent msg) -> msg
    | Val (TerminatedEvent msg) -> msg
    | Val (ThreadEvent msg) -> msg
    | Val (OutputEvent msg) -> msg
    | Val (BreakpointEvent msg) -> msg
    | Val (ModuleEvent msg) -> msg
    | Val (LoadedSourceEvent msg) -> msg
    | Val (ProcessEvent msg) -> msg
    | Val (CapabilitiesEvent msg) -> msg
    | Val (ProgressStartEvent msg) -> msg
    | Val (ProgressUpdateEvent msg) -> msg
    | Val (ProgressEndEvent msg) -> msg
    | Val (InvalidatedEvent msg) -> msg
    | Val (MemoryEvent msg) -> msg
    | Map (f, v) -> let f' = (eval f) and v' = eval v in (f' v')

end

let default_response_error e =
  let open Dap_message.Data in
  let id = Hashtbl.hash e in
  let variables = `O [("error", `String e)] in
  let error = Message.make ~id ~format:"{error}" ~variables () in
  let body = ErrorResponse_body.make ~error () in
  Response.Message.make ~seq ~request_seq ~success:false ~command:Dap_commands.error ~body ()
