open Dap_message_exi

module Helper = struct
  type _ t = Resp : response -> response t | Incr : (response -> response) t
  (* | App : ('b -> 'a) t * 'b t -> 'a t *)

  let eval : type r. r t -> r = function
    | Resp req -> req
    | Incr -> (
        function
        | LaunchResponse r ->
            let seq = 1 + ResponseMessage.request_seq r in
            ResponseMessage.set_seq r ~seq |> launchResponse
        | CompletionsResponse r ->
            let seq = 1 + ResponseMessage.request_seq r in
            ResponseMessage.set_seq r ~seq |> completionsResponse
        | _ -> failwith "TODO")
  (* | App (f, r) -> (eval f) (eval r) *)

  (* let thing r = eval @@ Incr @@ Resp r *)

  (* let thing = let req = eval @@ *)
  (* let setter f req = *)
  (*   let request_seq = RequestMessage.seq req in *)
  (*   let seq = 1 + request_seq in *)
  (*   let setter_resp msg = *)
  (*     let msg = ResponseMessage.set_request_seq msg ~request_seq in *)
  (*     let msg = ResponseMessage.set_seq msg ~seq in *)
  (*     msg *)
  (*   in *)
  (*   setter_resp (f req) *)

  (* let map = fun (type cmd args pargs) (f : (cmd, args, pargs) RequestMessage.t -> (cmd, args, pargs) RequestMessage.t) -> function *)
  (*   | CancelRequest req -> CancelRequest (f req) *)
  (*   | RunInTerminalRequest req -> RunInTerminalRequest (f req) *)
  (*   | InitializeRequest req -> InitializeRequest (f req) *)
  (*   | ConfigurationDoneRequest req -> ConfigurationDoneRequest (f req) *)
  (*   | LaunchRequest req -> LaunchRequest (f req) *)
  (*   | AttachRequest req -> AttachRequest (f req) *)
  (*   | RestartRequest req -> RestartRequest (f req) *)
  (*   | DisconnectRequest req -> DisconnectRequest (f req) *)
  (*   | TerminateRequest req -> TerminateRequest (f req) *)
  (*   | BreakpointLocationsRequest req -> BreakpointLocationsRequest (f req) *)
  (*   | SetBreakpointsRequest req -> SetBreakpointsRequest (f req) *)
  (*   | SetFunctionBreakpointsRequest req -> SetFunctionBreakpointsRequest (f req) *)
  (*   | SetExceptionBreakpointsRequest req -> SetExceptionBreakpointsRequest (f req) *)
  (*   | DataBreakpointInfoRequest req -> DataBreakpointInfoRequest (f req) *)
  (*   | SetDataBreakpointsRequest req -> SetDataBreakpointsRequest (f req) *)
  (*   | SetInstructionBreakpointsRequest req -> SetInstructionBreakpointsRequest (f req) *)
  (*   | ContinueRequest req -> ContinueRequest (f req) *)
  (*   | NextRequest req -> NextRequest (f req) *)
  (*   | StepInRequest req -> StepInRequest (f req) *)
  (*   | StepOutRequest req -> StepOutRequest (f req) *)
  (*   | StepBackRequest req -> StepBackRequest (f req) *)
  (*   | ReverseContinueRequest req -> ReverseContinueRequest (f req) *)
  (*   | RestartFrameRequest req -> RestartFrameRequest (f req) *)
  (*   | GotoRequest req -> GotoRequest (f req) *)
  (*   | PauseRequest req -> PauseRequest (f req) *)
  (*   | StackTraceRequest req -> StackTraceRequest (f req) *)
  (*   | ScopesRequest req -> ScopesRequest (f req) *)
  (*   | VariablesRequest req -> VariablesRequest (f req) *)
  (*   | SetVariableRequest req -> SetVariableRequest (f req) *)
  (*   | SourceRequest req -> SourceRequest (f req) *)
  (*   | ThreadsRequest req -> ThreadsRequest (f req) *)
  (*   | TerminateThreadsRequest req -> TerminateThreadsRequest (f req) *)
  (*   | ModulesRequest req -> ModulesRequest (f req) *)
  (*   | LoadedSourcesRequest req -> LoadedSourcesRequest (f req) *)
  (*   | EvaluateRequest req -> EvaluateRequest (f req) *)
  (*   | SetExpressionRequest req -> SetExpressionRequest (f req) *)
  (*   | StepInTargetsRequest req -> StepInTargetsRequest (f req) *)
  (*   | GotoTargetsRequest req -> GotoTargetsRequest (f req) *)
  (*   | CompletionsRequest req -> CompletionsRequest (f req) *)
  (*   | ExceptionInfoRequest req -> ExceptionInfoRequest (f req) *)
  (*   | ReadMemoryRequest req -> ReadMemoryRequest (f req) *)
  (*   | WriteMemoryRequest req -> WriteMemoryRequest (f req) *)
  (*   | DisassembleRequest req -> DisassembleRequest (f req) *)
end

module type Request_Response_Link = sig
  type cmd

  type args

  type pargs

  type body

  type pbody

  type t

  (* NOTE the cmd param is the same for both *)
  val make :
    ((cmd, args, pargs) RequestMessage.t ->
    (cmd, body, pbody) ResponseMessage.t Dap_result.t) ->
    t

  val handle : t -> request -> response Dap_result.t
end

let get_request_seq = function
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

let set_sequencing seq request_seq = function
  | ErrorResponse resp -> ErrorResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | CancelResponse resp -> CancelResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | RunInTerminalResponse resp -> RunInTerminalResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | InitializeResponse resp -> InitializeResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | ConfigurationDoneResponse resp -> ConfigurationDoneResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | LaunchResponse resp -> LaunchResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | AttachResponse resp -> AttachResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | RestartResponse resp -> RestartResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | DisconnectResponse resp -> DisconnectResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | TerminateResponse resp -> TerminateResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | BreakpointLocationsResponse resp -> BreakpointLocationsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | SetBreakpointsResponse resp -> SetBreakpointsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | SetFunctionBreakpointsResponse resp -> SetFunctionBreakpointsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | SetExceptionBreakpointsResponse resp -> SetExceptionBreakpointsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | DataBreakpointInfoResponse resp -> DataBreakpointInfoResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | SetDataBreakpointsResponse resp -> SetDataBreakpointsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | SetInstructionBreakpointsResponse resp -> SetInstructionBreakpointsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | ContinueResponse resp -> ContinueResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | NextResponse resp -> NextResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | StepInResponse resp -> StepInResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | StepOutResponse resp -> StepOutResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | StepBackResponse resp -> StepBackResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | ReverseContinueResponse resp -> ReverseContinueResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | RestartFrameResponse resp -> RestartFrameResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | GotoResponse resp -> GotoResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | PauseResponse resp -> PauseResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | StackTraceResponse resp -> StackTraceResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | ScopesResponse resp -> ScopesResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | VariablesResponse resp -> VariablesResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | SetVariableResponse resp -> SetVariableResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | SourceResponse resp -> SourceResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | ThreadsResponse resp -> ThreadsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | TerminateThreadsResponse resp -> TerminateThreadsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | ModulesResponse resp -> ModulesResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | LoadedSourcesResponse resp -> LoadedSourcesResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | EvaluateResponse resp -> EvaluateResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | SetExpressionResponse resp -> SetExpressionResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | StepInTargetsResponse resp -> StepInTargetsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | GotoTargetsResponse resp -> GotoTargetsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | CompletionsResponse resp -> CompletionsResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | ExceptionInfoResponse resp -> ExceptionInfoResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | ReadMemoryResponse resp -> ReadMemoryResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | WriteMemoryResponse resp -> WriteMemoryResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)
  | DisassembleResponse resp -> DisassembleResponse (let resp = ResponseMessage.set_seq resp ~seq in ResponseMessage.set_request_seq resp ~request_seq)

module WithSeqr (L : Request_Response_Link) :
  Request_Response_Link
    with type t = L.t
     and type cmd := L.cmd
     and type args := L.args
     and type pargs := L.pargs
     and type body := L.body
     and type pbody := L.pbody = struct
  type t = L.t

  let make f = L.make f

  let handle t req =
    let request_seq = get_request_seq req in
    let seq = 1 + request_seq in
    let setter_resp msg =
      let msg = ResponseMessage.set_request_seq msg ~request_seq in
      let msg = ResponseMessage.set_seq msg ~seq in
      msg
    in
    L.handle t req |> Result.map (set_sequencing seq request_seq) |> Result.map_error (setter_resp)
end
