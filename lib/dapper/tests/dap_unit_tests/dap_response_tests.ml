open Dap_response
module D = Dap_messages.Data

module ResponseTests = struct
  let equal ~equal_body t1 t2 =
    let equal_f = Message.equal ~equal_body in
    eval @@ equal ~equal_f t1 t2

  let tester ~count ~name ~equal_body (ctor, gen, enc) =
    let arb = QCheck.make gen in
    QCheck.Test.make ~long_factor:100 ~count ~name arb (fun t ->
        let t1 = ctor t in
        let js = Data_encoding.Json.(construct enc t |> to_string) in
        let t2 =
          ctor
          @@ Data_encoding.Json.(
               from_string js |> Result.get_ok |> destruct enc)
        in
        equal ~equal_body t1 t2)

  let count = 10

  let test_errorResponse =
    tester
      ~count
      ~name:"errorResponse"
      ~equal_body:D.ErrorResponse_body.equal
      gen_errorResponse

  let test_cancelResponse =
    tester
      ~count
      ~name:"cancelResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_cancelResponse

  let test_runInTerminalResponse =
    tester
      ~count
      ~name:"runInTerminalResponse"
      ~equal_body:D.RunInTerminalResponse_body.equal
      gen_runInTerminalResponse

  let test_initializeResponse =
    tester
      ~count
      ~name:"initializeResponse"
      ~equal_body:(Option.equal D.Capabilities.equal)
      gen_initializeResponse

  let test_configurationDoneResponse =
    tester
      ~count
      ~name:"configurationDoneResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_configurationDoneResponse

  let test_launchResponse =
    tester
      ~count
      ~name:"launchResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_launchResponse

  let test_attachResponse =
    tester
      ~count
      ~name:"attachResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_attachResponse

  let test_restartResponse =
    tester
      ~count
      ~name:"restartResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_restartResponse

  let test_disconnectResponse =
    tester
      ~count
      ~name:"disconnectResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_disconnectResponse

  let test_terminateResponse =
    tester
      ~count
      ~name:"terminateResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_terminateResponse

  let test_breakpointLocationsResponse =
    tester
      ~count
      ~name:"breakpointLocationsResponse"
      ~equal_body:D.BreakpointLocationsResponse_body.equal
      gen_breakpointLocationsResponse

  let test_setBreakpointsResponse =
    tester
      ~count:1
      ~name:"setBreakpointsResponse"
      ~equal_body:D.SetBreakpointsResponse_body.equal
      gen_setBreakpointsResponse

  let test_setFunctionBreakpointsResponse =
    tester
      ~count
      ~name:"setFunctionBreakpointsResponse"
      ~equal_body:D.SetFunctionBreakpointsResponse_body.equal
      gen_setFunctionBreakpointsResponse

  let test_setExceptionBreakpointsResponse =
    tester
      ~count:1 (* this one takes ages to run *)
      ~name:"setExceptionBreakpointsResponse"
      ~equal_body:(Option.equal D.SetExceptionBreakpointsResponse_body.equal)
      gen_setExceptionBreakpointsResponse

  let test_dataBreakpointInfoResponse =
    tester
      ~count
      ~name:"dataBreakpointInfoResponse"
      ~equal_body:D.DataBreakpointInfoResponse_body.equal
      gen_dataBreakpointInfoResponse

  let test_setDataBreakpointsResponse =
    tester
      ~count
      ~name:"setDataBreakpointsResponse"
      ~equal_body:D.SetDataBreakpointsResponse_body.equal
      gen_setDataBreakpointsResponse

  let test_setInstructionBreakpointsResponse =
    tester
      ~count
      ~name:"setInstructionBreakpointsResponse"
      ~equal_body:D.SetInstructionBreakpointsResponse_body.equal
      gen_setInstructionBreakpointsResponse

  let test_continueResponse =
    tester
      ~count
      ~name:"continueResponse"
      ~equal_body:D.ContinueResponse_body.equal
      gen_continueResponse

  let test_nextResponse =
    tester
      ~count
      ~name:"nextResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_nextResponse

  let test_stepInResponse =
    tester
      ~count
      ~name:"stepInResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_stepInResponse

  let test_stepOutResponse =
    tester
      ~count
      ~name:"stepOutResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_stepOutResponse

  let test_stepBackResponse =
    tester
      ~count
      ~name:"stepBackResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_stepBackResponse

  let test_reverseContinueResponse =
    tester
      ~count
      ~name:"reverseContinueResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_reverseContinueResponse

  let test_restartFrameResponse =
    tester
      ~count
      ~name:"restartFrameResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_restartFrameResponse

  let test_gotoResponse =
    tester
      ~count
      ~name:"gotoResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_gotoResponse

  let test_pauseResponse =
    tester
      ~count
      ~name:"pauseResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_pauseResponse

  let test_stackTraceResponse =
    tester
      ~count:1
      ~name:"stackTraceResponse"
      ~equal_body:D.StackTraceResponse_body.equal
      gen_stackTraceResponse

  let test_scopesResponse =
    tester
      ~count
      ~name:"scopesResponse"
      ~equal_body:D.ScopesResponse_body.equal
      gen_scopesResponse

  let test_variablesResponse =
    tester
      ~count
      ~name:"variablesResponse"
      ~equal_body:D.VariablesResponse_body.equal
      gen_variablesResponse

  let test_setVariableResponse =
    tester
      ~count
      ~name:"setVariableResponse"
      ~equal_body:D.SetVariableResponse_body.equal
      gen_setVariableResponse

  let test_sourceResponse =
    tester
      ~count
      ~name:"sourceResponse"
      ~equal_body:D.SourceResponse_body.equal
      gen_sourceResponse

  let test_threadsResponse =
    tester
      ~count
      ~name:"threadsResponse"
      ~equal_body:D.ThreadsResponse_body.equal
      gen_threadsResponse

  let test_terminateThreadsResponse =
    tester
      ~count
      ~name:"terminateThreadsResponse"
      ~equal_body:(Option.equal D.EmptyObject.equal)
      gen_terminateThreadsResponse

  let test_modulesResponse =
    tester
      ~count
      ~name:"modulesResponse"
      ~equal_body:D.ModulesResponse_body.equal
      gen_modulesResponse

  let test_loadedSourcesResponse =
    tester
      ~count
      ~name:"loadedSourcesResponse"
      ~equal_body:D.LoadedSourcesResponse_body.equal
      gen_loadedSourcesResponse

  let test_evaluateResponse =
    tester
      ~count
      ~name:"evaluateResponse"
      ~equal_body:D.EvaluateResponse_body.equal
      gen_evaluateResponse

  let test_setExpressionResponse =
    tester
      ~count
      ~name:"setExpressionResponse"
      ~equal_body:D.SetExpressionResponse_body.equal
      gen_setExpressionResponse

  let test_stepInTargetsResponse =
    tester
      ~count
      ~name:"stepInTargetsResponse"
      ~equal_body:D.StepInTargetsResponse_body.equal
      gen_stepInTargetsResponse

  let test_gotoTargetsResponse =
    tester
      ~count
      ~name:"gotoTargetsResponse"
      ~equal_body:D.GotoTargetsResponse_body.equal
      gen_gotoTargetsResponse

  let test_completionsResponse =
    tester
      ~count
      ~name:"completionsResponse"
      ~equal_body:D.CompletionsResponse_body.equal
      gen_completionsResponse

  let test_exceptionInfoResponse =
    tester
      ~count
      ~name:"exceptionInfoResponse"
      ~equal_body:D.ExceptionInfoResponse_body.equal
      gen_exceptionInfoResponse

  let test_readMemoryResponse =
    tester
      ~count
      ~name:"readMemoryResponse"
      ~equal_body:(Option.equal D.ReadMemoryResponse_body.equal)
      gen_readMemoryResponse

  let test_writeMemoryResponse =
    tester
      ~count
      ~name:"writeMemoryResponse"
      ~equal_body:(Option.equal D.WriteMemoryResponse_body.equal)
      gen_writeMemoryResponse

  let test_disassembleResponse =
    tester
      ~count
      ~name:"disassembleResponse"
      ~equal_body:(Option.equal D.DisassembleResponse_body.equal)
      gen_disassembleResponse
  (* let test_cancelResponse = *)
  (*   tester *)
  (*     ~count *)
  (*     ~name:"cancelResponse" *)
  (*     ~equal_arguments:(Option.equal D.CancelArguments.equal) *)
  (*     gen_cancelResponse *)

  let test_different_responses_same_args_never_equal =
    let (ctor1, gen1, _enc1) = gen_attachResponse in
    let (ctor2, gen2, _enc2) = gen_breakpointLocationsResponse in
    let arb1 = QCheck.make gen1 in
    let arb2 = QCheck.make gen2 in
    QCheck.Test.make
      ~count
      ~name:"different responses, bodies set to equal"
      (QCheck.pair arb1 arb2)
      (fun (t1, t2) ->
        let t1' = ctor1 t1 in
        let t2' = ctor2 t2 in
        let equal_body _ _ = true in
        not @@ equal ~equal_body t1' t2')

  let test_different_responses_different_args_never_equal =
    let (ctor1, gen1, _enc1) = gen_attachResponse in
    let (ctor2, gen2, _enc2) = gen_breakpointLocationsResponse in
    let arb1 = QCheck.make gen1 in
    let arb2 = QCheck.make gen2 in
    QCheck.Test.make
      ~count
      ~name:"different responses, bodies set to not equal"
      (QCheck.pair arb1 arb2)
      (fun (t1, t2) ->
        let t1' = ctor1 t1 in
        let t2' = ctor2 t2 in
        let equal_body _ _ = false in
        not @@ equal ~equal_body t1' t2')

  let suite_js_roundtrip =
    List.map
      (QCheck_alcotest.to_alcotest ~long:false)
      [
        test_errorResponse;
        test_cancelResponse;
        test_runInTerminalResponse;
        test_initializeResponse;
        test_configurationDoneResponse;
        test_launchResponse;
        test_attachResponse;
        test_restartResponse;
        test_disconnectResponse;
        test_terminateResponse;
        test_breakpointLocationsResponse;
        test_setBreakpointsResponse;
        test_setFunctionBreakpointsResponse;
        test_setExceptionBreakpointsResponse;
        test_dataBreakpointInfoResponse;
        test_setDataBreakpointsResponse;
        test_setInstructionBreakpointsResponse;
        test_continueResponse;
        test_nextResponse;
        test_stepInResponse;
        test_stepOutResponse;
        test_stepBackResponse;
        test_reverseContinueResponse;
        test_restartFrameResponse;
        test_gotoResponse;
        test_pauseResponse;
        test_stackTraceResponse;
        test_scopesResponse;
        test_variablesResponse;
        test_setVariableResponse;
        test_sourceResponse;
        test_threadsResponse;
        test_terminateThreadsResponse;
        test_modulesResponse;
        test_loadedSourcesResponse;
        test_evaluateResponse;
        test_setExpressionResponse;
        test_stepInTargetsResponse;
        test_gotoTargetsResponse;
        test_completionsResponse;
        test_exceptionInfoResponse;
        test_readMemoryResponse;
        test_writeMemoryResponse;
        test_disassembleResponse;
      ]

  let suite_basic_equality_checks =
    List.map
      (QCheck_alcotest.to_alcotest ~long:false)
      [
        test_different_responses_same_args_never_equal;
        test_different_responses_different_args_never_equal;
      ]
end
