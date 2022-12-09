open Dap_request
module D = Dap_messages.Data

module RequestTests = struct
  let equal ~equal_arguments t1 t2 =
    let equal_f = Eq (Message.equal ~equal_arguments) in
    eval @@ equal ~equal_f t1 t2

  let tester ~count ~name ~equal_arguments (ctor, gen, enc) =
    let arb = QCheck.make gen in
    QCheck.Test.make ~long_factor:100 ~count ~name arb (fun t ->
        let t1 = ctor t in
        let js = Data_encoding.Json.(construct enc t |> to_string) in
        let t2 =
          ctor
          @@ Data_encoding.Json.(
               from_string js |> Result.get_ok |> destruct enc)
        in
        equal ~equal_arguments t1 t2)

  let count = 10

  let test_cancelRequest =
    tester
      ~count
      ~name:"cancelRequest"
      ~equal_arguments:(Option.equal D.CancelArguments.equal)
      gen_cancelRequest

  let test_runInTerminalRequest =
    tester
      ~count
      ~name:"runInTerminalRequest"
      ~equal_arguments:D.RunInTerminalRequestArguments.equal
      gen_runInTerminalRequest

  let test_initializeRequest =
    tester
      ~count
      ~name:"initializeRequest"
      ~equal_arguments:D.InitializeRequestArguments.equal
      gen_initializeRequest

  let test_configurationDoneRequest =
    tester
      ~count
      ~name:"configurationDoneRequest"
      ~equal_arguments:(Option.equal D.ConfigurationDoneArguments.equal)
      gen_configurationDoneRequest

  let test_launchRequest =
    tester
      ~count
      ~name:"launchRequest"
      ~equal_arguments:D.LaunchRequestArguments.equal
      gen_launchRequest

  let test_attachRequest =
    tester
      ~count
      ~name:"attachRequest"
      ~equal_arguments:D.AttachRequestArguments.equal
      gen_attachRequest

  let test_restartRequest =
    tester
      ~count
      ~name:"restartRequest"
      ~equal_arguments:(Option.equal D.RestartArguments.equal)
      gen_restartRequest

  let test_disconnectRequest =
    tester
      ~count
      ~name:"disconnectRequest"
      ~equal_arguments:(Option.equal D.DisconnectArguments.equal)
      gen_disconnectRequest

  let test_terminateRequest =
    tester
      ~count
      ~name:"terminateRequest"
      ~equal_arguments:(Option.equal D.TerminateArguments.equal)
      gen_terminateRequest

  let test_breakpointLocationsRequest =
    tester
      ~count
      ~name:"breakpointLocationsRequest"
      ~equal_arguments:(Option.equal D.BreakpointLocationsArguments.equal)
      gen_breakpointLocationsRequest

  let test_setBreakpointsRequest =
    tester
      ~count
      ~name:"setBreakpointsRequest"
      ~equal_arguments:D.SetBreakpointsArguments.equal
      gen_setBreakpointsRequest

  let test_setFunctionBreakpointsRequest =
    tester
      ~count
      ~name:"setFunctionBreakpointsRequest"
      ~equal_arguments:D.SetFunctionBreakpointsArguments.equal
      gen_setFunctionBreakpointsRequest

  let test_setExceptionBreakpointsRequest =
    tester
      ~count:1 (* this one takes ages to run *)
      ~name:"setExceptionBreakpointsRequest"
      ~equal_arguments:D.SetExceptionBreakpointsArguments.equal
      gen_setExceptionBreakpointsRequest

  let test_dataBreakpointInfoRequest =
    tester
      ~count
      ~name:"dataBreakpointInfoRequest"
      ~equal_arguments:D.DataBreakpointInfoArguments.equal
      gen_dataBreakpointInfoRequest

  let test_setDataBreakpointsRequest =
    tester
      ~count
      ~name:"setDataBreakpointsRequest"
      ~equal_arguments:D.SetDataBreakpointsArguments.equal
      gen_setDataBreakpointsRequest

  let test_setInstructionBreakpointsRequest =
    tester
      ~count
      ~name:"setInstructionBreakpointsRequest"
      ~equal_arguments:D.SetInstructionBreakpointsArguments.equal
      gen_setInstructionBreakpointsRequest

  let test_continueRequest =
    tester
      ~count
      ~name:"continueRequest"
      ~equal_arguments:D.ContinueArguments.equal
      gen_continueRequest

  let test_nextRequest =
    tester
      ~count
      ~name:"nextRequest"
      ~equal_arguments:D.NextArguments.equal
      gen_nextRequest

  let test_stepInRequest =
    tester
      ~count
      ~name:"stepInRequest"
      ~equal_arguments:D.StepInArguments.equal
      gen_stepInRequest

  let test_stepOutRequest =
    tester
      ~count
      ~name:"stepOutRequest"
      ~equal_arguments:D.StepOutArguments.equal
      gen_stepOutRequest

  let test_stepBackRequest =
    tester
      ~count
      ~name:"stepBackRequest"
      ~equal_arguments:D.StepBackArguments.equal
      gen_stepBackRequest

  let test_reverseContinueRequest =
    tester
      ~count
      ~name:"reverseContinueRequest"
      ~equal_arguments:D.ReverseContinueArguments.equal
      gen_reverseContinueRequest

  let test_restartFrameRequest =
    tester
      ~count
      ~name:"restartFrameRequest"
      ~equal_arguments:D.RestartFrameArguments.equal
      gen_restartFrameRequest

  let test_gotoRequest =
    tester
      ~count
      ~name:"gotoRequest"
      ~equal_arguments:D.GotoArguments.equal
      gen_gotoRequest

  let test_pauseRequest =
    tester
      ~count
      ~name:"pauseRequest"
      ~equal_arguments:D.PauseArguments.equal
      gen_pauseRequest

  let test_stackTraceRequest =
    tester
      ~count
      ~name:"stackTraceRequest"
      ~equal_arguments:D.StackTraceArguments.equal
      gen_stackTraceRequest

  let test_scopesRequest =
    tester
      ~count
      ~name:"scopesRequest"
      ~equal_arguments:D.ScopesArguments.equal
      gen_scopesRequest

  let test_variablesRequest =
    tester
      ~count
      ~name:"variablesRequest"
      ~equal_arguments:D.VariablesArguments.equal
      gen_variablesRequest

  let test_setVariableRequest =
    tester
      ~count
      ~name:"setVariableRequest"
      ~equal_arguments:D.SetVariableArguments.equal
      gen_setVariableRequest

  let test_sourceRequest =
    tester
      ~count
      ~name:"sourceRequest"
      ~equal_arguments:D.SourceArguments.equal
      gen_sourceRequest

  let test_threadsRequest =
    tester
      ~count
      ~name:"threadsRequest"
      ~equal_arguments:(Option.equal D.EmptyObject.equal)
      gen_threadsRequest

  let test_terminateThreadsRequest =
    tester
      ~count
      ~name:"terminateThreadsRequest"
      ~equal_arguments:D.TerminateThreadsArguments.equal
      gen_terminateThreadsRequest

  let test_modulesRequest =
    tester
      ~count
      ~name:"modulesRequest"
      ~equal_arguments:D.ModulesArguments.equal
      gen_modulesRequest

  let test_loadedSourcesRequest =
    tester
      ~count
      ~name:"loadedSourcesRequest"
      ~equal_arguments:(Option.equal D.LoadedSourcesArguments.equal)
      gen_loadedSourcesRequest

  let test_evaluateRequest =
    tester
      ~count
      ~name:"evaluateRequest"
      ~equal_arguments:D.EvaluateArguments.equal
      gen_evaluateRequest

  let test_setExpressionRequest =
    tester
      ~count
      ~name:"setExpressionRequest"
      ~equal_arguments:D.SetExpressionArguments.equal
      gen_setExpressionRequest

  let test_stepInTargetsRequest =
    tester
      ~count
      ~name:"stepInTargetsRequest"
      ~equal_arguments:D.StepInTargetsArguments.equal
      gen_stepInTargetsRequest

  let test_gotoTargetsRequest =
    tester
      ~count
      ~name:"gotoTargetsRequest"
      ~equal_arguments:D.GotoTargetsArguments.equal
      gen_gotoTargetsRequest

  let test_completionsRequest =
    tester
      ~count
      ~name:"completionsRequest"
      ~equal_arguments:D.CompletionsArguments.equal
      gen_completionsRequest

  let test_exceptionInfoRequest =
    tester
      ~count
      ~name:"exceptionInfoRequest"
      ~equal_arguments:D.ExceptionInfoArguments.equal
      gen_exceptionInfoRequest

  let test_readMemoryRequest =
    tester
      ~count
      ~name:"readMemoryRequest"
      ~equal_arguments:D.ReadMemoryArguments.equal
      gen_readMemoryRequest

  let test_writeMemoryRequest =
    tester
      ~count
      ~name:"writeMemoryRequest"
      ~equal_arguments:D.WriteMemoryArguments.equal
      gen_writeMemoryRequest

  let test_disassembleRequest =
    tester
      ~count
      ~name:"disassembleRequest"
      ~equal_arguments:D.DisassembleArguments.equal
      gen_disassembleRequest

  let test_different_requests_same_args_never_equal =
    let (ctor1, gen1, _enc1) = gen_attachRequest in
    let (ctor2, gen2, _enc2) = gen_launchRequest in
    let arb1 = QCheck.make gen1 in
    let arb2 = QCheck.make gen2 in
    QCheck.Test.make
      ~count
      ~name:"different requests, bodies set to equal"
      (QCheck.pair arb1 arb2)
      (fun (t1, t2) ->
        let t1' = ctor1 t1 in
        let t2' = ctor2 t2 in
        let equal_arguments _ _ = true in
        not @@ equal ~equal_arguments t1' t2')

  let test_different_requests_different_args_never_equal =
    let (ctor1, gen1, _enc1) = gen_attachRequest in
    let (ctor2, gen2, _enc2) = gen_launchRequest in
    let arb1 = QCheck.make gen1 in
    let arb2 = QCheck.make gen2 in
    QCheck.Test.make
      ~count
      ~name:"different requests, bodies set to not equal"
      (QCheck.pair arb1 arb2)
      (fun (t1, t2) ->
        let t1' = ctor1 t1 in
        let t2' = ctor2 t2 in
        let equal_arguments _ _ = false in
        not @@ equal ~equal_arguments t1' t2')

  let suite_js_roundtrip =
    List.map
      (QCheck_alcotest.to_alcotest ~long:false)
      [
        test_cancelRequest;
        test_runInTerminalRequest;
        test_initializeRequest;
        test_configurationDoneRequest;
        test_launchRequest;
        test_attachRequest;
        test_restartRequest;
        test_disconnectRequest;
        test_terminateRequest;
        test_breakpointLocationsRequest;
        test_setBreakpointsRequest;
        test_setFunctionBreakpointsRequest;
        test_setExceptionBreakpointsRequest;
        test_dataBreakpointInfoRequest;
        test_setDataBreakpointsRequest;
        test_setInstructionBreakpointsRequest;
        test_continueRequest;
        test_nextRequest;
        test_stepInRequest;
        test_stepOutRequest;
        test_stepBackRequest;
        test_reverseContinueRequest;
        test_restartFrameRequest;
        test_gotoRequest;
        test_pauseRequest;
        test_stackTraceRequest;
        test_scopesRequest;
        test_variablesRequest;
        test_setVariableRequest;
        test_sourceRequest;
        test_threadsRequest;
        test_terminateThreadsRequest;
        test_modulesRequest;
        test_loadedSourcesRequest;
        test_evaluateRequest;
        test_setExpressionRequest;
        test_stepInTargetsRequest;
        test_gotoTargetsRequest;
        test_completionsRequest;
        test_exceptionInfoRequest;
        test_readMemoryRequest;
        test_writeMemoryRequest;
        test_disassembleRequest;
      ]

  let suite_basic_equality_checks =
    List.map
      (QCheck_alcotest.to_alcotest ~long:false)
      [
        test_different_requests_same_args_never_equal;
        test_different_requests_different_args_never_equal;
      ]
end
