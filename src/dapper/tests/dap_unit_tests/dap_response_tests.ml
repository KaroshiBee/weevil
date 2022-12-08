open Dap_response
module D = Dap_message.Data

module ResponseTests = struct
  let equal ~equal_body t1 t2 =
    let t1 = val_ t1 in
    let t2 = val_ t2 in
    let f = Message.equal ~equal_body in
    eval @@ equal_ (val_ @@ eq_ f, t1, t2)

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
        (* test_cancelResponse; *)
        (* test_runInTerminalResponse; *)
        (* test_initializeResponse; *)
        (* test_configurationDoneResponse; *)
        (* test_launchResponse; *)
        (* test_attachResponse; *)
        (* test_restartResponse; *)
        (* test_disconnectResponse; *)
        (* test_terminateResponse; *)
        (* test_breakpointLocationsResponse; *)
        (* test_setBreakpointsResponse; *)
        (* test_setFunctionBreakpointsResponse; *)
        (* test_setExceptionBreakpointsResponse; *)
        (* test_dataBreakpointInfoResponse; *)
        (* test_setDataBreakpointsResponse; *)
        (* test_setInstructionBreakpointsResponse; *)
        (* test_continueResponse; *)
        (* test_nextResponse; *)
        (* test_stepInResponse; *)
        (* test_stepOutResponse; *)
        (* test_stepBackResponse; *)
        (* test_reverseContinueResponse; *)
        (* test_restartFrameResponse; *)
        (* test_gotoResponse; *)
        (* test_pauseResponse; *)
        (* test_stackTraceResponse; *)
        (* test_scopesResponse; *)
        (* test_variablesResponse; *)
        (* test_setVariableResponse; *)
        (* test_sourceResponse; *)
        (* test_threadsResponse; *)
        (* test_terminateThreadsResponse; *)
        (* test_modulesResponse; *)
        (* test_loadedSourcesResponse; *)
        (* test_evaluateResponse; *)
        (* test_setExpressionResponse; *)
        (* test_stepInTargetsResponse; *)
        (* test_gotoTargetsResponse; *)
        (* test_completionsResponse; *)
        (* test_exceptionInfoResponse; *)
        (* test_readMemoryResponse; *)
        (* test_writeMemoryResponse; *)
        (* test_disassembleResponse; *)
      ]

  let suite_basic_equality_checks =
    List.map
      (QCheck_alcotest.to_alcotest ~long:false)
      [
        test_different_responses_same_args_never_equal;
        test_different_responses_different_args_never_equal;
      ]
end
