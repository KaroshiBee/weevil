open Dap_event
module D = Dap_messages.Data

module EventTests = struct
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

  let test_initializedEvent =
    tester
      ~count
      ~name:"initializedEvent"
      ~equal_body:(Option.equal Dap_base.EmptyObject.equal)
      gen_initializedEvent

  let test_stoppedEvent =
    tester
      ~count
      ~name:"stoppedEvent"
      ~equal_body:D.StoppedEvent_body.equal
      gen_stoppedEvent

  let test_continuedEvent =
    tester
      ~count
      ~name:"continuedEvent"
      ~equal_body:D.ContinuedEvent_body.equal
      gen_continuedEvent

  let test_exitedEvent =
    tester
      ~count
      ~name:"exitedEvent"
      ~equal_body:D.ExitedEvent_body.equal
      gen_exitedEvent

  let test_terminatedEvent =
    tester
      ~count
      ~name:"terminatedEvent"
      ~equal_body:(Option.equal D.TerminatedEvent_body.equal)
      gen_terminatedEvent

  let test_threadEvent =
    tester
      ~count
      ~name:"threadEvent"
      ~equal_body:D.ThreadEvent_body.equal
      gen_threadEvent

  let test_outputEvent =
    tester
      ~count
      ~name:"outputEvent"
      ~equal_body:D.OutputEvent_body.equal
      gen_outputEvent

  let test_breakpointEvent =
    tester
      ~count
      ~name:"breakpointEvent"
      ~equal_body:D.BreakpointEvent_body.equal
      gen_breakpointEvent

  let test_moduleEvent =
    tester
      ~count:1000
      ~name:"moduleEvent"
      ~equal_body:D.ModuleEvent_body.equal
      gen_moduleEvent

  let test_loadedSourceEvent =
    tester
      ~count
      ~name:"loadedSourceEvent"
      ~equal_body:D.LoadedSourceEvent_body.equal
      gen_loadedSourceEvent

  let test_processEvent =
    tester
      ~count
      ~name:"processEvent"
      ~equal_body:D.ProcessEvent_body.equal
      gen_processEvent

  let test_capabilitiesEvent =
    tester
      ~count
      ~name:"capabilitiesEvent"
      ~equal_body:D.CapabilitiesEvent_body.equal
      gen_capabilitiesEvent

  let test_progressStartEvent =
    tester
      ~count
      ~name:"progressStartEvent"
      ~equal_body:D.ProgressStartEvent_body.equal
      gen_progressStartEvent

  let test_progressUpdateEvent =
    tester
      ~count
      ~name:"progressUpdateEvent"
      ~equal_body:D.ProgressUpdateEvent_body.equal
      gen_progressUpdateEvent

  let test_progressEndEvent =
    tester
      ~count
      ~name:"progressEndEvent"
      ~equal_body:D.ProgressEndEvent_body.equal
      gen_progressEndEvent

  let test_invalidatedEvent =
    tester
      ~count
      ~name:"invalidatedEvent"
      ~equal_body:D.InvalidatedEvent_body.equal
      gen_invalidatedEvent

  let test_memoryEvent =
    tester
      ~count
      ~name:"memoryEvent"
      ~equal_body:D.MemoryEvent_body.equal
      gen_memoryEvent

  let test_different_events_same_bodies_never_equal =
    let (ctor1, gen1, _enc1) = gen_initializedEvent in
    let (ctor2, gen2, _enc2) = gen_stoppedEvent in
    let arb1 = QCheck.make gen1 in
    let arb2 = QCheck.make gen2 in
    QCheck.Test.make
      ~count
      ~name:"different events, bodies set to equal"
      (QCheck.pair arb1 arb2)
      (fun (t1, t2) ->
        let t1' = ctor1 t1 in
        let t2' = ctor2 t2 in
        let equal_body _ _ = true in
        not @@ equal ~equal_body t1' t2')

  let test_different_events_different_bodies_never_equal =
    let (ctor1, gen1, _enc1) = gen_initializedEvent in
    let (ctor2, gen2, _enc2) = gen_stoppedEvent in
    let arb1 = QCheck.make gen1 in
    let arb2 = QCheck.make gen2 in
    QCheck.Test.make
      ~count
      ~name:"different events, bodies set to not equal"
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
        test_initializedEvent;
        test_stoppedEvent;
        test_continuedEvent;
        test_exitedEvent;
        test_terminatedEvent;
        test_threadEvent;
        test_outputEvent;
        test_breakpointEvent;
        test_moduleEvent;
        test_loadedSourceEvent;
        test_processEvent;
        test_capabilitiesEvent;
        test_progressStartEvent;
        test_progressUpdateEvent;
        test_progressEndEvent;
        test_invalidatedEvent;
        test_memoryEvent;
      ]

  let suite_basic_equality_checks =
    List.map
      (QCheck_alcotest.to_alcotest ~long:false)
      [
        test_different_events_same_bodies_never_equal;
        test_different_events_different_bodies_never_equal;
      ]
end
