open Dap_event
module D = Dap_message.Data

module EventTests = struct

  let equal ~equal_body = fun ev1 ev2 ->
    let ev1 = val_ ev1 in
    let ev2 = val_ ev2 in
    let f = Message.equal ~equal_body in
    eval @@ equal_ (val_ @@ eq_ f, ev1, ev2)

  let test_init_event =
    let ctor, gen, enc = gen_initializedevent in
    let arb = QCheck.make gen in
    QCheck.Test.make ~count:1000 ~name:"initialized event" arb (fun ev ->
        let ev1 = ctor ev in
        let js = Data_encoding.Json.(construct enc ev |> to_string) in
        let ev2 = ctor @@ Data_encoding.Json.(from_string js |> Result.get_ok |> destruct enc) in
        let equal_body = fun x y -> match (x, y) with
          | None, None -> true
          | Some b1, Some b2 -> Dap_base.EmptyObject.equal b1 b2
          | _, _ -> false
        in
        equal ~equal_body ev1 ev2
      )

  let test_stopped_event =
    let ctor, gen, enc = gen_stoppedevent in
    let arb = QCheck.make gen in
    QCheck.Test.make ~count:1000 ~name:"stopped event" arb (fun ev ->
        let ev1 = ctor ev in
        let js = Data_encoding.Json.(construct enc ev |> to_string) in
        let ev2 = ctor @@ Data_encoding.Json.(from_string js |> Result.get_ok |> destruct enc) in
        equal ~equal_body:D.StoppedEvent_body.equal ev1 ev2
      )

  let test_different_events_same_bodies_never_equal =
    let ctor1, gen1, _enc1 = gen_initializedevent in
    let ctor2, gen2, _enc2 = gen_stoppedevent in
    let arb1 = QCheck.make gen1 in
    let arb2 = QCheck.make gen2 in
    QCheck.Test.make ~count:1000 ~name:"different events, bodies set to equal"
      (QCheck.pair arb1 arb2)
      (fun (ev1, ev2) ->
        let ev1' = ctor1 ev1 in
        let ev2' = ctor2 ev2 in
        let equal_body = fun _ _ -> true in
        not @@ equal ~equal_body ev1' ev2'
      )

  let test_different_events_different_bodies_never_equal =
    let ctor1, gen1, _enc1 = gen_initializedevent in
    let ctor2, gen2, _enc2 = gen_stoppedevent in
    let arb1 = QCheck.make gen1 in
    let arb2 = QCheck.make gen2 in
    QCheck.Test.make ~count:1000 ~name:"different events, bodies set to not equal"
      (QCheck.pair arb1 arb2)
      (fun (ev1, ev2) ->
        let ev1' = ctor1 ev1 in
        let ev2' = ctor2 ev2 in
        let equal_body = fun _ _ -> false in
        not @@ equal ~equal_body ev1' ev2'
      )

  let suite_js_roundtrip =
    List.map QCheck_alcotest.to_alcotest
      [ test_init_event; test_stopped_event; ]

  let suite_basic_equality_checks =
    List.map QCheck_alcotest.to_alcotest
      [ test_different_events_same_bodies_never_equal; test_different_events_different_bodies_never_equal; ]

end
