open Dap_event
module D = Dap_message.Data

module EventTests = struct

  let equal ~equal_body = fun ev1 ev2 ->
    let ev1 = val_ ev1 in
    let ev2 = val_ ev2 in
    let f = Message.equal ~equal_body in
    let eq = eval @@ equal_ (val_ @@ Eq f, ev1, ev2) in
    eq

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

  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ test_init_event; test_stopped_event; ]

end
