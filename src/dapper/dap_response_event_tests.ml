module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module ProcessLaunched = Dap_response_event.WithSeqr (struct
  type cmd = Dap.Commands.launch
  type body = Dap.EmptyObject.t option
  type pbody = Dap.Presence.opt
  type ev = Dap.Events.process
  type body_ = Dap.ProcessEvent_body.t
  type pbody_ = Dap.Presence.req
end)


let%expect_test "Check sequencing response/event" =
  let handler =
    let l = ProcessLaunched.make
        ~handler:(fun _ ->
            let startMethod = Dap.ProcessEvent_body_startMethod.Launch in
            let body =
              Dap.ProcessEvent_body.make
                ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
                ~startMethod
                ()
            in
            Ev.default_event_req Dap.Events.process body
            |> Ev.processEvent
            |> Dap_result.ok
          )
        ~ctor:Ev.processEvent
    in
    ProcessLaunched.handle l
  in

  let resp_launch = Res.(launchResponse @@ Message.make_opt ~seq:111 ~request_seq:110 ~success:true ~command:Dap.Commands.launch ~body:(Dap.EmptyObject.make ()) ()) in
  let enc_launch = Ev.(Message.enc Dap.Events.process Dap.ProcessEvent_body.enc) in

  let s =
    handler resp_launch |> Dap_result.map (function
        | Ev.ProcessEvent ev ->
          Js.construct enc_launch ev |> Js.to_string
        | _ -> assert false
      ) |> Dap_result.get_ok
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 112, "type": "event", "event": "process",
      "body":
        { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
          "startMethod": "launch" } } |}];

  (* should also have the correct seq numbers if error happens during handling *)
  let handler_err =
    let l = ProcessLaunched.make
      ~handler:(fun _req ->
            Dap.default_response_error "testing error"
            |> Res.errorResponse
            |> Dap_result.error
          )
      ~ctor:Ev.processEvent
    in
    ProcessLaunched.handle l
  in
  let s = handler_err resp_launch |> Dap_result.get_error_str
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 112, "type": "response", "request_seq": 111, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
