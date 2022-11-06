include Test_utils.Include

module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module ProcessLaunched = Dap_response_event.Make (struct
  type cmd = Dap.Commands.launch
  type body = Dap.EmptyObject.t option
  type pbody = Dap.Presence.opt
  type ev = Dap.Events.process
  type body_ = Dap.ProcessEvent_body.t
  type pbody_ = Dap.Presence.req

  module In_msg = Res.Message
  module In = Res
  module Out_msg = Ev.Message
  module Out = Ev

  type in_msg = (cmd, body, pbody) In_msg.t
  type out_msg = (ev, body_, pbody_) Out_msg.t

  let ctor_in = In.launchResponse
  let enc_in = In_msg.enc_opt Dap.Commands.launch Dap.EmptyObject.enc
  let ctor_out = Out.processEvent
  let enc_out = Out_msg.enc Dap.Events.process Dap.ProcessEvent_body.enc

end)

let%expect_test "Check sequencing response/event" =
  let config = Dap.Config.make () in
  let handler =
    let l = ProcessLaunched.make
        ~handler:(fun _ _ ->
            let startMethod = Dap.ProcessEvent_body_startMethod.Launch in
            let body =
              Dap.ProcessEvent_body.make
                ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
                ~startMethod
                ()
            in
            Ev.default_event_req Dap.Events.process body
            |> ProcessLaunched.Out.ctor
            |> Dap_result.ok
          )
    in
    ProcessLaunched.handle l
  in

  let resp_launch = Res.(launchResponse @@ Message.make_opt ~seq:111 ~request_seq:110 ~success:true ~command:Dap.Commands.launch ~body:(Dap.EmptyObject.make ()) ()) in
  let enc_launch = Ev.(Message.enc Dap.Events.process Dap.ProcessEvent_body.enc) in

  let%lwt s =
    handler ~config resp_launch
    |> Dap_result.map ~f:(function
        | Ev.ProcessEvent ev ->
          Js.construct enc_launch ev |> Js.to_string
        | _ -> assert false
      )
    |> Dap_result.to_lwt_result
  in
  Printf.printf "%s" @@ Result.get_ok s;
  let%lwt () = [%expect {|
    { "seq": 112, "type": "event", "event": "process",
      "body":
        { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
          "startMethod": "launch" } } |}]
  in

  (* should also have the correct seq numbers if error happens during handling *)
  let handler_err =
    let l = ProcessLaunched.make
      ~handler:(fun _ _req ->
            Dap.default_response_error "testing error"
            |> Res.errorResponse
            |> Dap_result.error
          )
    in
    ProcessLaunched.handle l
  in
  let%lwt s =
    handler_err ~config resp_launch
    |> Dap_result.to_lwt_error_as_str
  in
  Printf.printf "%s" @@ Result.get_error s;
  [%expect {|
    { "seq": 112, "type": "response", "request_seq": 111, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
