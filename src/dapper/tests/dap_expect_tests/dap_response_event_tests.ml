include Test_utils.Include
module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module ProcessLaunched =
  Dap_handlers.Response_event.Make
    (struct
      type enum = Dap.Commands.launch

      type contents = Dap.Data.EmptyObject.t option

      type presence = Dap.Data.Presence.opt

      type msg = (enum, contents, presence) Res.Message.t

      type t = msg Res.t

      let ctor = Res.launchResponse

      let enc = Res.Message.enc_opt Dap.Commands.launch Dap.Data.EmptyObject.enc
    end)
    (struct
      type enum = Dap.Events.process

      type contents = Dap.Data.ProcessEvent_body.t

      type presence = Dap.Data.Presence.req

      type msg = (enum, contents, presence) Ev.Message.t

      type t = msg Ev.t

      let ctor = Ev.processEvent

      let enc = Ev.Message.enc Dap.Events.process Dap.Data.ProcessEvent_body.enc
    end)
    (Dap_handlers.State)

let%expect_test "Check sequencing response/event" =
  let config = Dap.Config.make () in
  let state = Dap_handlers.State.make in
  let handler =
    let l =
      ProcessLaunched.make ~handler:(fun _ _ _ ->
          let startMethod = Dap.Data.ProcessEvent_body_startMethod.Launch in
          let body =
            Dap.Data.ProcessEvent_body.make
              ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
              ~startMethod
              ()
          in
          Ev.default_event_req Dap.Events.process body
          |> Ev.processEvent |> Dap_result.ok)
    in
    ProcessLaunched.handle l
  in

  let enc = Res.Message.enc_opt Dap.Commands.launch Dap.Data.EmptyObject.enc in
  let resp_launch =
    Res.(
      Message.make_opt
        ~seq:111
        ~request_seq:110
        ~success:true
        ~command:Dap.Commands.launch
        ~body:(Dap.Data.EmptyObject.make ())
        ()
      |> Js.construct enc |> Js.to_string)
  in

  let%lwt s = handler ~state ~config resp_launch in
  Printf.printf "%s" @@ Result.get_ok s ;
  let%lwt () =
    [%expect
      {|
    { "seq": 112, "type": "event", "event": "process",
      "body":
        { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
          "startMethod": "launch" } } |}]
  in

  (* should also have the correct seq numbers if error happens during handling *)
  let handler_err =
    let l =
      ProcessLaunched.make ~handler:(fun _ _ _req ->
          Res.default_response_error "testing error"
          |> Res.errorResponse |> Dap_result.error)
    in
    ProcessLaunched.handle l
  in
  let%lwt s = handler_err ~state ~config resp_launch in
  Printf.printf "%s" @@ Result.get_error s ;
  [%expect
    {|
    { "seq": 112, "type": "response", "request_seq": 111, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
