module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module BreakStopped = Dap_raise_event.WithSeqr (struct
  type ev = Dap.Events.breakpoint
  type body = Dap.BreakpointEvent_body.t
  type pbody = Dap.Presence.req
  type ev_ = Dap.Events.stopped
  type body_ = Dap.StoppedEvent_body.t
  type pbody_ = Dap.Presence.req
end)


let%expect_test "Check sequencing event/event" =
  let handler =
    let l = BreakStopped.make
      ~handler:(fun _ ->
        let reason = Dap.StoppedEvent_body_reason.Breakpoint in
        let body =
          Dap.StoppedEvent_body.make
            ~reason
            ()
        in
        Ev.default_event_req Dap.Events.stopped body
        |> Ev.stoppedEvent
        |> Dap_result.ok
          )
      ~ctor:Ev.stoppedEvent
    in
    BreakStopped.handle l
  in

  let break_ev =
    let breakpoint = Dap.Breakpoint.make ~verified:true () in
    let body = Dap.BreakpointEvent_body.make ~reason:Dap.BreakpointEvent_body_reason.New ~breakpoint () in
    Ev.(breakpointEvent @@ Message.make ~seq:111 ~event:Dap.Events.breakpoint ~body ()) in
  let enc_stopper = Ev.Message.enc Dap.Events.stopped Dap.StoppedEvent_body.enc in

  let s =
    handler break_ev
    |> Dap_result.map ~f:(function
        | Ev.StoppedEvent ev ->
          Js.construct enc_stopper ev |> Js.to_string
        | _ -> assert false
      )
    |> Dap_result.get_ok
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 112, "type": "event", "event": "stopped",
      "body": { "reason": "breakpoint" } } |}];

  (* should also have the correct seq numbers if error happens during handling *)
  let handler_err =
    let l = BreakStopped.make
        ~handler:(fun _req ->
            Dap.default_response_error "testing error"
            |> Res.errorResponse
            |> Dap_result.error
          )
        ~ctor:Ev.stoppedEvent
    in
    BreakStopped.handle l
  in
  let s = handler_err break_ev |> Dap_result.get_error_str
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 112, "type": "response", "request_seq": 111, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
