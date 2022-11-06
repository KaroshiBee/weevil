include Test_utils.Include

module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module BreakStopped = Dap_raise_event.Make (struct
  type ev = Dap.Events.breakpoint
  type body = Dap.BreakpointEvent_body.t
  type pbody = Dap.Presence.req
  type ev_ = Dap.Events.stopped
  type body_ = Dap.StoppedEvent_body.t
  type pbody_ = Dap.Presence.req
  type in_msg = (ev, body, pbody) Ev.Message.t
  type out_msg = (ev_, body_, pbody_) Ev.Message.t

  let ctor_in = Ev.breakpointEvent
  let enc_in = Ev.Message.enc Dap.Events.breakpoint Dap.BreakpointEvent_body.enc
  let ctor_out = Ev.stoppedEvent
  let enc_out = Ev.Message.enc Dap.Events.stopped Dap.StoppedEvent_body.enc

end)

let%expect_test "Check sequencing event/event" =
  let config = Dap.Config.make () in
  let handler =
    let l = BreakStopped.make
      ~handler:(fun _ _ ->
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
    in
    BreakStopped.handle l
  in

  let break_ev =
    let breakpoint = Dap.Breakpoint.make ~verified:true () in
    let body = Dap.BreakpointEvent_body.make ~reason:Dap.BreakpointEvent_body_reason.New ~breakpoint () in
    Ev.(breakpointEvent @@ Message.make ~seq:111 ~event:Dap.Events.breakpoint ~body ()) in
  let enc_stopper = Ev.Message.enc Dap.Events.stopped Dap.StoppedEvent_body.enc in

  let%lwt s =
    handler ~config break_ev
    |> Dap_result.map ~f:(function
        | Ev.StoppedEvent ev ->
          Js.construct enc_stopper ev |> Js.to_string
        | _ -> assert false
      )
    |> Dap_result.to_lwt_result

  in
  Printf.printf "%s" @@ Result.get_ok s;
  let%lwt () = [%expect {|
    { "seq": 112, "type": "event", "event": "stopped",
      "body": { "reason": "breakpoint" } } |}]
  in

  (* should also have the correct seq numbers if error happens during handling *)
  let handler_err =
    let l = BreakStopped.make
        ~handler:(fun _ _req ->
            Dap.default_response_error "testing error"
            |> Res.errorResponse
            |> Dap_result.error
          )
    in
    BreakStopped.handle l
  in
  let%lwt s =
    handler_err ~config break_ev
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
