include Test_utils.Include

module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module BreakStopped = Dap_handlers.Raise_event.Make (struct
  type in_enum = Dap.Events.breakpoint
  type in_contents = Dap.Data.BreakpointEvent_body.t
  type in_presence = Dap.Data.Presence.req
  type out_enum = Dap.Events.stopped
  type out_contents = Dap.Data.StoppedEvent_body.t
  type out_presence = Dap.Data.Presence.req
  type in_msg = (in_enum, in_contents, in_presence) Ev.Message.t
  type out_msg = (out_enum, out_contents, out_presence) Ev.Message.t

  let ctor_in = Ev.breakpointEvent
  let enc_in = Ev.Message.enc Dap.Events.breakpoint Dap.Data.BreakpointEvent_body.enc
  let ctor_out = Ev.stoppedEvent
  let enc_out = Ev.Message.enc Dap.Events.stopped Dap.Data.StoppedEvent_body.enc

end)

let%expect_test "Check sequencing event/event" =
  let config = Dap.Config.make () in
  let handler =
    let l = BreakStopped.make
      ~handler:(fun _ _ ->
        let reason = Dap.Data.StoppedEvent_body_reason.Breakpoint in
        let body =
          Dap.Data.StoppedEvent_body.make
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
    let breakpoint = Dap.Data.Breakpoint.make ~verified:true () in
    let body = Dap.Data.BreakpointEvent_body.make ~reason:Dap.Data.BreakpointEvent_body_reason.New ~breakpoint () in
    Ev.(breakpointEvent @@ Message.make ~seq:111 ~event:Dap.Events.breakpoint ~body ()) in
  let enc_stopper = Ev.Message.enc Dap.Events.stopped Dap.Data.StoppedEvent_body.enc in

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
            Res.default_response_error "testing error"
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