include Test_utils.Include
module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module BreakStopped =
  Dap_handlers.Raise_event.Make
    (struct
      type enum = Dap.Events.breakpoint

      type contents = Dap.Data.BreakpointEvent_body.t

      type presence = Dap.Data.Presence.req

      type msg = (enum, contents, presence) Ev.Message.t

      type t = msg Ev.t

      let ctor = Ev.breakpointEvent

      let enc =
        Ev.Message.enc Dap.Events.breakpoint Dap.Data.BreakpointEvent_body.enc
    end)
    (struct
      type enum = Dap.Events.stopped

      type contents = Dap.Data.StoppedEvent_body.t

      type presence = Dap.Data.Presence.req

      type msg = (enum, contents, presence) Ev.Message.t

      type t = msg Ev.t

      let ctor = Ev.stoppedEvent

      let enc = Ev.Message.enc Dap.Events.stopped Dap.Data.StoppedEvent_body.enc
    end)
    (Dap_handlers.State)

let%expect_test "Check sequencing event/event" =
  let config = Dap.Config.make () in
  let state = Dap_handlers.State.make in
  let handler =
    let l =
      BreakStopped.make ~handler:(fun _ _ _ ->
          let reason = Dap.Data.StoppedEvent_body_reason.Breakpoint in
          let body = Dap.Data.StoppedEvent_body.make ~reason () in
          Ev.default_event_req Dap.Events.stopped body
          |> Ev.stoppedEvent |> Dap_result.ok)
    in
    BreakStopped.handle l
  in

  let enc =
    Ev.Message.enc Dap.Events.breakpoint Dap.Data.BreakpointEvent_body.enc
  in
  let break_ev =
    let breakpoint = Dap.Data.Breakpoint.make ~verified:true () in
    let body =
      Dap.Data.BreakpointEvent_body.make
        ~reason:Dap.Data.BreakpointEvent_body_reason.New
        ~breakpoint
        ()
    in
    Ev.(
      Message.make ~seq:111 ~event:Dap.Events.breakpoint ~body ()
      |> Js.construct enc |> Js.to_string)
  in

  let%lwt s = handler ~state ~config break_ev in

  Printf.printf "%s" @@ Result.get_ok s ;
  let%lwt () =
    [%expect
      {|
    { "seq": 112, "type": "event", "event": "stopped",
      "body": { "reason": "breakpoint" } } |}]
  in

  (* should also have the correct seq numbers if error happens during handling *)
  let handler_err =
    let l =
      BreakStopped.make ~handler:(fun _ _ _req ->
          Res.default_response_error "testing error"
          |> Res.errorResponse |> Dap_result.error)
    in
    BreakStopped.handle l
  in
  let%lwt s = handler_err ~state ~config break_ev in
  Printf.printf "%s" @@ Result.get_error s ;
  [%expect
    {|
    { "seq": 112, "type": "response", "request_seq": 111, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
