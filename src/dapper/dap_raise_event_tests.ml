module Js = Data_encoding.Json
open Dap_message_exi
open Dap_utils

module EE = Dap_raise_event

module BreakStopped = EE.WithSeqr (struct
  type ev = Dap_events.breakpoint
  type body = BreakpointEvent_body.t
  type pbody = Dap_base.Presence.req
  type ev_ = Dap_events.stopped
  type body_ = StoppedEvent_body.t
  type pbody_ = Dap_base.Presence.req

  type t = {
    f: (ev, body, pbody) EventMessage.t -> (ev_, body_, pbody_) EventMessage.t Dap_result.t;
  }

  let make f = {f;}

  let handle {f;} = function
    | BreakpointEvent ev -> f ev |> Dap_result.map stoppedEvent
    | _ -> Dap_result.error @@ default_response_error "wrong event: expected Breakpoint event"

end)


let%expect_test "Check sequencing event/event" =
  let handler =
    let l = BreakStopped.make (fun _ ->
        let reason = StoppedEvent_body_reason.Breakpoint in
        let body =
          StoppedEvent_body.make
            ~reason
            ()
        in
        default_event_req Dap_events.stopped body
        |> Dap_result.ok
      )
    in
    BreakStopped.handle l
  in

  let break_ev =
    let breakpoint = Breakpoint.make ~verified:true () in
    let body = BreakpointEvent_body.make ~reason:BreakpointEvent_body_reason.New ~breakpoint () in
    breakpointEvent @@ EventMessage.make ~seq:11 ~event:Dap_events.breakpoint ~body () in
  let enc_stopper = EventMessage.enc Dap_events.stopped StoppedEvent_body.enc in

  let s =
    handler break_ev |> Dap_result.map (function
        | StoppedEvent ev ->
          Js.construct enc_stopper ev |> Js.to_string
        | _ -> assert false
      ) |> Dap_result.get_ok
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 12, "type": "event", "event": "stopped",
      "body": { "reason": "breakpoint" } } |}];

  let ev_exit = exitedEvent @@ EventMessage.make ~seq:111 ~event:Dap_events.exited ~body:(ExitedEvent_body.make ~exitCode:0 ()) () in

  (* should error with correct seq numbers if given the wrong event type *)
  let s = handler ev_exit |> Dap_result.get_error_str
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 112, "type": "response", "request_seq": 111, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 365625516, "format": "{error}",
              "variables": { "error": "wrong event: expected Breakpoint event" } } } } |}];

  (* should also have the correct seq numbers if error happens even with correct input *)
  let handler_err =
    let l = BreakStopped.make (fun _req ->
        default_response_error "testing error"
        |> Dap_result.error
      )
    in
    BreakStopped.handle l
  in
  let s = handler_err break_ev |> Dap_result.get_error_str
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 12, "type": "response", "request_seq": 11, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
