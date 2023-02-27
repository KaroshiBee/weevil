include Utilities.Testing_utils
module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module TestState = Dap_state.T ()

module BreakStopped =
  Dap_handlers.Raise_event.Make
    (struct
      type enum = Dap.Events.stopped

      type contents = Dap.Data.StoppedEvent_body.t

      type presence = Dap.Data.Presence.req

      type msg = (enum, contents, presence) Ev.Message.t

      type t = msg Ev.t

      let ctor = Ev.stoppedEvent

      let enc = Ev.Message.enc Dap.Events.stopped Dap.Data.StoppedEvent_body.enc
    end)
    (TestState)

let _reset state =
  (* need to set the state as if messsages have already been exchanged *)
  TestState.set_seqr state @@ Dap.Seqr.make ~seq:111 ~request_seq:110 ()

let%expect_test "Check sequencing raise event" =
  let state = TestState.make () in
  _reset state;

  let handler =
    BreakStopped.make ~handler:(fun ~state:_ _ ->
        let reason = Dap.Data.StoppedEvent_body_reason.Breakpoint in
        let body = Dap.Data.StoppedEvent_body.make ~reason () in
        Ev.default_event_req Dap.Events.stopped body
        |> Ev.stoppedEvent |> Dap_result.ok)
  in

  let seqr = TestState.current_seqr state in
  let request_seq = Dap.Seqr.request_seq seqr in
  Printf.printf "request_seq %d" request_seq;
  let%lwt () =
    [%expect {| request_seq 110 |}]
  in

  let seq = Dap.Seqr.seq seqr in
  Printf.printf "seq %d" seq;
  let%lwt () =
    [%expect {| seq 111 |}]
  in

  let%lwt s = handler ~state "" in

  Printf.printf "%s" @@ Result.get_ok s ;
  let%lwt () =
    [%expect
      {|
    { "seq": 112, "type": "event", "event": "stopped",
      "body": { "reason": "breakpoint" } } |}]
  in

  let seqr = TestState.current_seqr state in
  let request_seq = Dap.Seqr.request_seq seqr in
  Printf.printf "request_seq %d" request_seq;
  let%lwt () =
    [%expect {| request_seq 110 |}]
  in

  let seq = Dap.Seqr.seq seqr in
  Printf.printf "seq %d" seq;
  let%lwt () =
    [%expect {| seq 112 |}]
  in

  (* should also have the correct seq numbers if error happens during handling *)
  _reset state;
  let handler_err =
    BreakStopped.make ~handler:(fun ~state:_ _req ->
        Res.default_response_error "testing error"
        |> Res.errorResponse |> Dap_result.error)
  in
  let%lwt s = handler_err ~state "" in
  Printf.printf "%s" @@ Result.get_error s ;
  let%lwt () = [%expect
    {|
    { "seq": 112, "type": "response", "request_seq": 110, "success": false,
      "command": "error", "message": "testing error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
  in
  let seqr = TestState.current_seqr state in
  let request_seq = Dap.Seqr.request_seq seqr in
  Printf.printf "request_seq %d" request_seq;
  let%lwt () =
    [%expect {| request_seq 110 |}]
  in

  let seq = Dap.Seqr.seq seqr in
  Printf.printf "seq %d" seq;
  [%expect {| seq 112 |}]
