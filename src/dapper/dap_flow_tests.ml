module Js = Data_encoding.Json
open Dap_t


let%expect_test "Check sequencing request/response/event" =
  let arguments = Dap_message.CancelArguments.make ~requestId:1 () in
  let cancel = RequestMessage.make_opt
      ~seq:10
      ~command:Dap_command.cancel
      ~arguments
      () in
  let enc = RequestMessage.enc_opt Dap_message.CancelArguments.enc in
  let s = Js.construct enc cancel |> Js.to_string in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 10, "type": "request", "command": "cancel",
      "arguments": { "requestId": 1 } } |}];

  let f = fun req ->
    let command = RequestMessage.command req in
    let body = EmptyObject.make () in
    ResponseMessage.make_opt
      ~seq:0
      ~request_seq:0
      ~success:true
      ~command
      ~body
      ()
    |> Result.ok
  in
  let v = Dap_flow.(
      of_req cancel
      |> fun v -> req_resp v f
    )
  in
  let s =
    match v with
    | Result.Ok resp ->
      let enc = ResponseMessage.enc_opt EmptyObject.enc in
      Js.construct enc resp |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,
      "command": "cancel", "body": {} } |}];

  let f = fun _resp ->
    let event = Dap_event.terminated in
    let body = None in
    EventMessage.make_opt
        ~seq:0
        ~event
        ?body
        ()
    |> Result.ok
  in
  let v = Dap_flow.resp_ev v f in
  let s =
    match v with
    | Result.Ok ev ->
      let enc = EventMessage.enc_opt Dap_message.TerminatedEvent_body.enc in
      Js.construct enc ev |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {| { "seq": 12, "type": "event", "event": "terminated" } |}];

  let f = fun _ev ->
    let event = Dap_event.exited in
    let body = Dap_message.ExitedEvent_body.make ~exitCode:0 () in
    EventMessage.make
      ~seq:0
      ~event
      ~body
      ()
    |> Result.ok
  in
  let v = Dap_flow.next_ev v f in
  let s = match v with
    | Result.Ok ev ->
      let enc = EventMessage.enc Dap_message.ExitedEvent_body.enc in
      Js.construct enc ev |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {| { "seq": 13, "type": "event", "event": "exited", "body": { "exitCode": 0 } } |}]
