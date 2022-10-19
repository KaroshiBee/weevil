module Js = Data_encoding.Json
open Dap_message


let%expect_test "Check sequencing request/response/event" =
  let arguments = CancelArguments.make ~requestId:1 () in
  let cancel = RequestMessage.make_opt
      ~seq:10
      ~command:Dap_commands.cancel
      ~arguments
      () in
  let enc = RequestMessage.enc_opt CancelArguments.enc in
  let s = Js.construct enc cancel |> Js.to_string in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 10, "type": "request", "command": "cancel",
      "arguments": { "requestId": 1 } } |}];

  let f = fun _ ->
    (* NOTE could also get the command off input *)
    let command = Dap_commands.cancel in
    let body = Dap_base.EmptyObject.make () in
    CancelResponse (
      ResponseMessage.make_opt
        ~seq:0
        ~request_seq:0
        ~success:true
        ~command
        ~body
        ()
    )
    |> Dap_flow.from_response
  in
  let v = Dap_flow.(
      from_request @@ CancelRequest cancel
      |> fun v -> on_request v f
    )
  in
  let s =
    match Dap_flow.to_result v with
    | Result.Ok (CancelResponse resp) ->
      let enc = ResponseMessage.enc_opt Dap_base.EmptyObject.enc in
      Js.construct enc resp |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,
      "command": "cancel", "body": {} } |}];

  let f = fun _ ->
    let event = Dap_events.terminated in
    let body  = None in
    TerminatedEvent (
      EventMessage.make_opt
        ~seq:0
        ~event
        ?body
        ()
    )
    |> Dap_flow.from_event
  in
  let v = Dap_flow.on_response v f in
  let s =
    match Dap_flow.to_result v with
    | Result.Ok (TerminatedEvent ev) ->
      let enc = EventMessage.enc_opt TerminatedEvent_body.enc in
      Js.construct enc ev |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {| { "seq": 12, "type": "event", "event": "terminated" } |}];

  let f = fun _ ->
    let event = Dap_events.exited in
    let body = ExitedEvent_body.make ~exitCode:0 () in
    ExitedEvent (
      EventMessage.make
        ~seq:0
        ~event
        ~body
        ()
    )
    |> Dap_flow.from_event
  in
  let v = Dap_flow.raise_event v f in
  let s = match Dap_flow.to_result v with
    | Result.Ok (ExitedEvent ev) ->
      let enc = EventMessage.enc ExitedEvent_body.enc in
      Js.construct enc ev |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {| { "seq": 13, "type": "event", "event": "exited", "body": { "exitCode": 0 } } |}]
