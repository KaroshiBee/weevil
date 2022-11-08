module Js = Data_encoding.Json
open Dap_message


let%expect_test "Check sequencing request/response/event" =
  let arguments = CancelArguments.make ~requestId:1 () in
  let cancel = RequestMessage.make_opt
      ~seq:10
      ~command:Dap_commands.cancel
      ~arguments
      () in
  let enc = RequestMessage.enc_opt Dap_commands.cancel CancelArguments.enc in
  let s = Js.construct enc cancel |> Js.to_string in

  (* Check json encoding of a request *)
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 10, "type": "request", "command": "cancel",
      "arguments": { "requestId": 1 } } |}];

  (* check request/response state change *)
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
      |> fun v -> request_response v f
    )
  in
  let s =
    match Dap_flow.to_result v with
    | Result.Ok (CancelResponse resp) ->
      let enc = ResponseMessage.enc_opt Dap_commands.cancel Dap_base.EmptyObject.enc in
      Js.construct enc resp |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,
      "command": "cancel", "body": {} } |}];

  (* check request/error state change *)
  let f = fun _ ->
    let e = "something went wrong" in
    let id = Hashtbl.hash e in
    let variables = `O [("error", `String e)] in
    let error = Message.make ~id ~format:"{error}" ~variables () in
    let body = ErrorResponse_body.make ~error () in
    let command = Dap_commands.error in
    ErrorResponse (
      ResponseMessage.make
        ~seq:0
        ~request_seq:0
        ~success:false
        ~command
        ~body
        ()
    ) |> Dap_flow.from_response
  in
  let v = Dap_flow.(
      from_request @@ CancelRequest cancel
      |> fun v -> raise_error v f
    )
  in
  let s =
    match Dap_flow.to_result v with
    | Result.Ok (ErrorResponse resp) ->
      let enc = ResponseMessage.enc Dap_commands.error ErrorResponse_body.enc in
      Js.construct enc resp |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 11, "type": "response", "request_seq": 10, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 1022476669, "format": "{error}",
              "variables": { "error": "something went wrong" } } } } |}];

  (* check response/event state change *)
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
  let v = Dap_flow.response_event v f in
  let s =
    match Dap_flow.to_result v with
    | Result.Ok (TerminatedEvent ev) ->
      let enc = EventMessage.enc_opt Dap_events.terminated TerminatedEvent_body.enc in
      Js.construct enc ev |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {| { "seq": 12, "type": "event", "event": "terminated" } |}];

  (* check event/event state change *)
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
      let enc = EventMessage.enc Dap_events.exited ExitedEvent_body.enc in
      Js.construct enc ev |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {| { "seq": 13, "type": "event", "event": "exited", "body": { "exitCode": 0 } } |}]