include Test_utils

open Dap_handlers

let config : config = {launch_mode=`Attach}

module CancelH = Handler (Cancel)
module InitializeH = Handler (Initialize)

let%expect_test "Check cancel handler" =
  let s = {| { "seq": 10, "type": "request", "command": "cancel", "arguments": { "requestId": 1 } } |} in
  let hdl = CancelH.make in
  let%lwt ss = CancelH.handle hdl ~config s in
  Printf.printf "%s" ss;

  let%lwt _ = [%expect {|
    Content-Length: 103
    
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,  "command": "cancel", "body": {} } |}] in

  (* test a bad input - NOTE the wrong command *)
  let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "requestId": 1 } } |} in
  let hdl = CancelH.make in
  let%lwt s = try%lwt
      CancelH.handle hdl ~config s
    with
      | JsMsg.Wrong_encoder err ->
        Lwt.return err
  in
  Printf.printf "%s" s;

  [%expect {| cannnot destruct: Failure("expected 'cancel', got 'initialize'") |}]

let%expect_test "Check initialize handler" =
  let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "adapterID": "weevil", "clientID":"1" } } |} in
  let hdl = InitializeH.make in
  let%lwt ss = InitializeH.handle hdl ~config s in
  Printf.printf "%s" ss;

  let%lwt _ =
    [%expect {|
    Content-Length: 107
    
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,  "command": "initialize", "body": {} }Content-Length: 66
    
    { "seq": 12, "type": "event", "event": "initialized", "body": {} } |}] in

  (* test a bad input - NOTE the clientId not clientID *)
  let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "adapterID": "weevil", "clientId":"1" } } |} in
  let hdl = InitializeH.make in
  let%lwt s = try%lwt
      InitializeH.handle hdl ~config s
    with
      | JsMsg.Wrong_encoder err ->
        Lwt.return err
  in
  Printf.printf "%s" s;

  [%expect {|
    cannnot destruct: Json_encoding.Unexpected_field("clientId") |}]
