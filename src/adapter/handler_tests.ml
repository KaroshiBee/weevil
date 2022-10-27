include Test_utils
module Js_msg = Dapper.Dap_js_msg

let config = Dapper.Dap_config.make ~launch_mode:(`Attach 9001) ()

(* let mock_backend = Lwt_io.(zero, null) *)
let mock_callback = fun _ -> Lwt.return_unit
let mock_subprocess = None

let%expect_test "Check cancel handler" =
  let open Cancel in
  let s = {| { "seq": 10, "type": "request", "command": "cancel", "arguments": { "requestId": 1 } } |} in
  let backend = Cancel.make_empty mock_subprocess mock_callback in
  let%lwt o = string_to_input s |> handle backend config in
  let%lwt ss = output_to_string o in
  Printf.printf "%s" @@ Result.get_ok ss;

  let%lwt _ = [%expect {|
    Content-Length: 103
    
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,  "command": "cancel", "body": {} } |}] in


  (* test a bad input - NOTE the wrong command *)
  let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "requestId": 1 } } |} in
  let ss =
    try
      let _ = string_to_input s in ""
    with
    | Js_msg.Wrong_encoder err -> err
  in
  Printf.printf "%s" ss;

  [%expect {| cannnot destruct: expected 'cancel', got 'initialize' |}]

let%expect_test "Check initialize handler" =
  let open Initialize in
  let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "adapterID": "weevil", "clientID":"1" } } |} in
  let backend = Initialize.make_empty mock_subprocess mock_callback in
  let%lwt o = string_to_input s |> handle backend config in
  let%lwt ss = output_to_string o in
  Printf.printf "%s" @@ Result.get_ok ss;

  let%lwt _ =
    [%expect {|
    Content-Length: 107
    
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,  "command": "initialize", "body": {} }Content-Length: 66
    
    { "seq": 12, "type": "event", "event": "initialized", "body": {} } |}] in

  (* test a bad input - NOTE the clientId not clientID *)
  let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "adapterID": "weevil", "clientId":"1" } } |} in
  let ss =
    try
      let _ = string_to_input s in ""
    with
    | Js_msg.Wrong_encoder err -> err
  in
  Printf.printf "%s" ss;

  [%expect {|
    cannnot destruct: Json_encoding.Unexpected_field("clientId") |}]
