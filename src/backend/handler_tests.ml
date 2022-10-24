include Test_utils
open Handler_t

let config : config = {
  launch_mode=`Attach;
  ic=None;
  oc=None;
}


let%expect_test "Check cancel handler" =
  let open Cancel in
  let s = {| { "seq": 10, "type": "request", "command": "cancel", "arguments": { "requestId": 1 } } |} in
  let%lwt o = from_string s |> handle ~config in
  let%lwt ss = to_string o in
  Printf.printf "%s" @@ Result.get_ok ss;

  let%lwt _ = [%expect {|
    Content-Length: 103
    
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,  "command": "cancel", "body": {} } |}] in


  (* test a bad input - NOTE the wrong command *)
  let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "requestId": 1 } } |} in
  let ss =
    try
      let _ = from_string s in ""
    with
    | Js_msg.Wrong_encoder err -> err
  in
  Printf.printf "%s" ss;

  [%expect {| cannnot destruct: expected 'cancel', got 'initialize' |}]

let%expect_test "Check initialize handler" =
  let open Initialize in
  let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "adapterID": "weevil", "clientID":"1" } } |} in
  let%lwt o = from_string s |> handle ~config in
  let%lwt ss = to_string o in
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
      let _ = from_string s in ""
    with
    | Js_msg.Wrong_encoder err -> err
  in
  Printf.printf "%s" ss;

  [%expect {|
    cannnot destruct: Json_encoding.Unexpected_field("clientId") |}]
