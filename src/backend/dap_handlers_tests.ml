module Js = Data_encoding.Json
open Dap_handlers

let config : config = {launch_mode=`Attach}

let%expect_test "Check cancel handler" =
  let s = {| { "seq": 10, "type": "request", "command": "cancel", "arguments": { "requestId": 1 } } |} in
  Dap_handlers.Cancel.handle ~config s
  |> Result.iter (fun ss -> Printf.printf "%s" ss);
  [%expect {|
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,
      "command": "cancel", "body": {} } |}]