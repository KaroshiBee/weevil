module Js = Data_encoding.Json
open Dap_t
open Dap_handlers

let config : config = {launch_mode=`Attach}

let%expect_test "Check on_cancel" =
  let s = {| { "seq": 10, "type": "request", "command": "cancel", "arguments": { "requestId": 1 } } |} in
  let enc = RequestMessage.enc_opt Dap_message.CancelArguments.enc in
  let cancel =
    Js.from_string s
    |> Result.map (Js.destruct enc)
    |> Result.map (fun x -> Dap_message.CancelRequest x)
  in
  let v = Dap_handlers.on_cancel ~config cancel in
  let s =
    match v with
    | Result.Ok (Dap_message.CancelResponse resp) ->
      let enc = ResponseMessage.enc_opt EmptyObject.enc in
      Js.construct enc resp |> Js.to_string
    | _ -> assert false
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,
      "command": "cancel", "body": {} } |}];
