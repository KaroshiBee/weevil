module Js = Data_encoding.Json
open Dap_t
open Dap_handlers

let config : config = {launch_mode=`Attach}

let%expect_test "Check on_cancel" =
  let arguments = Dap_message.CancelArguments.make ~requestId:1 () in
  let cancel = Dap_message.CancelRequest (
      RequestMessage.make_opt
        ~seq:10
        ~command:Dap_command.cancel
        ~arguments
        ()
    ) in

  let v = Dap_handlers.on_cancel ~config (Result.Ok cancel) in
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
