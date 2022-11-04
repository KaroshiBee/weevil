module Js = Data_encoding.Json
module Req = Dap.Request
module Res = Dap.Response

module Launch = Dap_request_response.WithSeqr (struct
  type cmd = Dap.Commands.launch
  type args = Dap.LaunchRequestArguments.t
  type pargs = Dap.Presence.req
  type body = Dap.EmptyObject.t option
  type pbody = Dap.Presence.opt
end)


let%expect_test "Check sequencing request/response" =
  let handler =
    let l = Launch.make
        ~handler:(fun _req ->
            let body = Dap.EmptyObject.make () in
            Res.default_response_opt Dap.Commands.launch body
            |> Res.launchResponse
            |> Dap_result.ok
          )
        ~ctor:Res.launchResponse
    in
    Launch.handle l
  in

  let req_launch = Req.(launchRequest @@ Message.make ~seq:101 ~command:Dap.Commands.launch ~arguments:(Dap.LaunchRequestArguments.make ()) ()) in
  let enc_launch = Res.(Message.enc_opt Dap.Commands.launch Dap.EmptyObject.enc) in

  let s =
    handler req_launch |> Dap_result.map (function
        | Res.LaunchResponse resp ->
          Js.construct enc_launch resp |> Js.to_string
        | _ -> assert false
      ) |> Dap_result.get_ok
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 102, "type": "response", "request_seq": 101, "success": true,
      "command": "launch", "body": {} } |}];


  (* NOTE can no longer pass wrong type in *)
  (* let req_attach = Req.(attachRequest @@ Message.make ~seq:100 ~command:Dap.Commands.attach ~arguments:(AttachRequestArguments.make ()) ()) in *)
  (* let s = handler req_attach |> Dap_result.get_error_str - NOTE wont compile *)

  (* should also have the correct seq numbers if error happens during handling *)
  let handler_err =
    let l = Launch.make
        ~handler:(fun _req ->
            Dap.default_response_error "testing error"
            |> Res.errorResponse
            |> Dap_result.error
          )
        ~ctor:Res.launchResponse
    in
    Launch.handle l
  in
  let s = handler_err req_launch |> Dap_result.get_error_str
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 102, "type": "response", "request_seq": 101, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
