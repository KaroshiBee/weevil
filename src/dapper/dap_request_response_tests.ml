include Test_utils.Include

module Js = Data_encoding.Json
module Req = Dap.Request
module Res = Dap.Response

module Launch = Dap_request_response.WithSeqr (struct
  type cmd = Dap.Commands.launch
  type args = Dap.LaunchRequestArguments.t
  type pargs = Dap.Presence.req
  type body = Dap.EmptyObject.t option
  type pbody = Dap.Presence.opt
  type in_msg = (cmd, args, pargs) Req.Message.t
  type out_msg = (cmd, body, pbody) Res.Message.t

  let ctor_in = Req.launchRequest
  let enc_in = Req.Message.enc Dap.Commands.launch Dap.LaunchRequestArguments.enc
  let ctor_out = Res.launchResponse
  let enc_out = Res.Message.enc_opt Dap.Commands.launch Dap.EmptyObject.enc

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
    in
    Launch.handle l
  in

  let req_launch = Req.(launchRequest @@ Message.make ~seq:101 ~command:Dap.Commands.launch ~arguments:(Dap.LaunchRequestArguments.make ()) ()) in
  let enc_launch = Res.(Message.enc_opt Dap.Commands.launch Dap.EmptyObject.enc) in

  let%lwt s =
    handler req_launch
    |> Dap_result.map ~f:(function
        | Res.LaunchResponse resp ->
          Js.construct enc_launch resp |> Js.to_string
        | _ -> assert false
      )
    |> Dap_result.to_lwt_result
  in
  Printf.printf "%s" @@ Result.get_ok s;
  let%lwt () = [%expect {|
    { "seq": 102, "type": "response", "request_seq": 101, "success": true,
      "command": "launch", "body": {} } |}]
  in

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
    in
    Launch.handle l
  in
  let%lwt s =
    handler_err req_launch
    |> Dap_result.to_lwt_error_as_str
  in
  Printf.printf "%s" @@ Result.get_error s;
  [%expect {|
    { "seq": 102, "type": "response", "request_seq": 101, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
