include Test_utils.Include

module Js = Data_encoding.Json
module Req = Dap.Request
module Res = Dap.Response

module Launch = Dap_handlers.Request_response.Make (struct
  type enum = Dap.Commands.launch
  type in_contents = Dap.LaunchRequestArguments.t
  type in_presence = Dap.Presence.req
  type out_contents = Dap.EmptyObject.t option
  type out_presence = Dap.Presence.opt
  type in_msg = (enum, in_contents, in_presence) Req.Message.t
  type out_msg = (enum, out_contents, out_presence) Res.Message.t

  let ctor_in = Req.launchRequest
  let enc_in = Req.Message.enc Dap.Commands.launch Dap.LaunchRequestArguments.enc
  let ctor_out = Res.launchResponse
  let enc_out = Res.Message.enc_opt Dap.Commands.launch Dap.EmptyObject.enc

end)

let%expect_test "Check sequencing request/response" =
  let config = Dap.Config.make () in
  let handler =
    let l = Launch.make
        ~handler:(fun _cfg _req ->
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
    handler ~config req_launch
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
        ~handler:(fun _ _req ->
            Dap.default_response_error "testing error"
            |> Res.errorResponse
            |> Dap_result.error
          )
    in
    Launch.handle l
  in
  let%lwt s =
    handler_err ~config req_launch
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
