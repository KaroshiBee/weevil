module Js = Data_encoding.Json
module Req = Dap_utils.Request
module Res = Dap_utils.Response
module RR = Dap_request_response
module M = Dap_message_exi

module Launch = RR.WithSeqr (struct
    type cmd = Dap_commands.launch
    type args = M.LaunchRequestArguments.t
    type pargs = M.Presence.req
    type body = M.EmptyObject.t option
    type pbody = M.Presence.opt
  end)


let%expect_test "Check sequencing request/response" =
  let handler =
    let l = Launch.make
        ~handler:(fun _req ->
            let body = M.EmptyObject.make () in
            Res.default_response_opt Dap_commands.launch body
            |> Res.launchResponse
            |> Dap_result.ok
          )
        ~ctor:Res.launchResponse
    in
    Launch.handle l
  in

  let req_launch = Req.(launchRequest @@ Message.make ~seq:10 ~command:Dap_commands.launch ~arguments:(M.LaunchRequestArguments.make ()) ()) in
  let enc_launch = Res.(Message.enc_opt Dap_commands.launch M.EmptyObject.enc) in

  let s =
    handler req_launch |> Dap_result.map (function
        | Res.LaunchResponse resp ->
          Js.construct enc_launch resp |> Js.to_string
        | _ -> assert false
      ) |> Dap_result.get_ok
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,
      "command": "launch", "body": {} } |}];


  (* NOTE can no longer pass wrong type in *)
  (* let req_attach = Req.(attachRequest @@ Message.make ~seq:100 ~command:Dap_commands.attach ~arguments:(M.AttachRequestArguments.make ()) ()) in *)
  (* (\* should error with correct seq numbers if given the wrong request type *\) *)
  (* let s = handler req_attach |> Dap_result.get_error_str *)
  (* in *)
  (* Printf.printf "%s" s; *)
  (* [%expect {| *)
     (*   { "seq": 101, "type": "response", "request_seq": 100, "success": false, *)
     (*     "command": "error", *)
     (*     "body": *)
     (*       { "error": *)
     (*           { "id": 342703193, "format": "{error}", *)
     (*             "variables": { "error": "wrong request: expected LaunchRequest" } } } } |}]; *)

  (* should also have the correct seq numbers if error happens even with correct input *)
  let handler_err =
    let l = Launch.make
        ~handler:(fun _req ->
            Dap_utils.default_response_error "testing error"
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
    { "seq": 11, "type": "response", "request_seq": 10, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
