module Js = Data_encoding.Json
open Dap_message_exi
open Dap_utils

module RR = Dap_request_response

module Launch = RR.WithSeqr (struct
  type cmd = Dap_commands.launch
  type args = LaunchRequestArguments.t
  type pargs = Dap_base.Presence.req
  type body = EmptyObject.t option
  type pbody = Dap_base.Presence.opt
  type t = {
    f: (cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t Dap_result.t;
  }

  let make f = {f;}

  let handle {f;} = function
    | LaunchRequest req -> f req |> Result.map launchResponse
    | _ -> Result.error @@ default_response_error "wrong request: expected LaunchRequest"

end)


let%expect_test "Check sequencing request/response" =
  let handler =
    let l = Launch.make (fun _req ->
        let body = EmptyObject.make () in
        default_response_opt Dap_commands.launch body
        |> Result.ok
      )
    in
    Launch.handle l
  in

  let req_launch = launchRequest @@ RequestMessage.make ~seq:10 ~command:Dap_commands.launch ~arguments:(LaunchRequestArguments.make ()) () in
  let enc_launch = ResponseMessage.enc_opt Dap_commands.launch Dap_base.EmptyObject.enc in

  let s =
    handler req_launch |> Result.map (function
        | LaunchResponse resp ->
          Js.construct enc_launch resp |> Js.to_string
        | _ -> assert false
      ) |> Result.get_ok
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 11, "type": "response", "request_seq": 10, "success": true,
      "command": "launch", "body": {} } |}];

  let req_attach = attachRequest @@ RequestMessage.make ~seq:100 ~command:Dap_commands.attach ~arguments:(AttachRequestArguments.make ()) () in
  let enc_error = ResponseMessage.enc Dap_commands.error ErrorResponse_body.enc in

  (* should error with correct seq numbers if given the wrong request type *)
  let s =
    handler req_attach |> Result.map_error (fun resp ->
        Js.construct enc_error resp |> Js.to_string
      ) |> Result.get_error
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 101, "type": "response", "request_seq": 100, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 342703193, "format": "{error}",
              "variables": { "error": "wrong request: expected LaunchRequest" } } } } |}];

  (* should also have the correct seq numbers if error happens even with correct input *)
  let handler_err =
    let l = Launch.make (fun _req ->
        default_response_error "testing error"
        |> Result.error
      )
    in
    Launch.handle l
  in
  let s =
    handler_err req_launch |> Result.map_error (fun resp ->
        Js.construct enc_error resp |> Js.to_string
      ) |> Result.get_error
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 11, "type": "response", "request_seq": 10, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
