include Test_utils.Include
module Js = Data_encoding.Json
module Req = Dap.Request
module Res = Dap.Response

module Launch =
  Dap_handlers.Request_response.Make
    (struct
      type enum = Dap.Commands.launch

      type contents = Dap.Data.LaunchRequestArguments.t

      type presence = Dap.Data.Presence.req

      type msg = (enum, contents, presence) Req.Message.t

      type t = msg Req.t

      let ctor = Req.launchRequest

      let enc =
        Req.Message.enc Dap.Commands.launch Dap.Data.LaunchRequestArguments.enc
    end)
    (struct
      type enum = Dap.Commands.launch

      type contents = Dap.Data.EmptyObject.t option

      type presence = Dap.Data.Presence.opt

      type msg = (enum, contents, presence) Res.Message.t

      type t = msg Res.t

      let ctor = Res.launchResponse

      let enc = Res.Message.enc_opt Dap.Commands.launch Dap.Data.EmptyObject.enc
    end)
    (Dap_handlers.State)

let%expect_test "Check sequencing request/response" =
  let config = Dap.Config.make () in
  let state = Dap_handlers.State.make in
  let handler =
    let l =
      Launch.make ~handler:(fun _st _cfg _req ->
          let body = Dap.Data.EmptyObject.make () in
          Res.default_response_opt Dap.Commands.launch body
          |> Res.launchResponse |> Dap_result.ok)
    in
    Launch.handle l
  in

  let enc =
    Req.Message.enc Dap.Commands.launch Dap.Data.LaunchRequestArguments.enc
  in
  let req_launch =
    Req.(
      Message.make
        ~seq:101
        ~command:Dap.Commands.launch
        ~arguments:(Dap.Data.LaunchRequestArguments.make ())
        ()
      |> Js.construct enc |> Js.to_string)
  in

  let%lwt s = handler ~state ~config req_launch in
  Printf.printf "%s" @@ Result.get_ok s ;
  let%lwt () =
    [%expect
      {|
    { "seq": 102, "type": "response", "request_seq": 101, "success": true,
      "command": "launch", "body": {} } |}]
  in

  (* NOTE can no longer pass wrong type in *)
  (* let req_attach = Req.(attachRequest @@ Message.make ~seq:100 ~command:Dap.Commands.attach ~arguments:(AttachRequestArguments.make ()) ()) in *)
  (* let s = handler req_attach |> Dap_result.get_error_str - NOTE wont compile *)

  (* should also have the correct seq numbers if error happens during handling *)
  let handler_err =
    let l =
      Launch.make ~handler:(fun _ _ _req ->
          Res.default_response_error "testing error"
          |> Res.errorResponse |> Dap_result.error)
    in
    Launch.handle l
  in
  let%lwt s = handler_err ~state ~config req_launch in
  Printf.printf "%s" @@ Result.get_error s ;
  [%expect
    {|
    { "seq": 102, "type": "response", "request_seq": 101, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
