module Js = Data_encoding.Json
open Dap_message_exi
open Dap_utils

module RE = Dap_response_event

module ProcessLaunched = RE.WithSeqr (struct
  type cmd = Dap_commands.launch
  type body = EmptyObject.t option
  type pbody = Dap_base.Presence.opt
  type ev = Dap_events.process
  type body_ = ProcessEvent_body.t
  type pbody_ = Dap_base.Presence.req

  type t = {
    f: (cmd, body, pbody) ResponseMessage.t -> (ev, body_, pbody_) EventMessage.t Dap_result.t;
  }

  let make f = {f;}

  let handle {f;} = function
    | LaunchResponse resp -> f resp |> Result.map processEvent
    | _ -> Result.error @@ default_response_error "wrong request: expected LaunchResponse"

end)


let%expect_test "Check sequencing response/event" =
  let handler =
    let l = ProcessLaunched.make (fun _ ->
        let startMethod = ProcessEvent_body_startMethod.Launch in
        let body =
          ProcessEvent_body.make
            ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
            ~startMethod
            ()
        in
        default_event_req Dap_events.process body
        |> Result.ok
      )
    in
    ProcessLaunched.handle l
  in

  let resp_launch = launchResponse @@ ResponseMessage.make_opt ~seq:11 ~request_seq:10 ~success:true ~command:Dap_commands.launch ~body:(EmptyObject.make ()) () in
  let enc_launch = EventMessage.enc Dap_events.process ProcessEvent_body.enc in

  let s =
    handler resp_launch |> Result.map (function
        | ProcessEvent ev ->
          Js.construct enc_launch ev |> Js.to_string
        | _ -> assert false
      ) |> Result.get_ok
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 12, "type": "event", "event": "process",
      "body":
        { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
          "startMethod": "launch" } } |}];

  let resp_cancel = cancelResponse @@ ResponseMessage.make_opt ~seq:111 ~request_seq:110 ~success:true ~command:Dap_commands.cancel ~body:(EmptyObject.make ()) () in
  let enc_error = ResponseMessage.enc Dap_commands.error ErrorResponse_body.enc in

  (* should error with correct seq numbers if given the wrong response type *)
  let s =
    handler resp_cancel |> Result.map_error (fun resp ->
        Js.construct enc_error resp |> Js.to_string
      ) |> Result.get_error
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 112, "type": "response", "request_seq": 111, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 584404924, "format": "{error}",
              "variables": { "error": "wrong request: expected LaunchResponse" } } } } |}];

  (* should also have the correct seq numbers if error happens even with correct input *)
  let handler_err =
    let l = ProcessLaunched.make (fun _req ->
        default_response_error "testing error"
        |> Result.error
      )
    in
    ProcessLaunched.handle l
  in
  let s =
    handler_err resp_launch |> Result.map_error (fun resp ->
        Js.construct enc_error resp |> Js.to_string
      ) |> Result.get_error
  in
  Printf.printf "%s" s;
  [%expect {|
    { "seq": 12, "type": "response", "request_seq": 11, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
