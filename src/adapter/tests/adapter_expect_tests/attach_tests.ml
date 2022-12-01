include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json
module Helpers = Utils.Helpers

module StateMock = Helpers.StateMock
module Attach = Attach.T (StateMock)

let%expect_test "Check sequencing etc for attach" =
  let state = StateMock.make () in
  let command = Dap.Commands.attach in
  let req =
    Dap.Request.(
      Helpers.attach_msg ~seq:20
      |> Js.construct (Message.enc command D.AttachRequestArguments.enc)
      |> Js.to_string
    )
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {| { "seq": 20, "type": "request", "command": "attach", "arguments": {} } |}]
  in

  match Attach.handlers ~state with
  | f_resp :: f_ev :: f_attached :: [] ->
    (* happy path *)
    let%lwt resp = f_resp req in
    let resp = Result.get_ok resp in
    Printf.printf "%s" resp ;
    let%lwt () =
      [%expect
        {|
      { "seq": 21, "type": "response", "request_seq": 20, "success": true,
        "command": "attach", "body": {} } |}]
    in

    let%lwt ev = f_ev "string doesnt matter" in
    let ev = Result.get_ok ev in
    Printf.printf "%s" ev ;
    let%lwt () = [%expect {|
        { "seq": 22, "type": "event", "event": "process",
          "body":
            { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
              "startMethod": "attach" } } |}] in

    let%lwt ev = f_attached "string doesnt matter" in
    let ev = Result.get_ok ev in
    Printf.printf "%s" ev ;
    let%lwt () = [%expect {|
        { "seq": 23, "type": "event", "event": "stopped",
          "body":
            { "reason": "entry", "threadId": 1, "preserveFocusHint": true,
              "allThreadsStopped": true } } |}] in

    let lmode =
      match state |> StateMock.launch_mode |> Option.get with
      | `Attach -> "attach"
      | _ -> failwith "error: expected 'Attach' launch mode"
    in
    Printf.printf "%s" lmode;
    let%lwt () = [%expect {| attach |}] in

    Lwt.return_unit

  | _ -> failwith "error: expected three handlers for attach"


let%expect_test "Check bad input for attach" =
  let state = StateMock.make () in
  let lmode = state |> StateMock.launch_mode |> Option.map Dap.Launch_mode.show |> Option.value ~default:"not set" in
  Printf.printf "%s" lmode;
  let%lwt () = [%expect {| not set |}] in
  let command = Dap.Commands.launch in
  let req =
    Dap.Request.(
      Helpers.launch_msg ~seq:20
      |> Js.construct (Message.enc command D.LaunchRequestArguments.enc)
      |> Js.to_string
    )
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {| { "seq": 20, "type": "request", "command": "launch", "arguments": {} } |}]
  in

  match Attach.handlers ~state with
  | f_resp :: _f_ev :: _f_attached :: [] ->
    (* unhappy path, f_resp is expecting an attach request *)
    let%lwt err =
      try%lwt
        f_resp req
      with
      | Dap.Wrong_encoder err -> Lwt_result.fail err
    in
    Printf.printf "%s" @@ Result.get_error err ;
    let%lwt () =
      [%expect {| cannnot destruct: expected 'attach', got 'launch' |}]
    in

    let lmode = state |> StateMock.launch_mode |> Option.map Dap.Launch_mode.show |> Option.value ~default:"not set" in
    Printf.printf "%s" lmode;
    let%lwt () = [%expect {| not set |}] in

    Lwt.return_unit

  | _ -> failwith "error: expected three handlers for attach"
