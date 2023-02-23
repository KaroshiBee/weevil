include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json
module Helpers = Utils.Helpers

module StateMock = Helpers.StateMock
module Attach = Attach.T (StateMock)

let script_filename = "data/multiply_2_x_25_equals_50.tz"
let storage = "Unit"
let parameter = "Unit"
let entrypoint = "default"

let%expect_test "Check sequencing etc for attach" =
  let state = StateMock.make () in
  let command = Dap.Commands.attach in
  let req =
    Dap.Request.(
      Dap.Utils.attach_msg ~seq:20 ~script_filename ~storage ~parameter ~entrypoint ()
      |> Js.construct (Message.enc command D.AttachRequestArguments.enc)
      |> Js.to_string
    )
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {|
        { "seq": 20, "type": "request", "command": "attach",
          "arguments":
            { "type": "tezos-weevil-tcp", "request": "attach", "mode": "attach",
              "name": "Tezos-Weevil::Attach<2>", "host": "localhost",
              "debugServer": 9000,
              "script_filename": "data/multiply_2_x_25_equals_50.tz",
              "storage": "Unit", "parameter": "Unit", "entrypoint": "default" } } |}]
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
      { "seq": 1, "type": "response", "request_seq": 20, "success": true,
        "command": "attach", "body": {} } |}]
    in

    let%lwt ev = f_ev "string doesnt matter" in
    let ev = Result.get_ok ev in
    Printf.printf "%s" ev ;
    let%lwt () = [%expect {|
        { "seq": 2, "type": "event", "event": "process",
          "body":
            { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
              "startMethod": "attach" } } |}] in

    let%lwt ev = f_attached "string doesnt matter" in
    let ev = Result.get_ok ev in
    Printf.printf "%s" ev ;
    let%lwt () = [%expect {|
        { "seq": 3, "type": "event", "event": "stopped",
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
      Dap.Utils.launch_msg ~seq:20 ~script_filename ~storage ~parameter ~entrypoint ()
      |> Js.construct (Message.enc command D.LaunchRequestArguments.enc)
      |> Js.to_string
    )
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {|
        { "seq": 20, "type": "request", "command": "launch",
          "arguments":
            { "type": "tezos-weevil-tcp", "request": "launch", "mode": "launch",
              "name": "Tezos-Weevil::Launch<2>", "host": "localhost",
              "debugServer": 9000,
              "script_filename": "data/multiply_2_x_25_equals_50.tz",
              "storage": "Unit", "parameter": "Unit", "entrypoint": "default" } } |}]
  in

  match Attach.handlers ~state with
  | f_resp :: _f_ev :: _f_attached :: [] ->
    (* unhappy path, f_resp is expecting an attach request *)
    let%lwt err =
      try%lwt
        f_resp req
      with
      | Dap.Wrong_encoder (err, _) -> Lwt_result.fail err
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
