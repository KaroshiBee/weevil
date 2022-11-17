include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json
module Helpers = Utils.Helpers

module StateMock = Helpers.StateMock
module Init = Initialize.T(StateMock)

let%expect_test "Check sequencing etc for init" =
  let state = StateMock.make () in
  let command = Dap.Commands.initialize in
  let req =
    Dap.Request.(
      Helpers.initialize_msg ~seq:20
      |> Js.construct (Message.enc command D.InitializeRequestArguments.enc)
      |> Js.to_string
    )
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {|
        { "seq": 20, "type": "request", "command": "initialize",
          "arguments": { "clientID": "12345", "adapterID": "weevil" } } |}]
  in

  match Init.handlers ~state with
  | f_resp :: f_ev :: [] ->
    (* happy path *)
    let%lwt resp = f_resp req in
    let resp = Result.get_ok resp in
    Printf.printf "%s" resp ;
    let%lwt () =
      [%expect
        {|
      { "seq": 21, "type": "response", "request_seq": 20, "success": true,
        "command": "initialize",
        "body":
          { "supportsConfigurationDoneRequest": true,
            "supportsRestartRequest": true, "supportsTerminateRequest": true } } |}]
    in

    let%lwt ev = f_ev "string doesnt matter" in
    let ev = Result.get_ok ev in
    Printf.printf "%s" ev ;
    let%lwt () = [%expect {|
        { "seq": 22, "type": "event", "event": "initialized", "body": {} } |}] in

    (* should have set the client config *)
    let client_cfg =
      StateMock.client_config state
      |> Option.get
    in
    Printf.printf "%s" @@ D.InitializeRequestArguments.adapterID client_cfg;
    let%lwt () = [%expect {| weevil |}] in
    Printf.printf "%s" (D.InitializeRequestArguments.clientID client_cfg |> Option.get);
    let%lwt () = [%expect {| 12345 |}] in
    Printf.printf "%s" (D.InitializeRequestArguments.clientName client_cfg |> Option.value ~default:"not set");
    let%lwt () = [%expect {| not set |}] in
    Lwt.return_unit
  | _ -> failwith "error: expected two handlers for init"


let%expect_test "Check bad input for init" =
  let state = StateMock.make () in
  let adapter_id =
    StateMock.client_config state
    |> Option.map D.InitializeRequestArguments.adapterID
    |> Option.get
  in
  Printf.printf "%s" adapter_id;
  let%lwt () = [%expect {| MOCK |}] in

  let command = Dap.Commands.attach in
  (* unhappy path, f_resp is expecting an init request *)
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

  match Init.handlers ~state with
  | f_resp :: _f_ev :: [] ->

    let%lwt err =
      try%lwt
        f_resp req
      with
      | Dap.Wrong_encoder err -> Lwt_result.fail err
    in
    Printf.printf "%s" @@ Result.get_error err ;
    let%lwt () =
      [%expect {| cannnot destruct: expected 'initialize', got 'attach' |}]
    in

    (* should not have set the client config *)
    let adapter_id =
      StateMock.client_config state
      |> Option.map D.InitializeRequestArguments.adapterID
      |> Option.get
    in
    Printf.printf "%s" adapter_id;
    let%lwt () = [%expect {| MOCK |}] in

    Lwt.return_unit

  | _ -> failwith "error: expected two handlers for init"
