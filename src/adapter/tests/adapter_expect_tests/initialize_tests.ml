include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json

module StateMock = struct
  type t = {
    mutable launch_mode : Launch_mode.t option;
    mutable seqr: D.Seqr.t;
    mutable config : Config.t;
  }

  let make () = {
    launch_mode = None;
    seqr = D.Seqr.make ~seq:0 ();
       config=Config.make ();
  }

  let connect_backend _ip _port = failwith "MOCK connect"

  let process_none _t = failwith "MOCK process none"

  let start_backend _t _ip _port _cmd = failwith "MOCK start backend"

  let ic _t = failwith "MOCK ic"

  let oc _t = Some Lwt_io.stdout

  let launch_mode t = t.launch_mode

  let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode

  let current_seqr t = t.seqr

  let set_seqr t seqr = t.seqr <- seqr

  let config t = t.config

  let set_config t config = t.config <- config

end

module Init = Initialize.T(StateMock)

let%expect_test "Check sequencing etc for attach" =
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
          "arguments": { "adapterID": "weevil" } } |}]
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


    (* unhappy path, f_resp is expecting a request *)
    let%lwt err =
      try%lwt
        f_resp ev
      with
      | Dap.Wrong_encoder err -> Lwt_result.fail err
    in
    Printf.printf "%s" @@ Result.get_error err ;
    let%lwt () =
      [%expect {| cannnot destruct: Json_encoding.Cannot_destruct at /: Missing object field command |}]
    in

    Lwt.return_unit

  | _ -> failwith "error: expected two handlers for attach"
