include Dapper.Dap.Testing_utils
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json

module StateMock = Utils.StateMock
module ConfigDone = Configuration_done.T(StateMock)

let%expect_test "Check sequencing etc for configurationDone" =
  let state = StateMock.make () in
  let command = Dap.Commands.configurationDone in
  let req =
    Dap.Request.(
      Utils.configurationDone_msg ~seq:20
      |> Js.construct (Message.enc_opt command D.ConfigurationDoneArguments.enc)
      |> Js.to_string
    )
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {|
        { "seq": 20, "type": "request", "command": "configurationDone",
          "arguments": {} } |}]
  in

  match ConfigDone.handlers ~state with
  | f_resp :: [] ->
    (* happy path *)
    let%lwt resp = f_resp req in
    let resp = Result.get_ok resp in
    Printf.printf "%s" resp ;
    let%lwt () =
      [%expect
        {|
      { "seq": 1, "type": "response", "request_seq": 20, "success": true,
        "command": "configurationDone", "body": {} } |}]
    in

    (* unhappy path, f_resp is expecting a request *)
    let%lwt err =
      try%lwt
        f_resp "sfasfsdfsfd"
      with
      | Dap.Wrong_encoder (err, _) -> Lwt_result.fail err
    in
    Printf.printf "%s" @@ Result.get_error err ;
    let%lwt () =
      [%expect {|
        { "seq": -1, "type": "response", "request_seq": -1, "success": false,
          "command": "error",
          "message": "JSON.of_buffer expected value or array end (value or ']')",
          "body":
            { "error":
                { "id": 698498095, "format": "{error}",
                  "variables":
                    { "error":
                        "JSON.of_buffer expected value or array end (value or ']')" } } } } |}]
    in

    Lwt.return_unit

  | _ -> failwith "error: expected one handler for configurationDone"
