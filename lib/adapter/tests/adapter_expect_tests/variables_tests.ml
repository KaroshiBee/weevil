include Dapper.Dap.Testing_utils
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json

module StateMock = Utils.StateMock
module Variables = Variables.T (StateMock)

let%expect_test "Check sequencing etc for variables" =
  let st = StateMock.make () in
  let command = Dap.Commands.variables in
  let req =
    Dap.Request.(
      Utils.variables_msg ~seq:20 ~vref:3
      |> Js.construct (Message.enc command D.VariablesArguments.enc)
      |> Js.to_string)
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {|
        { "seq": 20, "type": "request", "command": "variables",
          "arguments": { "variablesReference": 3 } } |}]
  in

  match Variables.handlers ~state:st with
  | f_resp :: [] ->
    (* happy path *)
    let%lwt resp = f_resp req in
    let resp = Result.get_ok resp in
    Printf.printf "%s" resp ;
    let%lwt () =
      [%expect
        {|
      { "seq": 1, "type": "response", "request_seq": 20, "success": true,
        "command": "variables",
        "body":
          { "variables":
              [ { "name": "parameter", "value": "..", "variablesReference": 5 },
                { "name": "storage", "value": "..", "variablesReference": 6 },
                { "name": "gas", "value": "..", "variablesReference": 7 },
                { "name": "stack", "value": "[..]", "variablesReference": 8 } ] } } |}]
    in
    Lwt.return_unit

  | _ -> failwith "error: expected one handler for variables"
