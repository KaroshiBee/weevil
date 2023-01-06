include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json
module Helpers = Utils.Helpers

module StateMock = Helpers.StateMock
module Scopes = Scopes.T (StateMock)

let%expect_test "Check sequencing etc for scopes" =
  let st = StateMock.make () in
  let command = Dap.Commands.scopes in
  let req =
    Dap.Request.(
      Helpers.scopes_msg ~seq:20
      |> Js.construct (Message.enc command D.ScopesArguments.enc)
      |> Js.to_string)
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {|
        { "seq": 20, "type": "request", "command": "scopes",
          "arguments": { "frameId": 2 } } |}]
  in

  match Scopes.handlers ~state:st with
  | f_resp :: [] ->
    (* happy path *)
    let%lwt resp = f_resp req in
    let resp = Result.get_ok resp in
    Printf.printf "%s" resp ;
    let%lwt () =
      [%expect
        {|
      { "seq": 1, "type": "response", "request_seq": 20, "success": true,
        "command": "scopes",
        "body":
          { "scopes":
              [ { "name": "Script Locals", "presentationHint": "locals",
                  "variablesReference": 3, "expensive": false } ] } } |}]
    in
    Lwt.return_unit

  | _ -> failwith "error: expected one handler for scopes"
