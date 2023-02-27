include Dapper.Dap.Testing_utils
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json
module Helpers = Utils.Helpers

module StateMock = Helpers.StateMock
module Threads = Threads.T (StateMock)

let%expect_test "Check sequencing etc for threads" =
  let st = StateMock.make () in
  let command = Dap.Commands.threads in
  let req =
    Dap.Request.(
      Helpers.threads_msg ~seq:20
      |> Js.construct (Message.enc_opt command D.EmptyObject.enc)
      |> Js.to_string)
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {| { "seq": 20, "type": "request", "command": "threads", "arguments": {} } |}]
  in

  match Threads.handlers ~state:st with
  | f_resp :: [] ->
    (* happy path *)
    let%lwt resp = f_resp req in
    let resp = Result.get_ok resp in
    Printf.printf "%s" resp ;
    let%lwt () =
      [%expect
        {|
      { "seq": 1, "type": "response", "request_seq": 20, "success": true,
        "command": "threads",
        "body": { "threads": [ { "id": 1, "name": "main thread" } ] } } |}]
    in
    Lwt.return_unit

  | _ -> failwith "error: expected one handler for threads"
