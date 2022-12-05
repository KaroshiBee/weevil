include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json
module Helpers = Utils.Helpers

module StateMock = struct
  include Helpers.StateMock

  let backend_ic t = t.ic

  let backend_oc t = t.oc

  let set_io t ic oc =
    t.ic <- Some ic;
    t.oc <- Some oc

end

let%expect_test "Check sequencing etc for stack trace" =
  let st = StateMock.make () in
  Lwt_io.with_temp_file ~temp_dir:"/dev/shm" (fun (_, oc) ->
      let () = StateMock.set_io st (Lwt_io.stdin) oc in
      let command = Dap.Commands.stackTrace in
      let req =
        Dap.Request.(
          Helpers.stack_trace_msg ~seq:20
          |> Js.construct (Message.enc command D.StackTraceArguments.enc)
          |> Js.to_string)
      in
      Printf.printf "%s" req ;
      let%lwt () =
        [%expect
          {|
        { "seq": 20, "type": "request", "command": "stackTrace",
          "arguments": { "threadId": 1 } } |}]
      in

      match Stack_trace.handlers ~state:st with
      | f_resp :: [] ->
        (* happy path *)
        let%lwt resp = f_resp req in
        let resp = Result.get_ok resp in
        Printf.printf "%s" resp ;
        let%lwt () =
          [%expect
            {|
              { "seq": 21, "type": "response", "request_seq": 20, "success": true,
                "command": "stackTrace",
                "body":
                  { "stackFrames":
                      [ { "id": 1, "name": "main",
                          "source":
                            { "name": "example.tz",
                              "path": "/home/wyn/dev/weevil/example.tz" }, "line": 6,
                          "column": 0 } ], "totalFrames": 1 } } |}]
        in
        Lwt.return_unit

      | _ -> failwith "error: expected one handler for stack trace"
    )
