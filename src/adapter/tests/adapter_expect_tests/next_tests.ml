include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json
module Helpers = Utils.Helpers

module StateMock = struct
  include Helpers.StateMock

  let backend_oc t = t.oc

  let set_io t oc =
    t.oc <- Some oc

end

module Next = Next.T (StateMock)

let%expect_test "Check sequencing etc for next" =
  let state = StateMock.make () in
  Lwt_io.with_temp_file ~temp_dir:"/dev/shm" (fun (fname, oc) ->
      let () = StateMock.set_io state oc in
      let command = Dap.Commands.next in
      let req =
        Dap.Request.(
          Helpers.next_msg ~seq:20
          |> Js.construct (Message.enc command D.NextArguments.enc)
          |> Js.to_string
        )
      in
      Printf.printf "%s" req ;
      let%lwt () =
        [%expect
          {|
            { "seq": 20, "type": "request", "command": "next",
              "arguments": { "threadId": 1 } } |}]
      in

      match Next.handlers ~state with
      | f_resp :: f_ev :: [] ->
        (* happy path *)
        let%lwt resp = f_resp req in
        let resp = Result.get_ok resp in
        Printf.printf "%s" resp ;
        let%lwt () =
          [%expect
            {|
      { "seq": 1, "type": "response", "request_seq": 20, "success": true,
        "command": "next", "body": {} } |}]
        in

        let%lwt ev = f_ev "string doesnt matter" in
        let ev = Result.get_ok ev in
        Printf.printf "%s" ev ;
        let%lwt () = [%expect {|
        { "seq": 2, "type": "event", "event": "stopped",
          "body":
            { "reason": "step", "threadId": 1, "preserveFocusHint": true,
              "allThreadsStopped": true } } |}] in

        let%lwt () =
          let%lwt () = Lwt_io.flush oc in
          In_channel.with_open_text fname (fun ic ->
              let s = In_channel.input_all ic in
              Printf.printf "%s" s;
              let%lwt () = [%expect {|
                Content-Length: 31
                
                { "event": { "step_size": 1 } } |}] in
              Lwt.return_unit
            )
        in

        Lwt.return_unit

      | _ -> failwith "error: expected two handlers for next"
    )
