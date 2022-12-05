include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json
module Helpers = Utils.Helpers
module Model = Mdb.Mdb_model

module LaunchStateMock = struct
  include Helpers.StateMock

  let set_connect_backend t _ip _port =
    Dap.Result.ok @@ Option.(Lwt_io.stdin, get t.oc)

  let backend_svc _t =
    Option.some @@ Lwt_process.open_process_none ("", [|":"|])

  let set_start_backend _t _ip _port _cmd = Dap.Result.ok ()

  let backend_oc t = t.oc

  let set_io t oc =
    t.oc <- Some oc

end

module Launch = Launch.T (LaunchStateMock)

let script_filename = "data/multiply_2_x_25_equals_50.tz"
let storage = "Unit"
let parameter = "Unit"

let%expect_test "Check sequencing etc for launch" =
  let st = LaunchStateMock.make () in
  Lwt_io.with_temp_file ~temp_dir:"/dev/shm" (fun (fname, oc) ->
      let () = LaunchStateMock.set_io st oc in
      let command = Dap.Commands.launch in
      let req =
        Dap.Request.(
          Helpers.launch_msg ~seq:20 ~script_filename ~storage ~parameter
          |> Js.construct (Message.enc command D.LaunchRequestArguments.enc)
          |> Js.to_string)
      in
      Printf.printf "%s" req ;
      let%lwt () =
        [%expect
          {|
            { "seq": 20, "type": "request", "command": "launch",
              "arguments":
                { "script_filename": "data/multiply_2_x_25_equals_50.tz",
                  "storage": "Unit", "parameter": "Unit" } } |}]
      in

      match Launch.handlers ~state:st with
      | f_resp :: f_ev :: f_ev_launched :: [] ->
        (* happy path *)
        let%lwt resp = f_resp req in
        let resp = Result.get_ok resp in
        Printf.printf "%s" resp ;
        let%lwt () =
          [%expect
            {|
      { "seq": 21, "type": "response", "request_seq": 20, "success": true,
        "command": "launch", "body": {} } |}]
        in

        let%lwt ev = f_ev "doesnt matter" in
        let ev = Result.get_ok ev in
        Printf.printf "%s" ev ;
        let%lwt () = [%expect {|
        { "seq": 22, "type": "event", "event": "process",
          "body":
            { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
              "startMethod": "launch" } } |}] in

        let%lwt ev = f_ev_launched "doesnt matter" in
        let ev = Result.get_ok ev in
        Printf.printf "%s" ev ;
        let%lwt () = [%expect {|
        { "seq": 23, "type": "event", "event": "stopped",
          "body":
            { "reason": "entry", "threadId": 1, "preserveFocusHint": true,
              "allThreadsStopped": true } } |}] in

        let lmode =
          match st |> LaunchStateMock.launch_mode |> Option.get with
          | `Launch -> "launch"
          | _ -> failwith "error: expected 'Launch' launch mode"
        in
        Printf.printf "%s" lmode;
        let%lwt () = [%expect {| launch |}] in

        let%lwt () =
          let%lwt () = Lwt_io.flush oc in
          In_channel.with_open_text fname (fun ic ->
              let s = In_channel.input_all ic in
              Printf.printf "%s" s;
              let%lwt () =
                [%expect {|
                  Content-Length: 64
                  
                  { "event": { "cmd": "dune exec -- weevil stepper example.tz" } } |}]
              in
              Lwt.return_unit
            )
        in
        Lwt.return_unit

      | _ -> failwith "error: expected three handlers for launch"
    )

let%expect_test "Check bad input for launch" =
  let st = LaunchStateMock.make () in
  (* first show that the launch mode is not set *)
  let lmode = st |> LaunchStateMock.launch_mode |> Option.map Dap.Launch_mode.show |> Option.value ~default:"not set" in
  Printf.printf "%s" lmode;
  let%lwt () = [%expect {| not set |}] in
  (* now show that given an attach command, it errors correctly *)
  Lwt_io.with_temp_file ~temp_dir:"/dev/shm" (fun (fname, oc) ->
      let () = LaunchStateMock.set_io st oc in
      let command = Dap.Commands.attach in
      let req =
        Dap.Request.(
          Helpers.attach_msg ~seq:20 ~script_filename ~storage ~parameter
          |> Js.construct (Message.enc command D.AttachRequestArguments.enc)
          |> Js.to_string)
      in
      Printf.printf "%s" req ;
      let%lwt () =
        [%expect
          {|
            { "seq": 20, "type": "request", "command": "attach",
              "arguments":
                { "script_filename": "data/multiply_2_x_25_equals_50.tz",
                  "storage": "Unit", "parameter": "Unit" } } |}]
      in

      match Launch.handlers ~state:st with
      | f_resp :: _f_ev_process :: _f_ev_stopped :: [] ->
        (* unhappy path *)
        let%lwt err =
          try%lwt
            f_resp req
          with
          | Dap.Wrong_encoder err -> Lwt_result.fail err
        in
        Printf.printf "%s" @@ Result.get_error err ;
        let%lwt () =
          [%expect {| cannnot destruct: expected 'launch', got 'attach' |}]
        in

        (* show that the launch mode doesnt get set when in error *)
        let lmode = st |> LaunchStateMock.launch_mode |> Option.map Dap.Launch_mode.show |> Option.value ~default:"not set" in
        Printf.printf "%s" lmode;
        let%lwt () = [%expect {| not set |}] in

        let%lwt () =
          let%lwt () = Lwt_io.flush oc in
          In_channel.with_open_text fname (fun ic ->
              let s = In_channel.input_all ic in
              Printf.printf "%s" s;
              let%lwt () =
                [%expect {| |}]
              in
              Lwt.return_unit
            )
        in
        Lwt.return_unit

      | _ -> failwith "error: expected three handlers for launch"
    )
