include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json

module StateMock = struct
  type t = {
    mutable launch_mode : D.Launch_mode.t option;
    mutable ic: Lwt_io.input_channel option;
    mutable oc: Lwt_io.output_channel option;
  }

  let make_empty = {
    launch_mode = None;
    ic=None;
    oc=None;
  }

  let connect _t _ip _port = Lwt.return_unit

  let process_none _t =
    Option.some @@ Lwt_process.open_process_none ("", [|":"|])

  let set_process_none _t _process = failwith "MOCK"

  let ic _t = failwith "MOCK ic"

  let oc t = t.oc

  let set_io t ?ic ?oc () =
    t.ic <- ic;
    t.oc <- oc

  let launch_mode t = t.launch_mode

  let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode
end

module Launch = Launch.T (StateMock)

let%expect_test "Check sequencing etc for launch" =
  let config = Dap.Config.make () in
  let t = Launch.make () in
  let st = Launch.state t in
  Lwt_io.with_temp_file ~temp_dir:"/dev/shm" (fun (_, oc) ->
      let () = StateMock.set_io st ?ic:None ~oc () in
      let command = Dap.Commands.launch in
      let req =
        Dap.Request.(
          Message.make
            ~seq:20
            ~command
            ~arguments:(D.LaunchRequestArguments.make ())
            ()
          |> Js.construct (Message.enc command D.LaunchRequestArguments.enc)
          |> Js.to_string)
      in
      Printf.printf "%s" req ;
      let%lwt () =
        [%expect
          {| { "seq": 20, "type": "request", "command": "launch", "arguments": {} } |}]
      in

      match Launch.handlers ~config t with
      | f_resp :: f_ev :: [] ->
        let%lwt resp = f_resp req in
        Printf.printf "%s" resp ;
        let%lwt () =
          [%expect
            {|
      { "seq": 21, "type": "response", "request_seq": 20, "success": true,
        "command": "launch", "body": {} } |}]
        in

        let%lwt ev = f_ev resp in
        Printf.printf "%s" ev ;
        let%lwt () = [%expect {|
        { "seq": 22, "type": "event", "event": "process",
          "body":
            { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
              "startMethod": "launch" } } |}] in

        let lmode =
          match Launch.state t |> StateMock.launch_mode |> Option.get with
          | `Launch -> "launch"
          | _ -> failwith "error: expected 'Launch' launch mode"
        in
        Printf.printf "%s" lmode;
        let%lwt () = [%expect {| launch |}] in

        Lwt.return_unit

      | _ -> failwith "error: expected two handlers for launch"
    )
