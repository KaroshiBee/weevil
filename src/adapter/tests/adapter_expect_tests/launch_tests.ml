include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json
module Helpers = Utils.Helpers

module LaunchStateMock = struct
  type t = {
    mutable launch_mode : Dap.Launch_mode.t option;
    mutable oc: Lwt_io.output_channel option;
    mutable seqr: D.Seqr.t;
    mutable config : Dap.Config.t;
    mutable client_config : D.InitializeRequestArguments.t option;
  }

  let make () = {
    launch_mode = None;
    oc=None;
    seqr=D.Seqr.make ~seq:0 ();
    config=Dap.Config.make ();
    client_config=Option.some @@ D.InitializeRequestArguments.make ~adapterID:"FAKE" ();
  }

  let set_connect_backend t _ip _port =
    Dap.Result.ok @@ Option.(Lwt_io.stdin, get t.oc)

  let backend_svc _t =
    Option.some @@ Lwt_process.open_process_none ("", [|":"|])

  let set_start_backend _t _ip _port _cmd = Dap.Result.ok ()

  let backend_ic _t = failwith "MOCK ic"

  let backend_oc t = t.oc

  let set_io t oc =
    t.oc <- Some oc

  let launch_mode t = t.launch_mode

  let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode

  let current_seqr t = t.seqr

  let set_seqr t seqr = t.seqr <- seqr

  let config t = t.config

  let set_config t config = t.config <- config

  let client_config t = t.client_config

  let set_client_config t config = t.client_config <- Some config

end

module Launch = Launch.T (LaunchStateMock)

let%expect_test "Check sequencing etc for launch" =
  let st = LaunchStateMock.make () in
  Lwt_io.with_temp_file ~temp_dir:"/dev/shm" (fun (_, oc) ->
      let () = LaunchStateMock.set_io st oc in
      let command = Dap.Commands.launch in
      let req =
        Dap.Request.(
          Helpers.launch_msg ~seq:20
          |> Js.construct (Message.enc command D.LaunchRequestArguments.enc)
          |> Js.to_string)
      in
      Printf.printf "%s" req ;
      let%lwt () =
        [%expect
          {| { "seq": 20, "type": "request", "command": "launch", "arguments": {} } |}]
      in

      match Launch.handlers ~state:st with
      | f_resp :: f_ev_process :: f_ev_stopped :: [] ->
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

        let%lwt ev = f_ev_process "doesnt matter" in
        let ev = Result.get_ok ev in
        Printf.printf "%s" ev ;
        let%lwt () = [%expect {|
        { "seq": 22, "type": "event", "event": "process",
          "body":
            { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
              "startMethod": "launch" } } |}] in

        let%lwt ev = f_ev_stopped "doesnt matter" in
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
  Lwt_io.with_temp_file ~temp_dir:"/dev/shm" (fun (_, oc) ->
      let () = LaunchStateMock.set_io st oc in
      let command = Dap.Commands.attach in
      let req =
        Dap.Request.(
          Helpers.attach_msg ~seq:20
          |> Js.construct (Message.enc command D.AttachRequestArguments.enc)
          |> Js.to_string)
      in
      Printf.printf "%s" req ;
      let%lwt () =
        [%expect
          {| { "seq": 20, "type": "request", "command": "attach", "arguments": {} } |}]
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

        Lwt.return_unit

      | _ -> failwith "error: expected three handlers for launch"
    )
