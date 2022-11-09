include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json

module StateMock = struct
  type t = {mutable launch_mode : D.Launch_mode.t option}

  let make_empty = {launch_mode = None}

  let connect _ip _port = failwith "MOCK"

  let process_none _t = failwith "MOCK"

  let set_process_none _t _process = failwith "MOCK"

  let ic _t = failwith "MOCK"

  let oc _t = Some Lwt_io.stdout

  let set_io _t ?ic:_ ?oc:_ () = failwith "MOCK"

  let launch_mode t = t.launch_mode

  let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode
end

module Attach = Attach.T (StateMock)

let%expect_test "Check sequencing etc for attach" =
  let config = Dap.Config.make () in
  let t = Attach.make () in
  let command = Dap.Commands.attach in
  let req =
    Dap.Request.(
      Message.make
        ~seq:20
        ~command
        ~arguments:(D.AttachRequestArguments.make ())
        ()
      |> Js.construct (Message.enc command D.AttachRequestArguments.enc)
      |> Js.to_string
    )
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {| { "seq": 20, "type": "request", "command": "attach", "arguments": {} } |}]
  in

  match Attach.handlers ~config t with
  | f_resp :: f_ev :: [] ->
    (* happy path *)
    let%lwt resp = f_resp req in
    Printf.printf "%s" resp ;
    let%lwt () =
      [%expect
        {|
      { "seq": 21, "type": "response", "request_seq": 20, "success": true,
        "command": "attach", "body": {} } |}]
    in

    let%lwt ev = f_ev resp in
    Printf.printf "%s" ev ;
    let%lwt () = [%expect {|
        { "seq": 22, "type": "event", "event": "process",
          "body":
            { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
              "startMethod": "attach" } } |}] in

    let lmode =
      match Attach.state t |> StateMock.launch_mode |> Option.get with
      | `Attach -> "attach"
      | _ -> failwith "error: expected 'Attach' launch mode"
    in
    Printf.printf "%s" lmode;
    let%lwt () = [%expect {| attach |}] in

    (* unhappy paths *)
    let%lwt err =
      try%lwt
        f_ev req
      with
      | Dap.Wrong_encoder err -> Lwt.return err
    in
    Printf.printf "%s" err ;
    let%lwt () =
      [%expect {| cannnot destruct: Json_encoding.Cannot_destruct at /: Missing object field request_seq |}]
    in

    (* unhappy paths *)
    let%lwt err =
      try%lwt
        f_resp ev
      with
      | Dap.Wrong_encoder err -> Lwt.return err
    in
    Printf.printf "%s" err ;
    let%lwt () =
      [%expect {| cannnot destruct: Json_encoding.Cannot_destruct at /: Missing object field command |}]
    in

    Lwt.return_unit

  | _ -> failwith "error: expected two handlers for attach"
