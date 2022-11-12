include Test_utils.Include
module Dap = Dapper.Dap
module D = Dap.Data
module Js = Data_encoding.Json

module StateMock = struct
  type t = {
    mutable launch_mode : D.Launch_mode.t option;
    mutable seqr: D.Seqr.t;
  }

  let make = {launch_mode = None; seqr = D.Seqr.make ~seq:0 ()}

  let connect_backend _ip _port = failwith "MOCK connect"

  let process_none _t = failwith "MOCK process none"

  let start_backend _t _ip _port _cmd = failwith "MOCK start backend"

  let ic _t = failwith "MOCK ic"

  let oc _t = Some Lwt_io.stdout

  let launch_mode t = t.launch_mode

  let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode

  let current_seqr t = t.seqr

  let set_seqr t seqr = t.seqr <- seqr

end

module Attach = Attach.T (StateMock)

let%expect_test "Check sequencing etc for attach" =
  let state = StateMock.make in
  let config = Dap.Config.make () in
  let command = Dap.Commands.attach in
  let req =
    Dap.Request.(
      Helpers.attach_msg ~seq:20
      |> Js.construct (Message.enc command D.AttachRequestArguments.enc)
      |> Js.to_string
    )
  in
  Printf.printf "%s" req ;
  let%lwt () =
    [%expect
      {| { "seq": 20, "type": "request", "command": "attach", "arguments": {} } |}]
  in

  match Attach.handlers ~state ~config with
  | f_resp :: f_ev :: [] ->
    (* happy path *)
    let%lwt resp = f_resp req in
    let resp = Result.get_ok resp in
    Printf.printf "%s" resp ;
    let%lwt () =
      [%expect
        {|
      { "seq": 21, "type": "response", "request_seq": 20, "success": true,
        "command": "attach", "body": {} } |}]
    in

    let%lwt ev = f_ev "string doesnt matter" in
    let ev = Result.get_ok ev in
    Printf.printf "%s" ev ;
    let%lwt () = [%expect {|
        { "seq": 22, "type": "event", "event": "process",
          "body":
            { "name": "TODO PROCESS EVENT NAME e.g. test.tz",
              "startMethod": "attach" } } |}] in

    let lmode =
      match state |> StateMock.launch_mode |> Option.get with
      | `Attach -> "attach"
      | _ -> failwith "error: expected 'Attach' launch mode"
    in
    Printf.printf "%s" lmode;
    let%lwt () = [%expect {| attach |}] in

    (* unhappy path *)
    let%lwt err =
      try%lwt
        f_resp ev
      with
      | Dap.Wrong_encoder err -> Lwt_result.fail err
    in
    Printf.printf "%s" @@ Result.get_error err ;
    let%lwt () =
      [%expect {| cannnot destruct: Json_encoding.Cannot_destruct at /: Missing object field command |}]
    in

    Lwt.return_unit

  | _ -> failwith "error: expected two handlers for attach"
