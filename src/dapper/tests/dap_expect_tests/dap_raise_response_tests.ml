include Test_utils.Include
module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module TestState = Dap_handlers.State ()

module ProcessLaunched =
  Dap_handlers.Raise_response.Make
    (struct
      type enum = Dap.Commands.launch

      type contents = Dap.Data.EmptyObject.t option

      type presence = Dap.Data.Presence.opt

      type msg = (enum, contents, presence) Res.Message.t

      type t = msg Res.t

      let ctor = Res.launchResponse

      let enc = Res.Message.enc_opt Dap.Commands.launch Dap.Data.EmptyObject.enc
    end)
    (TestState)

let _reset state =
  (* need to set the state as if messsages have already been exchanged *)
  TestState.set_seqr state @@ Dap.Seqr.make ~seq:121 ~request_seq:120 ()

let%expect_test "Check sequencing raise response" =
  let config = Dap.Config.make () in
  let state = TestState.make in
  _reset state;

  let handler =
    let l =
      ProcessLaunched.make ~handler:(fun _ _ _ ->
          Res.(
            let body = Dap.Data.EmptyObject.make () in
            let msg = default_response_opt Dap.Commands.launch body in
            launchResponse msg
            |> Dap_result.ok
          )
        )
    in
    ProcessLaunched.handle l
  in

  let seqr = TestState.current_seqr state in
  let request_seq = Dap.Seqr.request_seq seqr in
  Printf.printf "request_seq %d" request_seq;
  let%lwt () =
    [%expect {| request_seq 120 |}]
  in

  let seq = Dap.Seqr.seq seqr in
  Printf.printf "seq %d" seq;
  let%lwt () =
    [%expect {| seq 121 |}]
  in

  let%lwt s = handler ~state ~config "" in
  Printf.printf "%s" @@ Result.get_ok s ;
  let%lwt () =
    [%expect
      {|
        { "seq": 122, "type": "response", "request_seq": 121, "success": true,
          "command": "launch", "body": {} } |}]
  in

  let seqr = TestState.current_seqr state in
  let request_seq = Dap.Seqr.request_seq seqr in
  Printf.printf "request_seq %d" request_seq;
  let%lwt () =
    [%expect {| request_seq 121 |}]
  in

  let seq = Dap.Seqr.seq seqr in
  Printf.printf "seq %d" seq;
  let%lwt () =
    [%expect {| seq 122 |}]
  in

  (* should also have the correct seq numbers if error happens during handling *)
  _reset state;
  let handler_err =
    let l =
      ProcessLaunched.make ~handler:(fun _ _ _req ->
          Res.default_response_error "testing error"
          |> Res.errorResponse |> Dap_result.error)
    in
    ProcessLaunched.handle l
  in
  let%lwt s = handler_err ~state ~config "" in
  Printf.printf "%s" @@ Result.get_error s ;
  let%lwt () = [%expect
    {|
    { "seq": 122, "type": "response", "request_seq": 121, "success": false,
      "command": "error",
      "body":
        { "error":
            { "id": 400237674, "format": "{error}",
              "variables": { "error": "testing error" } } } } |}]
  in

  let seqr = TestState.current_seqr state in
  let request_seq = Dap.Seqr.request_seq seqr in
  Printf.printf "request_seq %d" request_seq;
  let%lwt () =
    [%expect {| request_seq 121 |}]
  in

  let seq = Dap.Seqr.seq seqr in
  Printf.printf "seq %d" seq;
  [%expect {| seq 122 |}]
