include Utilities.Testing_utils
module Js = Data_encoding.Json
module Res = Dap.Response
module Ev = Dap.Event

module TestState = Dap_state.T ()

module Raise_error =
  Dap_handlers.Raise_error.Make
    (struct
      type enum = Dap_commands.error

      type contents = Dap.Data.ErrorResponse_body.t

      type presence = Dap.Data.Presence.req

      type msg = (enum, contents, presence) Res.Message.t

      type t = msg Res.t

      let ctor = Res.errorResponse

      let enc = Res.Message.enc Dap.Commands.error Dap.Data.ErrorResponse_body.enc
    end)
    (TestState)

let _reset state =
  (* need to set the state as if messsages have already been exchanged *)
  TestState.set_seqr state @@ Dap.Seqr.make ~seq:121 ~request_seq:120 ()

let%expect_test "Check sequencing raise response" =
  let state = TestState.make () in
  _reset state;

  let handler =
    Raise_error.make ~handler:(fun ~state:_ _ ->
        Res.(
          let msg = default_response_error "flux capacitor ran out of gigawatts" in
          errorResponse msg
          |> Dap_result.ok
        )
      )
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

  let%lwt s = handler ~state "" in
  Printf.printf "%s" @@ Result.get_ok s ;
  let%lwt () =
    [%expect
      {|
        { "seq": 122, "type": "response", "request_seq": 121, "success": false,
          "command": "error", "message": "flux capacitor ran out of gigawatts",
          "body":
            { "error":
                { "id": 61389606, "format": "{error}",
                  "variables": { "error": "flux capacitor ran out of gigawatts" } } } } |}]
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
