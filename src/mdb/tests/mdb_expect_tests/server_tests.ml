include Test_utils.Include
open Lwt
module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Mdb_svc = Mdb.Mdb_server
module MichEvent = Mdb.Mdb_event
module Helpers = Utils.Helpers

(* use different port than default *)
let port = 9002

module Msgs : sig
  type t
  val to_string : t -> string
  val runscript : string -> string
  val terminate : string
  val step1 : string
  val connect : int -> (Lwt_io.input_channel * Lwt_io.output_channel) Lwt.t
  val run : fname:string -> (Lwt_io.input_channel * Lwt_io.output_channel) -> unit Lwt.t
end
= struct

  type t = MichEvent.t

  let to_string t =
    Js.(construct MichEvent.enc t |> to_string)
    |> Dapper.Dap_header.wrap

  let runscript cmd =
    let event = MichEvent.(make ~event:(RunScript {cmd}) ()) in
    to_string event

  let terminate =
    let event = MichEvent.(make ~event:(Terminate {terminate=()}) ()) in
    to_string event

  let step1 =
    let event = MichEvent.(make ~event:(Step {step_size=1}) ()) in
    to_string event

  let connect port =
    let ip = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr in
    let client = `TCP (`IP ip, `Port port) in
    let%lwt ctx = Conduit.init () in
    let%lwt (_, ic, oc) = Helpers.loop_connect ~ctx ~client ~port 5 in
    Lwt.return (ic, oc)

  let run ~fname (_ic, oc) =
    let cmd = Printf.sprintf "dune exec -- weevil stepper --headless %s" fname in
    let stepper =
      Lwt_io.write_line oc @@ runscript cmd >>= fun _ ->
      Lwt_io.write_line oc @@ step1 >>= fun _ ->
      Lwt_io.write_line oc @@ step1 >>= fun _ ->
      Lwt_io.write_line oc @@ step1 >>= fun _ ->
      Lwt_io.write_line oc @@ step1 >>= fun _ ->
      (* NOTE sleeps to make deterministic *)
      Lwt_unix.sleep 1.0 >>= fun _ ->
      Lwt_io.write_line oc @@ terminate  >>= fun _ ->
      Lwt_unix.sleep 1.0
    in
    stepper

end

let () = Logs.set_reporter (Logs.format_reporter ())
let () = Logs.set_level (Some Logs.Info)

let%expect_test "Check loading/stepping a contract" =
  let actions = Msgs.(connect port >>= run ~fname:"data/multiply_2_x_25_equals_50.tz") in
  let%lwt _ = Mdb_svc.lwt_svc ~stopper:actions port in
  let%lwt () = [%expect {|
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002 with stopper |}] in

  Lwt.return_unit

let%expect_test "check for bad filename" =
  let actions = Msgs.(connect port >>= run ~fname:"data/notthere.tz") in
  let%lwt _ = Mdb_svc.lwt_svc ~stopper:actions port in
  let%lwt () = [%expect {|
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002 with stopper
    inline_test_runner_mdb_expect_tests.exe: [ERROR] [STEPPER ERR] step_err_handler: { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"data/notthere.tz\"))" } ] } |}] in
  Lwt.return_unit


let%expect_test "check for bad michelson" =
  let actions = Msgs.(connect port >>= run ~fname:"data/bad_michelson.tz") in
  let%lwt _ = Mdb_svc.lwt_svc ~stopper:actions port in
  let%lwt () = [%expect {|
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002 with stopper
    inline_test_runner_mdb_expect_tests.exe: [ERROR] [STEPPER ERR] step_err_handler: { "error":    [ { "kind": "permanent",        "id": "proto.014-PtKathma.michelson_v1.invalid_primitive_name",        "expression":          [ { "prim": "parameter", "args": [ { "prim": "unit" } ] },            { "prim": "storage", "args": [ { "prim": "unit" } ] },            { "prim": "code",              "args":                [ [ { "prim": "DROP" },                    { "prim": "PUSH",                      "args": [ { "prim": "mutez" }, { "int": "25" } ] },                    { "prim": "PUSH",                      "args": [ { "prim": "nat" }, { "int": "2" } ] },                    { "prim": "MUL" }, { "prim": "DROP_IT_LIKE_ITS_HOT" },                    { "prim": "UNIT" },                    { "prim": "NIL", "args": [ { "prim": "operation" } ] },                    { "prim": "PAIR" } ] ] } ], "location": 15 },      { "kind": "permanent",        "id": "proto.014-PtKathma.michelson_v1.unknown_primitive_name",        "wrong_primitive_name": "DROP_IT_LIKE_ITS_HOT" } ] } |}] in
  Lwt.return_unit
