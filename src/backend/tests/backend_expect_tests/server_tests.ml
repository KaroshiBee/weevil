include Test_utils.Include
open Lwt
module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Svc = Backend.Server
module MichEvent = Svc.MichEvent
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
    |> Str.global_replace (Str.regexp "\n") ""

  let runscript fname =
    let event = MichEvent.(make ~event:(RunScript fname) ()) in
    to_string event

  let terminate =
    let event = MichEvent.(make ~event:Terminate ()) in
    to_string event

  let step1 =
    let event = MichEvent.(make ~event:(Step 1) ()) in
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
  let%lwt _ = Svc.lwt_svc ~stopper:actions port in
  let%lwt () = [%expect {|
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got connection
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event":    "dune exec -- weevil stepper --headless data/multiply_2_x_25_equals_50.tz" }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] starting new stepper with cmd 'dune exec -- weevil stepper --headless data/multiply_2_x_25_equals_50.tz'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] stepper_process_start
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] starting
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess ''
    inline_test_runner_backend_expect_tests.exe: [INFO] other:
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_interp @ location 7'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_interp @ location 7
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess ''
    inline_test_runner_backend_expect_tests.exe: [INFO] other:
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_entry @ location 7'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_entry @ location 7
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# got 'step''
    inline_test_runner_backend_expect_tests.exe: [INFO] # got 'step'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_exit @ location 7, line 1'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_exit @ location 7, line 1
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '{ "location": 1, "gas": "92.555 units remaining", "stack": [ "" ] }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 1, "gas": "92.555 units remaining", "stack": [ "" ] }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess ''
    inline_test_runner_backend_expect_tests.exe: [INFO] other:
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_entry @ location 8'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_entry @ location 8
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# got 'step''
    inline_test_runner_backend_expect_tests.exe: [INFO] # got 'step'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_exit @ location 8, line 2'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_exit @ location 8, line 2
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '{ "location": 2, "gas": "92.545 units remaining", "stack": [ "25" ] }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 2, "gas": "92.545 units remaining", "stack": [ "25" ] }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess ''
    inline_test_runner_backend_expect_tests.exe: [INFO] other:
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_entry @ location 11'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_entry @ location 11
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# got 'step''
    inline_test_runner_backend_expect_tests.exe: [INFO] # got 'step'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_exit @ location 11, line 3'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_exit @ location 11, line 3
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '{ "location": 3, "gas": "92.535 units remaining", "stack": [ "2", " 25" ] }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 3, "gas": "92.535 units remaining", "stack": [ "2", " 25" ] }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess ''
    inline_test_runner_backend_expect_tests.exe: [INFO] other:
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_entry @ location 14'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_entry @ location 14
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# got 'step''
    inline_test_runner_backend_expect_tests.exe: [INFO] # got 'step'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_exit @ location 14, line 4'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_exit @ location 14, line 4
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '{ "location": 4, "gas": "92.535 units remaining", "stack": [ "50" ] }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 4, "gas": "92.535 units remaining", "stack": [ "50" ] }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess ''
    inline_test_runner_backend_expect_tests.exe: [INFO] other:
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '# log_entry @ location 15'
    inline_test_runner_backend_expect_tests.exe: [INFO] # log_entry @ location 15
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] waiting for messages
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": {} }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] subprocess complete |}] in

  Lwt.return_unit

let%expect_test "check for bad filename" =
  let actions = Msgs.(connect port >>= run ~fname:"data/notthere.tz") in
  let%lwt _ = Svc.lwt_svc ~stopper:actions port in
  let%lwt () = [%expect {|
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got connection
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": "dune exec -- weevil stepper --headless data/notthere.tz" }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] starting new stepper with cmd 'dune exec -- weevil stepper --headless data/notthere.tz'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] stepper_process_start
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] starting
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [ERROR] step_err_handler: weevil: [ERROR] Sys_error("data/notthere.tz: No such file or directory")
    inline_test_runner_backend_expect_tests.exe: [ERROR] step_err_handler: tezos-weevil: Content-Length: 136
    inline_test_runner_backend_expect_tests.exe: [ERROR] step_err_handler:
    inline_test_runner_backend_expect_tests.exe: [ERROR] step_err_handler:               { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Sys_error(\"data/notthere.tz: No such file or directory\")" } ] }
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": {} }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] already terminated |}] in
  Lwt.return_unit


let%expect_test "check for bad michelson" =
  let actions = Msgs.(connect port >>= run ~fname:"data/bad_michelson.tz") in
  let%lwt _ = Svc.lwt_svc ~stopper:actions port in
  let%lwt () = [%expect {|
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got connection
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": "dune exec -- weevil stepper --headless data/bad_michelson.tz" }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] starting new stepper with cmd 'dune exec -- weevil stepper --headless data/bad_michelson.tz'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] stepper_process_start
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] starting
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": 1 }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": 1 }

    inline_test_runner_backend_expect_tests.exe: [ERROR] step_err_handler: tezos-weevil: Content-Length: 982
    inline_test_runner_backend_expect_tests.exe: [ERROR] step_err_handler:
    inline_test_runner_backend_expect_tests.exe: [ERROR] step_err_handler:               { "error":    [ { "kind": "permanent",        "id": "proto.014-PtKathma.michelson_v1.invalid_primitive_name",        "expression":          [ { "prim": "parameter", "args": [ { "prim": "unit" } ] },            { "prim": "storage", "args": [ { "prim": "unit" } ] },            { "prim": "code",              "args":                [ [ { "prim": "DROP" },                    { "prim": "PUSH",                      "args": [ { "prim": "mutez" }, { "int": "25" } ] },                    { "prim": "PUSH",                      "args": [ { "prim": "nat" }, { "int": "2" } ] },                    { "prim": "MUL" }, { "prim": "DROP_IT_LIKE_ITS_HOT" },                    { "prim": "UNIT" },                    { "prim": "NIL", "args": [ { "prim": "operation" } ] },                    { "prim": "PAIR" } ] ] } ], "location": 15 },      { "kind": "permanent",        "id": "proto.014-PtKathma.michelson_v1.unknown_primitive_name",        "wrong_primitive_name": "DROP_IT_LIKE_ITS_HOT" } ] }
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": {} }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] already terminated |}] in
  Lwt.return_unit
