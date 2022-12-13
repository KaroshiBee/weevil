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
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got connection
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 108'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 108 in 'Content-Length: 108'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 108
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 108 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event":    { "cmd":        "dune exec -- weevil stepper --headless data/multiply_2_x_25_equals_50.tz" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event":    { "cmd":        "dune exec -- weevil stepper --headless data/multiply_2_x_25_equals_50.tz" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting new stepper with cmd 'dune exec -- weevil stepper --headless data/multiply_2_x_25_equals_50.tz'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] stepper_process_start
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] starting
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage 'Content-Length: 84'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content length 84 in 'Content-Length: 84'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 84
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 84 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with message '{ "location": 7, "gas": "92.565 units remaining",  "stack": [ "(Pair Unit Unit)" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '{ "location": 7, "gas": "92.565 units remaining",  "stack": [ "(Pair Unit Unit)" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 7, "gas": "92.565 units remaining",  "stack": [ "(Pair Unit Unit)" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage 'Content-Length: 67'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content length 67 in 'Content-Length: 67'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 67
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 67 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with message '{ "location": 7, "gas": "92.555 units remaining", "stack": [ "" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '{ "location": 7, "gas": "92.555 units remaining", "stack": [ "" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 7, "gas": "92.555 units remaining", "stack": [ "" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage 'Content-Length: 69'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content length 69 in 'Content-Length: 69'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 69
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 69 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with message '{ "location": 8, "gas": "92.545 units remaining", "stack": [ "25" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '{ "location": 8, "gas": "92.545 units remaining", "stack": [ "25" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 8, "gas": "92.545 units remaining", "stack": [ "25" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage 'Content-Length: 76'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content length 76 in 'Content-Length: 76'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 76
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 76 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with message '{ "location": 11, "gas": "92.535 units remaining", "stack": [ "2", " 25" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '{ "location": 11, "gas": "92.535 units remaining", "stack": [ "2", " 25" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 11, "gas": "92.535 units remaining", "stack": [ "2", " 25" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage 'Content-Length: 70'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content length 70 in 'Content-Length: 70'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 70
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with length 70 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got content-length message with message '{ "location": 14, "gas": "92.535 units remaining", "stack": [ "50" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got msg from subprocess '{ "location": 14, "gas": "92.535 units remaining", "stack": [ "50" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 14, "gas": "92.535 units remaining", "stack": [ "50" ] }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 41'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 41 in 'Content-Length: 41'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 41
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 41 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "terminate": "Terminate" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "terminate": "Terminate" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages |}] in

  Lwt.return_unit

let%expect_test "check for bad filename" =
  let actions = Msgs.(connect port >>= run ~fname:"data/notthere.tz") in
  let%lwt _ = Mdb_svc.lwt_svc ~stopper:actions port in
  let%lwt () = [%expect {|
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got connection
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 84'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 84 in 'Content-Length: 84'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 84
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 84 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event":    { "cmd": "dune exec -- weevil stepper --headless data/notthere.tz" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event":    { "cmd": "dune exec -- weevil stepper --headless data/notthere.tz" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting new stepper with cmd 'dune exec -- weevil stepper --headless data/notthere.tz'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] stepper_process_start
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] starting
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got messsage 'tezos-weevil: '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] no content length in 'tezos-weevil: ' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got messsage 'Content-Length: 166'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got content length 166 in 'Content-Length: 166'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got content-length message with length 166
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got content-length message with length 166 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got content-length message with message '{ "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"data/notthere.tz\"))" } ] }'
    inline_test_runner_mdb_expect_tests.exe: [ERROR] [STEPPER ERR] step_err_handler: { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"data/notthere.tz\"))" } ] }
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] connection closed
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] connection closed
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 41'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 41 in 'Content-Length: 41'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 41
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 41 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "terminate": "Terminate" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "terminate": "Terminate" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] already terminated
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages |}] in
  Lwt.return_unit


let%expect_test "check for bad michelson" =
  let actions = Msgs.(connect port >>= run ~fname:"data/bad_michelson.tz") in
  let%lwt _ = Mdb_svc.lwt_svc ~stopper:actions port in
  let%lwt () = [%expect {|
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting backend server on port 9002
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got connection
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 89'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 89 in 'Content-Length: 89'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 89
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 89 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event":    { "cmd": "dune exec -- weevil stepper --headless data/bad_michelson.tz" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event":    { "cmd": "dune exec -- weevil stepper --headless data/bad_michelson.tz" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] starting new stepper with cmd 'dune exec -- weevil stepper --headless data/bad_michelson.tz'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] stepper_process_start
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] starting
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 31 in 'Content-Length: 31'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 31 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "step_size": 1 } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] Got Next request
    { "event": { "step_size": 1 } }

    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got messsage 'tezos-weevil: '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] no content length in 'tezos-weevil: ' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got messsage 'Content-Length: 982'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got content length 982 in 'Content-Length: 982'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got content-length message with length 982
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got content-length message with length 982 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] got content-length message with message '{ "error":    [ { "kind": "permanent",        "id": "proto.014-PtKathma.michelson_v1.invalid_primitive_name",        "expression":          [ { "prim": "parameter", "args": [ { "prim": "unit" } ] },            { "prim": "storage", "args": [ { "prim": "unit" } ] },            { "prim": "code",              "args":                [ [ { "prim": "DROP" },                    { "prim": "PUSH",                      "args": [ { "prim": "mutez" }, { "int": "25" } ] },                    { "prim": "PUSH",                      "args": [ { "prim": "nat" }, { "int": "2" } ] },                    { "prim": "MUL" }, { "prim": "DROP_IT_LIKE_ITS_HOT" },                    { "prim": "UNIT" },                    { "prim": "NIL", "args": [ { "prim": "operation" } ] },                    { "prim": "PAIR" } ] ] } ], "location": 15 },      { "kind": "permanent",        "id": "proto.014-PtKathma.michelson_v1.unknown_primitive_name",        "wrong_primitive_name": "DROP_IT_LIKE_ITS_HOT" } ] }'
    inline_test_runner_mdb_expect_tests.exe: [ERROR] [STEPPER ERR] step_err_handler: { "error":    [ { "kind": "permanent",        "id": "proto.014-PtKathma.michelson_v1.invalid_primitive_name",        "expression":          [ { "prim": "parameter", "args": [ { "prim": "unit" } ] },            { "prim": "storage", "args": [ { "prim": "unit" } ] },            { "prim": "code",              "args":                [ [ { "prim": "DROP" },                    { "prim": "PUSH",                      "args": [ { "prim": "mutez" }, { "int": "25" } ] },                    { "prim": "PUSH",                      "args": [ { "prim": "nat" }, { "int": "2" } ] },                    { "prim": "MUL" }, { "prim": "DROP_IT_LIKE_ITS_HOT" },                    { "prim": "UNIT" },                    { "prim": "NIL", "args": [ { "prim": "operation" } ] },                    { "prim": "PAIR" } ] ] } ], "location": 15 },      { "kind": "permanent",        "id": "proto.014-PtKathma.michelson_v1.unknown_primitive_name",        "wrong_primitive_name": "DROP_IT_LIKE_ITS_HOT" } ] }
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER] connection closed
    inline_test_runner_mdb_expect_tests.exe: [INFO] [STEPPER ERR] connection closed
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage 'Content-Length: 41'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content length 41 in 'Content-Length: 41'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 41
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with length 41 and header_break '
    '
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got content-length message with message '{ "event": { "terminate": "Terminate" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] got msg '{ "event": { "terminate": "Terminate" } }'
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MICH] already terminated
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] got messsage ''
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] no content length in '' yet
    inline_test_runner_mdb_expect_tests.exe: [INFO] [MDB] waiting for content-length messages |}] in
  Lwt.return_unit
