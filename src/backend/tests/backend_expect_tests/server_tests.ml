include Test_utils.Include
open Lwt
module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Svc = Backend.Server
module MichEvent = Svc.MichEvent
module Helpers = Utils.Helpers

module Msgs : sig
  type t
  val to_string : t -> string
  val runscript : string -> string
  val terminate : string
  val step1 : string
  (* val logger : Lwt_io.input_channel -> unit Lwt.t *)
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

  (* let logger backend_ic = *)
  (*   let rec _aux ic = *)
  (*     let%lwt () = Printf.printf "logger\n"; Lwt.return_unit in *)
  (*     match%lwt Lwt_io.read_line_opt ic with *)
  (*     | Some _ln -> let _ = failwith "not happening" in _aux ic (\* Lwt_io.(printl ln >>= fun _ -> flush stdout >>= fun _ -> logger ic) *\) *)
  (*     | None -> let _ = failwith "blank" in Lwt.return_unit *)
  (*   in *)
  (*   let p, r = Lwt.task () in *)
  (*   let l = p >>= fun ic -> _aux ic in *)
  (*   let () = wakeup r backend_ic in *)
  (*   l *)

  let connect port =
    let ip = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr in
    let client = `TCP (`IP ip, `Port port) in
    let%lwt ctx = Conduit.init () in
    let%lwt (_, ic, oc) = Helpers.loop_connect ~ctx ~client ~port 5 in
    (* let%lwt () = Printf.printf "connected on port %d\n" port; Lwt.return_unit in *)
    Lwt.return (ic, oc)

  let run ~fname (_ic, oc) =
    (* let l = logger ic in *)
    let cmd = Printf.sprintf "dune exec -- weevil stepper %s" fname in
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
      (* Lwt.cancel l |> Lwt.return *)
    in
    stepper
    (* Lwt.join [l;stepper] *)

end

let () = Logs.set_reporter (Logs.format_reporter ())
let () = Logs.set_level (Some Logs.Info)

let%expect_test "Check loading/stepping a contract" =
  let port = 9001 in
  let actions = Msgs.(connect port >>= run ~fname:"data/multiply_2_x_25_equals_50.tz") in
  let%lwt () =
    match%lwt Svc.lwt_svc ~stopper:actions port with
    | `Ok _ -> Printf.printf "ok"; Lwt.return_unit
    | _ -> failwith "not ok"
  in
  let%lwt () = [%expect {|
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] starting backend server on port 9001
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got connection
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] got msg '{ "event": "dune exec -- weevil stepper data/multiply_2_x_25_equals_50.tz" }'
    inline_test_runner_backend_expect_tests.exe: [INFO] [MICH] starting new stepper with cmd 'dune exec -- weevil stepper data/multiply_2_x_25_equals_50.tz'
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
    inline_test_runner_backend_expect_tests.exe: [INFO] [STEPPER] subprocess complete
    ok |}] in

  Lwt.return_unit
