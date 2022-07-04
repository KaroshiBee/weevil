(* open Stepper *)
(* let () = Server.svc () *)

(* let () = *)
(*   let oc = open_out "weevil.log" in *)
(*   let contract = *)
(*     "{parameter unit;storage unit;code {DROP; UNIT; NIL operation; PAIR}}" in *)
(*   (\* let contract = *\) *)
(*   (\*   "{parameter unit;storage unit;code {DROP; PUSH mutez 2944023901536524477; \ *\) *)
(*   (\*    PUSH nat 2; MUL; PUSH mutez 100; ADD; DROP; UNIT; NIL operation; PAIR}}" in *\) *)

(*   let logger = Traced_interpreter.trace_logger oc () in *)

(*   let stepper = test_stepping contract logger >|= (fun _ -> ()) in *)
(*   let log_trace = Lwt.bind stepper (fun _ -> logger.get_log ()) in *)
(*   let ll = Lwt.map (fun tr -> Result.value ~default:None tr) log_trace in *)
(*   let ll = Lwt.map (fun tr -> Option.value ~default:[] tr) ll in *)
(*   (\* let ui = Lwt.bind ll (fun l -> Nottui_lwt.run (View.ui_main contract (Model.Weevil_trace.of_execution_trace l))) in *\) *)
(*   Lwt_main.run (Lwt.map (fun _ -> ()) ll) *)

let listen_address = Unix.inet_addr_loopback
let port = 9000
let cmd = "./run_stepper.sh"

let () =
  Server.svc ~listen_address ~port ~cmd ()
