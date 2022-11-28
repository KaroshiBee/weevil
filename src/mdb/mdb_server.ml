open Protocol
module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Dap = Dapper.Dap
module MichEvent = Mdb_event

(* NOTE type unparsing_mode = Optimized | Readable | Optimized_legacy, could we phantom this into the logger type? *)
module Interpreter_cfg = struct
  type input = string
  let to_string_input i = i

  type output = string
  let to_string_output o = o

  let unparsing_mode = Script_ir_translator.Readable
end

module Interpreter = Mdb_traced_interpreter.T (Interpreter_cfg)
module Stepper = Mdb_stepper2.T (Interpreter)

let protocol_str = "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg"
let base_dir = "/tmp/.weevil"

let rec main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process ic oc =
  let%lwt ln = Lwt_io.read_line_opt ic in
  match ln, stepper_process with
  | Some _msg, None -> (
      (*TODO  _msg will contain the script name *)
      let fname = "/home/wyn/mich/multiply_2_x_250_equals_500.tz" in
      let p = fname |> Lwt_preemptive.detach (fun filename ->
          match Stepper.step ~make_logger stepper filename with
          | Ok _ -> ()
          | Error errs ->
            Logs.info (fun m -> m "[MICH] preemptive: got errors '%s'" @@ Data_encoding.Json.(construct trace_encoding errs |> to_string))
        ) in
      let%lwt () = Logs_lwt.info (fun m -> m "[MICH] spawned '%s'" fname) in
      Lwt.join [p; main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process:(Some p) ic oc]
    )

  | Some msg, Some p -> (
    let%lwt () = Logs_lwt.info (fun m -> m "[MICH] process already spawned") in
    let%lwt () = Logs_lwt.info (fun m -> m "[MICH] process state %s" @@ match Lwt.state p with | Return _x -> "finished" | Sleep -> "sleep" | Fail _exn -> "failed") in
    match Lwt.state p with
    | Sleep ->
      let p = Lwt_mvar.put input_mvar msg in
      Lwt.join [p; main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process ic oc]
    | Return _ | Fail _ ->
      (* finished subprocess, reset to None *)
      main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process:None ic oc
  )

  | None, _ -> Logs_lwt.info (fun m -> m "[MICH] connection closed")

let on_exn exn =
  Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let on_connection ~stepper _flow ic oc =
  let%lwt () = Logs_lwt.info (fun m -> m "[MICH] got connection") in
  let input_mvar = Lwt_mvar.create_empty () in
  let output_mvar = Lwt_mvar.create_empty () in
  let make_logger = Interpreter.trace_logger ~input_mvar ~output_mvar in
  main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process:None ic oc

let lwt_svc ?stopper port =
  let open Lwt_result_syntax in
  let mode = `TCP (`Port port) in
  let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d" port) in
  (* we run the stepper in a preemptive thread
     so that can pause it without pausing the main thread,
     NOTE currently only allowing one interp process at a time *)
  let () = Lwt_preemptive.simple_init () in
  let () = Lwt_preemptive.set_bounds (1,1) in
  let* stepper = Stepper.init ~protocol_str ~base_dir () in
  let*! ctx = Conduit.init () in
  let*! ret =
    match stopper with
    | Some stop -> Conduit.serve ~stop ~on_exn ~ctx ~mode @@ on_connection ~stepper
    | None -> Conduit.serve ~on_exn ~ctx ~mode @@ on_connection ~stepper
  in
  return ret

let svc ~port =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  match Lwt_main.run (lwt_svc port) with
  | Ok _ -> `Ok ()
  | Error err -> `Error (true, Data_encoding.Json.(construct trace_encoding err |> to_string))
