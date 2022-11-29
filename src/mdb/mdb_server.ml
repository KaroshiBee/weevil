open Protocol
module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Dap = Dapper.Dap
module MichEvent = Mdb_event

(* NOTE type unparsing_mode = Optimized | Readable | Optimized_legacy, could we phantom this into the logger type? *)
module Interpreter_cfg = struct
  type input = string
  let to_string_input i = i
  let from_string_input s = Environment.Error_monad.ok s

  type output = string
  let to_string_output o = o
  let from_string_output s = Environment.Error_monad.ok s

  let unparsing_mode = Script_ir_translator.Readable
end

module Interpreter = Mdb_traced_interpreter.T (Interpreter_cfg)
module Stepper = Mdb_stepper2.T (Interpreter)

let protocol_str = "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg"
let base_dir = "/tmp/.weevil"

let rec main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process ic oc =
  let open Lwt_syntax in
  let* ln = Lwt_io.read_line_opt ic in
  match ln, stepper_process with
  | Some _msg, None -> (
      (*TODO  _msg will contain the script name *)
      let fname = "/home/wyn/mich/multiply_2_x_250_equals_500.tz" in
      let* () = Logs_lwt.info (fun m -> m "[MICH] spawning '%s', joining with main_handler" fname) in
      let nts = Lwt_preemptive.nbthreads () in
      let ntsbusy = Lwt_preemptive.nbthreadsbusy () in
      let ntsq = Lwt_preemptive.nbthreadsqueued () in
      let* () = Logs_lwt.info (fun m -> m "[MICH] 1 preemptive info: nbthreads %d, busy %d, queued %d" nts ntsbusy ntsq) in
      let p = fname |> Lwt_preemptive.detach (fun filename ->
          Logs.info (fun m -> m "[MICH] preemptive: starting stepper");
          let nts = Lwt_preemptive.nbthreads () in
          let ntsbusy = Lwt_preemptive.nbthreadsbusy () in
          let ntsq = Lwt_preemptive.nbthreadsqueued () in
          let () = Logs.info (fun m -> m "[MICH] 2 preemptive info: nbthreads %d, busy %d, queued %d" nts ntsbusy ntsq) in
          match Stepper.step ~make_logger stepper filename with
          | Ok _ -> ()
          | Error errs ->
            Logs.info (fun m -> m "[MICH] preemptive: got errors '%s'" @@ Data_encoding.Json.(construct trace_encoding errs |> to_string))
        ) in
      let nts = Lwt_preemptive.nbthreads () in
      let ntsbusy = Lwt_preemptive.nbthreadsbusy () in
      let ntsq = Lwt_preemptive.nbthreadsqueued () in
      let* () = Logs_lwt.info (fun m -> m "[MICH] 3 preemptive info: nbthreads %d, busy %d, queued %d" nts ntsbusy ntsq) in
      let h = main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process:(Some p) ic oc
      in
      let* _ = both p h in return_unit
    )

  | Some msg, Some p -> (
    let* () = Logs_lwt.info (fun m -> m "[MICH] process already spawned") in
    let* () = Logs_lwt.info (fun m -> m "[MICH] process state %s" @@ match Lwt.state p with
      | Return _x -> "finished"
      | Sleep -> "sleep"
      | Fail _exn -> "failed"
      ) in

    match Lwt.state p with
    | Sleep ->
      let* _ = Lwt_mvar.put input_mvar msg
      and* _ = main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process ic oc
      in
      return_unit
    | Return _ | Fail _ ->
      (* finished subprocess, reset to None *)
      let* _ = return_unit
      and* _ = main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process:None ic oc
      in
      return_unit
  )

  | None, _ ->
    let* _ = return_unit
    and* _ = Logs_lwt.info (fun m -> m "[MICH] connection closed")
    in
    return_unit

let on_exn exn =
  Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let on_connection ~stepper _flow ic oc =
  let%lwt () = Logs_lwt.info (fun m -> m "[MICH] got connection") in
  let input_mvar = Lwt_mvar.create_empty () in
  let output_mvar = Lwt_mvar.create_empty () in
  let make_logger = Interpreter.trace_logger ~in_channel:stdin ~out_channel:stdout in
  main_handler ~stepper ~make_logger ~input_mvar ~output_mvar ~stepper_process:None ic oc

let lwt_svc ?stopper port =
  let open Lwt_result_syntax in
  let mode = `TCP (`Port port) in
  let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d" port) in
  (* we run the stepper in a preemptive thread
     so that can pause it without pausing the main thread,
     NOTE currently only allowing one interp process at a time *)
  (* let () = Lwt_preemptive.simple_init () in *)
  (* let () = Lwt_preemptive.set_bounds (1,1) in *)
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
