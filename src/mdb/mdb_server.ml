module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Dap = Dapper.Dap
module MichEvent = Mdb_event

open Lwt

let rec lines_from_in_channel =
  let read_line_ i = try (i |> Option.map input_line) with End_of_file -> None
  in
  fun i acc ->
    match (read_line_ i) with
    | None -> List.rev acc
    | Some s -> lines_from_in_channel i (s :: acc)

let read_file_exn filename () =
  let i : in_channel option ref = ref None in
  try
    let () = i := Option.some @@ open_in filename in
    let lns = lines_from_in_channel !i [] in
    Option.(!i |> map close_in |> value ~default:());
    lns
  with e ->
    Logs.err (fun m -> m "%s" @@ Printexc.to_string e);
    Option.(!i |> map close_in_noerr |> value ~default:());
    raise e

let file_arg = "FILE"

let process ~msg_mvar headless contract_file =
  (* special logger that halts at each michelson logger call-back *)
  let logger = Mdb_stepper.Traced_interpreter.trace_logger ~msg_mvar stdout () in

  (* incremental step through the contract text and halt until newline read from stdin *)
  let stepper =
    let module Tz = Tezos_base.TzPervasives in
    let open Tz.Error_monad.Legacy_monad_globals in

    (* convert exceptions into Tz.Error_monad traces *)
    let contract_txt =
      Lwt.return @@
      try
        Ok (
          (* reads the contract text and raises exceptions if file not exist or not given *)
          match contract_file with
          | Some fname ->
            let lns = read_file_exn fname () in
            String.concat " " lns
          | None -> raise @@ Invalid_argument (Printf.sprintf "required argument %s is missing" file_arg)
        )
      with
      | e -> Error [Tz.error_of_exn e]
    in

    (* step but catch any unhandled exceptions and convert to Tz.Error_monad traces *)
    let res =
      try%lwt
        contract_txt >>=? fun contract_text ->
        Logs.debug (fun m -> m "got contract data: '%s'" contract_text);
        Mdb_stepper.test_stepping contract_text logger
      with
      | Mdb_stepper.StepperExpr.Expression_from_string_with_locs errs -> Lwt.return @@ Error errs
      | e -> Lwt.return @@ Error [Tz.error_of_exn e]
    in

    (* convert Tz.Error_monad result to cmdline output, if headless then convert errors to json *)
    match%lwt res with
    | Ok _ -> (* TODO dont ignore OK output *) Lwt.return_unit
    | Error errs as e ->
      let ss =
        Format.asprintf "Stepper error - %a" Tz.Error_monad.pp_print_trace errs
      in
      let () =
        if headless then
          let enc = Tz.Error_monad.result_encoding Tz.Data_encoding.unit in
          let err_msg = Tz.Data_encoding.Json.(construct enc e |> to_string) |> Dapper.Dap.Header.wrap in
          Printf.fprintf stderr "%s" err_msg;
        else ()
      in
      (* if in headless mode then dont show --help if the cli has errors *)
      raise @@ Sys_error ss
  in
  Lwt_preemptive.run_in_main (fun () -> stepper)


let rec main_handler ~msg_mvar ~stepper_process ic oc =
  let%lwt ln = Lwt_io.read_line_opt ic in
  match ln, stepper_process with
  | Some _msg, None -> (
      let fname = "/home/wyn/dev/weevil/src/backend/tests/backend_cram_tests/stepper_test.t/multiply_2_x_250_equals_500.tz" in
      let p = Lwt_preemptive.detach (fun (headless, filename) -> process ~msg_mvar headless (Some filename)) (true, fname) in
      let%lwt () = Logs_lwt.info (fun m -> m "[MICH] spawned '%s'" fname) in
      Lwt.join [p; main_handler ~msg_mvar ~stepper_process:(Some p) ic oc]
    )

  | Some _msg, Some p -> (
    let%lwt () = Logs_lwt.info (fun m -> m "[MICH] process already spawned") in
    let%lwt () = Logs_lwt.info (fun m -> m "[MICH] process state %s" @@ match Lwt.state p with | Return _x -> "finished" | Sleep -> "sleep" | Fail _exn -> "failed") in
    match Lwt.state p with
    | Sleep ->
      let p = Lwt_mvar.put msg_mvar _msg in
      Lwt.join [p; main_handler ~msg_mvar ~stepper_process ic oc]
    | Return _ | Fail _ ->
      main_handler ~msg_mvar ~stepper_process:None ic oc
  )

  | None, _ -> Logs_lwt.info (fun m -> m "[MICH] connection closed")

let on_exn exn =
  Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let on_connection _flow ic oc =
  let%lwt () = Logs_lwt.info (fun m -> m "[MICH] got connection") in
  let msg_mvar = Lwt_mvar.create_empty () in
  main_handler ~msg_mvar ~stepper_process:None ic oc

let lwt_svc ?stopper port =
  let mode = `TCP (`Port port) in
  let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d" port) in
  let () = Lwt_preemptive.simple_init () in
  let () = Lwt_preemptive.set_bounds (1,1) in
  let ret =
    Conduit.init () >>= fun ctx -> (
      match stopper with
      | Some stop -> Conduit.serve ~stop ~on_exn ~ctx ~mode on_connection
      | None -> Conduit.serve ~on_exn ~ctx ~mode on_connection
    )
    >|= fun _ ->
    `Ok ()
  in
  ret

let svc ~port =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  Lwt_main.run (lwt_svc port)
