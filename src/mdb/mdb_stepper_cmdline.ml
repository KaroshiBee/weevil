module Interpreter = Mdb_traced_interpreter.T (Mdb_traced_interpreter_cfg)
module Stepper = Mdb_stepper.T (Interpreter)

let file_arg = "FILE"

let protocol_str = "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg"
let base_dir = "/tmp/.weevil"

let logger = Interpreter.trace_logger ~in_channel:stdin ~out_channel:stdout ()

(* NOTE unit on end is for the logging setup *)
let process headless script_filename_opt () =
  let open Lwt_result_syntax in

  (* let rec get_logging_records : unit -> _ Lwt.t = fun () -> *)
  (*   let pause = 0.1 in *)

  (*   let*! () = Logs_lwt.info (fun m -> m "get logging records") in *)
  (*   let*! recs = Stepper.get_execution_trace_updates ~logger in *)
  (*   let*! _ = match recs with *)
  (*     | Ok [] -> return () *)
  (*     | Ok recs -> *)
  (*       let*! ret = *)
  (*         List.iter_s (fun (loc, gas, exprs) -> *)
  (*             (\* TODO better handling of mich expressions/tickets *\) *)
  (*             let wrec = Mdb_model.Weevil_record.make loc gas (exprs |> List.map (fun e -> (e, None, false))) in *)
  (*             let wrec_js = Mdb_model.Weevil_record.to_weevil_json wrec in *)
  (*             let js = Data_encoding.Json.( *)
  (*                 construct Mdb_model.Weevil_json.enc wrec_js *)
  (*                 |> to_string *)
  (*                 |> Dapper.Dap.Header.wrap *)
  (*               ) in *)
  (*             Lwt_io.(write stdout js) *)
  (*           ) recs *)
  (*       in *)
  (*       return ret *)

  (*     | Error _ as e -> *)
  (*       let enc = result_encoding Data_encoding.unit in *)
  (*       let err_msg = Data_encoding.Json.(construct enc e |> to_string) |> Dapper.Dap.Header.wrap in *)
  (*       let*! ret = Lwt_io.(write stderr err_msg) in *)
  (*       return ret *)
  (*   in *)

  (*   let*! () = Logs_lwt.info (fun m -> m "get logging records sleeping") in *)
  (*   let*! () = Lwt_unix.sleep pause in *)
  (*   let*! () = Logs_lwt.info (fun m -> m "get logging records recurse") in *)
  (*   get_logging_records () *)
  (* in *)

  let stepper : unit -> Stepper.t tzresult Lwt.t = fun () ->
    match script_filename_opt with
    | Some script_filename ->
      let* stepper = Stepper.init ~protocol_str ~base_dir () in
      let* script = Stepper.typecheck ~script_filename stepper in
      Stepper.step ~logger ~script stepper
    | None ->
      let s = Printf.sprintf "required argument %s is missing" file_arg in
      Lwt.return @@ error_with_exn @@ Invalid_argument s
  in

  let post_process : Stepper.t tzresult Lwt.t -> unit Cmdliner.Term.ret Lwt.t = fun res ->
    let*! stepper_result = res in
    match stepper_result with
    | Ok _ -> (* TODO dont ignore OK output *) Lwt.return @@ `Ok ()
    | Error _ as e when headless ->
      let*! () =
        let enc = result_encoding Data_encoding.unit in
        let err_msg = Data_encoding.Json.(construct enc e |> to_string) |> Dapper.Dap.Header.wrap in
        Lwt_io.(write stderr err_msg)
      in
      (* if in headless mode then dont show --help if the cli has errors *)
      Lwt.return @@ `Error (false, "")
    | Error errs ->
      let ss =
        Format.asprintf "Stepper error - %a" pp_print_trace errs
      in
      (* if in headless mode then dont show --help if the cli has errors *)
      Lwt.return @@ `Error (not headless, ss)
  in

  let s () =
    let step = stepper () in post_process step
  in
  Lwt_main.run @@ s ()


module Tm = struct
  open Cmdliner
  let headless_arg =
    let doc =
      "Run the tool in headless mode, \
       this means that --help is not shown on error and \
       any errors are returned as JSON"
    in
    Arg.(
      value & flag & info ["h"; "headless"] ~doc
    )

  let contract_file_arg =
    let doc =
      "The Michelson contract filename that the weevil stepper will execute (required)."
    in
    (* NOTE we use value here rather than required because
       we want headless mode to not show --help when this arg is not given *)
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:file_arg
    )

  let term (setup_log:unit Term.t) =
    Term.(
      let t = (const process $ headless_arg $ contract_file_arg $ setup_log) in
      ret t
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil Michelson stepper for the backend service"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "stepper"
end

let cmd setup_log = Cmdliner.Cmd.v Manpage.info @@ Tm.term setup_log
