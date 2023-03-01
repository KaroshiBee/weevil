
let file_arg = "FILE"
let mich_arg = "MICHELSON"
let name_arg = "NAME"

let base_dir = "/tmp/.weevil"

(* NOTE unit on end is for the logging cmdline setup *)
let process headless script_filename_opt storage_opt input_opt entrypoint_opt () =
  let open Lwt_result_syntax in

  let storage = Option.value storage_opt ~default:"Unit" in
  let input = Option.value input_opt ~default:"Unit" in
  let entrypoint = Option.value entrypoint_opt ~default:"default" in

  let stepper : unit -> Mdb_stepper.t tzresult Lwt.t = fun () ->
    match script_filename_opt with
    | Some script_filename ->
      let* stepper = Mdb_stepper.init ~base_dir () in
      let* well_typed =
        Mdb_stepper.typecheck ~script_filename ~storage ~input ~entrypoint stepper in
      let* interp = Mdb_stepper.make_interp well_typed in
      Mdb_stepper.step ~interp well_typed stepper
    | None ->
      let s = Printf.sprintf "required argument %s is missing" file_arg in
      Lwt.return @@ error_with_exn @@ Invalid_argument s
  in

  let post_process : Mdb_stepper.t tzresult Lwt.t -> unit Cmdliner.Term.ret Lwt.t = fun res ->
    let*! stepper_result = res in
    match stepper_result with
    | Ok _ -> (* TODO dont ignore OK output *)
      Lwt.return @@ `Ok ()
    | Error _ as e when headless ->
      let*! () =
        let enc = result_encoding Data_encoding.unit in
        let err_msg = Data_encoding.Json.(construct enc e |> to_string) |> Dapper.Dap.Header.wrap in
        Lwt_io.(write stderr @@ err_msg ^ "\n") in
      let*! () = Lwt_io.(flush stderr)
      in
      (* if in headless mode then dont show --help if the cli has errors *)
      Lwt.return @@ `Error (false, "")
    | Error errs ->
      let ss =
        Format.asprintf "Stepper error - %a\n" pp_print_trace errs
      in
      (* not in headless mode so show help when errored *)
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
      "The Michelson contract filename that the weevil stepper will incrementally execute (required)."
    in
    (* NOTE we use value here rather than required because
       we want headless mode to not show --help when this arg is not given *)
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:file_arg
    )

  let storage_arg =
    let doc =
      "The initial Michelson storage that the weevil stepper will execute with (default 'Unit')."
    in
    Arg.(
      value & opt (some string) None & info ["s"; "storage"] ~doc ~docv:mich_arg
    )

  let input_arg =
    let doc =
      "The Michelson input parameter that the weevil stepper will execute with (default 'Unit')."
    in
    Arg.(
      value & opt (some string) None & info ["p"; "parameter"] ~doc ~docv:mich_arg
    )

  let entrypoint_arg =
    let doc =
      "The Michelson entrypoint function name that the weevil stepper will execute with (default 'default')."
    in
    Arg.(
      value & opt (some string) None & info ["e"; "entrypoint"] ~doc ~docv:name_arg
    )

  let term (setup_log:unit Term.t) =
    Term.(
      let t = (const process
               $ headless_arg
               $ contract_file_arg
               $ storage_arg
               $ input_arg
               $ entrypoint_arg
               $ setup_log
              ) in
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
