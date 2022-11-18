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

let process headless contract_file =
  (* special logger that halts at each michelson logger call-back *)
  let logger = Stepper.Traced_interpreter.trace_logger stdout () in

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
        Stepper.test_stepping contract_text logger
      with
      | Stepper.StepperExpr.Expression_from_string_with_locs errs -> Lwt.return @@ Error errs
      | e -> Lwt.return @@ Error [Tz.error_of_exn e]
    in

    (* convert Tz.Error_monad result to cmdline output, if headless then convert errors to json *)
    match%lwt res with
    | Ok _ -> (* TODO dont ignore OK output *) Lwt.return @@ `Ok ()
    | Error errs as e ->
      let ss =
        if headless then
          let enc = Tz.Error_monad.result_encoding Tz.Data_encoding.unit in
          Tz.Data_encoding.Json.(construct enc e |> to_string) |> Dapper.Dap.Header.wrap
        else
          Format.asprintf "Stepper error - %a" Tz.Error_monad.pp_print_trace errs
      in
      (* if in headless mode then dont show --help if the cli has errors *)
      Lwt.return @@ `Error (not headless, ss)
  in

  Lwt_main.run stepper


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

  let term =
    Term.(
      ret
        (const process $ headless_arg $ contract_file_arg)
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil Michelson stepper for the backend service"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "stepper"
end

let cmd = Cmdliner.Cmd.v Manpage.info Tm.term
