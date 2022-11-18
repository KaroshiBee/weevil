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

let process contract_file_arg =
  (* reads the contract text and raises exceptions if file not there or not given *)
  let read_contract_text () =
    let lns =
      match contract_file_arg with
      | None -> raise @@ Invalid_argument "expected contract filename"
      | Some contract_file -> read_file_exn contract_file ()
    in
    String.concat " " lns
  in

  (* special logger that halts at each michelson logger call-back *)
  let logger = Stepper.Traced_interpreter.trace_logger stdout () in

  (* step through the contract text and halt until newline read from stdin *)
  let stepper =

    let module Tz = Tezos_base.TzPervasives in
    let open Tz.Error_monad.Legacy_monad_globals in

    (* convert exceptions into Tz.Error_monad traces *)
    let contract_txt =
      Lwt.return @@
      try
        Ok (read_contract_text ())
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
      | e -> Lwt.return @@ Error [Tz.error_of_exn e]
    in

    (* convert Tz.Error_monad result to cmdline output *)
    match%lwt res with
    | Ok () -> Lwt.return @@ `Ok ()
    | Error errs ->
      let ss = Format.asprintf "Stepper error - %a" Tz.Error_monad.pp_print_trace errs in
      Lwt.return @@ `Error (false, ss)
  in

  Lwt_main.run stepper


module Term = struct
  let contract_file_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The Michelson contract filename that the weevil stepper will execute (required)."

    in
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:"FILE"
    )

  let term =
    Cmdliner.Term.(
      ret
        (const process $ contract_file_arg)
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil Michelson stepper for the backend service"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "stepper"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
