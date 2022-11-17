let rec lines_from_in_channel =
  let read_line_ i = try (i |> Option.map input_line) with End_of_file -> None
  in
  fun i acc ->
    match (read_line_ i) with
    | None -> List.rev acc
    | Some s -> lines_from_in_channel i (s :: acc)

let read_file filename () =
  let i : in_channel option ref = ref None in
  try
    let () = i := Option.some @@ open_in filename in
    let lns = lines_from_in_channel !i [] in
    Option.(!i |> map close_in |> value ~default:());
    lns
  with e ->
    Logs.err (fun m -> m "%s" @@ Printexc.to_string e);
    Option.(!i |> map close_in_noerr |> value ~default:());
    []

let process contract_file_arg =
  let contract_text =
    let lns =
      match contract_file_arg with
      | None -> raise @@ Invalid_argument "expected contract filename"
      | Some contract_file -> read_file contract_file ()
    in
    String.concat " " lns
  in

  let logger = Stepper.Traced_interpreter.trace_logger stdout () in

  let stepper =
    Logs.debug (fun m -> m "got contract data: '%s'" contract_text);
    let open Tezos_base.TzPervasives.Error_monad.Legacy_monad_globals in
    Stepper.test_stepping contract_text logger >|= (fun _ -> `Ok ()) in

  Lwt_main.run stepper


module Term = struct
  let contract_file_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The contract filename that the weevil stepper will execute.  \
        If not given then expects the contract code text to be passed from \
         the DAP service after initialisation"

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
