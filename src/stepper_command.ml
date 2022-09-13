let rec lines_from_in_channel =
  let read_line_ i = try Some (input_line i) with End_of_file -> None
  in
  fun i acc ->
    match (read_line_ i) with
    | None -> List.rev acc
    | Some s -> lines_from_in_channel i (s :: acc)

let read_file filename () =
  let i = open_in filename in
  try
    let lns = lines_from_in_channel i [] in
    close_in i;
    lns
  with _ ->
    close_in_noerr i;
    []

let process contract_file_arg =
  let contract_text =
    let lns =
      match contract_file_arg with
      | None -> let s = read_line () in [s]
      | Some contract_file -> read_file contract_file ()
    in
    String.concat " " lns
  in

  let logger = Stepper.Traced_interpreter.trace_logger stdout () in

  let stepper =
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
    "Run the Weevil stepper for the debugger (used by the Weevil service)"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "stepper"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
