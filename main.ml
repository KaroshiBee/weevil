let commands = [
  Stepper_command.cmd;
  Service_command.cmd;
]

let term =
  let open Cmdliner.Term in
  ret (const (`Help (`Pager, None)))

let info =
  let version = Tezos_version.Bin_version.version_string in
  Cmdliner.Term.info
    ~doc:"The Tezos Weevil tool"
    ~version
    "tezos-weevil"


let () =
  match Cmdliner.Term.eval_choice (term, info) commands with
  | `Error _ -> exit 1
  | `Help -> exit 0
  | `Version -> exit 0
  | `Ok () -> exit 0
