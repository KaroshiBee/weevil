open Weevil

let commands = [
  Stepper_command.cmd;
  Service_command.cmd;
]

let default =
  let open Cmdliner.Term in
  ret (const (`Help (`Pager, None)))

let info =
  let version = Tezos_version.Bin_version.version_string in
  Cmdliner.Cmd.info
    "tezos-weevil"
    ~doc:"The Tezos Weevil tool"
    ~version

let main_cmd =
  Cmdliner.Cmd.group ~default info commands

let () =
  exit (Cmdliner.Cmd.eval main_cmd)
