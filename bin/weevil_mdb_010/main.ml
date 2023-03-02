module U = Utilities.Cmdline_utils

(* group together all cmd lines *)
let commands = [
  Mdb.Mdb_stepper_cmdline.cmd U.setup_log;
]

let info = U.info ~doc:"The Tezos Weevil mdb v010 tool" "weevil_mdb_010"

let main_cmd =
  Cmdliner.Cmd.group info commands

let () =
  exit (Cmdliner.Cmd.eval main_cmd)
