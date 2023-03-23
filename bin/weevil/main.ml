module U = Utilities.Cmdline_utils

(* group together all cmd lines *)
let commands = [
  Mdb.Mdb_cmdline.cmd U.setup_log; (* backend subcmd *)
  Adapter.Service_cmdline.cmd U.setup_log; (* adapter subcmd *)
  Dapper.Dap_cmdline.cmd U.setup_log; (* dap-gen subcmd *)
]

let info = U.info ~doc:"The Tezos Weevil tool" "weevil"

let main_cmd =
  Cmdliner.Cmd.group info commands

let () =
  exit (Cmdliner.Cmd.eval main_cmd)
