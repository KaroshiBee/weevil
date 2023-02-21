(* a generic setup for logging *)
let setup_log =
  let init style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  Cmdliner.Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())

(* group together all cmd lines *)
let commands = [
  (* Mdb.Mdb_stepper_cmdline.cmd setup_log; *)
  Mdb.Mdb_cmdline.cmd setup_log;
  Adapter.Service_cmdline.cmd setup_log;
  Dapper.Dap_cmdline.cmd setup_log;
]

let info =
  let version = "0.1" in (* Tezos_version.Bin_version.version_string in *)
  Cmdliner.Cmd.info
    ~doc:"The Tezos Weevil tool"
    ~version
    "weevil"

let main_cmd =
  Cmdliner.Cmd.group info commands

let () =
  exit (Cmdliner.Cmd.eval main_cmd)
