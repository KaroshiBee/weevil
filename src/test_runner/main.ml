let commands = [
  Backend_runner_cmdline.cmd;
]

let info =
  let version = "1.0" in (* Tezos_version.Bin_version.version_string in *)
  Cmdliner.Cmd.info
    ~doc:"The Tezos Weevil test runner tool"
    ~version
    "tezos-weevil-test-runner"

let main_cmd =
  Cmdliner.Cmd.group info commands

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  exit (Cmdliner.Cmd.eval main_cmd)
