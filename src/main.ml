let commands = [
  Backend.Stepper_cmdline.cmd;
  Backend.Service_cmdline.cmd;
  Adapter.Service_cmdline.cmd;
  Dapper.Dap_cmdline.cmd;
]

(* let term = *)
(*   let open Cmdliner.Term in *)
(*   ret (const (`Help (`Pager, None))) *)

let info =
  let version = "1.0" in (* Tezos_version.Bin_version.version_string in *)
  Cmdliner.Cmd.info
    ~doc:"The Tezos Weevil tool"
    ~version
    "tezos-weevil"

let main_cmd =
  Cmdliner.Cmd.group info commands

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  exit (Cmdliner.Cmd.eval main_cmd)

