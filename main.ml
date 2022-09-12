let commands = [
  (* Stepper_command.cmd; *)
  (* Service_command.cmd; *)
]

let term =
  let open Cmdliner.Term in
  ret (const (`Help (`Pager, None)))

let info =
  let version = "1.0" in (* Tezos_version.Bin_version.version_string in *)
  Cmdliner.Cmd.info
    ~doc:"The Tezos Weevil tool"
    ~version
    "tezos-weevil"

let main_cmd =
  Cmdliner.Cmd.group info commands

let () =
  exit (Cmdliner.Cmd.eval main_cmd)


let () =
  print_endline "hello world";
  let _f = Data_encoding.Json.to_string in
  ()
