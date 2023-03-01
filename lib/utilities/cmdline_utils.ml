(* a generic setup for logging *)
let setup_log =
  let init style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  Cmdliner.Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())


let info ~doc name =
  let version = Defaults._VERSION in
  Cmdliner.Cmd.info
    ~doc
    ~version
    name
