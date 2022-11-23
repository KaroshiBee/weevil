module Defaults = Defaults.Vals
let default_port = Defaults._DEFAULT_BACKEND_PORT

let process port_arg =
  let p
      ?port_arg:(port:int=default_port)
      () =
    Server2.svc ~port
  in
  p ?port_arg ()

module Term = struct

  let listen_port_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The port that the debugger svc will use for IO.  \
        If not given defaults to %d" default_port

    in
    Arg.(
      value & pos 0 (some int) None & info [] ~doc ~docv:"PORT"
    )

  let term =
    Cmdliner.Term.(
      ret
        (const process $ listen_port_arg)
    )

end

module Manpage = struct
  let command_description =
    "Run the debugger service locally for the Weevil DAP service"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "backend"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
