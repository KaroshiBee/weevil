module Defaults = Dapper.Dap.Defaults
let default_port = Defaults._DEFAULT_ADAPTER_PORT

(* NOTE type unparsing_mode = Optimized | Readable | Optimized_legacy, could we phantom this into the logger type? *)
let process port_arg () =
  let p
      ?port_arg:(port:int=default_port)
      () =
    Server.svc ~port
  in
  p ?port_arg ()

module Term = struct

  let listen_port_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The port that the weevil DAP svc will use for IO.  \
        If not given defaults to %d" default_port

    in
    Arg.(
      value & pos 0 (some int) None & info [] ~doc ~docv:"PORT"
    )

  let term setup_log =
    Cmdliner.Term.(
      ret
        (const process $ listen_port_arg $ setup_log)
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil DAP service locally"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "adapter"
end

let cmd setup_log = Cmdliner.Cmd.v Manpage.info @@ Term.term setup_log
