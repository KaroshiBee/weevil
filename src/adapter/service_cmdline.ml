module Defaults = Defaults.Vals
let default_listen_address = Defaults._DEFAULT_LISTEN_ADDRESS
let default_port = Defaults._DEFAULT_ADAPTER_PORT

let process listen_address_arg port_arg =
  let p
      ?listen_address_arg:(listen_address:string=default_listen_address)
      ?port_arg:(port:int=default_port)
      () =
    Server.svc ~listen_address ~port
  in
  p ?listen_address_arg ?port_arg ()

module Term = struct

  let listen_address_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The address that the weevil DAP svc will use for IO.  \
        If not given defaults to <%s>" default_listen_address

    in
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:"ADDRESS"
    )

  let listen_port_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The port that the weevil DAP svc will use for IO.  \
        If not given defaults to %d" default_port

    in
    Arg.(
      value & pos 1 (some int) None & info [] ~doc ~docv:"PORT"
    )

  let term =
    Cmdliner.Term.(
      ret
        (const process $ listen_address_arg $ listen_port_arg)
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil DAP service"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "adapter"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
