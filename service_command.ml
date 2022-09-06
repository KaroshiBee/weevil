let make_address addr_str = match Defaults._DEFAULT_LISTEN_ADDRESS = addr_str with
  | true ->
    Unix.inet_addr_loopback
  | false ->
    Unix.inet_addr_of_string addr_str

let process listen_address_arg port_arg =
  let p
      ?listen_address_arg:(listen_address:string=Defaults._DEFAULT_LISTEN_ADDRESS)
      ?port_arg:(port:int=Defaults._DEFAULT_PORT)
      () =
    let listen_address = make_address listen_address in
    Server.svc ~listen_address ~port
  in
  p ?listen_address_arg ?port_arg ()

module Term = struct

  let listen_address_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The address that the weevil svc will use for IO.  \
        If not given defaults to <%s>" Defaults._DEFAULT_LISTEN_ADDRESS

    in
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:"ADDRESS"
    )

  let listen_port_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The port that the weevil svc will use for IO.  \
        If not given defaults to %d" Defaults._DEFAULT_PORT

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
    "Run the Weevil service for the debugger"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "svc"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
