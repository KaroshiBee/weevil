open Lwt
open Mdb.Mdb_server
module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json

module Msgs : sig
  type t
  val [@warning "-32"] to_string : t -> string
  val runscript : string -> string
  val [@warning "-32"] terminate : string
  val [@warning "-32"] step1 : string
  val run : string -> unit

end
= struct

  type t = MichEvent.t

  let to_string t =
    Js.(construct MichEvent.enc t |> to_string)
    |> Str.global_replace (Str.regexp "\n") ""

  let runscript fname =
    let event = MichEvent.(make ~event:(RunScript fname) ()) in
    to_string event

  let terminate =
    let event = MichEvent.(make ~event:Terminate ()) in
    to_string event

  let step1 =
    let event = MichEvent.(make ~event:(Step 1) ()) in
    to_string event

  let run fname =
    let cmd = Printf.sprintf "dune exec -- weevil stepper %s" fname in
    let ip = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr in
    let port = 9001 in
    let c = `TCP (`IP ip, `Port port) in
    Lwt_main.run (
      Conduit.init () >>= fun ctx ->
      Conduit.connect ~ctx c >>= fun (_, _ic, oc) ->
      Lwt_io.write_line oc @@ runscript cmd >>= fun _ ->
      Lwt_io.write_line oc @@ step1 >>= fun _ ->
      Lwt_io.write_line oc @@ step1 >>= fun _ ->
      Lwt_io.write_line oc @@ step1 >>= fun _ ->
      (* NOTE sleeps to make deterministic *)
      Lwt_unix.sleep 1.0 >>= fun _ ->
      Lwt_io.write_line oc @@ terminate  >>= fun _ ->
      Lwt_unix.sleep 1.0
    )
end

let process fname_arg =
  let p
      ?fname_arg:(fname:string="")
      () =
    `Ok (Msgs.run fname)
  in
  p ?fname_arg ()

module Term = struct

  let filename_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The Michelson filename to test with.  \
        If not given defaults to ''"

    in
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:"FILENAME"
    )

  let term =
    Cmdliner.Term.(
      ret
        (const process $ filename_arg)
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil Test Runner locally"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "backend"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
