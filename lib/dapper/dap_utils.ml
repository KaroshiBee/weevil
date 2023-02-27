open Conduit_lwt_unix
open Lwt
module Data = Dap_messages.Data

module StateMock = struct
  type t = {
    mutable seqr: Data.Seqr.t;
    mutable config : Dap_config.t;
  }

  let make () = {
    seqr = Data.Seqr.make ~seq:0 ();
    config=Dap_config.make ();
  }

  let current_seqr t = t.seqr

  let set_seqr t seqr = t.seqr <- seqr

  let config t = t.config

  let set_config t config = t.config <- config

end


let ip = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr

let c9000 = `TCP (`IP ip, `Port 9000)

let c9001 = `TCP (`IP ip, `Port 9001)

let make_connection c =
  let x = init () >>= fun ctx -> connect ~ctx c in
  x >|= fun (_, ic, oc) -> (ic, oc)


(* loop a fixed number of times [n] with a sleep, to make sure to connect when up *)
let loop_connect ~ctx ~client ~port n =
  let rec _aux i =
    let%lwt () =
      Logs_lwt.debug (fun m ->
          m "[%d] trying to connect on locahost port: %d" i port)
    in
    let%lwt () = Lwt_unix.sleep @@ float_of_int i in
    try%lwt
      let%lwt cn = connect ~ctx client in
      let%lwt () =
        Logs_lwt.debug (fun m -> m "connected on locahost port: %d" port)
      in
      Lwt.return cn
    with Unix.Unix_error (Unix.ECONNREFUSED, "connect", "") as e ->
      if i > n then raise e else _aux (i + 1)
  in
  _aux 1
