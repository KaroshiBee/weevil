open Conduit_lwt_unix
open Lwt

(* TODO do better *)
let addr = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr
let client = `TCP (`IP addr, `Port Defaults._DEFAULT_PORT)



let () =
  let ui =
    init () >>= fun ctx ->
    connect ~ctx client >>= fun (_flow, ic, oc) ->
    Nottui_lwt.run (View.ui_main ic oc)
  in
  Lwt_main.run ui
