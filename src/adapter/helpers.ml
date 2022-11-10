open Dapper.Dap
open Conduit_lwt_unix
open Lwt
open Data_encoding

let ip = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr

let c9000 = `TCP (`IP ip, `Port 9000)

let c9001 = `TCP (`IP ip, `Port 9001)

let make_connection c =
  let x = init () >>= fun ctx -> connect ~ctx c in
  x >|= fun (_, ic, oc) -> (ic, oc)

let launch_req ~seq =
  let arguments = Data.LaunchRequestArguments.make () in
  let req = Request.Message.make ~seq ~command:Commands.launch ~arguments () in
  Request.launchRequest req

let attach_req ~seq =
  let arguments = Data.AttachRequestArguments.make () in
  let req = Request.Message.make ~seq ~command:Commands.attach ~arguments () in
  Request.attachRequest req

let to_msg (type cmd args presence) :
    (cmd, args, presence) Request.Message.t Request.t -> string = function
  | LaunchRequest req ->
      let enc =
        Request.Message.enc Commands.launch Data.LaunchRequestArguments.enc
      in
      Json.(construct enc req |> to_string) |> Header.wrap
  | AttachRequest req ->
      let enc =
        Request.Message.enc Commands.attach Data.AttachRequestArguments.enc
      in
      Json.(construct enc req |> to_string) |> Header.wrap
  | _ -> assert false

module M = Backend.Server.MichEvent

let step = M.make ~event:(M.Step 1) () |> Json.construct M.enc |> Json.to_string
