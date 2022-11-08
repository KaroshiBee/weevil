open Dapper.Dap_message
open Conduit_lwt_unix
module JS = Data_encoding.Json
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
open Lwt
module M = Backend.Server.MichEvent

let ip = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr
let c9000 = `TCP (`IP ip, `Port 9000)
let c9001 = `TCP (`IP ip, `Port 9001)
let make_connection c =
  let x = init () >>= (fun ctx -> connect ~ctx c) in
  x >|= (fun (_, ic, oc) -> (ic, oc))

let s = M.make ~event:(M.Step 1) () |> JS.construct M.enc |> JS.to_string

let launch_req =
  let arguments = LaunchRequestArguments.make () in
  let req = RequestMessage.make ~seq:0 ~command:Dap_commands.launch ~arguments () in
  LaunchRequest req

let attach_req =
  let arguments = AttachRequestArguments.make () in
  let req = RequestMessage.make ~seq:0 ~command:Dap_commands.attach ~arguments () in
  AttachRequest req

let to_msg (type cmd args presence) : (cmd, args, presence) request -> string = function
  | LaunchRequest req ->
    let enc = RequestMessage.enc Dap_commands.launch LaunchRequestArguments.enc in
    JS.(construct enc req |> to_string) |> Dapper.Dap_header.wrap
  | AttachRequest req ->
    let enc = RequestMessage.enc Dap_commands.attach AttachRequestArguments.enc in
    JS.(construct enc req |> to_string) |> Dapper.Dap_header.wrap
  | _ -> assert false
