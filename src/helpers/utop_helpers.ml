module JS = Data_encoding.Json
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
open Dapper.Dap_message


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
