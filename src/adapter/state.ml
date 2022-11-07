open Conduit_lwt_unix
module Launch_mode = Dapper.Dap.Data.Launch_mode

type io = Lwt_io.input_channel * Lwt_io.output_channel
type t = {
  (* the backend svc process, using process_none to allow for std redirection if needed later on *)
  mutable process: Lwt_process.process_none option;
  (* the backend comms channels *)
  mutable io: io option;
  mutable launch_mode: Launch_mode.t option;
}

let make_empty = {
  process=None; io=None; launch_mode=None;
}

let connect ip port =
  let client = `TCP (`IP ip, `Port port) in
  let%lwt ctx = init () in
  let%lwt (_, ic, oc) = connect ~ctx client in
  Lwt.return (ic, oc)

let process_none t = t.process
let set_process_none t process = t.process <- Some process

let ic t = t.io |> Option.map fst
let oc t = t.io |> Option.map snd
let set_io t ic oc = t.io <- Some (ic, oc)

let launch_mode t = t.launch_mode
let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode
