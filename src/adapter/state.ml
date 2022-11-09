open Conduit_lwt_unix
module Launch_mode = Dapper.Dap.Data.Launch_mode

type t = {
  (* the backend svc process, using process_none to allow for std redirection if needed later on *)
  mutable process: Lwt_process.process_none option;
  (* the backend comms channels *)
  mutable ic: Lwt_io.input_channel option;
  mutable oc: Lwt_io.output_channel option;
  mutable launch_mode: Launch_mode.t option;
}

let make_empty = {
  process=None; ic=None; oc=None; launch_mode=None;
}

let process_none t = t.process
let set_process_none t process = t.process <- Some process

let ic t = t.ic
let oc t = t.oc
let set_io t ?ic ?oc () =
  t.ic <- ic;
  t.oc <- oc

let connect t ip port =
  let client = `TCP (`IP ip, `Port port) in
  let%lwt ctx = init () in
  let%lwt (_, ic, oc) =
    (* loop a fixed number of times with a sleep, to make sure to connect when up *)
    let rec aux i =
      let%lwt () = Logs_lwt.debug (fun m -> m "[%d] trying to connect on locahost port: %d" i port) in
      let%lwt () = Lwt_unix.sleep @@ float_of_int i in
      try%lwt
        connect ~ctx client
      with
      | Unix.Unix_error(Unix.ECONNREFUSED, "connect", "") as e -> if i > 5 then raise e else aux (i+1)
    in
    aux 1
  in
  let%lwt () = Logs_lwt.debug (fun m -> m "connected on locahost port: %d" port) in
  Lwt.return @@ set_io t ~ic ~oc ()

let launch_mode t = t.launch_mode
let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode
