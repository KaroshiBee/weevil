open Dapper.Dap
open Conduit_lwt_unix
open Lwt
open Data_encoding

module StateMock = struct
  type t = {
    mutable launch_mode : Launch_mode.t option;
    mutable seqr: Data.Seqr.t;
    mutable config : Config.t;
    mutable client_config : Data.InitializeRequestArguments.t option;
  }

  let make () = {
    launch_mode = None;
    seqr = Data.Seqr.make ~seq:0 ();
    config=Config.make ();
    client_config=Option.some @@ Data.InitializeRequestArguments.make ~adapterID:"MOCK" ();
  }

  let set_connect_backend _ip _port = failwith "MOCK connect"

  let backend_svc _t = failwith "MOCK process none"

  let set_start_backend _t _ip _port _cmd = failwith "MOCK start backend"

  let backend_ic _t = failwith "MOCK ic"

  let backend_oc _t = Some Lwt_io.stdout

  let launch_mode t = t.launch_mode

  let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode

  let current_seqr t = t.seqr

  let set_seqr t seqr = t.seqr <- seqr

  let config t = t.config

  let set_config t config = t.config <- config

  let client_config t = t.client_config

  let set_client_config t config = t.client_config <- Some config

end


let ip = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr

let c9000 = `TCP (`IP ip, `Port 9000)

let c9001 = `TCP (`IP ip, `Port 9001)

let make_connection c =
  let x = init () >>= fun ctx -> connect ~ctx c in
  x >|= fun (_, ic, oc) -> (ic, oc)

let launch_msg ~seq =
  let arguments = Data.LaunchRequestArguments.make () in
  Request.Message.make ~seq ~command:Commands.launch ~arguments ()

let launch_req ~seq = Request.launchRequest @@ launch_msg ~seq

let attach_msg ~seq =
  let arguments = Data.AttachRequestArguments.make () in
  Request.Message.make ~seq ~command:Commands.attach ~arguments ()

let attach_req ~seq = Request.attachRequest @@ attach_msg ~seq

let initialize_msg ~seq =
  let arguments = Data.InitializeRequestArguments.make ~adapterID:"weevil" ~clientID:"12345" () in
  Request.Message.make ~seq ~command:Commands.initialize ~arguments ()

let initialize_req ~seq = Request.initializeRequest @@ initialize_msg ~seq

let configurationDone_msg ~seq =
  let arguments = Data.ConfigurationDoneArguments.make () in
  Request.Message.make_opt ~seq ~command:Commands.configurationDone ~arguments ()

let configurationDone_req ~seq = Request.configurationDoneRequest @@ configurationDone_msg ~seq

let to_msg (type cmd args presence) :
    (cmd, args, presence) Request.Message.t Request.t -> string = function
  | InitializeRequest req ->
      let enc =
        Request.Message.enc
          Commands.initialize
          Data.InitializeRequestArguments.enc
      in
      Json.(construct enc req |> to_string) |> Header.wrap
  | ConfigurationDoneRequest req ->
      let enc =
        Request.Message.enc_opt
          Commands.configurationDone
          Data.ConfigurationDoneArguments.enc
      in
      Json.(construct enc req |> to_string) |> Header.wrap
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