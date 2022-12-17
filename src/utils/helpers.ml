open Dapper.Dap
open Conduit_lwt_unix
open Lwt
open Data_encoding
module Model = Mdb.Mdb_model
module PP = Tezos_micheline.Micheline_parser

let deduplicate_stable xs =
  (* de-dups the string list but keeps it in order *)
  let module SSet = Set.Make (String) in
  let (_, ys) =
    let init = (SSet.empty, []) in
    List.fold_left
      (fun (sset, out) -> function
         | x when SSet.mem x sset -> (sset, out)
         | _ as x ->
           let sset = SSet.add x sset in
           let out = x :: out in
           (sset, out))
      init
      xs
  in
  List.rev ys


module StateMock = struct
  type t = {
    mutable ic : Lwt_io.input_channel option;
    mutable oc : Lwt_io.output_channel option;
    mutable launch_mode : Launch_mode.t option;
    mutable seqr: Data.Seqr.t;
    mutable config : Config.t;
    mutable client_config : Data.InitializeRequestArguments.t option;
    mutable mdb_config : Mdb.Mdb_types.Mich_config.t option;
    mutable log_records : Model.Weevil_json.t list;
    mutable should_restart_on_terminate : bool option;

  }

  let make () = {
    ic=None;
    oc=None;
    launch_mode = None;
    seqr = Data.Seqr.make ~seq:0 ();
    config=Config.make ();
    client_config=Option.some @@ Data.InitializeRequestArguments.make ~adapterID:"MOCK" ();
    mdb_config=None;
    log_records=Model.Weevil_json.([
        {location=PP.{start={point=1;byte=1;line=1;column=0}; stop={point=1;byte=1;line=1;column=3}}; gas="10"; stack=["1";"2";"3"]};
        {location=PP.{start={point=2;byte=2;line=2;column=0}; stop={point=2;byte=2;line=2;column=3}}; gas="9"; stack=["1";"2";"3";"4"]};
        {location=PP.{start={point=3;byte=3;line=3;column=0}; stop={point=3;byte=3;line=3;column=3}}; gas="8"; stack=["1";"2";"7"]};
        ]);
    should_restart_on_terminate = None;
  }

  let reset_backend t =
    t.ic <- None;
    t.oc <- None

  let set_connect_backend _ip _port = failwith "MOCK connect"

  let backend_svc _t = failwith "MOCK process none"

  let set_start_backend _t _ip _port _cmd = failwith "MOCK start backend"

  let backend_ic _t = failwith "MOCK ic"

  let backend_oc _t = Some Lwt_io.stdout

  let set_new_log_records t recs =
    t.log_records <- (recs @ t.log_records)

  let log_records t = t.log_records

  let should_restart_on_terminate t = t.should_restart_on_terminate

  let launch_mode t = t.launch_mode

  let set_launch_mode t launch_mode = t.launch_mode <- Some launch_mode

  let current_seqr t = t.seqr

  let set_seqr t seqr = t.seqr <- seqr

  let config t = t.config

  let set_config t config = t.config <- config

  let client_config t = t.client_config

  let set_client_config t config = t.client_config <- Some config

  let mdb_config t = t.mdb_config

  let set_mdb_config t config = t.mdb_config <- Some config

  let set_should_restart_on_terminate t restart = t.should_restart_on_terminate <- restart

end


let ip = Unix.inet_addr_loopback |> Ipaddr_unix.of_inet_addr

let c9000 = `TCP (`IP ip, `Port 9000)

let c9001 = `TCP (`IP ip, `Port 9001)

let make_connection c =
  let x = init () >>= fun ctx -> connect ~ctx c in
  x >|= fun (_, ic, oc) -> (ic, oc)

let initialize_msg ~seq =
  let arguments = Data.InitializeRequestArguments.make ~adapterID:"weevil" ~clientID:"12345" () in
  Request.Message.make ~seq ~command:Commands.initialize ~arguments ()

let initialize_req ~seq = Request.initializeRequest @@ initialize_msg ~seq

let launch_msg ~seq ~script_filename ~storage ~parameter =
  let arguments = Data.LaunchRequestArguments.make ~script_filename ~storage ~parameter () in
  Request.Message.make ~seq ~command:Commands.launch ~arguments ()

let launch_req ~seq ~script_filename ~storage ~parameter = Request.launchRequest @@ launch_msg ~seq ~script_filename ~storage ~parameter

let attach_msg ~seq ~script_filename ~storage ~parameter =
  let arguments = Data.AttachRequestArguments.make ~script_filename ~storage ~parameter () in
  Request.Message.make ~seq ~command:Commands.attach ~arguments ()

let attach_req ~seq ~script_filename ~storage ~parameter = Request.attachRequest @@ attach_msg ~seq ~script_filename ~storage ~parameter

let configurationDone_msg ~seq =
  let arguments = Data.ConfigurationDoneArguments.make () in
  Request.Message.make_opt ~seq ~command:Commands.configurationDone ~arguments ()

let configurationDone_req ~seq = Request.configurationDoneRequest @@ configurationDone_msg ~seq

let threads_msg ~seq =
  let arguments = Data.EmptyObject.make () in
  Request.Message.make_opt ~seq ~command:Commands.threads ~arguments ()

let threads_req ~seq = Request.threadsRequest @@ threads_msg ~seq

let stack_trace_msg ~seq =
  let arguments = Data.StackTraceArguments.make ~threadId:1 () in
  Request.Message.make ~seq ~command:Commands.stackTrace ~arguments ()

let stack_trace_req ~seq = Request.stackTraceRequest @@ stack_trace_msg ~seq

let scopes_msg ~seq =
  let arguments = Data.ScopesArguments.make ~frameId:1 () in
  Request.Message.make ~seq ~command:Commands.scopes ~arguments ()

let scopes_req ~seq = Request.scopesRequest @@ scopes_msg ~seq

let variables_msg ~seq =
  let arguments = Data.VariablesArguments.make ~variablesReference:0 () in
  Request.Message.make ~seq ~command:Commands.variables ~arguments ()

let variables_req ~seq = Request.variablesRequest @@ variables_msg ~seq

let next_msg ~seq =
  let arguments = Data.NextArguments.make ~threadId:1 () in
  Request.Message.make ~seq ~command:Commands.next ~arguments ()

let next_req ~seq = Request.nextRequest @@ next_msg ~seq

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
  | ThreadsRequest req ->
      let enc =
        Request.Message.enc_opt Commands.threads Data.EmptyObject.enc
      in
      Json.(construct enc req |> to_string) |> Header.wrap
  | StackTraceRequest req ->
      let enc =
        Request.Message.enc Commands.stackTrace Data.StackTraceArguments.enc
      in
      Json.(construct enc req |> to_string) |> Header.wrap
  | ScopesRequest req ->
      let enc =
        Request.Message.enc Commands.scopes Data.ScopesArguments.enc
      in
      Json.(construct enc req |> to_string) |> Header.wrap
  | VariablesRequest req ->
      let enc =
        Request.Message.enc Commands.variables Data.VariablesArguments.enc
      in
      Json.(construct enc req |> to_string) |> Header.wrap
  | NextRequest req ->
      let enc =
        Request.Message.enc Commands.next Data.NextArguments.enc
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
