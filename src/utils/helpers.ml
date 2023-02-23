open Dapper.Dap
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
    mutable mdb_config : Mdb.Mdb_config.t option;
    mutable log_records : Model.t list;
    mutable should_restart_on_terminate : bool option;

  }

  let make () = {
    ic=None;
    oc=None;
    launch_mode = None;
    seqr = Data.Seqr.make ~seq:0 ();
    config=Config.make ();
    client_config=Option.some @@ Data.InitializeRequestArguments.make ~adapterID:"MOCK" ();
    mdb_config=Option.some @@ Mdb.Mdb_config.make
        ~script_filename:"/home/kbee/dev/weevil/example.tz"
        ~storage:"Unit"
        ~parameter:"Unit"
        ~entrypoint:"default"
        ();
    log_records=Model.([
        make ~location:PP.{start={point=1;byte=1;line=1;column=0}; stop={point=1;byte=1;line=1;column=3}} ~gas:"10" ~stack:["1";"2";"3"] ();
        make ~location:PP.{start={point=2;byte=2;line=2;column=0}; stop={point=2;byte=2;line=2;column=3}} ~gas:"9"  ~stack:["1";"2";"3";"4"] ();
        make ~location:PP.{start={point=3;byte=3;line=3;column=0}; stop={point=3;byte=3;line=3;column=3}} ~gas:"8"  ~stack:["1";"2";"7"] ();
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
